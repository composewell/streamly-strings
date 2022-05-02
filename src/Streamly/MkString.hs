{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | String interpolation.
--
-- Template Haskell code. This code needs to be in a separate
-- module because of GHC stage restriction.
--
-- XXX Rename the module to StringInterpolation. Also, move this to an
-- appropriate package.
--
-- Some of the code in this module is inspired by neat-interpolation by
-- nikita-volkov.

module Streamly.MkString
    ( line
    , multi
    , cmdline
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Applicative (Alternative(..))
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Data.Char (isAlphaNum)
import Streamly.Internal.Data.Parser (Parser)
import Streamly.QuotedString (shellWords)
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- streamly-0.8.0 does not expose char parser
-- We need to get all the utils working with streamly-0.8.1 for migration
charP :: MonadCatch m => Char -> Parser m Char Char
charP c = Parser.satisfy (== c)

-- streamly-0.8.0 does not expose alphaNum parser
-- We need to get all the utils working with streamly-0.8.1 for migration
alphaNumP :: MonadCatch m => Parser m Char Char
alphaNumP = Parser.satisfy isAlphaNum

type Line = [LineContent]

data LineContent
    = LineContentText String
    | LineContentIdentifier String
    deriving (Show, Eq)

lineParser :: MonadCatch m => Parser m Char Line
lineParser = Parser.many content Fold.toList

    where

    -- Reference to a haskell symbol, starts with a $ symbol followed by the
    -- Haskell identifier.
    identifierSimple =
        Parser.some (alphaNumP <|> charP '\'' <|> charP '_') Fold.toList
    identifierInBraces = charP '{' *> identifierSimple <* charP '}'
    identifier =
        fmap LineContentIdentifier
            $ charP '$' *> (identifierInBraces <|> identifierSimple)

    -- A "$$" is parsed as a single "$"
    escapedDollar = fmap (LineContentText . (: [])) $ charP '$' *> charP '$'

    -- Anything other than an identifier or a $$ is regular text
    --
    -- "Parser.count" is undefined. The current implementation eats the
    -- malformed '$' instead of erroring out. This should be fixed if 'count' is
    -- used.
    -- escapedDollar =
    --     fmap LineContentText
    --         $ charP '$' *> Parser.count 1 (charP '$') Fold.toList
    anySingle = Parser.satisfy (const True)
    end =
        void (Parser.lookAhead escapedDollar)
            <|> void (Parser.lookAhead identifier)
            <|> Parser.eof
    contentText = LineContentText <$> Parser.manyTill anySingle end Fold.toList

    content = escapedDollar <|> identifier <|> contentText

contentExp :: LineContent -> Q Exp
contentExp (LineContentText text) = stringE text
contentExp (LineContentIdentifier name) = do
    valueName <- lookupValueName name
    case valueName of
        Just vn -> varE vn
        Nothing ->
            fail
                $ "Interpolated string: Haskell symbol `" ++ name
                ++ "` is not in scope"

lineExp :: Line -> Q Exp
lineExp xs = appE [| concat |] $ listE $ map contentExp xs

shellExp :: Q Exp -> Q Exp
shellExp e = do
    appE [| unwords |]
        $ appE [| unsafePerformIO . Stream.parse shellWords |]
        $ appE [| Stream.fromList |] e

smartStringE :: String -> Q Exp
smartStringE ln =
    case Stream.parse lineParser (Stream.fromList ln) of
        Left _ -> fail "Parsing of interpolated string failed."
        Right xs ->
            -- We need to remove the ' ' at the end that we add for the hack.
            let x = last xs
            in lineExp
                $ case x of
                    LineContentText t | last t == ' ' ->
                        init xs <> [LineContentText (init t)]
                    _ -> xs

--------------------------------------------------------------------------------
-- Single line string
--------------------------------------------------------------------------------

-- | Replace a value with another in a list.
replace :: Eq a => a -> a -> [a] -> [a]
replace i j = map replaceF

    where

    replaceF x
        | x == i = j
        | otherwise = x

-- | Strip contiguous space and newline characters at the beginning and at the
-- end of the string.
--
-- Add a space at the end to workaround the parser alternative instance bug
stripAndPad :: String -> String
stripAndPad =
    reverse
        . (' ' :)
        . dropWhile isSpaceOrNewLine . reverse . dropWhile isSpaceOrNewLine

    where

    -- XXX Should we use isSpace instead?
    isSpaceOrNewLine x = x == ' ' || x == '\n'

-- We can merge line and multi by making parsing smarter
--
-- | A QuasiQuoter that treats the input as a single line string (replaces
-- newlines in the input by spaces), and replaces any @$symbol@ by the value of
-- the Haskell symbol @symbol@ which is in scope. Any leading or trailing
-- spaces are removed.
line :: QuasiQuoter
line =
    QuasiQuoter
        { quoteExp = smartStringE . replace '\n' ' ' . stripAndPad
        , quotePat = notSupported
        , quoteType = notSupported
        , quoteDec = notSupported
        }

    where

    notSupported = error "line: Not supported."

--------------------------------------------------------------------------------
-- Multi line string
--------------------------------------------------------------------------------

-- XXX Rename to "para"?

-- | Like 'line' but treats the input as a multiline string any newline
-- characters in the input are retained as is.
multi :: QuasiQuoter
multi =
    QuasiQuoter
        { quoteExp = smartStringE . (++ " ")
        , quotePat = notSupported
        , quoteType = notSupported
        , quoteDec = notSupported
        }

    where

    notSupported = error "multi: Not supported."

--------------------------------------------------------------------------------
-- Shell command
--------------------------------------------------------------------------------

-- | cmdline quasiquoter parses the string into words as shell would and then
-- joins the words together so that any insignificant space is removed.
--
-- Quotes may interact in interesting ways with string interpolation
-- leading to surprising behavior because it may not be what you would expect
-- in shell.  In shell:
--
-- * each quote level would remove a "\".
-- * A single quote is not special inside a double quote and vice-versa
-- * Shell variables are expanded after quote processing
--
-- For example,
--
-- @
-- > x = "a\"quoted\"b"
-- > run [line|echo "$x"|]
-- aquotedb
-- @
--
-- This is because the command being executed by the shell is @echo
-- "a"quoted"b"@.  If @x@ were a shell variable then @echo "$x"@ in a shell
-- would give the output @a"quoted"b@ instead.
--
-- You need to have the Haskell string appropriately quoted/escaped for shell
-- usage. An alternative work around for the above case is to pass the string
-- as an environment variable and then use shell variable expansion. For
-- exmaple,
--
-- @
-- > setEnv "var" x
-- > run [line|echo "$$var"|]
-- a"quoted"b
--
-- TODO: "cmdline" quasiquoter can potentially take care of such cases by
-- escaping the quotes before expansion if the expansion occurs inside
-- quotes.
cmdline :: QuasiQuoter
cmdline =
    QuasiQuoter
        { quoteExp = shellExp . smartStringE . replace '\n' ' ' . stripAndPad
        , quotePat = notSupported
        , quoteType = notSupported
        , quoteDec = notSupported
        }

    where

    notSupported = error "line: Not supported."
