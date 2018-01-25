{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CssParser where

import           Control.Monad
import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Text            hiding (concat, count, map, takeWhile, toLower, toUpper)
import           Prelude              hiding (takeWhile)
import           Numeric              (showHex)

data CssRule
  = QualifiedRule
  | AtRule
  deriving (Show)

newtype Stylesheet =
  Stylesheet [CssRule]
  deriving (Show)

data Selector = Simple SimpleSelectorSequence | Other deriving (Show)

type El = String

data SimpleSelectorSequence = SimpleSelectorSequence El [SimpleSelector] deriving (Show)
data SimpleSelector = IdSelector String | ClassSelector String | AttribSelector String | PseudoSelector String deriving (Show)

-- Lexer rules
comment :: Parser ()
comment = void (string "/*" *> manyTill anyChar (string "*/"))

whiteSpace :: Parser ()
whiteSpace = void $ many (comment <|> (skip isSpace >> skipWhile isSpace))

identToken :: Parser String
identToken = do
  -- hyphen <- option "" (char '-' >> return "-")
  -- lead <- satisfy (/= '-')
  -- restT <- takeWhile (\x -> isPrint x || x == '_')
  -- let rest = unpack restT
  -- return $ hyphen ++ lead : rest
  hyphen <- option "" (char '-' >> return "-")
  start <- nmStart
  rest <- join <$> many nmChar
  return $ hyphen ++ start ++ rest

nmStart :: Parser String
nmStart = ((: []) <$> satisfy (inClass "_a-zA-Z")) <|> nonascii <|> escape
nmChar :: Parser String
nmChar = ((: []) <$> satisfy (inClass "_a-zA-Z0-9\\-")) <|> nonascii <|> escape
nonascii :: Parser String
nonascii = (: []) <$> satisfy (not . isAscii)
escape :: Parser String
escape = charEscape
  where
-- hexEscape = undefined
    charEscape = do
      slash <- char '\\'
      c <- satisfy $ notInClass "\r\n\f0-9a-fA-F"
      return [slash, c]


quotedString :: Parser String
quotedString = do
  _ <- char '"'
  content <- many character
  _ <- char '"'
  return $ concat content
  where
    stringEscape = do
      d <- char '\\'
      c <- satisfy $ inClass "\\\"0nrvtbf" -- this may perform poorly
      return [d, c]
    nonEscape = (: []) <$> satisfy (notInClass "\\\"\0\n\r\v\t\b\f")
    character = nonEscape <|> stringEscape

-- stylesheet :: Parser Stylesheet
-- stylesheet = many stylesheetElement >>= \es -> return $ Stylesheet es
--   where
--     stylesheetElement =
--       Trace.trace "stylesheetElement" $ do
--         _ <- whiteSpace
--         e <- atRule <|> qualifiedRule
--         _ <- whiteSpace
--         return e

newlineOrSpace :: Parser ()
newlineOrSpace = option () $ cr <|> ws
  where
    cr = void (string "\r\n")
    ws = void (satisfy (inClass " \t\r\n\f"))


mediaKeyword :: Parser ()
mediaKeyword = atKeyword "media"

importKeyword :: Parser ()
importKeyword = atKeyword "import"

atKeyword :: String -> Parser ()
atKeyword kw = char '@' *> mapM_ keywordChar kw

keywordChar :: Char -> Parser ()
keywordChar c =
  simpleChar <|> escapedChar <|> if not (validHex c) then hexChar else fail "keywordChar"
  where
    simpleChar = void (char $ toLower c) <|> void (char $ toUpper c)
    escapedChar = do
      _ <- char '\\'
      _ <- zeros
      _ <- string (pack (toHex $ toLower c)) <|> string (pack (toHex $ toUpper c))
      newlineOrSpace
      return ()
    hexChar = do
      _ <- char '\\'
      _ <- simpleChar
      return ()
    validHex ch =
      let
        k = toLower ch
      in
        k >= 'a' && k <= 'f'

    zeros = count 4 (option () $ void (char '0'))
    toHex ch = showHex (ord ch) ""


-- Selectors
-- selector :: Parser Selector
-- selector = simpleSelectorSequence `sepBy1` combinator
--   where
--     combinator = (char '+' <|> char '>' <|> char '~'  <* whiteSpace

selector :: Parser Selector
selector = Simple <$> simpleSelectorSequence

simpleSelectorSequence :: Parser SimpleSelectorSequence
simpleSelectorSequence = bareSelector <|> quantifiedSelector
  where
    bareSelector = do
      s <- many1 simpleSelector
      return $ SimpleSelectorSequence "*" s

    quantifiedSelector = do
      el <- typeSelector <|> universal
      s <- many simpleSelector
      return $ SimpleSelectorSequence el s

    typeSelector = do
      prefix <- option "" typeSelectorPrefix
      elName <- identToken
      return $ prefix ++ elName

    universal = do
      prefix <- option "" typeSelectorPrefix
      _ <- char '*'
      return $ prefix ++ "*"

    typeSelectorPrefix = do
      p <- identToken <|> (unpack <$> string "*")
      _ <- char '|'
      return $ p ++ "|"



simpleSelector :: Parser SimpleSelector
simpleSelector =
        IdSelector <$> (char '#' *> elementIdentifier)
    <|> ClassSelector <$> (char '.' *> identToken)
    <|> AttribSelector <$> (char '[' *> attribContents <* char ']' )
    <|> PseudoSelector <$> do
      c1 <- unpack <$> string ":"
      c2 <- option "" (unpack <$> string ":")
      sel <- identToken
      return $ c1 ++ c2 ++ sel

    where
      attribContents = unpack <$> takeWhile (/= ']')

elementIdentifier :: Parser String
elementIdentifier = join <$> many1 nmChar
