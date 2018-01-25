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

data Selector = Simple SimpleSelector | Other

data SimpleSelector = IdSelector String | ClassSelector String | AttribSelector String | PseudoSelector String | Negation SimpleSelector

-- Lexer rules
comment :: Parser ()
comment = void (string "/*" *> manyTill anyChar (string "*/"))

whiteSpace :: Parser ()
whiteSpace = void $ many (comment <|> (skip isSpace >> skipWhile isSpace))

identToken :: Parser String
identToken = do
  hyphen <- option "" (char '-' >> return "-")
  lead <- satisfy (/= '-')
  restT <- takeWhile (\x -> isPrint x || x == '_')
  let rest = unpack restT
  return $ hyphen ++ lead : rest

quotedString :: Parser String
quotedString = do
  _ <- char '"'
  content <- many character
  _ <- char '"'
  return $ concat content
  where
    escape = do
      d <- char '\\'
      c <- satisfy $ inClass "\\\"0nrvtbf" -- this may perform poorly
      return [d, c]
    nonEscape = (: []) <$> satisfy (notInClass "\\\"\0\n\r\v\t\b\f")
    character = nonEscape <|> escape

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

simpleSelector :: Parser Selector
simpleSelector = elSelector <|> (Simple <$> bareSelector)
  where
    elSelector = do
      el <- identToken

      return Other
    bareSelector =
          IdSelector <$> (char '#' *> elementIdentifier)
      <|> ClassSelector <$> (char '.' *> identToken)
      <|> AttribSelector <$> (char '[' *> attribContents <* char ']' )
      <|> PseudoSelector <$> do
        c1 <- unpack <$> string ":"
        c2 <- option "" (unpack <$> string ":")
        sel <- identToken
        return $ c1 ++ c2 ++ sel

    attribContents = unpack <$> takeWhile (/= ']')

elementIdentifier = unpack <$> string "identifier" -- TODO
