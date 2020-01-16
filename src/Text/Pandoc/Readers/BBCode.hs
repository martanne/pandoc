{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.BBCode
   Copyright   : Copyright (C) 2020 Marc André Tanner
   License     : GNU GPL, version 2 or above

   Maintainer  :
   Stability   : alpha
   Portability : portable

Conversion of BBCode text to a 'Pandoc' document.
-}
module Text.Pandoc.Readers.BBCode (readBBCode) where

import Prelude
import Control.Monad
import Control.Monad.Except (throwError)
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (nested)
import Text.Pandoc.Shared

-- | Read BBCode from an input string and return a Pandoc document.
readBBCode :: PandocMonad m
             => ReaderOptions
             -> Text
             -> m Pandoc
readBBCode opts s = do
  let input = crFilter s
  res <- runParserT parseBBCode def {stateOptions = opts } "source" input
  case res of
       Left e  -> throwError $ PandocParsecError input e
       Right d -> return d

type BBParser = ParserT Text ParserState

-- * Utility functions

-- | Parse end-of-line, which can be either a newline or end-of-file.
eol :: Stream s m Char => ParserT s st m ()
eol = void newline <|> eof

nested :: PandocMonad m => BBParser m a -> BBParser m a
nested p = do
  nestlevel <- stateMaxNestingLevel <$>  getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

---
inlines :: PandocMonad m => BBParser m Inlines
inlines = mconcat <$> many inline

rawTitleBlockLine :: PandocMonad m => BBParser m Text
rawTitleBlockLine = do
  char '%'
  skipSpaces
  first <- anyLine
  rest <- many $ try $ do spaceChar
                          notFollowedBy blankline
                          skipSpaces
                          anyLine
  return $ trim $ T.unlines (first:rest)

metaLine :: PandocMonad m => BBParser m B.Inlines
metaLine = try $ do
  raw <- rawTitleBlockLine
  res <- parseFromString' inlines raw
  return $ B.trimInlines res

titleBlock :: PandocMonad m => BBParser m ()
titleBlock = pandocTitleBlock

pandocTitleBlock :: PandocMonad m => BBParser m ()
pandocTitleBlock = try $ do
  guardEnabled Ext_pandoc_title_block
  lookAhead (char '%')
  title <- option mempty metaLine
  author <- option mempty metaLine
  date <- option mempty metaLine
  optional blanklines
  let meta = (if B.isNull title then id else B.setMeta "title" title)
           . (if B.isNull author then id else B.setMeta "author" author)
           . (if B.isNull date then id else B.setMeta "date" date)
           $ nullMeta
  updateState $ \st -> st { stateMeta = stateMeta st <> meta }

-- | Parse BBCode document.
parseBBCode :: PandocMonad m => BBParser m Pandoc
parseBBCode = do
  optional titleBlock
  blocks <- mconcat <$> many block <* spaces <* eof
  st <- getState
  let meta = stateMeta st
  return $ Pandoc meta (B.toList blocks)

-- * Inline parsers

-- | Parse any inline element but softbreak.
inline' :: PandocMonad m => BBParser m B.Inlines
inline' = whitespace
      <|> br
      <|> bold
      <|> italic
      <|> underlined
      <|> strikeout
      <|> link
      <|> linkText
      <|> image
      <|> imageText
      <|> inlineCode
      <|> str
      <|> symbol
      <?> "inline"

-- | Parse any inline element, including soft break.
inline :: PandocMonad m => BBParser m B.Inlines
inline = endline <|> inline'

endline :: PandocMonad m => BBParser m B.Inlines
endline = try $ B.softbreak <$ skipMany spaceChar <* linebreak

whitespace :: PandocMonad m => BBParser m B.Inlines
whitespace = try $ B.space <$ skipMany1 spaceChar

br :: PandocMonad m => BBParser m B.Inlines
br = try $ B.linebreak <$ string "[br]" <* skipMany spaceChar

linebreak :: PandocMonad m => BBParser m B.Inlines
linebreak = newline >> notFollowedBy newline >> (lastNewline <|> innerNewline)
  where lastNewline  = mempty <$ eof
        innerNewline = pure B.space

between :: (Monoid c, PandocMonad m, Show b)
        => BBParser m a -> BBParser m b -> (BBParser m b -> BBParser m c)
        -> BBParser m c
between start end p =
  mconcat <$> try (start >> notFollowedBy whitespace >> many1Till (p end) end)

nestedInlines :: (Show a, PandocMonad m)
              => BBParser m a -> BBParser m B.Inlines
nestedInlines end = innerSpace <|> nestedInline
  where
    innerSpace   = try $ whitespace <* notFollowedBy end
    nestedInline = notFollowedBy whitespace >> nested inline

bold :: PandocMonad m => BBParser m B.Inlines
bold = try $ B.strong <$> between (string "[b]") (try $ string "[/b]") nestedInlines

italic :: PandocMonad m => BBParser m B.Inlines
italic = try $ B.emph <$> between (string "[i]") (try $ string "[/i]") nestedInlines

underlined :: PandocMonad m => BBParser m B.Inlines
underlined = try $ B.emph <$> between (string "[u]") (try $ string "[/u]") nestedInlines

strikeout :: PandocMonad m => BBParser m B.Inlines
strikeout = try $ B.strikeout <$> between (string "[s]") (try $ string "[/s]") nestedInlines

inlineCode :: PandocMonad m => BBParser m B.Inlines
inlineCode = try $ B.code <$ string "[icode]" <*> manyTillChar anyChar (try $ string "[/icode]")

makeLink :: (Text, Text) -> B.Inlines
makeLink (text, url) = B.link url "" $ B.str text

str :: PandocMonad m => BBParser m B.Inlines
str = B.str <$> (many1Char alphaNum <|> countChar 1 characterReference)

symbol :: PandocMonad m => BBParser m B.Inlines
symbol = B.str <$> countChar 1 nonspaceChar

link :: PandocMonad m => BBParser m B.Inlines
link = try $ do
  string "[url]"
  url <- manyTillChar anyChar (try $ string "[/url]")
  return $ makeLink (url, url)

linkText :: PandocMonad m => BBParser m B.Inlines
linkText = try $ do
  string "[url="
  url <- manyTillChar anyChar (try $ string "]")
  text <- B.trimInlines . mconcat <$> manyTill inline (try $ string "[/url]")
  return $ B.link url "" text

image :: PandocMonad m => BBParser m B.Inlines
image = try $ do
  string "[img]"
  url <- manyTillChar anyChar (try $ string "[/img]")
  return $ B.image url "" (B.str "")

imageText :: PandocMonad m => BBParser m B.Inlines
imageText = try $ do
  string "[img="
  url <- manyTillChar anyChar (try $ string "]")
  text <- B.trimInlines . mconcat <$> manyTill inline (try $ string "[/img]")
  return $ B.link url "" text

-- * Block parsers

block :: PandocMonad m => BBParser m B.Blocks
block = do
  res <- mempty <$ skipMany1 blankline
         <|> blockElements
         <|> para
  skipMany blankline
  trace (T.take 60 $ tshow $ B.toList res)
  return res

blockElements :: PandocMonad m => BBParser m B.Blocks
blockElements = header 1
            <|> header 2
            <|> header 3
            <|> header 4
            <|> header 5
            <|> header 6
            <|> list
            <|> quote
            <|> blockCode

header :: PandocMonad m => Int -> BBParser m B.Blocks
header level = try $ do
  string ("[h" ++ show level ++ "]")
  content <- B.trimInlines . mconcat <$> manyTill inline (try $ string ("[/h" ++ show level ++ "]"))
  attr <- registerHeader nullAttr content
  return $ B.headerWith attr level content

list :: PandocMonad m => BBParser m B.Blocks
list = bulletList <|> orderedList

bulletList :: PandocMonad m => BBParser m B.Blocks
bulletList = try $ B.bulletList <$> parseList "[list]"

orderedList :: PandocMonad m => BBParser m B.Blocks
orderedList = try $ B.orderedList <$> parseList "[list=1]"

parseList :: PandocMonad m
          => String
          -> BBParser m [B.Blocks]
parseList heading = try $ do
  skipSpaces
  string heading
  endline
  body <- many1 ((<>) <$> item <*> fmap mconcat (many continuation))
  skipSpaces
  string "[/list]"
  --skipSpaces
  optional endline
  return $ body
  where
    continuation = try $ list
    item = try $ skipSpaces *> string "[*]" *> itemContents
    itemContents = B.plain . mconcat <$> many1Till inline' eol

quote :: PandocMonad m => BBParser m B.Blocks
quote = try $ B.codeBlock
  <$  string "[quote]"
  <*> manyTillChar anyChar (try $ string "[/quote]")

blockCode :: PandocMonad m => BBParser m B.Blocks
blockCode = try $ B.codeBlock
  <$  string "[code]"
  <*> manyTillChar anyChar (try $ string "[/code]")

para :: PandocMonad m => BBParser m B.Blocks
para = result . mconcat <$> many1Till inline endOfParaElement
 where
   endOfParaElement = lookAhead $ endOfInput <|> endOfPara <|> newBlockElement
   endOfInput       = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara        = try $ blankline >> skipMany1 blankline
   newBlockElement  = try $ void blockElements
   result content   = if F.all (==Space) content
                      then mempty
                      else B.para $ B.trimInlines content
