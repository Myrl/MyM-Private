{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

import Control.Applicative hiding (many, (<|>), optional) -- Compatiblity with GHC 7.6.3

import Data.Char
import Data.Maybe
import Text.Parsec hiding (label)

import Types

blank = satisfy pred <?> "blank"
  where
    pred '\n' = False
    pred x    = isSpace x

blanks  = skipMany  blank
blanks1 = skipMany1 blank

ciChar c = char (toLower c) <|> char (toUpper c)

ciString s = try (sequence $ map ciChar s) <?> s

keyword  s = try $ ciString s *> blanks
keyword1 s = try $ ciString s *> blanks1

comma = char ',' *> blanks

number b s p  =  Num . foldl1 (\x y -> b * x + y)
             <$  try    (string s)
             <*> many1  (fromIntegral . digitToInt <$> p)
             <*  blanks
             <?> "numeric literal"

-- TODO: disallow using keywords as labels
label = (:) <$> letter <*> many alphaNum

immediate = choice
  [ number  2 "0b" $ oneOf "01"
  , number  4 "0q" $ oneOf "0123"
  , number  8 "0o" $ octDigit
  , number 10 "0d" $ digit
  , number 16 "0x" $ hexDigit 
  , number 10 ""   $ digit
  , Lbl <$> label ]

register = char 'r' *> many1 digit >>= final . read <?> "register"
  where
    final x | x < 32    = Reg x <$ blanks
            | otherwise = unexpected "register"

instr = choice
  [ Not <$ keyword  "NOT"
  , Tdf <$ keyword  "TDF"
  , Out <$ keyword  "OUT"
  , In  <$ keyword  "IN"
  , Add <$ keyword1 "ADD" <*> register
  , Sft <$ keyword1 "SFT" <*> immediate <* comma <*> register
  , Imm <$ keyword1 "IMM" <*> immediate
  , Jz  <$ keyword1 "JZ"  <*> immediate ]
  <?> "instruction"

tillEol = skipMany $ notFollowedBy newline <* anyChar

comment = char ';' *> tillEol <?> "comment"

line = blanks *> optionMaybe instr <* optional comment

code = catMaybes <$> sepEndBy line newline <* eof
