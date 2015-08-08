{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Char
import Data.Maybe
import Text.Parsec
import Data.Functor
import Data.Monoid
import Data.Word
import Data.Bits
import Options
import qualified Data.ByteString as B
import Control.Applicative

data Reg = Reg Int deriving Show

data Instr = Not
           | Tdf
           | Out
           | In
           | Add Reg
           | Sft Int Reg
           | Jz  Int
           | Imm Int
           deriving Show

data MainOptions = MainOptions
    { output :: String
    , tree   :: Bool }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "output" "a.o"
            "Output file name"
        <*> simpleOption "tree" False
            "Print Parse Tree"

blank = satisfy pred <?> "blank"
    where
        pred '\n' = False
        pred x    = isSpace x

blanks  = skipMany  blank
blanks1 = skipMany1 blank

keyword  s = try (string s) *> blanks
keyword1 s = try (string s) *> blanks1

comma = char ',' *> blanks

immediate = choice
    [ number  2 "0b" $ oneOf "01"
    , number  4 "0q" $ oneOf "0123"
    , number  8 "0o" $ octDigit
    , number 10 "0d" $ digit
    , number 16 "0x" $ hexDigit 
    , number 10 ""   $ digit]
        where
            number b s p  =  foldl1 (\x y -> b * x + y)
                <$  try    (string s)
                <*> many1  (digitToInt <$> p)
                <*  blanks
                <?> "immediate"

register = char 'r' *> many1 digit >>= final . read
    where
        final x | x >= 32   = unexpected "register"
                | otherwise = Reg x <$ blanks

instr = choice
    [ Not <$ keyword  "NOT"
    , Tdf  <$ keyword  "TDF"
    , Out <$ keyword  "OUT"
    , In  <$ keyword  "IN"
    , Add <$ keyword1 "ADD" <*> register
--    , Sft <$ keyword1 "SFT" <*> immediate <* comma <*> register
    , Imm <$ keyword1 "IMM" <*> immediate
    , Jz  <$ keyword1 "JZ"  <*> immediate ]
    <?> "instruction"

comment  =  char ';' 
         *> skipMany (notFollowedBy newline <* anyChar) 
        <?> "comment"

line = blanks *> optionMaybe instr <* Text.Parsec.optional comment

code = catMaybes <$> sepEndBy line newline <* eof

translate :: Instr -> Int
translate (Not        ) = 0o140000
translate (Tdf        ) = 0o140001
translate (Out        ) = 0o140002
translate (In         ) = 0o140003
translate (Add (Reg r)) = 0o100100 .|. fromIntegral r
translate (Jz  x      ) = 0o030000 .|. fromIntegral x
translate (Imm x      ) = 0o040000 .|. x

tow8 :: [Int] -> [Word8]
tow8 n = n >>= (\x -> map fromIntegral [x, shift x (-8)])

toByteCode = either (const B.empty) $ B.pack . tow8 . fmap translate

main = runCommand $ \opts args -> do
    source <- readFile (if null args then "a.asm" else head args)
    B.writeFile (output opts) $ toByteCode $ parse code "" source
    if tree opts then
        print $ parse code "" source
    else
        return ()
