module Main where

import Control.Applicative -- Compatiblity with GHC 7.6.3
import Options
import qualified Data.ByteString as B

import Parser
import Types
import Translate

data MainOptions = MainOptions
  { output :: String
  , tree   :: Bool }

instance Options MainOptions where
  defineOptions  =  MainOptions
                <$> simpleOption "output" "a.bin" "Output file name"
                <*> simpleOption "tree" False "Print Parse Tree"

main = runCommand $ \opts args -> do
    source <- readFile (if null args then "a.asm" else head args)
    B.writeFile (output opts) $ toByteCode $ parse code "" source
    if tree opts then
        print $ parse code "" source
    else
        return ()
