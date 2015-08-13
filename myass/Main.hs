module Main where

{-
import Options
import qualified Data.ByteString as B

import Parser
import Types

data MainOptions = MainOptions
  { output :: String
  , tree   :: Bool }

instance Options MainOptions where
  defineOptions  =  MainOptions
                <$> simpleOption "output" "a.o" "Output file name"
                <*> simpleOption "tree" False "Print Parse Tree"

main = runCommand $ \opts args -> do
    source <- readFile (if null args then "a.asm" else head args)
    B.writeFile (output opts) $ toByteCode $ parse code "" source
    if tree opts then
        print $ parse code "" source
    else
        return ()
-}
