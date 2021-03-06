{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Translate where

import Control.Applicative -- Compatiblity with GHC 7.6.3
import Control.Monad       -- Compatiblity with GHC 7.6.3
import Data.Monoid         -- Compatiblity with GHC 7.6.3

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bits
import Data.Word
import Types

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T

immediate (Num x) = pure x
immediate (Lbl s) = ask >>= foo . H.lookup s
  where
    foo (Just x) = pure x
    foo Nothing  = throwError $ "undefined label: " `mappend` s

toByteCode = 


translate Not     = pure $ 0o000000
translate Tdf     = pure $ 0o000001
translate Out     = pure $ 0o000002
translate In      = pure $ 0o000003
translate (Add r) = pure $ 0o000100 .|. fromIntegral (unReg r)
translate (Jz  i) = (.|.) 0o030000 <$> immediate i
translate (Imm i) = (.|.) 0o001000 <$> immediate i
