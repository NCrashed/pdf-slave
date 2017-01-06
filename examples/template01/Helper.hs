module Helper where

import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import System.IO.Unsafe

defaultInput :: FromJSON a => a
{-# NOINLINE defaultInput #-}
defaultInput = case unsafePerformIO $ fmap eitherDecode' $ BS.readFile "input.json" of
  Left e -> error (show e)
  Right a -> a