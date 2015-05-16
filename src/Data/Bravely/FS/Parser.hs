module Data.Bravely.FS.Parser where

import qualified Data.ByteString.Lazy as BL
import Data.Word8
import Data.Bravely.FS.Internal.Types
import Data.Binary.Get 

parseEntry :: Get IndexEntry
parseEntry = do
            offset <- getWord32le
            size <- getWord32le
            checksum <- getWord32le
            filename <- getLazyByteStringNul
            return $ IndexEntry offset size checksum filename

parseIndex :: Get [IndexEntry]
parseIndex = do    
    next <- getWord32le
    entry <- parseEntry
    prev <- bytesRead
    if next == 0
        then return [entry]
        else do
            skip $ fromIntegral (fromIntegral next - prev)
            entries <- parseIndex
            return $ entry : entries
