module Data.Bravely.FS.Parser(parseIndex, parseCrowd, parseFs, processFsDir) where
import Codec.Compression.Zlib.Raw
import Control.Monad
import Data.Binary.Get 
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Word8
import System.FilePath

import Data.Bravely.FS.Internal.Types

skip' :: Integral a => a -> Get ()
skip' = skip . fromIntegral

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
            skip' $ fromIntegral next - prev
            entries <- parseIndex
            return $ entry : entries

parseCrowd :: [IndexEntry] -> Get [CrowdEntry]
parseCrowd [] = return []
parseCrowd (e:es) = do
    bytesRead >>= (\prev -> skip' $ fromIntegral (entryOffset e) - prev)
    header <- getWord32le
    let uncSize = shiftR header 8
    let magic =  header .&. 0xff
    when (magic /= 0x60) $ error ("Unknown magic " ++ show magic ++ " for entry " ++ show e)
    -- entrySize stores complete record length, we only need remainder
    compressed <- getLazyByteString . fromIntegral $ (entrySize e - 4)
    rest <- parseCrowd es
    return $ CrowdEntry e uncSize compressed : rest


-- TODO: doesn't check checksm
parseFs :: [CrowdEntry] -> [ArchiveFile]
parseFs [] = []
parseFs (e:es) | uncompressedSize e == fromIntegral (BL.length uncomp) = ArchiveFile (entryFilename $ indexEntry e) uncomp : parseFs es
               | otherwise = error $ "incorrect uncompressed size for entry " ++ show (indexEntry e)
    where
        uncomp = decompress $ compressedData e

-- |Read the index.fs/crowd.fs from the given directory, process it and write decompressed files into the directory
processFsDir :: FilePath -> IO ()
processFsDir dir = do
    indexFile <- BL.readFile $ dir </> "index.fs"
    let is = runGet parseIndex indexFile
    crowdFile <- BL.readFile $ dir </> "crowd.fs"
    let cs = runGet (parseCrowd is) crowdFile
    let files = parseFs cs
    forM_ files (\e -> BL.writeFile (dir </> C.unpack (archiveFilename e)) $ archiveData e)