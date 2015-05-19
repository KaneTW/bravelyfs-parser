module Data.Bravely.FS.Internal.Types where
import qualified Data.ByteString.Lazy as BL
import Data.Word

data IndexEntry = IndexEntry {
    entryOffset :: Word32,
    entrySize :: Word32,
    entryChecksum :: Word32,
    entryFilename :: BL.ByteString
} deriving (Show, Eq)

data CrowdEntry = CrowdEntry {
    indexEntry :: IndexEntry,
    uncompressedSize :: Word32,
    compressedData :: BL.ByteString
} deriving (Show, Eq)

data ArchiveFile = ArchiveFile {
    archiveFilename :: BL.ByteString,
    archiveData :: BL.ByteString
} deriving (Show, Eq)