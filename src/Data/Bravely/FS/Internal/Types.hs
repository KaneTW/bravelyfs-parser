module Data.Bravely.FS.Internal.Types where
import qualified Data.ByteString.Lazy as BL
import Data.Word

data IndexEntry = IndexEntry {
    entryOffset :: Word32,
    entrySize :: Word32,
    entryChecksum :: Word32,
    entryFilename :: BL.ByteString
} deriving (Show, Eq)