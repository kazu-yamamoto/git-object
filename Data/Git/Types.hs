module Data.Git.Types where

import Data.ByteString (ByteString)
import Data.Char
import Numeric

type Size = Int
data GitObject = GoBlob   Size ByteString
               | GoTree   Size [GitTreeEntry]
               | GoCommit Size ByteString -- FIXME
               | GoTag    Size ByteString -- FIXME
               deriving (Eq,Show)

data GitType     = GtBlob  | GtTree  | GtCommit  | GtTag deriving (Eq,Show)
data GitTreeType = GttBlob | GttTree | GttCommit deriving (Eq,Show)

type Mode = Int
data GitTreeEntry = GitTreeEntry GitTreeType Mode FilePath SHA1
                    deriving (Eq,Show)

newtype SHA1 = SHA1 String deriving Eq

instance Show SHA1 where
    show (SHA1 x) = foldr (.) id (map (showHex . ord) x) $ ""

