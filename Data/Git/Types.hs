module Data.Git.Types where

import Data.ByteString (ByteString)
import System.Posix.Types (FileMode)

----------------------------------------------------------------

data GitType = GtBlob | GtTree | GtCommit | GtTag deriving (Eq,Show)

----------------------------------------------------------------

type Size = Int
data GitObject = GoBlob   Size Blob
               | GoTree   Size [GitTreeEntry]
               | GoCommit Size GitCommit
               | GoTag    Size GitTag
               deriving (Eq,Show)

----------------------------------------------------------------

type Blob = ByteString

----------------------------------------------------------------

data GitTreeEntry = GitTreeEntry {
    fileType :: FileType
  , fileName :: FilePath
  , fileRef  :: SHA1
  } deriving (Eq,Show)

data FileType = RegularFile FileMode
              | Directory
              | SymbolicLink
              | GitLink
              deriving (Eq,Show)

----------------------------------------------------------------

data GitCommit = GitCommit {
    commitRef     :: SHA1
  , commitParents :: [SHA1]
  , commitAuthor  :: ByteString
  , committer     :: ByteString
  , commitLog     :: ByteString
  } deriving (Eq,Show)

----------------------------------------------------------------

data GitTag = GitTag {
    tagRef :: SHA1
  , tagType :: ByteString
  , tagName :: ByteString
  , tagger  :: ByteString
  , tagLog  :: ByteString
  } deriving (Eq,Show)

----------------------------------------------------------------

newtype SHA1 = SHA1 String deriving Eq

instance Show SHA1 where
    show (SHA1 x) = x
