module Data.Git.Types where

import Data.ByteString (ByteString)
import System.Posix.Types (FileMode)

type Size = Int
data GitObject = GoBlob   Size Blob
               | GoTree   Size [GitTreeEntry]
               | GoCommit Size GitCommit
               | GoTag    Size GitTag
               deriving (Eq,Show)

data GitType = GtBlob | GtTree | GtCommit | GtTag deriving (Eq,Show)

type Blob = ByteString

data FileType = RegularFile FileMode
              | Directory
              | SymbolicLink
              | GitLink
              deriving (Eq,Show)

data GitTreeEntry = GitTreeEntry FileType FilePath SHA1 deriving (Eq,Show)

data GitCommit = GitCommit SHA1 [SHA1] Author Committer LogMsg deriving (Eq,Show)
type Author = ByteString
type Committer = ByteString
type LogMsg = ByteString

data GitTag = GitTag SHA1 TagType TagName Tagger TagMsg deriving (Eq,Show)
type TagType = ByteString
type TagName = ByteString
type Tagger = ByteString
type TagMsg = ByteString

newtype SHA1 = SHA1 String deriving Eq

instance Show SHA1 where
    show (SHA1 x) = x
