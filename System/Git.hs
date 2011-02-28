{-# LANGUAGE DeriveDataTypeable #-}

{-|
  Manipulating 'GitObject'.
-}

module System.Git (
    gitPathToGitObject
  , GitError(..)
  , GitDir, GitPath
  , findGitDir
  , rootSha1
  , rootCommitObj
  , gitPathToSha1
  , gitPathToObj
  , sha1ToObjFile
  , sha1ToObj
  ) where

import Control.Applicative
import Control.Exception
import Data.Git
import Data.Typeable (Typeable)
import Prelude hiding (catch)
import System.Directory
import System.FilePath
import System.IO

----------------------------------------------------------------

{-|
  Type for the path to Git repository directories.
-}
type GitDir = FilePath

{-|
  Type for the absolute path from the project root.
-}
type GitPath = FilePath

data GitError = GitDirNotExist | GitEntryNotExist
              deriving (Show, Typeable)
instance Exception GitError

----------------------------------------------------------------

{-|
  Getting 'GitObject' of 'GoBlob'/'GoTree' corresponding to 'GitPath'.
-}
gitPathToGitObject :: GitPath -> IO (Either SomeException GitObject)
gitPathToGitObject path = pathtoobj `catch` errorhandle
  where
   pathtoobj = findGitDir >>= gitPathToObj path >>= return . Right
   errorhandle :: SomeException -> IO (Either SomeException GitObject)
   errorhandle = return . Left

{-|
  Getting 'GitObject' of 'GoBlob'/'GoTree' corresponding to 'GitPath'.
-}
gitPathToObj :: GitPath -> GitDir -> IO GitObject
gitPathToObj path gitDir = gitPathToSha1 path gitDir >>= flip sha1ToObj gitDir

{-|
  Getting 'SHA1' corresponding to 'GitPath'.
-}
gitPathToSha1 :: GitPath -> GitDir -> IO SHA1
gitPathToSha1 path gitDir = do
    GoCommit _ commit <- rootCommitObj gitDir
    let sha1OfRootTreeObj = commitRef commit
    pathToSha1 ps sha1OfRootTreeObj gitDir
  where
    ps = tail $ splitFilePath path

pathToSha1 :: [String] -> SHA1 -> GitDir -> IO SHA1
pathToSha1 []     sha _      = return sha
pathToSha1 (f:fs) sha gitDir = do
    obj <- sha1ToObj sha gitDir
    case obj of
        GoTree _ ents -> case lokup f ents of
            Nothing -> throw GitEntryNotExist
            Just sha' -> pathToSha1 fs sha' gitDir
        _ -> throw GitEntryNotExist

{-
search :: GitDir -> [FilePath] -> GitObject -> IO GitObject
search gitDir fs (GoCommit _ com) =
    sha1ToObj (commitRef com) gitDir >>= search gitDir fs
search _ [] obj = return obj
search gitDir (f:fs) (GoTree _ ents) = case lokup f ents of
    Nothing -> throw GitEntryNotExist
    Just sha -> sha1ToObj sha gitDir >>= search gitDir fs
search _ _ _ = throw GitEntryNotExist
-}

lokup :: FilePath -> [GitTreeEntry] -> Maybe SHA1
lokup _ [] = Nothing
lokup key (e:es)
  | key == fileName e = Just (fileRef e)
  | otherwise         = lokup key es

----------------------------------------------------------------

{-|
  Getting 'GitObject' of 'GoBlob' corresponding to the project root.
-}
rootCommitObj :: GitDir -> IO GitObject
rootCommitObj gitDir = rootSha1 gitDir >>= flip sha1ToObj gitDir

{-|
  Getting 'SHA1' of the project root.
-}
rootSha1 :: GitDir -> IO SHA1
rootSha1 gitDir = SHA1 <$> (getRootRefFile gitDir >>= readFileLine)

getRootRefFile :: GitDir -> IO FilePath
getRootRefFile gitDir = fieldToFile <$> readFileLine headFile
  where
    fieldToFile field = gitDir </> drop 5 field
    headFile = gitDir </> "HEAD"

----------------------------------------------------------------

{-|
  Finding 'GitDir' by tracking from the current directory
  to the root of the file system.
-}
findGitDir :: IO GitDir
findGitDir = getCurrentDirectory >>= loop
  where
    loop dir = do
        let gitDir = dir </> ".git"
        exist <- doesDirectoryExist gitDir
        if exist
           then return gitDir
           else if dir == "/"
                then throw GitDirNotExist
                else loop (takeDirectory dir)

----------------------------------------------------------------

{-|
  Getting 'GitObject' according to 'SHA1'.
-}
sha1ToObj :: SHA1 -> GitDir -> IO GitObject
sha1ToObj sha gitDir = parseGitObject $ sha1ToObjFile sha gitDir

{-|
  Getting 'FilePath' to the Git object file according to 'SHA1'.
-}
sha1ToObjFile :: SHA1 -> GitDir -> FilePath
sha1ToObjFile (SHA1 hash) gitDir =
    gitDir </> "objects" </> take 2 hash </> drop 2 hash

readFileLine :: FilePath -> IO String
readFileLine file = withBinaryFile file ReadMode hGetLine

----------------------------------------------------------------

-- splitFilePath "/foo/bar/baz/" -> ["","foo","bar","baz"]
splitFilePath :: FilePath -> [String]
splitFilePath "" = []
splitFilePath path = case break ('/'==) path of
    (xs,"") -> [xs]
    (xs,_:ys) -> xs : splitFilePath ys
