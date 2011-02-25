{-# LANGUAGE DeriveDataTypeable #-}

module System.Git where

import Control.Applicative
import Control.Exception
import Data.Git
import Data.Typeable (Typeable)
import Prelude hiding (catch)
import System.Directory
import System.FilePath
import System.IO

----------------------------------------------------------------

data GitError = GitDirNotExist | GitEntryNotExist
              deriving (Show, Typeable)
instance Exception GitError

----------------------------------------------------------------

pathToObj :: FilePath -> IO GitObject
pathToObj path = findGitDir >>= \gdir ->
    getRootSha1 gdir >>= sha1ToObj gdir >>= search gdir ps
  where
    ps = tail $ splitFilePath path

search :: FilePath -> [FilePath] -> GitObject -> IO GitObject
search gdir fs (GoCommit _ com) =
    sha1ToObj gdir (commitRef com) >>= search gdir fs
search _ [] obj = return obj
search gdir (f:fs) (GoTree _ ents) = case lokup f ents of
    Nothing -> throw GitEntryNotExist
    Just sha -> sha1ToObj gdir sha >>= search gdir fs
search _ _ _ = throw GitEntryNotExist

lokup :: FilePath -> [GitTreeEntry] -> Maybe SHA1
lokup _ [] = Nothing
lokup key (e:es)
  | key == fileName e = Just (fileRef e)
  | otherwise         = lokup key es

----------------------------------------------------------------

getRootSha1 :: FilePath -> IO SHA1
getRootSha1 gitDir = SHA1 <$> (getRootRefFile gitDir >>= readFileLine)

getRootRefFile :: FilePath -> IO FilePath
getRootRefFile gitDir = fieldToFile <$> readFileLine headFile
  where
    fieldToFile field = gitDir </> drop 5 field
    headFile = gitDir </> "HEAD"

----------------------------------------------------------------

findGitDir :: IO FilePath
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

sha1ToObj :: FilePath -> SHA1 -> IO GitObject
sha1ToObj gitDir sha = parseGitObject $ sha1ToObjFile gitDir sha

sha1ToObjFile :: FilePath -> SHA1 -> FilePath
sha1ToObjFile gitDir (SHA1 hash) =
    gitDir </> "objects" </> take 2 hash </> drop 2 hash

readFileLine :: FilePath -> IO String
readFileLine file = withBinaryFile file ReadMode hGetLine

----------------------------------------------------------------

-- splitFilePath "/foo/bar/baz/" -> ["","foo","bar","baz"]
splitFilePath :: FilePath -> [FilePath]
splitFilePath "" = []
splitFilePath path = case break ('/'==) path of
    (xs,"") -> [xs]
    (xs,_:ys) -> xs : splitFilePath ys
