{-|
  Types for Git objects and a parser of Git object files.
-}

module Data.Git (parseGitObject, module Data.Git.Types) where

import Codec.Zlib.Enum
import Data.Attoparsec.Enumerator
import Data.ByteString (ByteString)
import Data.Enumerator hiding (drop)
import qualified Data.Enumerator.Binary as EB
import Data.Git.Parser
import Data.Git.Types

{-|
  Parsing a Git file to 'GitObject'.
  This parser based on attoparsec-enumerator.
-}
parseGitObject :: FilePath -> IO GitObject
parseGitObject file = run_ $ EB.enumFile file
                          $$ decompress defaultWindowBits
                          =$ iterGitObject

iterGitObject :: Iteratee ByteString IO GitObject
iterGitObject = iterParser gitObject
