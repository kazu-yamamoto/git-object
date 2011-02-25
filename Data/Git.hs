module Data.Git (parseGitObject, module Data.Git.Types) where

import Codec.Zlib.Enum
import Data.Attoparsec.Enumerator
import Data.ByteString (ByteString)
import Data.Enumerator hiding (drop)
import qualified Data.Enumerator.Binary as EB
import Data.Git.Parser
import Data.Git.Types

parseGitObject :: FilePath -> IO GitObject
parseGitObject file = run_ $ EB.enumFile file
                          $$ decompress defaultWindowBits
                          =$ iterGitObject

iterGitObject :: Iteratee ByteString IO GitObject
iterGitObject = iterParser gitObject

infixr 0 =$
(=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
ee =$ ie = joinI $ ee $$ ie
