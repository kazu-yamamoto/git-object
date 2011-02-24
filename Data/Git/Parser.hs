{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Parser (gitObject) where

import Control.Applicative hiding (many)
import qualified Data.Attoparsec.Char8 as AP (take)
import Data.Attoparsec.Char8 hiding (take)
import qualified Data.ByteString.Char8 as BS
import Data.Git.Types

----------------------------------------------------------------

gitObject :: Parser GitObject
gitObject = do
    (typ,len) <- gitHeader
    case typ of
        GtBlob   -> GoBlob   len <$> AP.take len
        GtTree   -> GoTree   len <$> gitEntries
        GtCommit -> GoCommit len <$> AP.take len -- FIXME
        GtTag    -> GoTag    len <$> AP.take len -- FIXME

----------------------------------------------------------------

gitHeader :: Parser (GitType, Int)
gitHeader = (,) <$> (gitType <* spc) <*> (decimal <* nul)

gitType :: Parser GitType
gitType = GtBlob   <$ string "blob"
      <|> GtCommit <$ string "commit"
      <|> GtTree   <$ string "tree"
      <|> GtTag    <$ string "tag"

----------------------------------------------------------------

gitEntries :: Parser [GitTreeEntry]
gitEntries = many1 gitEntry

-- data GitTreeEntry = GitTreeEntry GitTreeType Mode FilePath SHA1
gitEntry :: Parser GitTreeEntry
gitEntry = GitTreeEntry GttBlob <$> mode <*> filepath <*> sha1

mode :: Parser Mode
mode = decimal <* spc

filepath :: Parser FilePath
filepath = many1 $ noneOf "\0" <* nul

sha1 :: Parser SHA1
sha1 = SHA1 . BS.unpack <$> AP.take 20

----------------------------------------------------------------

{-
oneOf :: String -> Parser Char
oneOf = satisfy . inClass
-}

noneOf :: String -> Parser Char
noneOf = satisfy . notInClass

spc :: Parser ()
spc = () <$ char ' '

nul :: Parser ()
nul = () <$ char '\0'
