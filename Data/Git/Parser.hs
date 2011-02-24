{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Parser (gitObject) where

import Control.Applicative hiding (many)
import qualified Data.Attoparsec.Char8 as AP (take)
import qualified Data.Attoparsec as AP (takeWhile)
import Data.Attoparsec.Char8 hiding (take)
import qualified Data.ByteString.Char8 as BS (unpack)
import qualified Data.ByteString as BS (foldl')
import Data.Git.Types

----------------------------------------------------------------

gitObject :: Parser GitObject
gitObject = do
    (typ,len) <- header
    case typ of
        GtBlob   -> GoBlob   len <$> blob len
        GtTree   -> GoTree   len <$> tree
        GtCommit -> GoCommit len <$> AP.take len -- FIXME
        GtTag    -> GoTag    len <$> AP.take len -- FIXME

----------------------------------------------------------------

header :: Parser (GitType, Int)
header = (,) <$> (gitType <* spc) <*> (decimal <* nul)

gitType :: Parser GitType
gitType = GtBlob   <$ string "blob"
      <|> GtTree   <$ string "tree"
      <|> GtCommit <$ string "commit"
      <|> GtTag    <$ string "tag"

----------------------------------------------------------------

blob :: Int -> Parser Blob
blob = AP.take

----------------------------------------------------------------

tree :: Parser [GitTreeEntry]
tree = many1 entry

entry :: Parser GitTreeEntry
entry = GitTreeEntry GttBlob <$> mode <*> filepath <*> sha1
  where
    mode = fromIntegral <$> octal <* spc
    filepath = many1 $ noneOf "\0" <* nul

----------------------------------------------------------------

sha1 :: Parser SHA1
sha1 = SHA1 . BS.unpack <$> AP.take 20

octal :: Parser Int
octal = BS.foldl' step 0 <$> AP.takeWhile isDig
  where
    isDig w  = w >= 48 && w <= 55
    step a w = a * 8 + fromIntegral (w - 48)

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
