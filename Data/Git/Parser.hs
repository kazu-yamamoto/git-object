{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Parser (gitObject) where

import Control.Applicative
import qualified Data.Attoparsec as AP (takeWhile)
import qualified Data.Attoparsec.Char8 as AP (take)
import Data.Attoparsec.Char8 hiding (take)
import Data.Bits
import qualified Data.ByteString as BS (foldl', foldr)
import qualified Data.ByteString.Char8 as BS (unpack)
import Data.Git.Types
import Data.Char

----------------------------------------------------------------

gitObject :: Parser GitObject
gitObject = do
    (typ,len) <- header
    case typ of
        GtBlob   -> GoBlob   len <$> blob len
        GtTree   -> GoTree   len <$> tree
        GtCommit -> GoCommit len <$> commit
        GtTag    -> GoTag    len <$> tag

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
entry = GitTreeEntry <$> (filetype <* spc)
                     <*> (filepath <* nul)
                     <*> binarySha1
  where
    filepath = many1 $ noneOf "\0"

filetype :: Parser FileType
filetype = getType . fromIntegral <$> octal
  where
    getMode x = x .&. 0o7777
    getType x
      | isBitSet x 0o0040000 = Directory
      | isBitSet x 0o0120000 = SymbolicLink
      | isBitSet x 0o0160000 = GitLink
      | otherwise            = RegularFile (getMode x)
    isBitSet x mask = x .&. mask == mask

----------------------------------------------------------------

commit :: Parser GitCommit
commit = GitCommit <$> tre <*> parents <*> author <*> owner <*> logmsg
  where
    tre       = string "tree "   *> sha1 <* endOfLine
    parents   = many parent
    parent    = string "parent " *> sha1 <* endOfLine
    author    = string "author "    *> line
    owner     = string "committer " *> line
    logmsg    = endOfLine *> AP.takeWhile (const True) <* endOfInput
    line = AP.takeWhile (not.isEndOfLine) <* endOfLine

----------------------------------------------------------------

tag :: Parser GitTag
tag = GitTag <$> obj <*> typ <*> name <*> owner <*> tagmsg
  where
    obj    = string "object " *> sha1 <* endOfLine
    typ    = string "type "   *> line
    name   = string "tag "    *> line
    owner  = string "tagger " *> line
    tagmsg = endOfLine *> AP.takeWhile (const True) <* endOfInput
    line = AP.takeWhile (not.isEndOfLine) <* endOfLine

----------------------------------------------------------------

binarySha1 :: Parser SHA1
binarySha1 = SHA1 . (flip toASCII "") <$> AP.take 20
  where
    toASCII = BS.foldr (\w ss -> toHex w . ss) id
    toHex w = let (u,d) = w `divMod` 16
                  hex2 = toH u : toH d : []
              in (hex2 ++)
    toH w
      | w >= 10   = chr $ baseA + (w' - 10)
      | otherwise = chr $ base0 + w'
      where
        w' = fromIntegral w
    baseA = ord 'a'
    base0 = ord '0'

sha1 :: Parser SHA1
sha1 = SHA1 . BS.unpack <$> AP.take 40

octal :: Parser Int
octal = BS.foldl' step 0 <$> AP.takeWhile isDig
  where
    isDig w  = 48 <= w && w <= 55
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
