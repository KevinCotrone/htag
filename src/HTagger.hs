{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HTagger where

import           HTagger.Internal

import           CorePrelude
import Control.Monad
import qualified Data.Foldable             as F
import qualified Data.Map.Strict           as M
import qualified Data.Sequence             as S
import qualified Data.Text                 as T
import           Data.Traversable
import qualified Data.Traversable          as TR

import qualified Filesystem                as FS
import qualified Filesystem.Path           as FP
import qualified Filesystem.Path.CurrentOS as OS

import qualified Sound.TagLib              as TL

data Artist = Artist {
  artistName :: String
} deriving (Eq, Show, Ord)

data Album = Album {
  albumName :: String
} deriving (Eq, Show, Ord)

data Song = Song {
  songTitle   :: String
, songYear    :: Integer
, songOldPath :: FP.FilePath
, songAlbum   :: Album
, songArtist  :: Artist
} deriving (Eq, Show, Ord)

type SongMap = M.Map Artist (M.Map Album [Song])


-- | List all directory contents. Used to get all files in a flat list
listAllDirectorContentsSeq :: FP.FilePath -> IO (S.Seq FP.FilePath)
listAllDirectorContentsSeq fp = do
  fpIsFile <- FS.isFile fp
  if fpIsFile
    then return S.empty
    else do
      listContents <- FS.listDirectory fp
      let contents = S.fromList listContents
      cContents <- TR.mapM listAllDirectorContentsSeq contents
      return $ contents S.>< concatSeq cContents


createSongMap :: FP.FilePath -> IO SongMap
createSongMap fp = do
  fileList <- listAllDirectorContentsSeq fp
  songList <- TR.traverse songFromFilePath (F.toList fileList)
  return $ F.foldl insertSongIntoMap M.empty (catMaybes songList)

insertSongIntoMap :: SongMap -> Song -> SongMap
insertSongIntoMap songMap song =
  let artist = songArtist song
      album = songAlbum song
  in case M.lookup artist songMap of
    (Just albumMap) ->
      case M.lookup album albumMap of
        (Just songList) -> M.insert artist (M.insert album (song:songList) albumMap) songMap
    Nothing ->
      let artistMap = M.singleton album [song]
      in M.insert artist artistMap songMap


songFromFilePath :: FP.FilePath -> IO (Maybe Song)
songFromFilePath fp = do
  let mText = eitherToMaybe $ T.unpack <$> OS.toText fp
  mTagFile <- TR.traverse TL.open mText
  mTag <- TR.traverse TL.tag (join mTagFile)
  TR.traverse (songFromTag fp) (join mTag)


tagMaybe :: Maybe TL.TagFile -> IO (Maybe TL.Tag)
tagMaybe (Just tf) = TL.tag tf
tagMaybe Nothing = return Nothing

openMaybe :: Maybe String -> IO (Maybe TL.TagFile)
openMaybe (Just t) = TL.open t
openMaybe Nothing = return Nothing

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right r) = Just r
eitherToMaybe (Left _) = Nothing

songFromTag :: FP.FilePath -> TL.Tag -> IO Song
songFromTag fp tag = do
  alb <- TL.album tag
  art <- TL.artist tag
  ttl <- TL.title tag
  yr <- TL.year tag
  return $ Song ttl yr fp (Album alb) (Artist art)

--traverseCreateFPArtist :: FP.FilePath -> S.Seq 

createPath :: FP.FilePath -> S.Seq FP.FilePath -> Song -> S.Seq FP.FilePath
createPath baseFP lastSeq song =
  let alb = OS.fromText . T.pack . albumName $ songAlbum song
      art = OS.fromText . T.pack . artistName $ songArtist song
      fname = FP.filename $ songOldPath song
  in lastSeq S.|> baseFP FP.</> art FP.</> alb FP.</> fname


--copySongsToDir :: FP.FilePath -> FP.FilePath -> SongMap -> IO (Maybe ())
--copySongsToDir currDir goalDir sm = do
--  songMap <- createSongMap currDir
--  let foldList = F.toList $ M.foldl (createPath goalDir) S.empty songMap
--  return foldList
