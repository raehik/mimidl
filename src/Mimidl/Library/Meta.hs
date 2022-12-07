{-# LANGUAGE OverloadedStrings #-}

module Mimidl.Library.Meta where

import Mimidl.Library.Internal.Json

import Mimidl.Mimicle.Types qualified as Mimicle

import GHC.Generics ( Generic )

import Data.Text ( Text )

import Text.Printf
import Data.Text qualified as Text

import Data.Map qualified as Map
import Control.Monad.State

import Numeric.Natural

data Work = Work
  { workNid :: Mimicle.Nid
  , workId :: Mimicle.Id
  , workRelease :: Release
  , workPreviousReleases :: [Release]
  } deriving stock (Generic, Show, Eq)
instance   ToJSON Work where    toJSON     = gtjml "work"
                                toEncoding = gteml "work"
instance FromJSON Work where parseJSON     = gpjml "work"

data Release = Release
  { releaseTag :: Maybe Text
  , releaseReleaseDate :: Maybe Text
  , releaseName :: Text

  , releaseTrackRenamings :: Maybe [TrackRenaming]
  -- ^ order matters: defines both input order, and output order for each group
  --   (it's easier this way and I doubt you'd want to change it)

  } deriving stock (Generic, Show, Eq)
instance   ToJSON Release where    toJSON     = gtjml "release"
                                   toEncoding = gteml "release"
instance FromJSON Release where parseJSON     = gpjml "release"

data TrackRenaming = TrackRenaming
  { trackRenamingMatch :: Text
  , trackRenamingName  :: Text
  , trackRenamingGroup :: Text
  } deriving stock (Generic, Show, Eq)
instance   ToJSON TrackRenaming where    toJSON     = gtjml "trackRenaming"
                                         toEncoding = gteml "trackRenaming"
instance FromJSON TrackRenaming where parseJSON     = gpjml "trackRenaming"

prepRenamingsMv :: Text -> [TrackRenaming] -> [Text]
prepRenamingsMv ext = flip evalState Map.empty . mapM go . zip [1..]
  where
    go (n, rnm) = do
        m <- get
        let (x, m') = Map.insertLookupWithKey (\_ _ v -> v+1) (trackRenamingGroup rnm) 1 m
            groupN = case x of Nothing -> 1; Just n' -> n'+1
        put m'
        let fnOrig = tprintf02d n<>"*"
            fnNew  = trackRenamingGroup rnm<>"/"<>tprintf02d groupN<>" "<>trackRenamingName rnm<>ext
        pure $ "mv "<>fnOrig<>" \""<>fnNew<>"\""

    tprintf02d :: Natural -> Text
    tprintf02d = Text.pack . printf "%02d"
