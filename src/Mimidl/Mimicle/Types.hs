{- | Partial model of types used in Mimicle's internal API.

Note that many metadata fields are nullable, but I can't be sure which. I go
with non-nullable until a parse error tells me otherwise.
-}

{-# LANGUAGE OverloadedStrings #-}

module Mimidl.Mimicle.Types where

import Mimidl.Mimicle.Types.Json
import Data.Aeson
import Web.HttpApiData ( ToHttpApiData(..) )
import GHC.Generics ( Generic )

import Data.Text ( Text )
import Numeric.Natural

-- | Pagination wrapper over 'a'.
--
-- Mimicle "paginates" many of their resources. This means the following:
--
--   * You're requesting a list of a single type.
--   * The list is ordered in some way (e.g. by a date field)
--   * You may only request between 0 and @n@ items at a time, in slices.
--
-- @n@ is 100 in at least one place.
data Paginated a = Paginated
  { paginatedItems :: [a]
  , paginatedTotal :: Natural
  , paginatedSkip  :: Natural
  , paginatedTake  :: Natural
  } deriving stock (Generic, Show, Eq)
instance FromJSON a => FromJSON (Paginated a) where parseJSON     = gpjm "paginated"
instance   ToJSON a =>   ToJSON (Paginated a) where    toJSON     = gtjm "paginated"
                                                       toEncoding = gtem "paginated"

newtype Id = Id { getId :: Text }
    deriving Eq via Text
    deriving stock (Generic, Show)
deriving via Text instance FromJSON Id
deriving via Text instance   ToJSON Id

type Nid = Text

newtype Date = Date { getDate :: Text }
    deriving Eq via Text
    deriving stock (Generic, Show)
deriving via Text instance FromJSON Date
deriving via Text instance   ToJSON Date

data Album = Album
  { albumId               :: Id
  , albumNid              :: Nid
  , albumTitle            :: Text
  , albumShortDescription :: Text
  , albumDescription      :: Maybe Text
  , albumPrice            :: Maybe Natural
  , albumPublishAt        :: Date
  , albumReleaseAt        :: Date
  , albumSeriesId         :: Maybe Id
  , albumCreatedAt        :: Date
  , albumUpdatedAt        :: Date
  , albumCoverArt         :: CoverArt
  , albumTracks           :: [Track]
  , albumTargetGender     :: TargetGender
  } deriving stock (Generic, Show, Eq)
instance FromJSON Album where parseJSON     = gpjm "album"
instance   ToJSON Album where    toJSON     = gtjm "album"
                                 toEncoding = gtem "album"

data SearchAlbum = SearchAlbum
  { searchAlbumId               :: Id
  , searchAlbumNid              :: Nid
  , searchAlbumStatus           :: Text
  , searchAlbumTitle            :: Text
  , searchAlbumShortDescription :: Text
  , searchAlbumDescription      :: Maybe Text
  , searchAlbumPrice            :: Maybe Natural
  , searchAlbumPublishAt        :: Date
  , searchAlbumReleaseAt        :: Date
  , searchAlbumCoverArt         :: CoverArt
  , searchAlbumTargetGender     :: TargetGender
  } deriving stock (Generic, Show, Eq)
instance FromJSON SearchAlbum where parseJSON     = gpjm "searchAlbum"
instance   ToJSON SearchAlbum where    toJSON     = gtjm "searchAlbum"
                                       toEncoding = gtem "searchAlbum"

data CoverArt = CoverArt
  { coverArtPath   :: Text
  , coverArtWidth  :: Natural
  , coverArtHeight :: Natural
  } deriving stock (Generic, Show, Eq)
instance FromJSON CoverArt where parseJSON     = gpjm "coverArt"
instance   ToJSON CoverArt where    toJSON     = gtjm "coverArt"
                                    toEncoding = gtem "coverArt"

data Track = Track
  { trackId :: Id
  , trackAlbumId :: Id
  , trackFileId :: Id
  , trackTitle :: Text
  , trackTrackNumber :: Natural
  , trackCreatedAt :: Date
  , trackUpdatedAt :: Date
  , trackFile :: TrackFile
  } deriving stock (Generic, Show, Eq)
instance FromJSON Track where parseJSON     = gpjm "track"
instance   ToJSON Track where    toJSON     = gtjm "track"
                                 toEncoding = gtem "track"

data TrackFile = TrackFile
  { trackFileFileType :: Text
  , trackFileDuration :: Double
  , trackFileFormats  :: TrackFileFormats
  } deriving stock (Generic, Show, Eq)
instance FromJSON TrackFile where parseJSON     = gpjm "trackFile"
instance   ToJSON TrackFile where    toJSON     = gtjm "trackFile"
                                     toEncoding = gtem "trackFile"

data TrackFileFormats = TrackFileFormats
  { trackFileFormatsSV1H :: TrackFileFormatStream
  , trackFileFormatsRV1  :: TrackFileFormatRecognize
  } deriving stock (Generic, Show, Eq)
instance FromJSON TrackFileFormats where
    parseJSON = withObject "TrackFileFormats" $ \o -> TrackFileFormats
        <$> o .: "s_v1_h"
        <*> o .: "r_v1"
instance   ToJSON TrackFileFormats where
    toJSON tff = object
        [ "s_v1_h" .= trackFileFormatsSV1H tff
        , "r_v1"   .= trackFileFormatsRV1  tff
        ]
    toEncoding tff = pairs
        (  "s_v1_h" .= trackFileFormatsSV1H tff
        <> "r_v1"   .= trackFileFormatsRV1  tff
        )

data TrackFileFormatStream = TrackFileFormatStream
  { trackFileFormatStreamBitRate  :: Text
  , trackFileFormatStreamFilePath :: Text
  } deriving stock (Generic, Show, Eq)
instance FromJSON TrackFileFormatStream where parseJSON     = gpjm "trackFileFormatStream"
instance   ToJSON TrackFileFormatStream where    toJSON     = gtjm "trackFileFormatStream"
                                                 toEncoding = gtem "trackFileFormatStream"

data TrackFileFormatRecognize = TrackFileFormatRecognize
  { trackFileFormatRecognizeFilePath :: Text
  } deriving stock (Generic, Show, Eq)
instance FromJSON TrackFileFormatRecognize where parseJSON     = gpjm "trackFileFormatRecognize"
instance   ToJSON TrackFileFormatRecognize where    toJSON     = gtjm "trackFileFormatRecognize"
                                                    toEncoding = gtem "trackFileFormatRecognize"

data Permission = Permission
  { permissionType        :: Text
  , permissionToken       :: Text
  , permissionContentPath :: Text
  } deriving stock (Generic, Show, Eq)
instance FromJSON Permission where parseJSON     = gpjm "permission"
instance   ToJSON Permission where    toJSON     = gtjm "permission"
                                      toEncoding = gtem "permission"

-- | Arbitrary "target gender" for a work. Determines the works the store
--   presents a user with. Each work is tagged as one of these.
--
-- You may select between 男性向け and 女性向け on the Store page, but not
-- "both". Upon testing, it works via the API.
--
-- A short commentary: Japanese ASMR being closely connected to softcore porn,
-- which is connected to porn as a whole, has historically categorized their
-- works in this rather substandard way. I'm glad to see mimicle smartly
-- providing a both option! But I wish they would just tag their works better,
-- like "male voice". I suppose it's a very simple service, and this is a
-- similarly simple - if naive - solution. Sadly, breaking the norm seems
-- difficult at this point.
--
-- TODO: Wait, I misunderstood? @both@ simply combines the two provided tags. I
-- thought some works were tagged as both, but apparently, they're fully
-- distinct. Huh.
data TargetGender
  = TargetGenderMale   -- ^ 男性向け
  | TargetGenderFemale -- ^ 女性向け
  | TargetGenderBoth
    deriving stock (Generic, Show, Eq, Enum, Bounded)

instance FromJSON TargetGender where parseJSON     = gpjm "TargetGender"
instance   ToJSON TargetGender where    toJSON     = gtjm "TargetGender"
                                        toEncoding = gtem "TargetGender"

-- TODO automate?... I guess I could write my own ToHttpApiData generics
instance ToHttpApiData TargetGender where
    toUrlPiece = \case
      TargetGenderMale   -> "male"
      TargetGenderFemale -> "female"
      TargetGenderBoth   -> "both"
