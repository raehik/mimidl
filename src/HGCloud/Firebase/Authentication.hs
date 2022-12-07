{- TODO
  * fix aeson instances (barabara)
  * bad JSON is implicitly handled by req - pull out
-}

{- | Partial bindings to Firebase Authentication (also Firebase Auth).

Identity Platform (IP) is an amalgamation of old Google services. It replaces
Identity Toolkit - which was replaced previously by Firebase Authentication.
Both share code, and Google appears to suggest IP over Firebase Auth. In
particular, they use some identical REST API endpoints left over from Identity
Token and other old Google APIs (@xyz.googleapis.com@).

The gogol package does not support IP or Firebase Auth, because they don't have
an entry in Google's API Discovery Service. (Probably because they don't fit, as
an amalgamation of other APIs.)

Parts of this may be reusable for Google Cloud Identity Platform.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module HGCloud.Firebase.Authentication where

import HGCloud.Firebase ( ApiKey(..) )
import HGCloud.Util.Json
import Util.Req

import Polysemy
import Network.HTTP.Req
import Data.Text ( Text )
import Data.Text.Encoding qualified as Text
import Data.Aeson
import GHC.Generics ( Generic )
import Data.ByteString qualified as B

-- | Firebase Auth ID token.
newtype IdToken = IdToken { getIdToken :: Text }
    deriving (Eq, Show, ToJSON, FromJSON) via Text
idTokenBytes :: IdToken -> B.ByteString
idTokenBytes = Text.encodeUtf8 . getIdToken

-- | Firebase Auth refresh token.
newtype RefreshToken = RefreshToken { getRefreshToken :: Text }
    deriving (Eq, Show, ToJSON, FromJSON) via Text

-- | Firebase Auth REST API.
--
-- https://firebase.google.com/docs/reference/rest/auth
data Api m a where
    Token :: RefreshToken -> Api m IdToken
    -- ^ Exchange a refresh token for an ID token.

makeSem ''Api

deriving stock instance Show (Api m a)

--------------------------------------------------------------------------------

-- | Interpret Firebase authentication API request using the live Google
--   service.
runApi
    :: Members '[Embed IO] r
    => ApiKey -> Sem (Api ': r) a -> Sem r a
runApi (ApiKey apiKey) = interpret $ \case
  Token (RefreshToken rtok) -> do
    r <- embedReq $
        let url  = https "securetoken.googleapis.com" /: "v1"/:"token"
            body = ReqBodyUrlEnc $ "grant_type"=:("refresh_token" :: Text) <> "refresh_token"=:rtok
            params = "key" =: apiKey
        in  req POST url body jsonResponse params
    pure $ apiTokenResponseIdToken $ responseBody r

-- | @/token@ endpoint response.
data ApiTokenResponse = ApiTokenResponse
  { apiTokenResponseRefreshToken :: RefreshToken
  , apiTokenResponseIdToken      :: IdToken
  , apiTokenResponseUserId       :: Text
  , apiTokenResponseProjectId    :: Text
  , apiTokenResponseExpiresIn    :: Text
  , apiTokenResponseTokenType    :: ApiTokenResponseTokenType
  } deriving stock (Generic, Show, Eq)
instance FromJSON ApiTokenResponse where parseJSON = gpjgc "apiTokenResponse"

data ApiTokenResponseTokenType = ApiTokenResponseTokenTypeBearer
    deriving stock (Generic, Show, Eq)
jcApiTokenResponseTokenType :: Options
jcApiTokenResponseTokenType = defaultOptions
    { constructorTagModifier = f
    , tagSingleConstructors = True }
  where f = drop (length ("ApiTokenResponseTokenType" :: String))
instance FromJSON ApiTokenResponseTokenType where
    parseJSON = genericParseJSON jcApiTokenResponseTokenType
