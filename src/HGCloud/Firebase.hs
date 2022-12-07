{- | Firebase bindings.

Firebase wasn't originally part of Google Cloud, but it's easiest to consider it
another offering due to how it interacts with other Google services.
-}

module HGCloud.Firebase where

import Data.Text ( Text )
import Data.Aeson ( ToJSON, FromJSON )

-- | Firebase Web API Key.
--
-- This identifies your Firebase project to Google. This is "public", in that
-- clients use it to send requests to Google services that your project uses.
newtype ApiKey = ApiKey { getApiKey :: Text }
    deriving (Eq, Show, ToJSON, FromJSON) via Text
