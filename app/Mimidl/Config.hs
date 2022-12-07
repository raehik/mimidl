module Mimidl.Config where

import GHC.Generics ( Generic )

import HGCloud.Firebase
import Mimidl.Mimicle.Types qualified as Mimicle
import Network.HTTP.Req ( Url, Scheme(..) )
import Mimidl.Mimicle.Api.Impl.MimicleCom.Authd qualified as Mimicle

-- | Top level CLI config.
data C = C
  { cCmd :: CCmd
  } deriving stock (Generic, Show, Eq)

data CCmd
  = CCmdDlWork' CCmdDlWork
    deriving stock (Generic, Show, Eq)

-- | Shared config for all commands that use Mimicle.
data CMimicle = CMimicle
  { cMimicleApiBase :: Url 'Https
  } deriving stock (Generic, Show, Eq)

-- | Shared config for commands that use Mimicle with authorization.
data CMimicleAuthd = CMimicleAuthd
  { cMimicleAuthdUnauthd :: CMimicle
  -- ^ Config also used by commands that do no authorization.

  , cMimicleAuthdApiKey  :: HGCloud.Firebase.ApiKey
  -- ^ Firebase API key (required by the Google services it uses).

  } deriving stock (Generic, Show, Eq)

-- | Command config: Download work(s).
data CCmdDlWork = CCmdDlWork
  { cCmdDlWorkApi  :: CMimicleAuthd
  , cCmdDlWorkNids :: [Mimicle.Nid]
  } deriving stock (Generic, Show, Eq)
