module Mimidl.Cli.Parse where

import Mimidl.Config

import Options.Applicative
import Network.HTTP.Req ( Url, Scheme(..) )
import Network.HTTP.Req qualified as Req
import Text.URI qualified as URI
import Data.Text qualified as Text
import Mimidl.Mimicle.Types qualified as Mimicle
import Mimidl.Mimicle.Api.Impl.MimicleCom.Authd qualified as Mimicle
import HGCloud.Firebase qualified
import HGCloud.Firebase.Authentication qualified
import Control.Monad.IO.Class

parse :: MonadIO m => m C
parse = execParserWithDefaults desc (C <$> pCCmd)
  where desc = "A set of tools for interacting with a mimicle-compatible service programatically."

pCCmd :: Parser CCmd
pCCmd = hsubparser $
       cmd' "dl-work" descDlWork (CCmdDlWork' <$> pCCmdDlWork)
  where
    descDlWork = "Download requested works. You may add multiple NIDs (the help doesn't show it correctly)."

pCMimicle :: Parser CMimicle
pCMimicle = CMimicle <$> pUrlHttps "api-base" "Base API URL"

pUrlHttps :: String -> String -> Parser (Url 'Https)
pUrlHttps optLong optHelp =
    option pPartUrlHttps $
           long optLong
        <> help optHelp
        <> metavar "URL"

pPartUrlHttps :: ReadM (Url 'Https)
pPartUrlHttps = eitherReader $ \s ->
    case URI.mkURI (Text.pack s) of
      Left err  -> Left $ show err
      Right uri ->
        case Req.useHttpsURI uri of
          Nothing -> Left "bad URL"
          Just (url, opts) ->
            case Req.queryParamToList opts of
              [] -> Right url
              _  -> Left "bad URL, had options"

pCMimicleAuthd :: Parser CMimicleAuthd
pCMimicleAuthd = CMimicleAuthd <$> pCMimicle <*> pFirebaseApiKey

pFirebaseApiKey :: Parser HGCloud.Firebase.ApiKey
pFirebaseApiKey = HGCloud.Firebase.ApiKey <$> strOption (long "firebase-api-key" <> help "Firebase API key")

pMimicleAuthToken :: Parser Mimicle.AuthToken
pMimicleAuthToken = pRefresh <|> pId
  where
    pRefresh = (Mimicle.AuthTokenRefresh . HGCloud.Firebase.Authentication.RefreshToken) <$> strOption (long "auth-token-refresh" <> help "Long-lived authentication token")
    pId      = (Mimicle.AuthTokenId . HGCloud.Firebase.Authentication.IdToken)           <$> strOption (long "auth-token-id" <> help "Short-lived authentication token")

pCCmdDlWork :: Parser CCmdDlWork
pCCmdDlWork = CCmdDlWork <$> pCMimicleAuthd <*> some pNid

pNid :: Parser Mimicle.Nid
pNid = strArgument $ help "Album NID (string in album URL)" <> metavar "NID"

--------------------------------------------------------------------------------

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))

-- | Shorthand for the way I always write commands.
cmd' :: String -> String -> Parser a -> Mod CommandFields a
cmd' name desc p = command name (info p (progDesc desc))
