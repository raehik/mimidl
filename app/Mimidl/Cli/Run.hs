{-# LANGUAGE RecordWildCards #-}

module Mimidl.Cli.Run where

import Mimidl.Config
import Mimidl.Download.Work qualified
import Data.Text.IO qualified as Text
import Mimidl.Mimicle.Api.Impl.MimicleCom.Authd qualified as Mimicle
import HGCloud.Firebase.Authentication qualified
import System.IO ( hFlush, stdout )

run :: C -> IO ()
run c = case cCmd c of
          CCmdDlWork' c' -> runCmdDlWork c'

runCmdDlWork :: CCmdDlWork -> IO ()
runCmdDlWork c = do
    putStr "Enter user refresh token: "
    hFlush stdout
    refreshTokenText <- Text.getLine
    let authToken = Mimicle.AuthTokenRefresh $ HGCloud.Firebase.Authentication.RefreshToken refreshTokenText
    Mimidl.Download.Work.run env authToken
  where
    env = Mimidl.Download.Work.Env{..}
    envNids = cCmdDlWorkNids c
    envApp = Mimidl.Download.Work.App{..}
    appApiBase = cMimicleApiBase . cMimicleAuthdUnauthd . cCmdDlWorkApi $ c
    appGCFBApiKey = cMimicleAuthdApiKey . cCmdDlWorkApi $ c
