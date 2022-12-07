{-# LANGUAGE TemplateHaskell #-}

module Mimidl.Library.Monad where

import Polysemy
import Mimidl.Mimicle.Types

import Path qualified
import Path ( Path, Abs, Dir, File, (</>) )
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text qualified as Text
import Data.ByteString qualified as B
import Data.Yaml qualified as Yaml
import System.Directory qualified

data Library m a where
    WriteWorkMeta  :: Album -> Library m ()

    SearchWorkMeta :: Id -> Library m (Maybe Album)
    -- ^ 'Nothing' indicates no such album in store. Other errors should be
    --   propagated through the monad.

deriving stock instance Show (Library m a)

makeSem ''Library

runLibraryFolderYaml
    :: Members '[Embed IO] r
    => Path Abs Dir
    -> Sem (Library ': r) a -> Sem r a
runLibraryFolderYaml d = interpret $ \case
  WriteWorkMeta  a -> liftIO $ do
    fpath <- libIdFilepath d $ albumId a
    B.writeFile (Path.toFilePath fpath) $ Yaml.encode a
  SearchWorkMeta aid -> liftIO $ do
    fpath <- libIdFilepath d aid
    let fpath' = Path.toFilePath fpath
    System.Directory.doesPathExist fpath' >>= \case
      False -> pure Nothing
      True  -> Yaml.decodeFileThrow fpath'

libIdFilepath :: MonadThrow m => Path Abs Dir -> Id -> m (Path Abs File)
libIdFilepath d i = do
    fname <- Path.parseRelFile $ Text.unpack (getId i)<>".yaml"
    pure $ d</>fname
