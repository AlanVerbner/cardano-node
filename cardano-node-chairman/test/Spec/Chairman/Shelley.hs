{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Chairman.Shelley
  ( hprop_chairman
  ) where

import           Control.Monad
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           System.Exit (ExitCode (..))
import           System.FilePath.Posix ((</>))
import           Text.Show

import qualified Data.List as L
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Test.Base as H
import qualified Test.Process as H
import qualified Testnet.Conf as H
import qualified Testnet.Shelley as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

hprop_chairman :: H.Property
hprop_chairman = H.integration . H.workspace "chairman" $ \tempAbsPath' -> do
  conf@H.Conf {..} <- H.mkConf tempAbsPath' 42

  allNodes <- H.testnet conf

  -- Run chairman
  forM_ (L.take 1 allNodes) $ \node1 -> do
    nodeStdoutFile <- H.noteTempFile logDir $ "chairman-" <> node1 <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ "chairman-" <> node1 <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node1)

    H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

    hNodeStdout <- H.evalIO $ IO.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.evalIO $ IO.openFile nodeStderrFile IO.WriteMode

    (_, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procChairman
        [ "--timeout", "100"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--config", tempAbsPath </> "configuration.yaml"
        , "--security-parameter", "2160"
        , "--testnet-magic", show @Int testnetMagic
        , "--slot-length", "20"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    chairmanResult <- H.waitSecondsForProcess 110 hProcess

    H.cat nodeStdoutFile
    H.cat nodeStderrFile

    case chairmanResult of
      Right ExitSuccess -> return ()
      _ -> do
        H.note_ $ "Failed with: " <> show chairmanResult
        forM_ allNodes $ \node -> do
          H.cat $ logDir </> node <> ".stdout.log"
          H.cat $ logDir </> node <> ".stderr.log"
        H.failure
