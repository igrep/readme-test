module Test.ReadmeTest
  ( readmeTestWith
  )
where

import Data.Conduit
import Data.Conduit.Process
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Control.Concurrent.Async (Concurrently (..))
import Control.Applicative ((*>))
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Monad (forM_)

import System.IO

import qualified Data.ByteString.Char8 as BSC8

import Test.ReadmeTest.Internal

readmeTestWith :: FilePath -> [FilePath] -> IO ()
readmeTestWith driverPath readmePaths = do
  forM_ readmePaths $ \ readmePath -> do
    (toProcess, fromProcess, Inherited, cph) <- streamingProcess (proc driverPath [])

    let input = runResourceT $ CB.sourceFile readmePath $= conduitDebug $$ toProcess
    let output = fromProcess $$ CB.sinkHandle stdout

    _ <- runConcurrently $
      Concurrently input *>
      Concurrently output *>
      Concurrently (waitForStreamingProcess cph)

    return ()

sourceSourceCodeLines :: MonadResource m => FilePath -> Source m BSC8.ByteString
sourceSourceCodeLines fp = CB.sourceFile fp $= CB.lines

conduitDebug :: MonadIO m => Conduit BSC8.ByteString m BSC8.ByteString
conduitDebug = awaitForever $ \ chunk -> do
  liftIO $ BSC8.putStrLn chunk
  return chunk
