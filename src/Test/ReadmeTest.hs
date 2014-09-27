module Test.ReadmeTest
  ( readmeTestWith
  )
where

import Data.Conduit
import Data.Conduit.Process
import Control.Concurrent.Async (Concurrently (..))
import Control.Applicative ((*>))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import System.IO
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BSC8
import Test.ReadmeTest.Internal

readmeTestWith :: FilePath -> [FilePath] -> IO ()
readmeTestWith driverPath readmePaths = do
  forM_ readmePaths $ \ readmePath -> do
    (toProcess, fromProcess, ClosedStream, cph) <- streamingProcess (proc driverPath [])

    let input = runResourceT $ sourceSourceCodeLines readmePath $$ toProcess
    let output = fromProcess $$ CB.sinkHandle stdout

    _ <- runConcurrently $
      Concurrently input *>
      Concurrently output *>
      Concurrently (waitForStreamingProcess cph)

    return ()

sourceSourceCodeLines :: MonadResource m => FilePath -> Source m BSC8.ByteString
sourceSourceCodeLines fp = CB.sourceFile fp $= CB.lines
