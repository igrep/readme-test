module Test.ReadmeTest
  ( readmeTestWith
  )
where

import Data.Conduit
import Data.Conduit.Process
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import System.IO
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BSC8
import Test.ReadmeTest.Internal

readmeTestWith :: FilePath -> [FilePath] -> IO ()
readmeTestWith driverPath readmePaths = do
  forM_ readmePaths $ \ readmePath -> runResourceT $ do
    (toProcess, ClosedStream, ClosedStream, cph) <- streamingProcess (proc driverPath [])
    sourceSourceCodeLines readmePath $$ toProcess

sourceSourceCodeLines :: MonadResource m => FilePath -> Source m BSC8.ByteString
sourceSourceCodeLines fp = CB.sourceFile fp $= CB.lines
