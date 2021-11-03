{-# LANGUAGE OverloadedStrings #-}
module BMT.Audio.Playback where
import qualified Data.ByteString.Lazy as BL
import System.IO (Handle, stdout, hFlush)
import System.Process.Typed
import Data.Binary.Put
import Data.Binary
import GHC.IO.Handle

aplay :: [Float] -> IO ()
aplay xs = do
    let config :: ProcessConfig Handle () ()
        config = setStdin createPipe
                $ setStdout closed
                $ setStderr closed
                $ shell "aplay -c 2 -f FLOAT_BE -r 44100"
    withProcessWait_ config $ \p -> do
        let inHandle = getStdin p
        BL.hPutStr inHandle $ runPut $ serializeFloatList xs
        hClose inHandle


serializeFloatList :: [Float] -> Put
serializeFloatList xs = mapM_ putFloatbe xs



