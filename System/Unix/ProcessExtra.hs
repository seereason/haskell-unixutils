-- |A place to collect and hopefully retire all the random ways of
-- running shell commands that have accumulated over the years.
module System.Unix.ProcessExtra where

import Control.Exception (ErrorCall(ErrorCall), try)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import Data.List (intercalate)
import System.Exit
import qualified System.IO as IO
import System.Unix.Process (Output(..), lazyCommand, collectOutput, lazyProcess)

cmdOutput :: String -> IO (Either ErrorCall L.ByteString)
cmdOutput cmd =
    do (out, err, code) <- lazyCommand cmd L.empty >>= return . collectOutput
       case code of
         ExitSuccess -> return (Right out)
         _ -> return . Left . ErrorCall $ "Failure: " ++ show cmd ++ " -> " ++ show code ++ "\n\nstdpout:\n\n" ++ show (L.unpack out) ++ "\n\nstderr:\n\n" ++ show (L.unpack err)

cmdOutputStrict :: String -> IO (Either ErrorCall B.ByteString)
cmdOutputStrict cmd =
    do (out, err, code) <- lazyCommand cmd L.empty >>= return . f . collectOutput
       case code of
         ExitSuccess -> return (Right out)
         _ -> return . Left . ErrorCall $ "Failure: " ++ show cmd ++ " -> " ++ show code ++ "\n\nstdpout:\n\n" ++ show (B.unpack out) ++ "\n\nstderr:\n\n" ++ show (B.unpack err)
    where
      f :: (L.ByteString, L.ByteString, ExitCode) -> (B.ByteString, B.ByteString, ExitCode)
      f (o, e, c) = (toStrict o, toStrict e, c)

toLazy :: B.ByteString -> L.ByteString
toLazy b = L.fromChunks [b]

toStrict :: L.ByteString -> B.ByteString
toStrict b = B.concat (L.toChunks b)

echoCommand :: String -> L.ByteString -> IO [Output]
echoCommand command input =
    ePutStrBl ("# " ++ command) >>
    liftIO (lazyCommand command input)

-- |Echo the process arguments and then run the process
echoProcess :: FilePath -> [String] -> L.ByteString -> IO [Output]
echoProcess exec args input =
    ePutStrBl (intercalate " " ("#" : exec : args)) >>
    liftIO (lazyProcess exec args Nothing Nothing input)

ePutStrBl :: String -> IO ()
ePutStrBl s = IO.hPutStrLn IO.stderr s
