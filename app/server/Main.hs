{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Paranet.Common

import Control.Concurrent (myThreadId, forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as T
import Control.Monad (void, unless)
import Control.Monad (forM_)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Function (fix)
import Data.List (minimumBy)

import qualified ListT

import Network.Run.TCP
import Network.Socket hiding (socket)
import Network.Socket.ByteString.Lazy

import Options.Applicative

import qualified StmContainers.Map as Map

import System.Random
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = run =<< execParser opts
  where opts = info (cmdLine <**> helper) (fullDesc <> progDesc "Runs the server for paranet")

data Args
    = Args
    { address :: String
    , port    :: String
    }

cmdLine :: Parser Args
cmdLine = Args
    <$> option str (long "address" <> short 'a' <> metavar "ADDRESS" <> value "127.0.0.1" <> help "Address to bind to")
    <*> option str (long "port" <> short 'p' <> metavar "PORT" <> value "9200" <> help "Port to bind to")

run :: Args -> IO ()
run Args{..} = do
    clientsConnected <- Map.newIO

    (tid, res) <- T.forkIO do
        runTCPServer (Just address) port (handle clientsConnected)

    putStrLn ("<" <> show tid <> "> Listening on " <> address <> ":" <> port <> "...")

    void res

clientsIDGen :: TVar Integer
clientsIDGen = unsafePerformIO (newTVarIO 0)

handle :: Clients -> Socket -> IO ()
handle clients sock = do
    tid <- myThreadId

    addr <- show <$> getPeerName sock
    putStrLn ("<" <> show tid <> "> Connected to " <> addr <> "!")

    cid <- readTVarIO clientsIDGen
    sendAll sock (encode @Integer cid)
    atomically do
        modifyTVar clientsIDGen (+1)

    actionQueue <- newTQueueIO

    atomically do
        Map.insert (Client sock actionQueue) addr clients

    lsid <- forkIO (loopSend sock actionQueue)
    loop

    killThread lsid

    (toHandle, clts) <- atomically do
        toHandle <- flushTQueue actionQueue
        Map.delete addr clients
        clts <- ListT.toList (Map.listT clients)

        pure (toHandle, clts)

    unless (null clts) do
        forM_ toHandle \m -> do
            idx <- randomRIO (0, length clts - 1)
            let (_, client) = clts !! idx
            atomically do
                writeTQueue (queue client) m

    putStrLn ("<" <> show tid <> "> Disconnected from " <> show addr <> ".")
  where loop = fix \f -> do
            msg <- recv sock maxMessageSize

            unless (BS.null msg) do
                -- putStr ("<" <> show tid <> "> Received message from " <> addr <> ": ")

                let Just msgs = decode @[Message] msg
                -- print message

                --unless (null msgs) do
                --    print msgs

                forM_ msgs (handleMessage sock clients)

                f

loopSend :: Socket -> TQueue Message -> IO ()
loopSend sock queue = fix \f -> do
    threadDelay 50000 -- 50 ms
    msgs <- atomically do flushTQueue queue
    sendAll sock (encode @[Message] msgs)
    f

handleMessage :: Socket -> Clients -> Message -> IO ()
handleMessage sock clients (Send expr mid) = do
    tid <- myThreadId

    clts <- atomically do
        ListT.toList (Map.listT clients)
    idx <- randomRIO (0, length clts - 1)
    let (_, client) = clts !! idx
    -- let (clientAddress, client) = minimumBy (\(_, a1) (_, a2) -> queueLen a1 `compare` queueLen a2) clts

    let sock' = socket client
    from <- show <$> getPeerName sock
    to <- show <$> getPeerName sock'

    let message = GiveAndReceive from to expr mid
    atomically do
        writeTQueue (queue client) message

    putStrLn ("<" <> show tid <> "> " <> from <> " ~~> '" <> show expr <> "' ~~> " <> to <> "#" <> show (nb mid) <> ":" <> show (sent mid))
handleMessage sock clients (Give to expr mid) = do
    tid <- myThreadId

    from <- show <$> getPeerName sock
    clt <- atomically do
        Map.lookup to clients

    putStrLn ("<" <> show tid <> "> " <> to <> " <~~ '" <> show expr <> "' <~~ " <> from <> "#" <> show (nb mid) <> ":" <> show (sent mid))

    case clt of
        Just client -> do
            atomically do
                writeTQueue (queue client) (Receive from expr mid)
        Nothing -> pure ()
handleMessage _ _ _ = pure ()

data Client
    = Client
    { socket :: Socket
    , queue :: TQueue Message
    }

type Clients = Map.Map String Client
