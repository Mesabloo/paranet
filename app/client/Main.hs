{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Paranet.Common
import Parser

import Control.Concurrent (myThreadId, threadDelay, forkIO, yield, killThread)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import qualified Control.Concurrent.Async as T
import Control.Monad (unless, when, forever)
import Control.Monad.Parallel (forM_)

import Data.Aeson
import Data.Bits ((.|.))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Function (fix)

import Foreign.Hoppy.Runtime (withScopedPtr)

import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import Graphics.UI.Qtah.Core.Types (alignVCenter, alignHCenter)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import Graphics.UI.Qtah.Event (onEvent)
import Graphics.UI.Qtah.Gui.QCloseEvent (QCloseEvent)
import Graphics.UI.Qtah.Gui.QKeyEvent (QKeyEvent)
import qualified Graphics.UI.Qtah.Gui.QKeyEvent as QKeyEvent
import Graphics.UI.Qtah.Signal (connect_)

import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString.Lazy

import Options.Applicative

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random.Shuffle (shuffleM)

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
    tid <- myThreadId
    putStrLn ("<" <> show tid <> "> Trying to connect to " <> address <> ":" <> port <> "...")
    runTCPClient address port handle

clientID :: TVar Integer
clientID = unsafePerformIO (newTVarIO 0)

handle :: Socket -> IO ()
handle sock = do
    tid <- myThreadId
    serverAddr <- show <$> getPeerName sock

    putStrLn ("<" <> show tid <> "> Successfully connected to " <> serverAddr <> "!")

    Just cid <- decode @Integer <$> recv sock maxMessageSize
    atomically do
        writeTVar clientID cid

    -- spawn thread for listening to prevent from blocking the main thread
    lrid <- forkIO (loopReceive sock)
    lsid <- forkIO (loopSendFromQueue sock)

    -- loopSend sock

    generateUI sock

    killThread lrid
    killThread lsid

    putStrLn ("<" <> show tid <> "> Disconnected from " <> serverAddr <> ".")

queue :: TQueue Message
queue = unsafePerformIO newTQueueIO

loopReceive :: Socket -> IO ()
loopReceive sock = fix \loop -> do
    tid <- myThreadId

    msg <- recv sock maxMessageSize
    unless (BS.null msg) do
        -- putStrLn (BS.unpack msg)
        let (Just msgs) = decode @[Message] msg

        --unless (null msgs) do
        --    print msgs

        forM_ msgs (handleMessage sock)

        loop

loopSendFromQueue :: Socket -> IO ()
loopSendFromQueue sock = fix \f -> do
    threadDelay 50000  -- 50 ms
    msgs <- atomically do
        flushTQueue queue
    sendAll sock (encode @[Message] msgs)
    yield
    f

syncVar :: TChan (Expression, MessageID)
syncVar = unsafePerformIO newTChanIO

handleMessage :: Socket -> Message -> IO ()
handleMessage sock (GiveAndReceive from to expr mid) = do
    forkIO do
        evaluate sock expr mid (True, from)
        pure ()
        -- atomically do writeTQueue queue (Give from result mid)
    pure ()
handleMessage sock (Receive from expr mid) = do
    tid <- myThreadId
    putStrLn ("<" <> show tid <> "> " <> show expr <> " <~~ " <> from <> "#" <> show (nb mid) <> ":" <> show (sent mid))
    atomically do
        writeTChan syncVar (expr, mid)
handleMessage _ _ = pure ()

generateUI :: Socket -> IO ()
generateUI sock = withScopedPtr (QApplication.new ([] :: [String])) \_ -> do
    app <- QCoreApplication.getInstance

    clientAddr <- show <$> getSocketName sock

    window <- QWidget.new
    QWidget.setWindowTitle window ("Paranet client - " <> clientAddr :: String)
    QWidget.setMinimumWidth window 500
    onEvent window \(_ :: QCloseEvent) -> False <$ QCoreApplication.quit

    layout <- QVBoxLayout.new
    QWidget.setLayout window layout

    codeEditor <- QLineEdit.new
    QLineEdit.setPlaceholderText codeEditor ("Enter some code" :: String)
    QWidget.setStyleSheet codeEditor ("font-family: monospace;" :: String)
    result <- QLabel.new
    QLabel.setAlignment result (alignHCenter .|. alignVCenter)
    connect_ codeEditor QLineEdit.returnPressedSignal do
        code <- QLineEdit.text codeEditor
        case parsePara code of
            Left err -> do
                QMessageBox.critical window ("Parse error!" :: String) (show err)
                pure ()
            Right ex -> do
                tid <- myThreadId
                QLineEdit.setText codeEditor ("" :: String)
                QLabel.setText result ("Evaluating..." :: String)
                QLineEdit.setReadOnly codeEditor True
                forkIO do
                    cid <- readTVarIO clientID
                    e <- evaluate sock ex (MID cid 0) (False, "")
                    QLabel.setText result (pretty e)
                    putStrLn ("<" <> show tid <> "> Result: " <> show e)
                    QLineEdit.setReadOnly codeEditor False
                pure ()
       
    let lyt = QLayout.toQLayout layout
    QLayout.addWidget lyt codeEditor
    QLayout.addWidget lyt result

    QWidget.show window

    QCoreApplication.exec

uniqueID :: TVar Integer
uniqueID = unsafePerformIO (newTVarIO 0)

evaluate :: Socket -> Expression -> MessageID -> (Bool, String) -> IO Expression
evaluate sock e@(EInt i) mid (sendBack, to) = do
    when sendBack do
        atomically do
            writeTQueue queue (Give to e mid)
    pure e
evaluate sock (e1 :| e2) mid (sendBack, to) = do
    server <- show <$> getPeerName sock
    tid <- myThreadId

    (mid1, mid2) <- atomically do
        msg1 <- readTVar uniqueID
        modifyTVar uniqueID (+2)
        pure (mid{sent = msg1 + 1}, mid{sent = msg1 + 2})

    let msgs = [Send e1 mid1, Send e2 mid2]
    shuffled <- shuffleM msgs
    forM_ msgs \m@(Send e mid) -> do
        atomically do
            writeTQueue queue m
        putStrLn ("<" <> show tid <> "> " <> show e <> " ~~> " <> server <> "#" <> show (nb mid) <> ":" <> show (sent mid))

    expr <- atomically do
        (e, mid') <- peekTChan syncVar
        unless (nb mid == nb mid' && (sent mid' == sent mid1 || sent mid' == sent mid2)) do
            retry
        e <$ readTChan syncVar
    forkIO do
        atomically do
            (e, mid') <- peekTChan syncVar
            unless (nb mid == nb mid' && (sent mid' == sent mid1 || sent mid' == sent mid2)) do
                retry
            readTChan syncVar
        pure ()

    when sendBack do
        atomically do
            writeTQueue queue (Give to expr mid)
    pure expr
evaluate sock (e1 :~ e2) mid (sendBack, to) = do
    server <- show <$> getPeerName sock
    tid <- myThreadId

    (mid1, mid2) <- atomically do
        msg1 <- readTVar uniqueID
        modifyTVar uniqueID (+2)
        pure (mid{sent = msg1 + 1}, mid{sent = msg1 + 2})

    let msgs = [Send e1 mid1, Send e2 mid2]
    shuffled <- shuffleM msgs
    forM_ msgs \m@(Send e mid) -> do
        atomically do
            writeTQueue queue m
        putStrLn ("<" <> show tid <> "> " <> show e <> " ~~> " <> server <> "#" <> show (nb mid) <> ":" <> show (sent mid))

    (e1', mid1') <- atomically do
        r@(e, mid') <- peekTChan syncVar
        unless (nb mid == nb mid' && (sent mid' == sent mid1 || sent mid' == sent mid2)) do
            retry
        r <$ readTChan syncVar
    (e2', mid2') <- atomically do
        r@(e, mid') <- peekTChan syncVar
        unless (nb mid == nb mid' && (sent mid' == sent mid1 || sent mid' == sent mid2)) do
            retry
        r <$ readTChan syncVar

    let expr = if sent mid1' >= sent mid2' then e2' :& e1' else e1' :& e2'
    when sendBack do
        atomically do
            writeTQueue queue (Give to expr mid)
    pure expr
evaluate sock (e1 :& e2) mid (sendBack, to) = do
    server <- show <$> getPeerName sock
    tid <- myThreadId

    (mid1, mid2) <- atomically do
        msg1 <- readTVar uniqueID
        modifyTVar uniqueID (+2)
        pure (mid{sent = msg1 + 1}, mid{sent = msg1 + 2})

    atomically do
        writeTQueue queue (Send e1 mid1)
    putStrLn ("<" <> show tid <> "> " <> show e1 <> " ~~> " <> server <> "#" <> show (nb mid1) <> ":" <> show (sent mid1))
    e1' <- atomically do
        (e, mid') <- peekTChan syncVar
        unless (mid' == mid1) do
            retry
        e <$ readTChan syncVar

    atomically do
        writeTQueue queue (Send e2 mid2)
    putStrLn ("<" <> show tid <> "> " <> show e2 <> " ~~> " <> server <> "#" <> show (nb mid2) <> ":" <> show (sent mid2))
    e2' <- atomically do
        (e, mid') <- peekTChan syncVar
        unless (mid' == mid2) do
            retry
        e <$ readTChan syncVar

    let expr = e1' :& e2'
    when sendBack do
        atomically do
            writeTQueue queue (Give to expr mid)
    pure expr
evaluate sock e mid (sendBack, to) = do
    when sendBack do
        atomically do
            writeTQueue queue (Give to e mid)
    pure e
--evaluate sock (e1 :~ e2) (sendBack, to) = pure (EInt 404) -- TODO
--evaluate _ e _ _  = pure e
