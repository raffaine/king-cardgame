module BaseClient
    ( ClientEnv(..)
    , ContextAwareAgent(..)
    , runAgentThread
    , networkLoop
    , runGameS
    )
 where

import Control.Monad.State ( StateT(runStateT), MonadIO(liftIO) )

import Control.Concurrent.STM
    ( TVar,
      atomically,
      newTVar,
      readTVar,
      writeTVar,
      newEmptyTMVar,
      putTMVar,
      takeTMVar,
      TMVar )
import Control.Concurrent.Async (concurrently_)
import System.IO ()
import System.Exit ()
import System.Environment ()
import qualified Data.ByteString.Char8 as BS
import System.ZMQ4.Monadic
    ( ZMQ,
      Sender,
      Receiver,
      Socket,
      connect,
      runZMQ,
      socket,
      Req(Req),
      Sub(Sub) )

import KingClient (KingGame, Player(..), ExpectedAction(..), executeActionS, mkGame, startGame, updateGame)

----------------------------------------------------------------------------------
-- 1. The STM Communication Bridge
----------------------------------------------------------------------------------
data ClientEnv = ClientEnv
    { envGameState   :: TVar KingGame
    , envActionReq   :: TMVar ExpectedAction
    , envActionRsp   :: TMVar BS.ByteString
    }

----------------------------------------------------------------------------------
-- 2. The Extensible Agent Typeclass
----------------------------------------------------------------------------------
class ContextAwareAgent a where
    -- | Called when the server expects an action (KPlay, KRule, etc.)
    -- Returns the formulated ZMQ command and the updated internal agent state.
    decideAction :: ExpectedAction -> KingGame -> a -> IO (BS.ByteString, a)

----------------------------------------------------------------------------------
-- 3. The Thread Runners
----------------------------------------------------------------------------------

-- | The Agent Thread: Blocks until the network asks for a decision, then thinks.
runAgentThread :: ContextAwareAgent a => ClientEnv -> a -> IO ()
runAgentThread env = loop
  where
    loop agentState = do
        (action, game) <- atomically $ do
            act <- takeTMVar (envActionReq env)
            g   <- readTVar (envGameState env)
            return (act, g)

        (decisionStr, newAgentState) <- decideAction action game agentState

        atomically $ putTMVar (envActionRsp env) decisionStr
        loop newAgentState

-- | The Network Thread: Handles all ZMQ traffic and state updates.
networkLoop :: (Sender s, Receiver s, Receiver r) => Socket z r -> Socket z s -> ClientEnv -> KingGame -> ZMQ z ()
networkLoop info srv env game = do
    (action, game') <- runStateT (updateGame srv info 100) game

    -- Sync objective game state to the bridge
    liftIO $ atomically $ writeTVar (envGameState env) game'

    case action of
        KOver msg -> liftIO $ putStrLn $ "Game has ended: " ++ msg
        KWait     -> networkLoop info srv env game'
        _         -> do
            -- Request a decision from the Agent thread
            liftIO $ atomically $ putTMVar (envActionReq env) action

            -- Wait for the Agent to reply
            decisionStr <- liftIO $ atomically $ takeTMVar (envActionRsp env)

            -- Execute and recurse
            _ <- executeActionS srv decisionStr
            networkLoop info srv env game'

runGameS :: ContextAwareAgent a => String -> String -> String -> String -> a -> IO ()
runGameS srv_addr sub_addr usrname passwrd initialAgent = do
    -- Initialize the STM Bridge
    env <- atomically $ do
        gState <- newTVar (mkGame (Player usrname "") "" "")
        req    <- newEmptyTMVar
        rsp    <- newEmptyTMVar
        return $ ClientEnv gState req rsp

    -- Run both threads simultaneously
    concurrently_
        (runAgentThread env initialAgent)
        (runZMQ $ do
            srv <- socket Req
            connect srv srv_addr

            info <- socket Sub
            connect info sub_addr

            (suc, g) <- runStateT (startGame srv info usrname passwrd) (mkGame (Player usrname "") "" "")
            if not suc
                then liftIO $ putStrLn "Error during game setup."
                else networkLoop info srv env g
        )
