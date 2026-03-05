{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.State

import Control.Concurrent.STM
import Control.Concurrent.Async (concurrently_)
import System.IO
import System.Exit
import System.Environment
import qualified Data.ByteString.Char8 as BS
import System.ZMQ4.Monadic
import Data.List (find)

import KingClient
import KingTypes
import GameRules (isValidPlay)

----------------------------------------------------------------------------------
-- 1. The STM Communication Bridge
----------------------------------------------------------------------------------
data BotBrain = BotBrain
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
runAgentThread :: ContextAwareAgent a => BotBrain -> a -> IO ()
runAgentThread brain initialState = loop initialState
  where
    loop agentState = do
        (action, game) <- atomically $ do
            act <- takeTMVar (envActionReq brain)
            g   <- readTVar (envGameState brain)
            return (act, g)
            
        (decisionStr, newAgentState) <- decideAction action game agentState
        
        atomically $ putTMVar (envActionRsp brain) decisionStr
        loop newAgentState

-- | The Network Thread: Handles all ZMQ traffic and state updates.
networkLoop :: (Sender s, Receiver s, Receiver r) => Socket z r -> Socket z s -> BotBrain -> KingGame -> ZMQ z ()
networkLoop info srv brain game = do
    (action, game') <- runStateT (updateGame srv info 100) game
    
    -- Sync objective game state to the bridge
    liftIO $ atomically $ writeTVar (envGameState brain) game'
    
    case action of
        KOver msg -> liftIO $ putStrLn $ "Game has ended: " ++ msg
        KWait     -> networkLoop info srv brain game'
        _         -> do
            -- Request a decision from the Agent thread
            liftIO $ atomically $ putTMVar (envActionReq brain) action
            
            -- Wait for the Agent to reply
            decisionStr <- liftIO $ atomically $ takeTMVar (envActionRsp brain)
            
            -- Execute and recurse
            _ <- executeActionS srv decisionStr
            networkLoop info srv brain game'

runGameS :: ContextAwareAgent a => String -> String -> String -> String -> a -> IO ()
runGameS srv_addr sub_addr usrname passwrd initialAgent = do
    -- Initialize the STM Bridge
    brain <- atomically $ do
        gState <- newTVar (mkGame (Player usrname "") "" "")
        req    <- newEmptyTMVar
        rsp    <- newEmptyTMVar
        return $ BotBrain gState req rsp

    -- Run both threads simultaneously
    concurrently_ 
        (runAgentThread brain initialAgent)
        (runZMQ $ do
            srv <- socket Req
            connect srv srv_addr

            info <- socket Sub
            connect info sub_addr

            (suc, g) <- runStateT (startGame srv info usrname passwrd) (mkGame (Player usrname "") "" "")
            if not suc
                then liftIO $ putStrLn "Error during game setup."
                else networkLoop info srv brain g
        )

----------------------------------------------------------------------------------
-- 4. A Concrete Agent Implementation (Replacing KingBotAgent)
----------------------------------------------------------------------------------
data SimpleBot = SimpleBot { sName :: String }

instance ContextAwareAgent SimpleBot where
    decideAction action game state = do
        let decisionStr = case action of
                KTrump  -> mkPlayStr game "GAME" (Just "POSITIVA H")
                KBid    -> mkPlayStr game "BID" (Just "0")
                KDecide -> mkPlayStr game "DECIDE" (Just "False")
                KRule   -> pickRule game
                KPlay   -> pickCard game
                _       -> ""
        return (decisionStr, state)

-- Helper to safely pick the first available valid card using the new GameRules
pickCard :: KingGame -> BS.ByteString
pickCard game = 
    let hand  = map parseCard $ roundCards game
        hands = gameHands game
    in case hands of
        (KingHand (Right rule) tableCards _ _ : _) -> 
            let table = map parseCard tableCards
            in case find (isValidPlay rule table hand) hand of
                Just validCard -> mkPlayStr game "PLAY" (Just $ unparseCard validCard)
                Nothing        -> mkPlayStr game "PLAY" (Just $ unparseCard $ head hand) -- Fallback
        _ -> mkPlayStr game "PLAY" (Just $ unparseCard $ head hand)

-- Helper to pick the first available rule
pickRule :: KingGame -> BS.ByteString
pickRule game = 
    case gameHands game of
        (KingHand (Left (r:_)) _ _ _ : _) -> mkPlayStr game "GAME" (Just $ show r)
        _ -> mkPlayStr game "GAME" (Just "POSITIVA")


main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ do
        hPutStrLn stderr "usage: king-game-client-exe <usrname> <password>"
        exitFailure
    let usrname = head args
        passwrd = args !! 1
        king_srv_addr = "tcp://localhost:5555"
        king_sub_addr = "tcp://localhost:5556"

    runGameS king_srv_addr king_sub_addr usrname passwrd (SimpleBot usrname)
