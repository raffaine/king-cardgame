{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.IO
import System.Exit
import System.Environment

import KingClient
import System.ZMQ4.Monadic

import qualified Data.ByteString.Char8 as BS

---------- Declares the Player for this Application (KingBotAgent) ---------------
--
-- Agents are the Interface used to request actions for the game, if I go further
--  and have a GUI implementation of the game, it's throgh the functions
--  of the Typeclass KingPlayer that I will state what actions should be sent to server
-- This particular release uses an automated Player (not a good one)
--  and as such no interactions are expected
----------------------------------------------------------------------------------

data KingBotAgent = KingBotAgent
    {   username :: String
    ,   password :: String
    ,   botData  :: String
    }

getFilter :: [KingCard] -> KingRule -> [KingCard] -> (KingCard -> Bool)
getFilter cs RKing (c:_) = \cur -> (compareSuit cur c) || (cur == "KH")
getFilter _ _ (c:_) = compareSuit c
getFilter _ r [] = if r `elem` [RCopas, RKing] then not . (compareSuit "KH") else \_ -> True

compareSuit :: KingCard -> KingCard -> Bool
compareSuit (_:s1:_) (_:s2:_) = s1 == s2

firstValid :: KingRule -> [KingCard] -> [KingCard] -> KingCard
firstValid rule table cards = case cards' of
            (c:cs) -> c
            []     -> head cards
    where cards' = filter (getFilter cards rule table) cards

type KingAgentS z = StateT KingBotAgent (ZMQ z)
type KingAgentSR z = ReaderT KingGame (KingAgentS z)

-- Just Choose the First on the list (This may also return the current rule, but it's unexpected)
chooseRule :: KingBotAgent -> KingGame -> Maybe KingRule
chooseRule _ game = case gameHands game of
    ((KingHand (Left (r:_)) _ _ _):_) -> Just r
    ((KingHand (Right r) _ _ _):_) -> Just r
    otherwise -> Nothing

-- Chooses first valid card or Nothing if no play is possible
choosePlay :: KingBotAgent -> KingGame -> Maybe KingCard
choosePlay _ (KingGame _ cards _ _ _ ((KingHand (Right rule) tbCards _ _ ):_)) = Just $ firstValid rule tbCards cards
choosePlay _ _ = Nothing

updateAgentState :: (Sender s, Receiver s) => Socket z s -> ExpectedAction -> KingAgentSR z Bool
updateAgentState _ (KOver s) = do
    liftIO $ putStrLn $ "Game has ended: " ++ s
    return False

updateAgentState _ KWait = do
    liftIO $ putStrLn $ "Agent could use this time to update some more."
    return True

updateAgentState srv KTrump = do
    game <- ask
    lift $ lift $ executeActionS srv $ mkPlayStr game "GAME" $ Just "POSITIVA H"
    return True

updateAgentState srv KBid = do
    game <- ask
    lift $ lift $ executeActionS srv $ mkPlayStr game "BID" $ Just "0"
    return True

updateAgentState srv KDecide = do
    game <- ask
    lift $ lift $ executeActionS srv $ mkPlayStr game "DECIDE" $ Just "False"
    return True

updateAgentState srv KRule = do
    game <- ask
    agent <- get
    case chooseRule agent game of
        Nothing     -> return False
        (Just rule) -> do
                lift $ lift $ executeActionS srv $ mkPlayStr game "GAME" $ Just $ show rule
                return True

updateAgentState srv KPlay = do
    game <- ask
    agent <- get
    case choosePlay agent game of
        Nothing     -> return False
        (Just card) -> do
                lift $ lift $ executeActionS srv $ mkPlayStr game "PLAY" $ Just card
                return True
    
updateAgentState _ _ = pure True

-- Runs the game in a loop after initial socket setup
runGameS :: String -> String -> KingBotAgent -> IO ()
runGameS srv_addr sub_addr agent = runZMQ $ do
    srv <- socket Req
    connect srv srv_addr

    -- Before Joining is important to subscribe to that Table's channel to not miss any message
    info <- socket Sub
    connect info sub_addr

    -- TODO: There must be a better way for me to connect the game start with the other actions on the State Monad
    (suc, g) <- runStateT (startGame srv info (username agent) (password agent)) $ mkGame (Player "" "") "" ""
    when (suc == False) $ do
        liftIO $ putStrLn "Error during game setup."
        return ()

    -- This is the Game Loop, once we return from here, game is over and state is destroyed
    loop info srv agent g
    where
        loop info srv agent game = do
            (action, game') <- runStateT (updateGame srv info 100) game
            (end, agent')   <- runStateT (runReaderT (updateAgentState srv action) game') agent
            --end <- evalStateT (updateAgent agent srv action) game'
            liftIO $ putStrLn $ show game'
            case end of
                False   -> return ()
                True    -> loop info srv agent' game'

-- Launchs an Agent with given username and password
main :: IO ()
main = do
    -- Capture Arguments, we expect 2 now, username and password
    args <- getArgs
    when (length args /= 2) $ do
        hPutStrLn stderr "usage: king-game-client-exe <usrname> <password>"
        exitFailure
    let usrname = head args
        passwrd = args !! 1
        king_srv_addr = "tcp://localhost:5555"
        king_sub_addr = "tcp://localhost:5556"

    runGameS  king_srv_addr king_sub_addr $ KingBotAgent usrname passwrd ""
