{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad
import System.IO
import System.Exit
import System.Environment

import KingClient

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

instance KingPlayer KingBotAgent where
    getName :: KingBotAgent -> String
    getName a = username a

    getPassword :: KingBotAgent -> String
    getPassword a = password a
    
    wantCreateTable :: KingBotAgent -> Bool
    wantCreateTable _ = True

    wantJoinTable :: KingBotAgent -> KingTable -> Bool
    wantJoinTable _ _ = True

    -- Always chooses Hearts (for now)
    chooseTrumpSuit :: KingBotAgent -> KingGame -> Maybe KingSuit
    chooseTrumpSuit _ _ = Just 'H'

    -- Always refuses
    chooseDecision :: KingBotAgent -> KingGame -> Maybe Bool
    chooseDecision _ _ = Just False

    -- Always forefeits
    chooseBid :: KingBotAgent -> KingGame -> Maybe Int
    chooseBid _ _ = Just 0

    -- Chooses first valid card or Nothing if no play is possible
    choosePlay :: KingBotAgent -> KingGame -> Maybe KingCard
    choosePlay _ (KingGame _ cards _ _ _ ((KingHand (Right rule) tbCards _ _ ):_)) = Just $ firstValid rule tbCards cards
    choosePlay _ _ = Nothing

    -- Just Choose the First on the list (This may also return the current rule, but it's unexpected)
    chooseRule :: KingBotAgent -> KingGame -> Maybe KingRule
    chooseRule _ game = case gameHands game of
        ((KingHand (Left (r:_)) _ _ _):_) -> Just r
        ((KingHand (Right r) _ _ _):_) -> Just r
        otherwise -> Nothing


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

    runGame king_srv_addr king_sub_addr $ KingBotAgent usrname passwrd
