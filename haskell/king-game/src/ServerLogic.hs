{-# LANGUAGE FlexibleContexts #-}

module ServerLogic
    ( ServerContext
    , GamePhase(..)
    , ServerTable(..)
    , MonadGameEnv(..)
    , emptyServerContext
    , handleCommand
    ) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, find, elemIndex)
import Data.Maybe (fromMaybe)
import Control.Monad (when)

import KingTypes (KingRule(..), KingCard, readRule, isPositiva, allNegativeRules, startingRules)
import GameRules (evaluateTrick, isValidPlay, isHandComplete)
import KingClient (KingGame(secret))

----------------------------------------------------------------------
-- The Typeclass (Environment Interface)
----------------------------------------------------------------------
class Monad m => MonadGameEnv m where
    generateId   :: m String         -- For Auth channels, Table IDs, Secrets
    generateDeck :: m [[KingCard]]   -- Always returns 4 lists of 13 cards

----------------------------------------------------------------------
-- The Data Types
----------------------------------------------------------------------

-- | Defines the exact state of the table
data GamePhase
    = Lobby                          -- < 4 players
    | WaitingForRule                 -- Waiting for the hand starter to pick a game
    | Bidding [(String, Int)]        -- Positiva bidding history
    | PlayingTrick [(String, KingCard)]  -- Cards played in the current trick
    | GameOver
    deriving (Show, Eq)

-- | Represents the `User` class in Python
data ServerUser = ServerUser
    { uName    :: String
    , uChannel :: String
    } deriving (Show, Eq)

data ServerPlayer = ServerPlayer
    { pName   :: String
    , pSecret :: String
    } deriving (Show, Eq)

-- | Represents the `KingTable` class in Python
data ServerTable = ServerTable
    { tName           :: String
    , tPlayers        :: [ServerPlayer]
    , tPhase          :: GamePhase
    , tActiveTurn     :: Int                           -- Index (0-3) of whose turn it is
    , tHandStarter    :: Int                           -- Index (0-3) of who starts the hand
    , tPlayedHands    :: [(String, KingRule)]          -- History of played hands (only rules for now)
    , tHands          :: Map.Map String [KingCard]     -- The Dealt Cards
    , tHandScores     :: [Int]                         -- Tracks current points [P0, P1, P2, P3]
    , tTotalScores    :: [Int]                         -- Tracls Total accumulated points
    } deriving (Show, Eq)

-- | Replaces global dicts g_users, g_tables, g_players
data ServerContext = ServerContext
    { scUsers  :: Map.Map String ServerUser
    , scTables :: Map.Map String ServerTable
    } deriving (Show, Eq)

-- | Calculates the available rules for a specific player based on the table's history log
availableRulesForPlayer :: [(String, KingRule)] -> String -> [KingRule]
availableRulesForPlayer history playerName =
    let -- 1. Identify which negative rules have been played by ANYONE
        playedNegatives = [ r | (_, r) <- history, not (isPositiva r) ]
        availableNegatives = filter (`notElem` playedNegatives) allNegativeRules

        -- 2. Check if THIS player has already called a Positiva
        hasPlayedPositiva = any (\(p, r) -> p == playerName && isPositiva r) history

        allPositivas = [RPositiva] -- They all "show" as POSITIVA, so just send one [RPositivaH, RPositivaC, RPositivaS, RPositivaD]

    in if hasPlayedPositiva
       then if null availableNegatives
            then allPositivas     -- Forced to pick Positiva because no negatives are left
            else availableNegatives -- Must pick an available negative, Positiva is locked
       else availableNegatives ++ allPositivas -- Can pick anything

-- | General helper for Either pipelines
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err = maybe (Left err) Right

emptyServerContext :: ServerContext
emptyServerContext = ServerContext Map.empty Map.empty

-- | Checks if a user is authorized based on their name and channel.
isAuthorized :: ServerContext -> String -> String -> Bool
isAuthorized ctx name channel =
    case Map.lookup name (scUsers ctx) of
        Just user -> uChannel user == channel
        Nothing   -> False

-- | Retrieves a Table given its ID
getTable :: ServerContext -> String -> Maybe ServerTable
getTable ctx tId = Map.lookup tId (scTables ctx)

-- | Given an Authorized User, attempt to join a table by its ID and Either Errors or Returns secret and new Server Context
joinTable :: ServerContext -> String -> String -> String -> Either String (String, ServerContext)
joinTable ctx name injectedId tId = do
    table <- maybeToEither "ERROR Table does not exist" (getTable ctx tId)
    let currentPlayers = tPlayers table
        alreadyInTable = name `elem` map pName currentPlayers
    if alreadyInTable
        then Left "ERROR User already in table"
        else if length currentPlayers >= 4
            then Left "ERROR Table is already full"
            else let newTable = table { tPlayers = currentPlayers ++ [ServerPlayer name injectedId] }
                     ctx' = ctx { scTables = Map.insert tId newTable (scTables ctx) }
                 in Right (injectedId, ctx')

-- | Attempts to start a Table given its ID
startTable :: ServerContext -> String -> Maybe (ServerTable, ServerContext)
startTable ctx tId = do
    table <- getTable ctx tId
    if length (tPlayers table) == 4
        then
            let pNames = map pName (tPlayers table)
                updatedTable = table { tPhase = WaitingForRule }
                ctx' = ctx { scTables = Map.insert tId updatedTable (scTables ctx) }
            in Just (updatedTable, ctx')
        else Nothing

-- | Retrieves a list of the Player's Names on the table, in the order they are sit.
getNamesOnTable :: ServerContext -> String -> [String]
getNamesOnTable ctx tId = maybe [] (map pName . tPlayers) (getTable ctx tId)

getNamesOnTable' :: ServerTable -> [String]
getNamesOnTable' = map pName . tPlayers

-- | Finds the table and player info for a given user
findUserTable :: ServerContext -> String -> String -> Maybe (String, ServerTable)
findUserTable ctx usr sec =
    let tablesWithUser = Map.toList $ Map.filter (\t -> ServerPlayer usr sec `elem` tPlayers t) (scTables ctx)
    in case tablesWithUser of
        (tId, table):_ -> Just (tId, table)
        [] -> Nothing

-- | Validates ONLY that the user is at the table and using the correct secret.
-- Turn validation is left to the specific commands.
validateTableMembership :: ServerContext -> String -> String -> Either String (String, ServerTable, Int)
validateTableMembership ctx usr sec = do
    (tId, table) <- maybeToEither "ERROR User not at table" (findUserTable ctx usr sec)
    let pIndex = fromMaybe 0 (usr `elemIndex` map pName (tPlayers table))
    Right (tId, table, pIndex)

-- | Helper to add score to an index of the given array
addScore :: Int -> Int -> [Int] -> [Int]
addScore targetIdx points = zipWith (\idx currentScore -> if idx == targetIdx then currentScore + points else currentScore) [0..3]

----------------------------------------------------------------------
-- | The core pure state machine.
-- Takes: Context -> InjectedID -> CommandWords
-- Returns: (DirectReply, OptionalBroadcast, NewContext)
----------------------------------------------------------------------
handleCommand :: MonadGameEnv m => ServerContext -> [String] -> m (String, [String], ServerContext)

----------------------------------------------------------------------
-- AUTHORIZE <username> <password>
----------------------------------------------------------------------
-- Note: In final system, password check against a DB here.
-- For this pure state, we trust the auth and generate the user.
handleCommand ctx ["AUTHORIZE", usr, _pwd] = do
    channel <- generateId
    let newUser = ServerUser usr channel
        ctx' = ctx { scUsers = Map.insert usr newUser (scUsers ctx) }
    return (channel, [], ctx')

----------------------------------------------------------------------
-- TABLE <username> <channel>
----------------------------------------------------------------------
handleCommand ctx ["TABLE", usr, chan]
    | not (isAuthorized ctx usr chan) = pure ("ERROR User not authorized", [], ctx)
    | otherwise = do
        tId <- generateId
        -- Python's server.py creates the table but does NOT automatically add the user to it.
        let newTable = ServerTable
                    { tName = tId
                    , tPlayers = []
                    , tPhase = Lobby
                    , tActiveTurn = 0
                    , tHandStarter = 0
                    , tPlayedHands = []
                    , tHands = Map.empty
                    , tHandScores = [0, 0, 0, 0]
                    , tTotalScores = [0, 0, 0, 0]
                    }
            ctx' = ctx { scTables = Map.insert tId newTable (scTables ctx) }
        return (tId, [], ctx')

----------------------------------------------------------------------
-- JOIN <username> <channel> <table_id>
----------------------------------------------------------------------
handleCommand ctx ["JOIN", usr, chan, tId]
    | not (isAuthorized ctx usr chan) = pure ("ERROR User not authorized", [], ctx)
    | otherwise = do
        secret <- generateId
        case joinTable ctx usr secret tId of
            Left error -> return (error, [], ctx)
            -- Join is good, now attempt to start the table if we have 4 players
            Right (secret, ctx') -> case startTable ctx' tId of
                Nothing               -> return (secret, [], ctx') -- Not enough, but the join was successful
                Just (table, ctx'') -> do
                    initialDeck <- generateDeck
                    let handsMap = Map.fromList $ zip (map pName (tPlayers table)) initialDeck
                        updatedT = table { tHands = handsMap }
                        finalCtx = ctx'' { scTables = Map.insert tId updatedT (scTables ctx'') }
                        tablePlayers = getNamesOnTable' table   -- TODO needs to be shuffled so that the hand starter is first in the list (turn has to be set as well to match)
                        msgTableOrder = tId ++ " START " ++ unwords tablePlayers
                        msgInitialHand = tId ++ " STARTHAND " ++ head tablePlayers ++ " " ++ unwords (map show startingRules)

                    return (secret, [msgTableOrder, msgInitialHand], finalCtx)

----------------------------------------------------------------------
-- GETHAND <username> <secret>
----------------------------------------------------------------------
handleCommand ctx ["GETHAND", usr, sec] = do
    case validateTableMembership ctx usr sec of
        Left err -> return (err, [], ctx)
        Right (_, table, _) ->
            let hand = Map.findWithDefault [] usr (tHands table)
                -- Converts ["2H", "3C"] to valid JSON string '["2H", "3C"]'
                jsonHand = "[" ++ intercalate ", " (map (\c -> "\"" ++ c ++ "\"") hand) ++ "]"
            in return (jsonHand, [], ctx)

----------------------------------------------------------------------
-- GAME <username> <secret> <rule_string>
----------------------------------------------------------------------
handleCommand ctx ["GAME", usr, sec, ruleStr] = do
    case validateTableMembership ctx usr sec of
        Left err -> return (err, [], ctx)
        Right (tId, table, pIndex) ->
            if tPhase table /= WaitingForRule
            then return ("ERROR Game is not waiting for a rule", [], ctx)
            else if tActiveTurn table /= pIndex
            then return ("ERROR Not your turn to choose a rule", [], ctx)
            else
                let rule = readRule ruleStr
                    allowedRules = availableRulesForPlayer (tPlayedHands table) usr
                in if rule `notElem` allowedRules
                then return ("ERROR Rule not available for this player at this time", [], ctx)
                else
                    let newPhase = PlayingTrick [] --if isPositiva rule then Bidding [] else PlayingTrick []
                        updatedTable = table
                            { tPhase = newPhase
                            , tPlayedHands = tPlayedHands table ++ [(usr, rule)]
                            }
                        ctx' = ctx { scTables = Map.insert tId updatedTable (scTables ctx) }
                        bcast = [tId ++ " GAME " ++ ruleStr]
                    in return ("ACK", bcast, ctx')

----------------------------------------------------------------------
-- PLAY <username> <secret> <card_string>
----------------------------------------------------------------------
handleCommand ctx ["PLAY", usr, sec, cardStr] = do
    case validateTableMembership ctx usr sec of
        Left err -> return (err, [], ctx)
        Right (tId, table, pIndex) -> do
            case tPhase table of
                PlayingTrick trickCards -> do
                    -- 1. Turn Validation
                    if tActiveTurn table /= pIndex
                    then return ("ERROR Not your turn", [], ctx)
                    else do
                        -- 2. Hand Validation (Do they actually have the card?)
                        let hand = Map.findWithDefault [] usr (tHands table)
                        if cardStr `notElem` hand
                        then return ("ERROR Card not in hand", [], ctx)
                        else do
                            -- 3. GameRules Validation (Must follow suit, etc.)
                            -- The active rule is the last one appended to the op-log
                            let currentRule = snd $ last (tPlayedHands table)
                                tableCards = map snd trickCards

                            if not (isValidPlay currentRule tableCards hand cardStr)
                            then return ("ERROR Invalid play: must follow suit", [], ctx)
                            else do
                                -- 4. State Update (Remove card from hand, add to table)
                                let newHand = filter (/= cardStr) hand
                                    newHandsMap = Map.insert usr newHand (tHands table)
                                    newTrickCards = trickCards ++ [(usr, cardStr)]

                                    isTrickOver = length newTrickCards == 4

                                if not isTrickOver
                                then do
                                    -- Trick continues, just advance the turn
                                    let nextTurn = (tActiveTurn table + 1) `mod` 4
                                        updatedTable = table
                                            { tPhase = PlayingTrick newTrickCards
                                            , tActiveTurn = nextTurn
                                            , tHands = newHandsMap
                                            }
                                        ctx' = ctx { scTables = Map.insert tId updatedTable (scTables ctx) }
                                        bcast = [ tId ++ " PLAY " ++ cardStr
                                                , tId ++ " TURN " ++ getNamesOnTable' updatedTable !! nextTurn
                                                ]
                                    return ("ACK", bcast, ctx')
                                else do
                                    -- 5. Trick Evaluation (4 cards played)
                                    let playedCards = map snd newTrickCards
                                        -- Calculate the round number (1-13) based on cards remaining
                                        roundNum = 14 - length newHand

                                        (winRelIdx, score) = evaluateTrick currentRule roundNum playedCards
                                        winnerName = fst (newTrickCards !! winRelIdx)
                                        winnerIdx = fromMaybe 0 (winnerName `elemIndex` map pName (tPlayers table))
                                        -- Add the trick's score to the winner's tally
                                        updatedScores = addScore winnerIdx score (tHandScores table)
                                        endRoundBcast = tId ++ " ENDROUND " ++ winnerName ++ " " ++ show score

                                        updatedTable = table
                                            { tPhase = PlayingTrick [] -- Clear the trick for the next round
                                            , tActiveTurn = winnerIdx  -- Winner leads the next trick
                                            , tHands = newHandsMap
                                            , tHandScores = updatedScores
                                            }
                                        ctx' = ctx { scTables = Map.insert tId updatedTable (scTables ctx) }

                                        bcast = [ tId ++ " PLAY " ++ cardStr
                                                , endRoundBcast
                                                ]
                                        -- Calculate the global remaining pool and check if hand ends
                                        remainingPool = concat (Map.elems newHandsMap)
                                        handEnded = isHandComplete currentRule remainingPool

                                    if not handEnded
                                    then return ("ACK", bcast, ctx') -- Returns ENDROUND sequence only
                                    else do
                                        let newTotalScores = zipWith (+) (tTotalScores table) updatedScores                                            
                                            endHandBCast  = tId ++ " ENDHAND " ++ "[" ++ unwords (map show updatedScores) ++ "]"
                                            isGameOver = length (tPlayedHands table) == 10 -- Game Finishes after 10 hands

                                        if isGameOver
                                            then do
                                                -- 10 hands complete. Calculate the global winner!
                                                let maxScore = maximum newTotalScores                                                
                                                    totalScoreStr = "[" ++ unwords (map show newTotalScores) ++ "]"
                                                    winnerGlobalIdx = fromMaybe 0 (maxScore `elemIndex` newTotalScores)
                                                    globalWinnerName = pName (tPlayers table !! winnerGlobalIdx)
                                                    endGameBCast = tId ++ " GAMEOVER " ++ globalWinnerName ++ " " ++ totalScoreStr
                                                    
                                                    updatedTable = table
                                                        { tPhase = GameOver
                                                        , tHandScores = updatedScores
                                                        , tTotalScores = newTotalScores
                                                        , tHands = Map.empty -- Empty the table
                                                        }
                                                    ctx'' = ctx' { scTables = Map.insert tId updatedTable (scTables ctx') }
                                                    
                                                    bcasts = [ tId ++ " PLAY " ++ cardStr
                                                            , endRoundBcast
                                                            , endHandBCast
                                                            , endGameBCast
                                                            ]
                                                return ("ACK", bcasts, ctx'')
                                            else do
                                                newDeck <- generateDeck
                                                let nextStarterIdx = (tHandStarter updatedTable + 1) `mod` 4
                                                    nextStarterName = getNamesOnTable' finalTable !! nextStarterIdx
                                                    availableRules = availableRulesForPlayer (tPlayedHands finalTable) nextStarterName                                                    
                                                    msgStartHand = tId ++ " STARTHAND " ++ nextStarterName ++ " " ++ unwords (map show availableRules)

                                                    finalTable = updatedTable
                                                        { tPhase = WaitingForRule
                                                        , tHandStarter = nextStarterIdx
                                                        , tActiveTurn = nextStarterIdx
                                                        , tHandScores = [0, 0, 0, 0]
                                                        , tTotalScores = newTotalScores
                                                        , tHands = Map.fromList $ zip (map pName (tPlayers table)) newDeck
                                                        }
                                                    ctx'' = ctx' { scTables = Map.insert tId finalTable (scTables ctx') }

                                                    -- Prepare STARTHAND message for the next hand
                                                    bcast'= bcast ++ [ endHandBCast, msgStartHand ]
                                                return ("ACK", bcast', ctx'')

                _ -> return ("ERROR Game is not in trick-playing phase", [], ctx)

----------------------------------------------------------------------
-- FALLBACK
----------------------------------------------------------------------
handleCommand ctx _ = pure ("ERROR Invalid or unrecognized command", [], ctx)
