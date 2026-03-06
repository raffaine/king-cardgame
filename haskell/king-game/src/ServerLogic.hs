module ServerLogic
    ( ServerContext
    , GamePhase(..)
    , ServerTable(..)
    , emptyServerContext
    , handleCommand
    , hasUser
    , hasTable
    , isAuthorized
    , getTable
    , scTables
    ) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate)

import KingTypes (KingRule(..), Card, isPositiva, allNegativeRules, startingRules)

-- | Defines the exact state of the table
data GamePhase
    = Lobby                          -- < 4 players
    | WaitingForRule               -- Waiting for the hand starter to pick a game
    | Bidding [(String, Int)]        -- Positiva bidding history
    | PlayingTrick [(String, Card)]  -- Cards played in the current trick
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
        
        allPositivas = [RPositiva, RPositivaH, RPositivaC, RPositivaS, RPositivaD]
        
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

-- | Helper for tests
hasUser :: ServerContext -> String -> Bool
hasUser ctx name = Map.member name (scUsers ctx)

-- | Helper for tests
hasTable :: ServerContext -> String -> Bool
hasTable ctx tId = Map.member tId (scTables ctx)


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
startTable :: ServerContext -> String -> Maybe (String, ServerContext)
startTable ctx tId =
    let numPlayers = maybe 0 (length . tPlayers) (getTable ctx tId)
    in if numPlayers == 4
       then Just (tId, ctx) -- TODO update the context to mark the table as started or move players to a game state
       else Nothing

-- | Retrieves a list of the Player's Names on the table, in the order they are sit.
getNamesOnTable :: ServerContext -> String -> [String]
getNamesOnTable ctx tId = maybe [] (map pName . tPlayers) (getTable ctx tId)

-- | The core pure state machine.
-- Takes: Context -> InjectedID -> CommandWords
-- Returns: (DirectReply, OptionalBroadcast, NewContext)
handleCommand :: ServerContext -> String -> [String] -> (String, Maybe String, ServerContext)

----------------------------------------------------------------------
-- AUTHORIZE <username> <password>
----------------------------------------------------------------------
-- Note: In final system, password check against a DB here.
-- For this pure state, we trust the auth and generate the user.
handleCommand ctx injectedId ["AUTHORIZE", usr, _pwd] = 
    let newUser = ServerUser usr injectedId
        ctx' = ctx { scUsers = Map.insert usr newUser (scUsers ctx) }
    in (injectedId, Nothing, ctx')

----------------------------------------------------------------------
-- TABLE <username> <channel>
----------------------------------------------------------------------
handleCommand ctx injectedId ["TABLE", usr, chan]
    | not (isAuthorized ctx usr chan) = ("ERROR User not authorized", Nothing, ctx)
    | otherwise =
        -- Python's server.py creates the table but does NOT automatically add the user to it.
        let newTable = ServerTable injectedId [] Lobby 0 0 []
            ctx' = ctx { scTables = Map.insert injectedId newTable (scTables ctx) }
        in (injectedId, Nothing, ctx')

----------------------------------------------------------------------
-- JOIN <username> <channel> <table_id>
----------------------------------------------------------------------
handleCommand ctx injectedId ["JOIN", usr, chan, tId]
    | not (isAuthorized ctx usr chan) = ("ERROR User not authorized", Nothing, ctx)
    | otherwise =
        case joinTable ctx usr injectedId tId of
            -- Join is good, now attempt to start the table if we have 4 players
            Right (secret, ctx') -> case startTable ctx' tId of
                Just (tableId, ctx'') -> (secret, Just $ tableId ++ " START " ++ unwords (getNamesOnTable ctx'' tId), ctx'')
                Nothing               -> (secret, Nothing, ctx') -- Not enough, but the join was successful
            Left error -> (error, Nothing, ctx)

----------------------------------------------------------------------
-- FALLBACK
----------------------------------------------------------------------
handleCommand ctx _ _ = ("ERROR Invalid or unrecognized command", Nothing, ctx)
