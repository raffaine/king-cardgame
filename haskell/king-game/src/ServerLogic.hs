module ServerLogic
    ( ServerContext
    , emptyServerContext
    , handleCommand
    , hasUser
    , hasTable
    ) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate)

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
    { tName    :: String
    , tPlayers :: [ServerPlayer]
    } deriving (Show, Eq)

-- | Replaces global dicts g_users, g_tables, g_players
data ServerContext = ServerContext
    { scUsers  :: Map.Map String ServerUser
    , scTables :: Map.Map String ServerTable
    } deriving (Show, Eq)

emptyServerContext :: ServerContext
emptyServerContext = ServerContext Map.empty Map.empty

-- | Helper for tests
hasUser :: ServerContext -> String -> Bool
hasUser ctx name = Map.member name (scUsers ctx)

-- | Helper for tests
hasTable :: ServerContext -> String -> Bool
hasTable ctx tId = Map.member tId (scTables ctx)


-- | Checks if a user is authorized based on their name and channel.
hasAuthorizedUser :: ServerContext -> String -> String -> Bool
hasAuthorizedUser ctx name channel =
    case Map.lookup name (scUsers ctx) of
        Just user -> uChannel user == channel
        Nothing   -> False

-- | Retrieves a Table given its ID
getTable :: ServerContext -> String -> Maybe ServerTable
getTable ctx tId = Map.lookup tId (scTables ctx)

-- | Given an Authorized User, attempt to join a table by its ID and Either Errors or Returns secret and new Server Context
joinTable :: ServerContext -> String -> String -> String -> Either String (String, ServerContext)
joinTable ctx name injectedId tId =
    case getTable ctx tId of
        Just table ->
            let currentPlayers = tPlayers table
                -- Check if the user is already in the table
                alreadyInTable = name `elem` map pName currentPlayers
            in if not alreadyInTable
                then if length currentPlayers < 4
                    then
                        let newTable = table { tPlayers = tPlayers table ++ [ServerPlayer name injectedId] }
                            ctx' = ctx { scTables = Map.insert tId newTable (scTables ctx) }
                        in Right (injectedId, ctx')
                    else Left "ERROR Table is already full"
                else Left "ERROR User already in table"
        Nothing    -> Left "ERROR Table does not exist"

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
handleCommand ctx injectedId cmd =
    case cmd of
        ["JOIN", name, channel, tId] ->
            if hasAuthorizedUser ctx name channel then
                case joinTable ctx name injectedId tId of
                    Right (secret, ctx') -> case startTable ctx' tId of
                        Just (tableId, ctx'') -> (secret, Just $ tableId ++ " START " ++ unwords (getNamesOnTable ctx'' tId), ctx'')
                        Nothing               -> (secret, Nothing, ctx')
                    Left error -> (error, Nothing, ctx)
            else ("ERROR User not authorized", Nothing, ctx)
        ["AUTHORIZE", name, pwd] ->  -- For simplicity we ignore pwd checks
            let newUser = ServerUser name injectedId
                ctx' = ctx { scUsers = Map.insert name newUser (scUsers ctx) }
            in (injectedId, Nothing, ctx')
        ["TABLE", name, channel] ->
            if hasAuthorizedUser ctx name channel then
                 let newTable = ServerTable injectedId [ServerPlayer name injectedId]
                     ctx' = ctx { scTables = Map.insert injectedId newTable (scTables ctx) }
                 in (injectedId, Nothing, ctx')
            else ("ERROR User not authorized", Nothing, ctx) -- Or return an error since user doesn't exist

        _ -> (injectedId, Nothing, ctx) -- For unrecognized commands, return unchanged context
