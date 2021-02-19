{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KingClient
    ( Player (..)
    , KingGame (..)
    , KingTable (..)
    , KingHand (..)
    , KingRule (..)
    , KingCard
    , KingSuit
    , KingGameS
    , mkGame
    , mkPlayStr
    , updateGame
    , startGame
    , executeActionS
    , ExpectedAction (..)
    , ActionResponse (..)
    ) where

import Data.Maybe
import Data.List
import Data.List.Split
import Control.Monad
import Control.Monad.State

import Text.JSON.Generic
import qualified Data.ByteString.Char8 as BS

import System.ZMQ4.Monadic

data ExpectedAction = KWait | KGetHand | KRule | KPlay | KBid | KDecide | KTrump | KOver String

type KingGameS z = StateT KingGame (ZMQ z)

evaluateMessageS :: (Sender s, Receiver s) => Socket z s -> String -> KingGameS z ExpectedAction
evaluateMessageS _   []   = pure KWait
evaluateMessageS srv info = do
    game <- get
    case splitOn " " info of
        t:m:ms | m == "START" -> do
            put $ setPlayers game ms
            return KWait
        t:m:s:ms | m == "STARTHAND" -> do
            let game' = startHand game s $ map readRule ms
            lift $ send srv [] $ mkPlayStr game' "GETHAND" Nothing
            -- I'm not dealing with errors here, it's not hard, I just need to check for the pattern
            cards <- lift $ receive srv
            put $ setupCards game' (decodeJSON (BS.unpack cards) :: [String])
            if user (player game') == s
                then return KRule
                else return KWait
        t:m:ms | m == "GAME" -> do
            put $ setHandRule game $ readRule (concat ms)
            return KWait
        t:m:ms | m == "TURN" -> do
            put $ moveTurn game $ concat ms
            if user (player game) `elem` ms
                then return KPlay
                else return KWait
        t:m:cs | m == "PLAY" -> do
            put $ playCard game $ concat cs
            return KWait
        t:m:ms | m == "BIDS" -> do
            -- put $ bidOffered game $ read $ concat cs
            return KWait
        t:m:ms | m == "BID" -> do
            -- put $ setBidder game $ concat cs
            if user (player game) `elem` ms
                then return KBid
                else return KWait
        t:m:ms | m == "DECIDE" -> do
            -- put $ setDecider game $ concat cs
            if user (player game) `elem` ms
                then return KDecide
                else return KWait
        t:m:ms | m == "CHOOSETRUMP" -> do
            -- put $ setTrumpChooser game $ concat cs
            if user (player game) `elem` ms
                then return KTrump
                else return KWait
        t:m:w:scr | m == "ENDROUND" -> do
            put $ endRound game w $ read $ concat scr
            return KWait
        t:m:scrs | m == "ENDHAND" -> do
            -- This message is a good to know info but I have it already
            -- put $ endHand game $ map read $ intercalate " " scrs
            return KWait
        t:m:scrs | m == "GAMEOVER" -> do
            return $ KOver $ intercalate " " scrs
        otherwise -> do
            return $ KOver $ "Unknown message"

startGame :: (Sender s, Receiver s, Subscriber r) => Socket z s -> Socket z r -> String -> String -> KingGameS z Bool
startGame srv sub usr pwd = StateT $ \_ -> do
        player <- authorize srv usr pwd
        table <- huntTable srv player

        -- Before Joining is important to subscribe to that Table's channel to not miss any message
        subscribe sub (BS.pack table)

        secret <- joinTable srv player table
        if isPrefixOf "ERROR" secret
            then
                return (False, mkGame player "" "")
            else
                return (True, mkGame player table secret)


updateGame :: (Sender s, Receiver s, Receiver r) => Socket z s -> Socket z r -> Timeout -> KingGameS z ExpectedAction
updateGame srv info timeout = do
    let evt = [Sock info [In] Nothing ]
    evts <- lift $ poll timeout evt
    case evts of
        [hs] | length hs == 0 -> return KWait
        otherwise -> do
            msg <- lift $ receive info
            liftIO $ putStrLn $ BS.unpack msg
            evaluateMessageS srv $ BS.unpack msg

-- Given a Server Socket, perform Action returning ActionResponse
data ActionResponse = KAck | KError String

executeActionS :: (Sender s, Receiver s) => Socket z s -> BS.ByteString -> ZMQ z ActionResponse
executeActionS srv action = do
    send srv [] action
    rsp <- receive srv
    case splitOn " " (BS.unpack rsp) of
        e:ms | e == "ERROR" -> return (KError $ intercalate " " ms)
        a:_  | a == "ACK" -> return KAck
        otherwise -> return $ KError $ "Unexpected Response: " ++ (BS.unpack rsp)

--------- Data Types used in the game ---------------
data Player = Player
    { user :: String
    , channel :: String
    } deriving (Eq, Show)

data KingTable = KingTable
    { name :: String
    , players :: [String]
    } deriving (Eq, Show, Data, Typeable)

data KingRule = RVaza | RMulheres | RHomens | RKing | RCopas | R2Ultimas |
                RPositiva | RPositivaH | RPositivaS | RPositivaD | RPositivaC
    deriving Eq

instance Show KingRule where
    show r = case r of
        RVaza -> "VAZA"
        RHomens -> "HOMENS"
        RMulheres -> "MULHERES"
        R2Ultimas -> "2ULTIMAS"
        RCopas -> "COPAS"
        RKing -> "KING"
        _ -> "POSITIVA"

readRule :: String -> KingRule
readRule rule = case rule of 
    m | m == "VAZA" -> RVaza
    m | m == "HOMENS" -> RHomens
    m | m == "MULHERES" -> RMulheres
    m | m == "2ULTIMAS" -> R2Ultimas
    m | m == "COPAS" -> RCopas
    m | m == "KING" -> RKing
    m | m == "POSITIVA" -> RPositiva
    m | m == "POSITIVAH" -> RPositivaH
    m | m == "POSITIVAC" -> RPositivaC
    m | m == "POSITIVAS" -> RPositivaS
    m | m == "POSITIVAD" -> RPositivaD

type KingCard = String
type KingSuit = Char

data KingHand = KingHand
    { handRule  :: Either [KingRule] KingRule
    , curRound  :: [KingCard]
    , handScore :: [[Int]]
    , roundTurn :: Int
    } deriving (Show)

-- Game State
data KingGame = KingGame
    { kingTable     :: KingTable
    , roundCards    :: [KingCard]
    , player        :: Player
    , secret        :: String
    , activeTurn    :: Int
    , gameHands     :: [KingHand]
    } deriving (Show)

--------- Helper Functions ---------------
replaceNth :: Int -> Int -> [Int] -> [Int]
replaceNth _ _ [] = []
replaceNth n val (x:xs)
    | n == 0    = val:xs
    | otherwise = x:(replaceNth (n-1) val xs)

removeFromList :: (Eq a) => a -> [a] -> [a]
removeFromList c cs = case cs of
        [] -> []
        (e:es) | e == c -> es
        (e:es) -> e:(removeFromList c es)

-- Makes a String to Request Table Actions from the Server
mkPlayerStr :: Player -> String
mkPlayerStr p = (user p) ++ " " ++ (channel p)

-------------- Hand Manipulation ----------------------------------
playRoundCard :: KingHand -> KingCard -> KingHand
playRoundCard hand card = KingHand (handRule hand) round (handScore hand) (roundTurn hand)
    where cards = curRound hand
          round = cards ++ [card] -- Don't look at me, it's not about performance, it's about consistency with player order

endHandRound :: KingHand -> Int -> Int -> KingHand
endHandRound hand winner score = KingHand (handRule hand) [] scores' winner
    where scores' = scores ++ [replaceNth winner score [0, 0, 0, 0]]
          scores  = handScore hand


-------------- Game Manipulation ---------------------------------
-- Makes a new Game
mkGame :: Player -> String -> String -> KingGame
mkGame player table secret = KingGame (KingTable table []) [] player secret 0 []

-- Sets the Players in a Game
setPlayers :: KingGame -> [String] -> KingGame
setPlayers game plrs = KingGame (KingTable tbl_name plrs) [] (player game) (secret game) 0 []
    where table = kingTable game
          tbl_name = name table

-- Sets up the new hand by giving the starting player and list of rules to choose
startHand :: KingGame -> String -> [KingRule] -> KingGame
startHand game starter rules = KingGame table (roundCards game) (player game) (secret game) starter_pos hands'
    where table       = kingTable game
          starter_pos = fromJust (starter `elemIndex` (players table))
          hands       = gameHands game
          hands'      = (KingHand (Left rules) [] [] starter_pos) : hands

-- Sets the Current Hand Rule in the Game
setHandRule :: KingGame -> KingRule -> KingGame
setHandRule game rule = KingGame table (roundCards game) (player game) (secret game) (activeTurn game) hands'
    where table        = kingTable game
          (hand:hands) = gameHands game
          hands'       = (KingHand (Right rule) [] [] (roundTurn hand)) : hands

-- Moves a Turn
moveTurn :: KingGame -> String -> KingGame
moveTurn game turn = KingGame table (roundCards game) (player game) (secret game) turn' (gameHands game)
    where table = kingTable game
          turn' = fromJust (turn `elemIndex` (players table))

-- Setup the players Cards for the Hand
setupCards :: KingGame -> [KingCard] -> KingGame
setupCards game cards = KingGame (kingTable game) cards (player game) (secret game) (activeTurn game) (gameHands game)

-- Current turn Player plays card on table
playCard :: KingGame -> KingCard -> KingGame
playCard game card = KingGame table cards' (player game) (secret game) turn hands'
    where table         = kingTable game
          turn          = activeTurn game
          nameCur       = (players table) !! turn
          name          = user $ player game
          (hand:hands)  = gameHands game
          hands'        = (playRoundCard hand card) : hands
          cards         = roundCards game
          cards'        = if nameCur == name then removeFromList card cards else cards

-- End of Round, clear played cards add winner's score
endRound :: KingGame -> String -> Int -> KingGame
endRound game winner score = KingGame table (roundCards game) (player game) (secret game) 0 hands'
    where table        = kingTable game
          wnr_pos      = fromJust (winner `elemIndex` (players table))
          (hand:hands) = gameHands game
          hands'       = (endHandRound hand wnr_pos score) : hands

-- Giving a running game, returs a string that executes action CMD with arguments m_args if any
mkPlayStr :: KingGame -> String -> Maybe String -> BS.ByteString
mkPlayStr game cmd m_args = BS.pack $ intercalate " " $ lst m_args
    where lst Nothing     = [cmd, usrname, usrsecret]
          lst (Just args) = [cmd, usrname, usrsecret, args]
          usrname = user $ player game
          usrsecret = secret game

------------------ Auxiliary Setup Requests -----------------------
-- Authorizes Given user using provided password
-- Returns Player (TODO: Either String Player and deal with errors)
authorize :: (Sender s, Receiver s) => Socket z s -> String -> String -> ZMQ z Player
authorize skt name secret = do
    liftIO $ putStrLn "Authorizing Player ..."
    send skt [] (BS.pack ("AUTHORIZE " ++ name ++ " " ++ secret))
    channel <- receive skt
    return $ Player name $ BS.unpack channel

-- Creates a Table for Authorized Player player on srv ZMQ Req Socket
createTable :: (Sender s, Receiver s) => Socket z s -> Player -> ZMQ z String
createTable srv player = do
    liftIO $ putStrLn "Requesting a Table to server ..."
    send srv [] (BS.pack ("TABLE " ++ (mkPlayerStr player)))
    table_name <- receive srv
    return $ BS.unpack table_name

-- Returns the name of an existing Table in the server, create if one does not exist
huntTable :: (Sender s, Receiver s) => Socket z s -> Player -> ZMQ z String
huntTable srv player = do
    liftIO $ putStrLn "Seeking a Table to play ..."
    send srv [] (BS.pack "LIST")
    lst <- receive srv
    let tables = (decodeJSON (BS.unpack lst) :: [KingTable])
    case tables of
        []  -> createTable srv player
        x:_ -> return $ name x

-- Joins Table with name table with Authorized Player player in srv ZMQ Req Socket
joinTable :: (Sender s, Receiver s) => Socket z s -> Player -> String -> ZMQ z String
joinTable srv player table = do
    send srv [] $ BS.pack ("JOIN " ++ (mkPlayerStr player) ++ " " ++ table)
    secret <- receive srv
    return $ BS.unpack secret
