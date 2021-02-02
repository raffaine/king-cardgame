{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib

import Data.Maybe
import Data.List
import Data.List.Split
import Data.String
import Control.Monad
import Control.Monad.State
import Text.JSON.Generic
import qualified Data.ByteString.Char8 as BS
import System.IO
import System.Exit
import System.Environment
import System.ZMQ4.Monadic

-- Simple DataType to hold Authenticated Player Data
data Player = Player
    { user :: String
    , channel :: String
    }

-- Makes a String to Request Table Actions from the Server
mkPlayerStr :: Player -> String
mkPlayerStr p = (user p) ++ " " ++ (channel p)

data Table = Table
    { name :: String
    , players :: [String]
    } deriving (Eq, Show, Data, Typeable)

data KingRule = RVaza | RMulheres | RHomens | RKing | RCopas | R2Ultimas |
                RPositiva | RPositivaH | RPositivaS | RPositivaD | RPositivaC

strRule :: KingRule -> String
strRule r = case r of
    RVaza -> "VAZA"
    RHomens -> "HOMENS"
    RMulheres -> "MULHERES"
    R2Ultimas -> "2ULTIMAS"
    RCopas -> "COPAS"
    RKing -> "KING"
    _ -> "POSITIVA"

mkRule :: String -> KingRule
mkRule rule = case rule of 
    m | m == "VAZA" -> RVaza
    m | m == "HOMENS" -> RHomens
    m | m == "MULHERES" -> RMulheres
    m | m == "2ULTIMAS" -> R2Ultimas
    m | m == "COPAS" -> RCopas
    m | m == "KING" -> RKing
    m | m == "POSITIVA" -> RPositiva
    m | m == "H" -> RPositivaH
    m | m == "C" -> RPositivaC
    m | m == "S" -> RPositivaS
    m | m == "D" -> RPositivaD

mkRules :: [String] -> [KingRule]
mkRules = map mkRule

type KingCard = String

data KingHand = KingHand
    { handRule  :: Either [KingRule] KingRule
    , curRound  :: [KingCard]
    , handScore :: [[Int]]
    , roundTurn :: Int
    }

-- Game State
data KingGame = KingGame
    { kingTable     :: Table
    , roundCards    :: [KingCard]
    , player        :: Player
    , secret        :: String
    , activeTurn    :: Int
    , activeHand    :: Maybe KingHand
    }

-- Simple Function to make the string requested on every play
mkPlayStr :: KingGame -> String -> String -> BS.ByteString
mkPlayStr game cmd args = BS.pack $ intercalate " " [cmd, usrname, usrsecret, args]
    where usrname = user $ player game
          usrsecret = secret game

-- Makes a new Game
mkGame :: Player -> String -> String -> KingGame
mkGame player table secret = KingGame (Table table []) [] player secret 0 Nothing

-- Sets the Players in a Game
setPlayers :: KingGame -> [String] -> KingGame
setPlayers game plrs = KingGame (Table tbl_name plrs) [] (player game) (secret game) 0 Nothing
    where table = kingTable game
          tbl_name = name table

-- Sets the Current Hand Rule in the Game
setHandRule :: KingGame -> String -> Either [KingRule] KingRule -> KingGame
setHandRule game starter rule = KingGame table [] (player game) (secret game) starter_pos (Just $ KingHand rule [] [] starter_pos)
    where table = kingTable game
          starter_pos = fromJust (starter `elemIndex` (players table)) 

-- Defines our GameState Type 
type KingGameEvaluator z = State KingGame z

-- What is the Action server Expects us to take (in the sense that we need to make it)
data ExpectedAction = KWaitStart | KWaitHand | KChooseHand | KGetHand | KPlay | KBid | KDecide | KTrump | KLeave

-- This takes a string given by the server and process it changing the game state and returning what is the expected action
evaluateGame :: String -> KingGameEvaluator ExpectedAction
evaluateGame info = do
    game <- get
    case splitOn " " info of
        [] -> return KWaitStart
        m:ms | m == "START" -> do
            put $ setPlayers game ms 
            return KWaitHand
        m:s:ms | m == "STARTHAND" -> do
            put $ setHandRule game s $ Left (mkRules ms)
            return KGetHand
        _ -> return KLeave 

-- Given a Game and an ExpectedEaction, uses Server Socket to perform Action returning Bool for Game Over
executeAction :: (Sender s, Receiver s) => KingGame -> ExpectedAction -> Socket z s -> ZMQ z Bool
executeAction game act srv = do
    send srv [] $ getAction game act
    rsp <- receive srv
    return True
    where getAction g a = case a of
            KGetHand    -> mkPlayStr g "GETHAND" ""
            KChooseHand -> mkPlayStr g "GAME" $ (strRule . chooseRule) g
            _           -> mkPlayStr g "LEAVE" ""


chooseRule :: KingGame -> KingRule
chooseRule game = case activeHand game of
   Just (KingHand (Left (r:_)) _ _ _) -> r
   Just (KingHand (Right r) _ _ _) -> r

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
    let tables = (decodeJSON (BS.unpack lst) :: [Table])
    case tables of
        []  -> createTable srv player
        x:_ -> return $ name x

-- Joins Table with name table with Authorized Player player in srv ZMQ Req Socket
joinTable :: (Sender s, Receiver s) => Socket z s -> Player -> String -> ZMQ z String
joinTable srv player table = do
    send srv [] $ BS.pack ("JOIN " ++ (mkPlayerStr player) ++ " " ++ table)
    secret <- receive srv
    return $ BS.unpack secret

gameLoop :: Player -> String -> String -> String -> ZMQ z ()
gameLoop player table srv_addr sub_addr = do
    -- Before Joining is important to subscribe to that Table's channel to not miss any message
    srv <- socket Req
    connect srv srv_addr

    info <- socket Sub
    connect info sub_addr
    subscribe info (BS.pack table)

    secret <- joinTable srv player table
    if isPrefixOf "ERROR" secret then return ()
    else
        loop info srv $ mkGame player table secret
        where
            loop info srv game = do
                msg <- receive info
                let (act, game') = runState (evaluateGame (BS.unpack msg)) game in
                    do
                        over <- executeAction game' act srv 
                        if over then return ()
                        else loop info srv game'


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

    -- This Do Block Encapsulates the ZMQ IO and the Authentication sequence
    runZMQ $ do
        liftIO $ putStrLn "Connecting to King Server ..."

        kingsrv <- socket Req
        connect kingsrv king_srv_addr

        player <- authorize kingsrv usrname passwrd
        liftIO $ putStrLn (user player)
        liftIO $ putStrLn (channel player)

        table <- huntTable kingsrv player
        liftIO $ putStrLn $ "Joining table " ++ table

        gameLoop player table king_srv_addr king_sub_addr


