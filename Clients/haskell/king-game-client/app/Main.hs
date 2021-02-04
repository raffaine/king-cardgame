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
    } deriving (Eq, Show)

-- Makes a String to Request Table Actions from the Server
mkPlayerStr :: Player -> String
mkPlayerStr p = (user p) ++ " " ++ (channel p)

data Table = Table
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

data KingHand = KingHand
    { handRule  :: Either [KingRule] KingRule
    , curRound  :: [KingCard]
    , handScore :: [[Int]]
    , roundTurn :: Int
    } deriving (Show)

playRoundCard :: KingHand -> KingCard -> KingHand
playRoundCard hand card = KingHand (handRule hand) round (handScore hand) (roundTurn hand)
    where cards = curRound hand
          round = cards ++ [card] -- Don't look at me, it's not about performance, it's about consistency with player order

replaceNth :: Int -> Int -> [Int] -> [Int]
replaceNth _ _ [] = []
replaceNth n val (x:xs)
    | n == 0    = val:xs
    | otherwise = x:(replaceNth (n-1) val xs)

endHandRound :: KingHand -> Int -> Int -> KingHand
endHandRound hand winner score = KingHand (handRule hand) [] scores' winner
    where scores' = scores ++ [replaceNth winner score [0, 0, 0, 0]]
          scores  = handScore hand

-- Game State
data KingGame = KingGame
    { kingTable     :: Table
    , roundCards    :: [KingCard]
    , player        :: Player
    , secret        :: String
    , activeTurn    :: Int
    , gameHands     :: [KingHand]
    } deriving (Show)

-- Simple Function to make the string requested on every play
mkPlayStr :: KingGame -> String -> Maybe String -> BS.ByteString
mkPlayStr game cmd m_args = BS.pack $ intercalate " " $ lst m_args
    where lst Nothing     = [cmd, usrname, usrsecret]
          lst (Just args) = [cmd, usrname, usrsecret, args]
          usrname = user $ player game
          usrsecret = secret game

-- Makes a new Game
mkGame :: Player -> String -> String -> KingGame
mkGame player table secret = KingGame (Table table []) [] player secret 0 []

-- Sets the Players in a Game
setPlayers :: KingGame -> [String] -> KingGame
setPlayers game plrs = KingGame (Table tbl_name plrs) [] (player game) (secret game) 0 []
    where table = kingTable game
          tbl_name = name table

-- Sets the Current Hand Rule in the Game
startHand :: KingGame -> String -> [KingRule] -> KingGame
startHand game starter rules = KingGame table (roundCards game) (player game) (secret game) starter_pos hands'
    where table       = kingTable game
          starter_pos = fromJust (starter `elemIndex` (players table))
          hands       = gameHands game
          hands'      = (KingHand (Left rules) [] [] starter_pos) : hands

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

removeFromList :: KingCard -> [KingCard] -> [KingCard]
removeFromList c cs = case cs of
        [] -> []
        (e:es) | e == c -> es
        (e:es) -> e:(removeFromList c es)

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

-- Defines our GameState Type 
type KingGameEvaluator z = State KingGame z

-- What is the Action server Expects us to take (in the sense that we need to make it)
data ExpectedAction = KChooseHand | KGetHand | KPlay | KBid | KDecide | KTrump | KWait | KLeave | KGameOver
    deriving (Eq, Show)

-- This takes a string given by the server and process it changing the game state and returning what is the expected action
evaluateGame :: String -> KingGameEvaluator [ExpectedAction]
evaluateGame info = do
    game <- get
    case splitOn " " info of
        [] -> return [KWait]
        t:m:ms | m == "START" -> do
            put $ setPlayers game ms 
            return [KWait]
        t:m:s:ms | m == "STARTHAND" -> do
            put $ startHand game s $ map readRule ms
            if user (player game) == s
            then return [KGetHand, KChooseHand]
            else return [KGetHand]
        t:m:ms | m == "GAME" -> do
            put $ setHandRule game $ readRule (concat ms)
            return [KWait]
        t:m:ms | m == "TURN" -> do
            put $ moveTurn game $ concat ms
            if user (player game) `elem` ms
            then return [KPlay]
            else return [KWait]
        t:m:cs | m == "PLAY" -> do
            put $ playCard game $ concat cs
            return [KWait]
        t:m:ms | m == "BIDS" -> do
            -- put $ bidOffered game $ read $ concat cs
            return [KWait]
        t:m:ms | m == "BID" -> do
            -- put $ setBidder game $ concat cs
            if user (player game) `elem`ms
            then return [KBid]
            else return [KWait]
        t:m:ms | m == "DECIDE" -> do
            if user (player game) `elem`ms
            then return [KDecide]
            else return [KWait]
        t:m:ms | m == "CHOOSETRUMP" -> do
            if user (player game) `elem`ms
            then return [KTrump]
            else return [KWait]
        t:m:w:scr | m == "ENDROUND" -> do
            put $ endRound game w $ read $ concat scr
            return [KWait]
        t:m:scrs | m == "ENDHAND" -> do
            -- This message is a good to know info but I have it already
            -- put $ endHand game $ map read $ intercalate " " scrs
            return [KWait]
        t:m:scrs | m == "GAMEOVER" -> do
            -- put $ endGame game $ map read $ intercalate " " scrs
            return [KGameOver]
        otherwise -> return [KLeave] 

data ActionData a = KOver | KAck | KError a | KHand a
    deriving (Eq, Show)

-- Given a Game and an ExpectedEaction, uses Server Socket to perform Action returning ActionData
executeAction :: (Sender s, Receiver s) => Socket z s -> KingGame -> ExpectedAction -> ZMQ z (ActionData String)
executeAction _   _  KWait = return KAck
executeAction srv game act = do
    send srv [] $ getAction game act
    rsp <- receive srv
    liftIO $ putStrLn $ "Action : " ++ show act
    liftIO $ putStrLn $ show game
    case splitOn " " (BS.unpack rsp) of
        e:ms | e == "ERROR" -> return (KError $ intercalate " " ms)
        a:_  | a == "ACK" -> return KAck
        cs -> return (KHand $ concat cs)
    where getAction g a = case a of
            KGetHand    -> mkPlayStr g "GETHAND" Nothing
            KChooseHand -> mkPlayStr g "GAME" $ Just $ show (chooseRule g)
            KPlay       -> mkPlayStr g "PLAY" $ Just $ chooseCard g
            KBid        -> mkPlayStr g "BID" $ Just $ chooseBid g
            KDecide     -> mkPlayStr g "DECIDE" $ Just $ makeBidDecision g
            KTrump      -> mkPlayStr g "TRUMP" $ Just $ chooseTrumpSuit g
            _           -> mkPlayStr g "LEAVE" Nothing


-- Always chooses Hearts (for now)
chooseTrumpSuit :: KingGame -> String
chooseTrumpSuit _ = "H"

-- Always refuses
makeBidDecision :: KingGame -> String
makeBidDecision _ = "False"

-- Always forefeits
chooseBid :: KingGame -> String
chooseBid _ = "0"

compareSuit :: KingCard -> KingCard -> Bool
compareSuit (_:s1:_) (_:s2:_) = s1 == s2

getFilter :: [KingCard] -> KingRule -> [KingCard] -> (KingCard -> Bool)
getFilter cs RKing (c:_) = \cur -> (compareSuit cur c) || (cur == "KH")
getFilter _ _ (c:_) = compareSuit c
getFilter _ r [] = if r `elem` [RCopas, RKing] then not . (compareSuit "KH") else \_ -> True

firstValid :: KingRule -> [KingCard] -> [KingCard] -> KingCard
firstValid rule table cards = case cards' of
            (c:cs) -> c
            []     -> head cards
    where cards' = filter (getFilter cards rule table) cards

-- Not much to do as Default, otherwise it will try the same thing all the time
-- Even Random requires some logic to avoid an Invalid Action
chooseCard :: KingGame -> String
chooseCard (KingGame _ cards _ _ _ ((KingHand (Right rule) tbCards _ _ ):_)) = firstValid rule tbCards cards
chooseCard _ = "KH"

-- Just Choose the First on the list
-- This can also be used to obtain the current rule
chooseRule :: KingGame -> KingRule
chooseRule game = case gameHands game of
   ((KingHand (Left (r:_)) _ _ _):_) -> r
   ((KingHand (Right r) _ _ _):_) -> r

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


checkResult :: ActionData a -> Bool
checkResult (KError _)  = False
checkResult KOver       = False
checkResult _           = True

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
                liftIO $ putStrLn $ BS.unpack msg
                let (acts, game') = runState (evaluateGame (BS.unpack msg)) game in
                    do
                    (res, srv, g) <- foldM (\(r, srv, game'') act -> do
                        result <- executeAction srv game'' act
                        case result of
                            KError e -> do
                                liftIO $ putStrLn $ "Server Returned Error: " ++ e
                                return (result, srv, game'')
                            KHand cs -> do
                                liftIO $ putStrLn $ "Server returned cards: " ++ cs
                                return (result, srv, setupCards game'' (decodeJSON cs :: [String]))
                            otherwise -> return (result, srv, game'')) (KAck, srv, game') acts
                    if checkResult res then loop info srv g
                    else return ()


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


