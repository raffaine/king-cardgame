{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KingClient
    ( KingPlayer (..)
    , KingGame (..)
    , KingTable (..)
    , KingHand (..)
    , KingRule (..)
    , KingCard
    , KingSuit
    , runGame
    ) where

import Data.Maybe
import Data.List
import Data.List.Split
import Control.Monad

import Text.JSON.Generic
import qualified Data.ByteString.Char8 as BS

import System.ZMQ4.Monadic

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

data GameMonad a = GM (KingGame -> (a, KingGame))

instance Monad GameMonad where
    GM c1 >>= fc2   =  GM (\s0 -> let (r,s1) = c1 s0
                                      GM c2 = fc2 r in
                                      c2 s1)
    return k        =  GM (\s -> (k,s))

instance Functor GameMonad where
    fmap f (GM c) = GM (\s0 -> let (r, s1) = c s0 in
                                   (f r, s1))

instance Applicative GameMonad where
    pure a             = GM (\g -> (a, g))

    GM t <*> GM c     = GM (\s0 -> let (r, s1) = c s0
                                       (f, s2) = t s1 in
                                       (f r, s2))

 -- extracts the state from the monad
readGameMonad :: GameMonad KingGame
readGameMonad = GM (\g -> (g, g))

getPlayerCards :: GameMonad [KingCard]
getPlayerCards = do
    g <- readGameMonad
    return $ roundCards g

getRoundCards :: GameMonad [KingCard]
getRoundCards = do
    g <- readGameMonad
    case gameHands g of
        [] -> return []
        (x:xs) -> return $ curRound x

 -- updates the state of the monad
updateGameMonad :: (KingGame -> KingGame) -> GameMonad ()  -- alters the state
updateGameMonad f =  GM (\g -> ((), f g))

-- run a computation in the GameMonad
runGameMonad :: KingGame -> GameMonad a -> (a, KingGame)
runGameMonad s0 (GM c) =  c s0

------------- TypeClass used for Applications and The Main Game Loop --------------

class KingPlayer a where
    choosePlay :: a -> KingGame -> Maybe KingCard
    chooseRule :: a -> KingGame -> Maybe KingRule
    chooseTrumpSuit :: a -> KingGame -> Maybe KingSuit
    chooseDecision :: a -> KingGame -> Maybe Bool
    chooseBid :: a -> KingGame -> Maybe Int
    getName :: a -> String
    getPassword :: a -> String
    wantCreateTable :: a -> Bool
    wantJoinTable :: a -> KingTable -> Bool

runGame :: (KingPlayer a) => String -> String -> a -> IO ()
runGame srv_addr sub_addr agent = runZMQ $ do
    srv <- socket Req
    connect srv srv_addr

    let username = getName agent
    let password = getPassword agent

    player <- authorize srv username password
    table <- huntTable srv player

    -- Before Joining is important to subscribe to that Table's channel to not miss any message
    info <- socket Sub
    connect info sub_addr
    subscribe info (BS.pack table)

    secret <- joinTable srv player table
    if isPrefixOf "ERROR" secret 
    then
        return ()
    else
        loop info srv $ mkGame player table secret
        where
            loop info srv game = do
                msg <- receive info
                liftIO $ putStrLn $ BS.unpack msg
                liftIO $ putStrLn $ show game
                game' <- evaluateMessage srv game agent (BS.unpack msg)
                liftIO $ putStrLn $ show game'
                case game' of
                    (Just g) -> loop info srv g
                    Nothing  -> return ()

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


------------------ Main Execution Routines ---------------------------------
data ActionData = KAck | KError String | KHand String
    deriving (Eq, Show)

-- Giving a running game, returs a string that executes action CMD with arguments m_args if any
mkPlayStr :: KingGame -> String -> Maybe String -> BS.ByteString
mkPlayStr game cmd m_args = BS.pack $ intercalate " " $ lst m_args
    where lst Nothing     = [cmd, usrname, usrsecret]
          lst (Just args) = [cmd, usrname, usrsecret, args]
          usrname = user $ player game
          usrsecret = secret game


-- Given a Server Socket, perform Action returning ActionData
executeAction :: (Sender s, Receiver s) => Socket z s -> BS.ByteString -> ZMQ z ActionData
executeAction srv action = do
    send srv [] action
    rsp <- receive srv
    case splitOn " " (BS.unpack rsp) of
        e:ms | e == "ERROR" -> return (KError $ intercalate " " ms)
        a:_  | a == "ACK" -> return KAck
        cs -> return (KHand $ concat cs)

requestAgentAction :: (KingPlayer a, Sender s, Receiver s) => Socket z s -> KingGame -> a -> String -> (a -> KingGame -> Maybe String) -> ZMQ z Bool
requestAgentAction srv game agent act_name action = loopRequest $ action agent game
    where 
        loopRequest Nothing = return False
        loopRequest (Just item) = do
            rsp <- executeAction srv $ mkPlayStr game act_name $ Just item
            case rsp of
                KAck -> return True
                otherwise -> loopRequest $ action agent game


evaluateMessage :: (KingPlayer a, Sender s, Receiver s) => Socket z s -> KingGame -> a -> String -> ZMQ z (Maybe KingGame)
evaluateMessage srv game agent info = do
    case splitOn " " info of
        [] -> return $ Just game
        t:m:ms | m == "START" -> do
            return $ Just $ setPlayers game ms 
        t:m:s:ms | m == "STARTHAND" -> do
            let game' = startHand game s $ map readRule ms
            cards <- executeAction srv $ mkPlayStr game' "GETHAND" Nothing
            case cards of
                (KHand cs) -> do
                                when (user (player game) == s) (do 
                                    -- This tricky fmap . fmap is so I can use show inside the Maybe that will be returned by chooseRule
                                    ans <- requestAgentAction srv game'' agent "GAME" ((fmap . fmap) show . chooseRule)
                                    when (ans) (do
                                        executeAction srv $ mkPlayStr game'' "LEAVE" Nothing
                                        return ())
                                    return ())
                                return $ Just game''
                            where   game'' = setupCards game' (decodeJSON cs :: [String])
                otherwise -> return Nothing
        t:m:ms | m == "GAME" -> do
            return $ Just $ setHandRule game $ readRule (concat ms)
        t:m:ms | m == "TURN" -> do
            let game' = moveTurn game $ concat ms
            when (user (player game) `elem` ms) (do
                ans <- requestAgentAction srv game' agent "PLAY" choosePlay
                when (ans) (do
                    executeAction srv $ mkPlayStr game' "LEAVE" Nothing
                    return ())
                return ())
            return $ Just game'
        t:m:cs | m == "PLAY" -> do
            return $ Just $ playCard game $ concat cs
        t:m:ms | m == "BIDS" -> do
            -- return $ Just $ bidOffered game $ read $ concat cs
            return $ Just $ game
        t:m:ms | m == "BID" -> do
            -- let game'= setBidder game $ concat cs
            let game' = game
            when (user (player game) `elem` ms) (do
                ans <- requestAgentAction srv game' agent "BID" ((fmap . fmap) show . chooseBid)
                when (ans) (do
                    executeAction srv $ mkPlayStr game' "LEAVE" Nothing
                    return ())
                return ())
            return $ Just game'
        t:m:ms | m == "DECIDE" -> do
            -- let game'= setDecider game $ concat cs
            let game' = game
            when (user (player game) `elem` ms) (do
                ans <- requestAgentAction srv game' agent "DECIDE" ((fmap . fmap) show . chooseDecision)
                when (ans) (do
                    executeAction srv $ mkPlayStr game' "LEAVE" Nothing
                    return ())
                return ())
            return $ Just game'
        t:m:ms | m == "CHOOSETRUMP" -> do
            -- let game'= setTrumpChooser game $ concat cs
            let game' = game
            when (user (player game) `elem` ms) (do
                ans <- requestAgentAction srv game' agent "TRUMP" ((fmap . fmap) show . chooseTrumpSuit)
                when (ans) (do
                    executeAction srv $ mkPlayStr game' "LEAVE" Nothing
                    return ())
                return ())
            return $ Just game'
        t:m:w:scr | m == "ENDROUND" -> do
            return $ Just $ endRound game w $ read $ concat scr
        t:m:scrs | m == "ENDHAND" -> do
            -- This message is a good to know info but I have it already
            -- return $ Just $ endHand game $ map read $ intercalate " " scrs
            return $ Just $ game
        t:m:scrs | m == "GAMEOVER" -> do
            return Nothing
        otherwise -> return Nothing

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
