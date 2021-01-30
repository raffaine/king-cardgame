{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib

import Data.List
import Data.String
import Control.Monad
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

-- Simple Function to make the string requested on every play
mkPlayStr :: Player -> String -> String
mkPlayStr p t = (user p) ++ " " ++ t


-- Authorizes Player using skt ZMQ Req Socket
authorize skt name secret = do
    liftIO $ putStrLn "Authorizing Player ..."
    send skt [] ( BS.pack ("AUTHORIZE " ++ name ++ " " ++ secret))
    channel <- receive skt
    return $ BS.unpack channel

-- Creates a Table for Authorized Player player on srv ZMQ Req Socket
createTable srv player = do
    liftIO $ putStrLn "Requesting a Table to server ..."
    send srv [] (BS.pack ("TABLE " ++ (mkPlayerStr player)))
    table_name <- receive srv
    return $ BS.unpack table_name

-- Returns the name of an existing Table in the server, create if one does not exist
huntTable srv player = do
    liftIO $ putStrLn "Seeking a Table to play ..."
    send srv [] (BS.pack "LIST")
    lst <- receive srv
    let tables = (decodeJSON (BS.unpack lst) :: [Table])
    case tables of
        []  -> createTable srv player
        x:_ -> return $ name x

-- Joins Table with name table with Authorized Player player in srv ZMQ Req Socket
joinTable srv player table = do
    send srv [] $ BS.pack ("JOIN " ++ (mkPlayerStr player) ++ " " ++ table)
    secret <- receive srv
    return $ BS.unpack secret

-- Leaves Table with secret table for Authorized Player player in srv ZMQ Req Socket
leaveTable srv player table = do
    send srv [] $ BS.pack ("LEAVE " ++ (mkPlayStr player table))
    ack <- receive srv
    return $ BS.unpack ack

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

        rsp <- authorize kingsrv usrname passwrd
        liftIO $ putStrLn rsp

        let player = Player usrname rsp

        table <- huntTable kingsrv player
        liftIO $ putStrLn table

        -- Before Joining is important to subscribe to that Table's channel to not miss any message
        info <- socket Sub
        connect info king_sub_addr
        subscribe info (BS.pack table)

        secret <- joinTable kingsrv player table
        liftIO $ putStrLn secret

        -- Leaves Table
        rsp <- leaveTable kingsrv player secret
        liftIO $ putStrLn rsp


