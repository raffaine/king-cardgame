{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified System.ZMQ4.Monadic as Z
import Control.Concurrent.MVar (newMVar, modifyMVar)
import Control.Monad (forever, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack, unpack)

import ServerLogic (emptyServerContext, handleCommand)
import ProductionEnv (runProductionM)

main :: IO ()
main = do
    putStrLn "Starting King Server (Haskell Edition)..."
    
    -- Initialize the global thread-safe state
    stateMVar <- newMVar emptyServerContext
    
    Z.runZMQ $ do
        -- 1. Setup Action Server (REP) on Port 5555
        actionServer <- Z.socket Z.Rep
        Z.bind actionServer "tcp://127.0.0.1:5555"
        
        -- 2. Setup Status Publisher (PUB) on Port 5556
        statusPublisher <- Z.socket Z.Pub
        Z.bind statusPublisher "tcp://127.0.0.1:5556"
        
        liftIO $ putStrLn "ZMQ Sockets Bound. Listening for commands..."
        
        -- 3. The Main Game Loop
        forever $ do
            -- Wait for a command from a client
            msgByteString <- Z.receive actionServer
            let cmdString = unpack msgByteString
                cmdTokens = words cmdString
                
            liftIO $ putStrLn $ "Received: " ++ cmdString
            
            -- Lock the state, run the pure logic, and update the state
            (reply, bcasts) <- liftIO $ modifyMVar stateMVar $ \ctx -> do
                
                -- Execute your pure logic inside the Production IO environment
                (repStr, bcastStrs, newCtx) <- runProductionM (handleCommand ctx cmdTokens)
                
                -- modifyMVar expects (new_state, return_value)
                return (newCtx, (repStr, bcastStrs))
                
            -- Broadcast all resulting state changes to all subscribers
            forM_ bcasts $ \bcastMsg -> do
                liftIO $ putStrLn $ "Broadcasting: " ++ bcastMsg
                Z.send statusPublisher [] (pack bcastMsg)
                
            -- Finally, send the ACK or ERROR reply back to the requester
            Z.send actionServer [] (pack reply)
