{-# LANGUAGE ForeignFunctionInterface #-}
module ClientFFI where

import Foreign.C.String
import Foreign.Ptr
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (void)

import BaseClient
import KingClient

----------------------------------------------------------------------------------
-- Global STM Bridge State (Required because C++ function calls lack Haskell context)
----------------------------------------------------------------------------------
globalResponse :: TMVar BS.ByteString
globalResponse = unsafePerformIO newEmptyTMVarIO
{-# NOINLINE globalResponse #-}

globalGameState :: TVar (Maybe KingGame)
globalGameState = unsafePerformIO (newTVarIO Nothing)
{-# NOINLINE globalGameState #-}

----------------------------------------------------------------------------------
-- The C++ Agent Instance
----------------------------------------------------------------------------------
newtype CppAgent = CppAgent (FunPtr (CString -> IO ()))

-- Dynamic wrapper to allow Haskell to invoke the provided C++ function pointer
foreign import ccall "dynamic"
    invokeCb :: FunPtr (CString -> IO ()) -> CString -> IO ()

instance ContextAwareAgent CppAgent where
    decideAction action game agent@(CppAgent cb) = do
        -- 1. Sync the latest game state into the global TVar for C++ to query
        atomically $ writeTVar globalGameState (Just game)

        -- 2. Alert C++ that an action is required
        withCString (show action) $ \c_action ->
            invokeCb cb c_action

        -- 3. Block the Agent thread until C++ calls `submit_action`
        rsp <- atomically $ takeTMVar globalResponse
        return (rsp, agent)

----------------------------------------------------------------------------------
-- C-API Exports
----------------------------------------------------------------------------------

-- | Boot up the client thread. C++ calls this once.
foreign export ccall "start_client" startClientFFI
    :: CString -> CString -> CString -> CString -> FunPtr (CString -> IO ()) -> IO ()

startClientFFI :: CString -> CString -> CString -> CString -> FunPtr (CString -> IO ()) -> IO ()
startClientFFI c_srv c_sub c_usr c_pass cb = do
    srv  <- peekCString c_srv
    sub  <- peekCString c_sub
    usr  <- peekCString c_usr
    pass <- peekCString c_pass

    -- Fork the game loop so the C++ main thread is not blocked
    void $ forkIO $ runGameS srv sub usr pass (CppAgent cb)

-- | C++ calls this to provide a decision (e.g., "PLAY Alice sec-A 10H")
foreign export ccall "submit_action" submitActionFFI :: CString -> IO ()

submitActionFFI :: CString -> IO ()
submitActionFFI c_action = do
    action <- peekCString c_action
    -- Use tryPutTMVar so C++ doesn't accidentally deadlock itself if it double-clicks
    void $ atomically $ tryPutTMVar globalResponse (BS.pack action)

-- | Example State Getter: Let C++ poll the current active turn
foreign export ccall "get_active_turn" getActiveTurnFFI :: IO Int

getActiveTurnFFI :: IO Int
getActiveTurnFFI = do
    mGame <- readTVarIO globalGameState
    case mGame of
        Nothing -> return (-1)
        Just g  -> return (activeTurn g)
