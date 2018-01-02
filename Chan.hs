module Chan where

import Control.Concurrent hiding (Chan)
import Control.Concurrent.STM hiding (TChan)

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)
data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))

newChan :: IO (Chan a)
newChan = do
        hole <- newEmptyMVar
        readVar <- newMVar hole
        writeVar <- newMVar hole
        return $ Chan readVar writeVar

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
        newHole <- newEmptyMVar
        oldHole <- takeMVar writeVar
        putMVar oldHole $ Item val newHole
        putMVar writeVar newHole

readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
        stream <- takeMVar readVar
    -- If using multicast-channels then this must use readMVar
    -- Otherwise takeMVar stream would be fine
        Item val next <- readMVar stream
        putMVar readVar next
        return val

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
        hole <- readMVar writeVar
        newReadVar <- newMVar hole
        return $ Chan newReadVar writeVar

type TVarList a = TVar (TList a)
data TList a = Nil
             | TCons a (TVarList a)

data TChan a = TChan (TVar (TVarList a)) (TVar (TVarList a))

newTChan :: STM (TChan a)
newTChan = do
        hole <- newTVar Nil
        read <- newTVar hole
        write <- newTVar hole
        return $ TChan read write

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ write) val = do
        newHole <- newTVar Nil
        oldHole <- readTVar write
        writeTVar oldHole $ TCons val newHole
        writeTVar write newHole

readTChan :: TChan a -> STM a
readTChan (TChan read _) = do
        stream <- readTVar read
        head <- readTVar stream
        case head of
          Nil -> retry
          TCons val next -> do
                  writeTVar read next
                  return val

unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan read _) val = do
        head <- readTVar read
        newHead <- newTVar (TCons val head)
        writeTVar read newHead

isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan (TChan read _) = do
        list <- readTVar read
        head <- readTVar list
        case head of
          Nil -> return True
          _    -> return False




