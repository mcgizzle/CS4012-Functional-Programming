module Chad where

import Control.Concurrent hiding (Chad)
import Control.Concurrent.STM hiding (TChad)

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)
data Chad a = Chad (MVar (Stream a)) (MVar (Stream a))

newChad :: IO (Chad a)
newChad = do
        hole <- newEmptyMVar
        readVar <- newMVar hole
        writeVar <- newMVar hole
        return $ Chad readVar writeVar

writeChad :: Chad a -> a -> IO ()
writeChad (Chad _ writeVar) val = do
        newHole <- newEmptyMVar
        oldHole <- takeMVar writeVar
        putMVar oldHole $ Item val newHole
        putMVar writeVar newHole

readChad :: Chad a -> IO a
readChad (Chad readVar _) = do
        stream <- takeMVar readVar
    -- If using multicast-channels then this must use readMVar
    -- Otherwise takeMVar stream would be fine
        Item val next <- readMVar stream
        putMVar readVar next
        return val

dupChad :: Chad a -> IO (Chad a)
dupChad (Chad _ writeVar) = do
        hole <- readMVar writeVar
        newReadVar <- newMVar hole
        return $ Chad newReadVar writeVar

type TVarList a = TVar (TList a)
data TList a = Nil
             | TCons a (TVarList a)

data TChad a = TChad (TVar (TVarList a)) (TVar (TVarList a))

newTChad :: STM (TChad a)
newTChad = do
        hole <- newTVar Nil
        read <- newTVar hole
        write <- newTVar hole
        return $ TChad read write

writeTChad :: TChad a -> a -> STM ()
writeTChad (TChad _ write) val = do
        newHole <- newTVar Nil
        oldHole <- readTVar write
        writeTVar oldHole $ TCons val newHole
        writeTVar write newHole

readTChad :: TChad a -> STM a
readTChad (TChad read _) = do
        stream <- readTVar read
        head <- readTVar stream
        case head of
          Nil -> retry
          TCons val next -> do
                  writeTVar read next
                  return val

unGetTChad :: TChad a -> a -> STM ()
unGetTChad (TChad read _) val = do
        head <- readTVar read
        newHead <- newTVar (TCons val head)
        writeTVar read newHead

isEmptyTChad :: TChad a -> STM Bool
isEmptyTChad (TChad read _) = do
        list <- readTVar read
        head <- readTVar list
        case head of
          Nil -> return True
          _    -> return False




