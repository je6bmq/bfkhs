{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bfk.Executer where

import Control.Monad.State
import Data.Bfk.Parser
import Data.Char (chr)
import Data.Int (Int8)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

type InstructionPointer = Int

type DataPointer = Int

executeInstructions :: V.Vector BfkInstruction -> IO ()
executeInstructions insts =
  evalStateT (do forM_ insts executeInstruction) (V.replicate 30000 0, 0)
  where
    executeInstruction :: BfkInstruction -> StateT (V.Vector Int8, DataPointer) IO ()
    executeInstruction IncrPtr = do
      (byteVec, dataPtr) <- get
      put (byteVec, dataPtr + 1)
    executeInstruction DecrPtr = do
      (byteVec, dataPtr) <- get
      put (byteVec, dataPtr -1)
    executeInstruction IncrValue = do
      (byteVec, dataPtr) <- get
      mutableVec <- liftIO $ V.thaw byteVec -- temporarily mutable
      liftIO $ VM.modify mutableVec (+ 1) dataPtr -- increment
      newByteVec <- V.freeze mutableVec
      put (newByteVec, dataPtr)
    executeInstruction DecrValue = do
      (byteVec, dataPtr) <- get
      mutableVec <- liftIO $ V.thaw byteVec -- temporarily mutable
      liftIO $ VM.modify mutableVec (\a -> a - 1) dataPtr -- decrement
      newByteVec <- V.freeze mutableVec
      put (newByteVec, dataPtr)
    executeInstruction ReadValue = do
      (byteVec, dataPtr) <- get
      line <- liftIO readLn
      let c :: Char = read line
      mutableVec <- liftIO $ V.thaw byteVec -- temporarily mutable
      liftIO $ VM.write mutableVec dataPtr $ fromIntegral $ fromEnum c -- write variable (fromIntegral used for convert Int to Int8)
      newByteVec <- V.freeze mutableVec
      put (newByteVec, dataPtr)
    executeInstruction OutputValue = do
      (byteVec, dataPtr) <- get
      let c = byteVec V.! dataPtr
      liftIO $ putStr [chr $ fromIntegral c] -- Int8 cannot apply to chr
      put (byteVec, dataPtr)
    executeInstruction (Loop loopInstruction) = do
      (byteVec, dataPtr) <- get
      when (byteVec V.! dataPtr /= 0) $ do
        forM_ loopInstruction executeInstruction
        executeInstruction $ Loop loopInstruction -- jump
