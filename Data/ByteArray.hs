{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.ByteArray where

import Data.Bits (bitSize, unsafeShiftL, unsafeShiftR)
import Data.Word
import GHC.Exts
import GHC.ST
import GHC.Word

data ByteArray = BA ByteArray#

data MutableByteArray s = MBA (MutableByteArray# s)

wordSize :: Int
wordSize = 1 `unsafeShiftL` (bitSize (undefined :: Int))

wordShift :: Int
wordShift
    | wordSize == 4 = 2
    | otherwise     = 3

class ByteArrayElem a where
    indexElemOff :: ByteArray -> Int -> a
    indexByteOff :: ByteArray -> Int -> a
    indexByteOffUnaligned :: ByteArray -> Int -> a

    readElemOff :: MutableByteArray s -> Int -> ST s a
    readByteOff :: MutableByteArray s -> Int -> ST s a
    readByteOffUnaligned :: MutableByteArray s -> Int -> ST s a

    writeElemOff :: MutableByteArray s -> Int -> a -> ST s ()
    writeByteOff :: MutableByteArray s -> Int -> a -> ST s ()
    writeByteOffUnaligned :: MutableByteArray s -> Int -> a -> ST s ()

    sizeOf :: a -> Int
    alignment :: a -> Int

instance ByteArrayElem Word8 where
    indexElemOff (BA ba) (I# i) = W8# (indexWord8Array# ba i)
    indexByteOff ba i = indexElemOff ba i
    indexByteOffUnaligned ba i = indexElemOff ba i
    readByteOff ba i = readElemOff ba i
    writeByteOff ba i x = writeElemOff ba i x
    sizeOf _ = 1
    alignment _ = 1

instance ByteArrayElem Word where
    indexElemOff (BA ba) (I# i) = W# (indexWordArray# ba i)
    indexByteOff ba i = indexElemOff ba (i `unsafeShiftR` wordShift)
    indexByteOffUnaligned ba i = indexByteOffUnalignedWord ba i

    readElemOff (MBA mba) (I# i) = ST $ \ s -> case readWordArray# mba i s of
        (# s2, x #) -> (# s2, W# x #)
    readByteOff mba i = readElemOff mba (i `unsafeShiftR` wordShift)
    readByteOffUnaligned mba i = readByteOffUnalignedWord mba i

    writeElemOff (MBA mba) (I# i) (W# x) = ST $ \ s -> case writeWordArray# mba i x s of
        s2 -> (# s2, () #)
    writeByteOff mba i x = writeElemOff mba (i `unsafeShiftR` wordShift) x
    writeByteOffUnaligned mba i x = writeByteOffUnalignedWord mba i x
    
    sizeOf _ = wordSize
    alignment _ = wordSize

instance ByteArrayElem Char where
    indexElemOff ba i = toEnum $ fromIntegral (indexElemOff ba i :: Word)
    indexByteOff ba i = toEnum $ fromIntegral (indexByteOff ba i :: Word)
    indexByteOffUnaligned ba i = toEnum $ fromIntegral (indexByteOffUnaligned ba i :: Word)

    readElemOff mba i = (toEnum . fromIntegral :: Word -> Char) `fmap` (readElemOff mba i)
    readByteOff mba i = (toEnum . fromIntegral :: Word -> Char) `fmap` (readByteOff mba i)
    readByteOffUnaligned mba i = (toEnum . fromIntegral :: Word -> Char) `fmap`
                                 (readByteOffUnaligned mba i)

    writeElemOff mba i x = writeElemOff mba i (fromIntegral $ fromEnum x :: Word)
    writeByteOff mba i x = writeByteOff mba i (fromIntegral $ fromEnum x :: Word)
    writeByteOffUnaligned mba i x = writeByteOffUnaligned mba i
                                    (fromIntegral $ fromEnum x :: Word)
    
    sizeOf _ = wordSize
    alignment _ = wordSize

-- TODO: Move these helpers into the instances and call them from the
--     Word instance.
indexByteOffUnalignedWord :: ByteArray -> Int -> Word
indexByteOffUnalignedWord ba i
    | wordSize == 4 =
        fromIntegral (indexElemOff ba i :: Word8) +
        fromIntegral (indexElemOff ba (i+1) :: Word8) `unsafeShiftL` 8 +
        fromIntegral (indexElemOff ba (i+2) :: Word8) `unsafeShiftL` 16 +
        fromIntegral (indexElemOff ba (i+3) :: Word8) `unsafeShiftL` 24
    | otherwise =
        fromIntegral (indexElemOff ba i :: Word8) +
        fromIntegral (indexElemOff ba (i+1) :: Word8) `unsafeShiftL` 8 +
        fromIntegral (indexElemOff ba (i+2) :: Word8) `unsafeShiftL` 16 +
        fromIntegral (indexElemOff ba (i+3) :: Word8) `unsafeShiftL` 24 +
        fromIntegral (indexElemOff ba (i+4) :: Word8) `unsafeShiftL` 32 +
        fromIntegral (indexElemOff ba (i+5) :: Word8) `unsafeShiftL` 40 +
        fromIntegral (indexElemOff ba (i+6) :: Word8) `unsafeShiftL` 48 +
        fromIntegral (indexElemOff ba (i+7) :: Word8) `unsafeShiftL` 56

readByteOffUnalignedWord :: MutableByteArray s -> Int -> ST s Word
readByteOffUnalignedWord mba i
    | wordSize == 4 = do
        x1 :: Word8 <- readElemOff mba i
        x2 :: Word8 <- readElemOff mba (i+1)
        x3 :: Word8 <- readElemOff mba (i+2)
        x4 :: Word8 <- readElemOff mba (i+3)
        return $! fromIntegral x1 +
            fromIntegral x2 `unsafeShiftL` 8 +
            fromIntegral x3 `unsafeShiftL` 16 +
            fromIntegral x4 `unsafeShiftL` 24
    | otherwise = do
        x1 :: Word8 <- readElemOff mba i
        x2 :: Word8 <- readElemOff mba (i+1)
        x3 :: Word8 <- readElemOff mba (i+2)
        x4 :: Word8 <- readElemOff mba (i+3)
        x5 :: Word8 <- readElemOff mba (i+3)
        x6 :: Word8 <- readElemOff mba (i+3)
        x7 :: Word8 <- readElemOff mba (i+3)
        x8 :: Word8 <- readElemOff mba (i+3)
        return $! fromIntegral x1 +
            fromIntegral x2 `unsafeShiftL` 8 +
            fromIntegral x3 `unsafeShiftL` 16 +
            fromIntegral x4 `unsafeShiftL` 24 +
            fromIntegral x5 `unsafeShiftL` 32 +
            fromIntegral x6 `unsafeShiftL` 40 +
            fromIntegral x7 `unsafeShiftL` 48 +
            fromIntegral x8 `unsafeShiftL` 56

writeByteOffUnalignedWord :: MutableByteArray s -> Int -> Word -> ST s ()
writeByteOffUnalignedWord mba i x
    | wordSize == 4 = do
        writeElemOff mba i (fromIntegral x :: Word8)
        writeElemOff mba (i+1) (fromIntegral (x `unsafeShiftR` 8) :: Word8)
        writeElemOff mba (i+2) (fromIntegral (x `unsafeShiftR` 16) :: Word8)
        writeElemOff mba (i+3) (fromIntegral (x `unsafeShiftR` 24) :: Word8)
    | otherwise = do
        writeElemOff mba i (fromIntegral x :: Word8)
        writeElemOff mba (i+1) (fromIntegral (x `unsafeShiftR` 8) :: Word8)
        writeElemOff mba (i+2) (fromIntegral (x `unsafeShiftR` 16) :: Word8)
        writeElemOff mba (i+3) (fromIntegral (x `unsafeShiftR` 24) :: Word8)
        writeElemOff mba (i+4) (fromIntegral (x `unsafeShiftR` 32) :: Word8)
        writeElemOff mba (i+5) (fromIntegral (x `unsafeShiftR` 40) :: Word8)
        writeElemOff mba (i+6) (fromIntegral (x `unsafeShiftR` 48) :: Word8)
        writeElemOff mba (i+7) (fromIntegral (x `unsafeShiftR` 56) :: Word8)

-- indexCharArray# :: ByteArray# -> Int# -> Char#
-- indexDoubleArray# :: ByteArray# -> Int# -> Double#
-- indexFloatArray# :: ByteArray# -> Int# -> Float#
-- indexInt16Array# :: ByteArray# -> Int# -> Int#
-- indexInt32Array# :: ByteArray# -> Int# -> Int#
-- indexInt64Array# :: ByteArray# -> Int# -> Int#
-- indexInt8Array# :: ByteArray# -> Int# -> Int#
-- indexIntArray# :: ByteArray# -> Int# -> Int#
-- indexStablePtrArray# :: ByteArray# -> Int# -> StablePtr# a
-- indexWideCharArray# :: ByteArray# -> Int# -> Char#
-- indexWord16Array# :: ByteArray# -> Int# -> Word#
-- indexWord32Array# :: ByteArray# -> Int# -> Word#
-- indexWord64Array# :: ByteArray# -> Int# -> Word#
-- indexWord8Array# :: ByteArray# -> Int# -> Word#
-- indexWordArray# :: ByteArray# -> Int# -> Word#
