{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.ByteArray
    (
      -- * The byte array types
      ByteArray
    , MutableByteArray

      -- * Reading and writing
    , ByteArrayElem(..)

      -- * Creating
    , Pinned
    , Unpinned
    , new
    , newPinned
    , newAlignedPinned

      -- * Size
    , sizeOfByteArray
    , sizeOfMutableByteArray

      -- * Comparing
    , sameByteArray
    , sameMutableByteArray
    , compareByteArray
    , compareMutableByteArray
    , compareByteArrayToMutableByteArray

      -- * Copying
    , copyByteArray
    , copyMutableByteArray
    , copyByteArrayToPtr
    , copyMutableByteArrayToPtr
    , copyPtrToMutableByteArray
    , fillByteArray

      -- * Freezing and thawing
    , unsafeFreeze
    , unsafeThaw

      -- * Converting between pinned and unpinned
    , byteArrayAsPinned
    , byteArrayAsUnpinned
    , mutableByteArrayAsPinned
    , mutableByteArrayAsUnpinned
    , byteArrayContents
    , mutableByteArrayContents

      -- * Searching
    , findIndexByteArray
    , findIndexMutableByteArray
    ) where

import Data.Bits (bitSize, unsafeShiftL, unsafeShiftR)
import Data.Word
import GHC.Exts
import GHC.ST
import GHC.Word

data ByteArray p = BA ByteArray#

data MutableByteArray p s = MBA (MutableByteArray# s)

wordSize :: Int
wordSize = 1 `unsafeShiftL` (bitSize (undefined :: Int))

wordShift :: Int
wordShift
    | wordSize == 4 = 2
    | otherwise     = 3

class ByteArrayElem a where
    indexElemOff :: ByteArray p -> Int -> a
    indexByteOff :: ByteArray p -> Int -> a
    indexByteOffUnaligned :: ByteArray p -> Int -> a

    readElemOff :: MutableByteArray p s -> Int -> ST s a
    readByteOff :: MutableByteArray p s -> Int -> ST s a
    readByteOffUnaligned :: MutableByteArray p s -> Int -> ST s a

    writeElemOff :: MutableByteArray p s -> Int -> a -> ST s ()
    writeByteOff :: MutableByteArray p s -> Int -> a -> ST s ()
    writeByteOffUnaligned :: MutableByteArray p s -> Int -> a -> ST s ()

    sizeOf :: a -> Int
    alignment :: a -> Int

instance ByteArrayElem Word8 where
    indexElemOff (BA ba) (I# i) = W8# (indexWord8Array# ba i)
    indexByteOff ba i = indexElemOff ba i
    indexByteOffUnaligned ba i = indexElemOff ba i

    readElemOff (MBA mba) (I# i) = ST $ \ s -> case readWord8Array# mba i s of
        (# s2, x #) -> (# s2, W8# x #)
    readByteOff ba i = readElemOff ba i
    readByteOffUnaligned mba i = readByteOff mba i

    writeElemOff (MBA mba) (I# i) (W8# x) = ST $ \ s ->
        case writeWord8Array# mba i x s of
            s2 -> (# s2, () #)
    writeByteOff ba i x = writeElemOff ba i x
    writeByteOffUnaligned mba i x = writeByteOff mba i x

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

    writeElemOff (MBA mba) (I# i) (W# x) = ST $ \ s ->
        case writeWordArray# mba i x s of
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
indexByteOffUnalignedWord :: ByteArray p -> Int -> Word
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

readByteOffUnalignedWord :: MutableByteArray p s -> Int -> ST s Word
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

writeByteOffUnalignedWord :: MutableByteArray p s -> Int -> Word -> ST s ()
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

-- ---------------------------------------------------------------------
-- Creating byte arrays

-- | The byte array is not know to be pinned, but might be.
data Unpinned

-- | The byte array is definitely pinned.
data Pinned

new :: Int -> ST s (MutableByteArray Unpinned s)
new (I# n) = ST $ \ s -> case newByteArray# n s of
    (# s2, mba #) -> (# s2, MBA mba #)

newPinned :: Int -> ST s (MutableByteArray Pinned s)
newPinned (I# n) = ST $ \ s -> case newPinnedByteArray# n s of
    (# s2, mba #) -> (# s2, MBA mba #)

newAlignedPinned :: Int
                 -> Int  -- ^ Alignment, in bytes. Must be power of 2
                 -> ST s (MutableByteArray Pinned s)
newAlignedPinned (I# n) (I# align)= ST $ \ s ->
    case newAlignedPinnedByteArray# n align s of
        (# s2, mba #) -> (# s2, MBA mba #)

-- ---------------------------------------------------------------------
-- Copying byte arrays

-- | The two arrays are not allowed to alias.
copyByteArray :: ByteArray p -> Int -> MutableByteArray p' s -> Int
              -> Int -> ST s ()
copyByteArray = undefined

-- | The two arrays are allowed to alias.
copyMutableByteArray :: MutableByteArray p s -> Int -> MutableByteArray p' s -> Int
                     -> Int -> ST s ()
copyMutableByteArray = undefined

-- | The two arrays are not allowed to alias.
copyByteArrayToPtr :: ByteArray p -> Int -> Ptr Word8 -> Int -> IO ()
copyByteArrayToPtr = undefined

-- | The two arrays are not allowed to alias.
copyMutableByteArrayToPtr :: MutableByteArray p RealWorld -> Int -> Ptr Word8
                          -> Int -> IO ()
copyMutableByteArrayToPtr = undefined

-- | The two arrays are not allowed to alias.
copyPtrToMutableByteArray :: Ptr Word8 -> MutableByteArray p RealWorld -> Int
                          -> Int -> IO ()
copyPtrToMutableByteArray = undefined

-- ---------------------------------------------------------------------
-- Freezing and thawing

-- | The mutable byte array must not be modified after freezing.
unsafeFreeze :: MutableByteArray p s -> ST s (ByteArray p)
unsafeFreeze (MBA mba) = ST $ \ s -> case unsafeFreezeByteArray# mba s of
    (# s2, ba #) -> (# s2, BA ba #)

-- | The original byte array must not be used after thawing.
unsafeThaw :: ByteArray p -> ST s (MutableByteArray p s)
unsafeThaw (BA ba) = return (MBA (unsafeCoerce# ba))

-- ---------------------------------------------------------------------
-- Size

sizeOfByteArray :: ByteArray p -> Int
sizeOfByteArray (BA ba) = I# (sizeofByteArray# ba)

sizeOfMutableByteArray :: MutableByteArray p s -> Int
sizeOfMutableByteArray (MBA mba) = I# (sizeofMutableByteArray# mba)

byteArrayContents :: ByteArray Pinned -> Ptr Word8
byteArrayContents = undefined

mutableByteArrayContents :: MutableByteArray Pinned s -> Ptr Word8
mutableByteArrayContents = byteArrayContents . unsafeCoerce#

fillByteArray :: MutableByteArray p s -> Int -> Int -> Word8 -> ST s ()
fillByteArray = undefined

-- | Returns -1 if the byte wasn't found.
findIndexByteArray :: ByteArray p -> Int -> Int -> Word8 -> Int
findIndexByteArray = undefined

-- | Returns -1 if the byte wasn't found.
findIndexMutableByteArray :: MutableByteArray p s -> Int -> Int -> Word8
                          -> ST s Int
findIndexMutableByteArray = undefined

-- ---------------------------------------------------------------------
-- Comparing

-- | See memchr (loose semantics) for details on the return value.
compareByteArray :: ByteArray p -> Int -> ByteArray p' -> Int -> Int -> Int
compareByteArray = undefined

-- | See memchr (loose semantics) for details on the return value.
compareMutableByteArray :: MutableByteArray p s -> Int -> MutableByteArray p' s
                        -> Int -> Int -> ST s Int
compareMutableByteArray = undefined

-- | See memchr (loose semantics) for details on the return value.
compareByteArrayToMutableByteArray :: ByteArray p -> Int
                                   -> MutableByteArray p' s -> Int -> Int
                                   -> ST s Int
compareByteArrayToMutableByteArray = undefined

sameByteArray :: ByteArray p -> ByteArray p' -> Bool
sameByteArray (BA ba1) (BA ba2) = sameMutableByteArray (unsafeCoerce# ba1)
                                  (unsafeCoerce# ba2)

sameMutableByteArray :: MutableByteArray p s -> MutableByteArray p' s -> Bool
sameMutableByteArray (MBA mba1) (MBA mba2) = sameMutableByteArray# mba1 mba2

-- ---------------------------------------------------------------------
-- Converting between pinned and unpinned byte arrays

byteArrayAsPinned :: ByteArray p -> Maybe (ByteArray Pinned)
byteArrayAsPinned (BA ba)
    | isPinned ba = Just (BA (unsafeCoerce# ba))
    | otherwise   = Nothing
  where
    isPinned = undefined

byteArrayAsUnpinned :: ByteArray p -> ByteArray Unpinned
byteArrayAsUnpinned (BA ba) = BA (unsafeCoerce# ba)

mutableByteArrayAsPinned :: MutableByteArray p s -> Maybe (MutableByteArray Pinned s)
mutableByteArrayAsPinned (MBA mba)
    | isPinned mba = Just (MBA (unsafeCoerce# mba))
    | otherwise    = Nothing
  where
    isPinned = undefined

mutableByteArrayAsUnpinned :: MutableByteArray p s -> MutableByteArray Unpinned s
mutableByteArrayAsUnpinned (MBA mba) = MBA (unsafeCoerce# mba)

{-
Primops to implement:
 * Byte-offset reads/writes that are unaligned/aligned
 * isPinned
-}
