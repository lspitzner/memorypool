{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonadComprehensions #-}

-- | A very basic memory pool imlemented in haskell.
--
-- The core idea is that the pool allocates large chunks of memory that are
-- some power-of-two factor (e.g. 256) of some base size (e.g. 10k).
-- The user of the pool allocates chunks of a power-of-two factor of the base
-- size (i.e. 10k, 20k, 40k, ..). This scheme avoids fragmentation due to
-- weirdly-sized holes, but keep in mind that no compaction takes place, so
-- this kind of fragmentation must be worked around manually if necessary.
--
-- The pool internally allocates memory on the C heap, i.e. outside of any
-- haskell/GC heap.
--
-- Uses a buddy allocation strategy internally.
module System.MemoryPool
  ( -- * type
    Pool
    -- * interface
  , create
  , allocate
  , allocateForeign
    -- * low-level and debugging functions
  , PoolData
  , debugShowPoolFillsData
  , debugTracePoolFills
  , getPtrFragmentation
  , unsafeGetPoolDataSnapshot
  )
where



import           Control.Monad ( msum, forM, mzero )
import           Control.Arrow ( second )
import           Data.Maybe    ( fromMaybe )
import           Foreign.ForeignPtr
import           Foreign.C.Types
import           Control.Concurrent.MVar
import           Control.Monad.Trans.Maybe ( runMaybeT, MaybeT(..) )

import           Data.Void ( Void )

import           Foreign.Ptr ( Ptr )

import qualified Data.Vector.Storable.Mutable as VectorSM

import qualified Data.IntMap as IntMap

import qualified Foreign.Ptr as Ptr
import qualified Foreign.Concurrent
import qualified Foreign.Marshal.Array
import qualified Foreign.Marshal.Alloc
import qualified System.Unsafe as Unsafe

import qualified Data.Bits as Bits

import           Control.Applicative ( (<|>) )

import           Debug.Trace ( trace )

import           Prelude



(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | Stateful pool object
newtype Pool = Pool (MVar PoolData)

-- | Internal pool type
data PoolData = PoolData
  { _mp_baseSize :: CSize
    -- ^ base size. all alocations inside this pool will be rounded up to
    -- a multiple of this.
  , _mp_blockSizeExp :: CSize
    -- ^ the pool internally manages a number of blocks, each containing
    -- (2**_mp_blockSizeExp * _mp_baseSize) bytes.
  , _mp_blockSize :: CSize
  , _mp_poolBlocks :: IntMap.IntMap PoolBlock
    -- ^ map to the blocks
  , _mp_freeHint :: Int
    -- ^ probably an index of some block that _probably_ has some unallocated
    -- regions left. no promises; may point to full block or not point to any
    -- block at all.
  }

data PoolBlock = PoolBlock (VectorSM.IOVector CSize) (Ptr Void)

instance Show PoolBlock where
  show (PoolBlock v ptr) = Unsafe.performIO $ do
    vStr <- fmap show $ [0 .. VectorSM.length v - 1] `forM` VectorSM.read v
    return $ "PoolBlock " ++ vStr ++ " " ++ show ptr

-- blocks use a serialized version of a balanced binary tree.
--     0
--   1   2
--  3 4 5 6
--
-- at each node, we annotate what the largest completely free subtree, using
-- and Int. 0 means in-use; an empty leaf has a 1; completely empty nodes have
-- n for a node with depth n; a partially filled node has n where n is the
-- annotation on the (largest) non-filled sub-tree.
--
-- for example, if the usage is
--     _
--   _   _
--  _ x _ _
--
-- where x denotes the only node currently in use, we annotate this as
--     2
--   1   2
--  1 0 1 1
--
-- or rather, sequentially:
--
-- 2,1,2,1,0,1,1.

-- | Create an empty pool. A pool internally allocates several relatively
-- large blocks via 'malloc' and returns chunks of these when requested via
-- 'allocate'.
--
-- Note that the rts does not "see" these allocations in any way. I.e. memory
-- allocated in the pool does not count towards heap space in the rts and
-- is not captured by heap profiling.
create
  :: CSize -- ^ base size.
  -> CSize -- ^ block exponent; each internal block holds
           -- (2**block_exp) * base_size.
           -- Must be at least 1.
  -> IO Pool
create base blockExp =
  do
    -- finPtr <- $(CInline.mkFunPtrFromName 'poolFin)
    let
      pool = PoolData
        { _mp_baseSize     = base
        , _mp_blockSizeExp = blockExp
        , _mp_blockSize    = Bits.shift 1 (fromIntegral blockExp) * base
        , _mp_poolBlocks   = IntMap.empty
        , _mp_freeHint     = 0
        -- , _pp_finalizerPtr = finPtr
        }
    fmap Pool $ newMVar pool

-- | Allocate memory inside the specified pool. The amount allocated is rounded
-- up to the next power-of-two multiple of the base size.
-- 
-- The number of bytes to allocated is limited in both directions:
-- The minimum amount is 1 (0 is an error).
-- The maximum is the number of a bytes in a block as specified by the
-- arguments to @'create'@.
--
-- No deallocation happens unless the provided deallocation action is executed.
-- (See 'allocateForeign' for a more automatic variant of this function.)
--
-- The deallocation action must not be called more than once.
allocate
  :: Pool -- ^ pool to allocate in
  -> CSize      -- ^ number of bytes to allocate.
  -> IO (IO (), Ptr a) -- ^ the ptr to the memory allocated in the pool,
                       -- plus a deallaction action.
allocate _ 0 = error "MemoryPool: allocating 0 bytes not supported."
allocate (Pool poolMVar) nBytes = -- traceShow ("allocate", nBytes) $
                                        do
  poolData <- takeMVar poolMVar
  let rounded = ((nBytes-1) `div` _mp_baseSize poolData) + 1
      sizeFact = if
        | rounded==1     -> 1
        | rounded==2     -> 2
        | rounded<=4     -> 4
        | rounded<=8     -> 8
        | rounded<=16    -> 16
        | rounded<=32    -> 32
        | rounded<=64    -> 64
        | rounded<=128   -> 128
        | rounded<=256   -> 256
        | rounded<=512   -> 512
        | rounded<=1024  -> 1024
        | rounded<=2048  -> 2048
        | rounded<=4096  -> 4096
        | rounded<=8192  -> 8192
        | rounded<=16384 -> 16384
        | rounded<=32768 -> 32768
        | rounded<=65536 -> 65536 -- not the most efficient, and not
        | otherwise      -> error "MemoryPool: allocation too large!"
  (poolData', blockInd, metaInd, fPtr)
    :: (PoolData, Int, Int, Ptr Void)
    <- allocateFromDepth poolData $ -- traceShow ("sizeFact", sizeFact) $
                                    sizeFact
  -- traceShow poolData' $
  putMVar poolMVar $ poolData'
  return (deallocateElem poolMVar blockInd metaInd sizeFact, Ptr.castPtr fPtr)

-- | Similar to 'allocate', but performs the deallocation automatically as
-- a finalizer on the returned ForeignPtr. This may lead to (arbitrary) delays
-- between dropping of the reference and actual freeing of pool memory, but
-- is much more convenient on usage side.
allocateForeign
  :: Pool -- ^ pool to allocate in
  -> CSize      -- ^ number of bytes to allocate.
  -> IO (ForeignPtr a) -- a ForeignPtr that has a finalizer connected to it
                       -- which performs the deallocation inside the pool.
allocateForeign pool nBytes = do
  (destr, ptr) <- allocate pool nBytes
  Foreign.Concurrent.newForeignPtr ptr destr

deallocateElem :: MVar PoolData -> Int -> Int -> CSize -> IO ()
deallocateElem poolMVar blockInd metaInd sizeFact = do
  poolData <- takeMVar poolMVar
  let curBlock = fromMaybe (error "MemoryPool internal error 919238912")
               $ IntMap.lookup blockInd (_mp_poolBlocks poolData)
  block' <- deallocateElemBlock curBlock metaInd sizeFact
  putMVar poolMVar $ poolData
    { _mp_poolBlocks =
        IntMap.update (const block') blockInd (_mp_poolBlocks poolData)
    }
deallocateElemBlock :: PoolBlock -> Int -> CSize -> IO (Maybe PoolBlock)
deallocateElemBlock block@(PoolBlock meta rawPtr) metaInd sizeFact = do
  VectorSM.write meta metaInd sizeFact
  isEmpty <- go metaInd sizeFact
  if isEmpty
    then do
      -- trace ("freeing memory at " ++ show (Ptr.ptrToIntPtr rawPtr)) $
      Foreign.Marshal.Alloc.free rawPtr
      return Nothing
    else return $ Just block
 where
  go :: Int -> CSize -> IO Bool
  go 0 _ = return True
  go n f = do
    let n' = ((n+1) `div` 2) - 1
    do
      l <- VectorSM.read meta (2*n'+1)
      r <- VectorSM.read meta (2*n'+2)
      case (l, r) of
        (x, y) | x==f && y==f -> do
          VectorSM.write meta n' (2*f)
          if n'==0
            then return True
            else go n' (2*f)
        (x, y) -> do
          VectorSM.write meta n' (max x y)
          if n'==0
            then return False
            else go n' (2*f)
allocateFromDepth
  :: PoolData
  -> CSize -- factor to baseSize to allocate
  -> IO (PoolData, Int, Int, Ptr Void)
allocateFromDepth pool sizeFact = do
  runMaybeT opts >>= \case
    Nothing -> do
      let firstUnusedIndex =
            id
              $ fst
              $ head
              $ filter (not . snd)
              $ [0..] <&> \i -> (i, IntMap.member i (_mp_poolBlocks pool))
      block <- allocBlock
      let pool' = pool
            { _mp_poolBlocks =
                IntMap.insert firstUnusedIndex block (_mp_poolBlocks pool)
            , _mp_freeHint = firstUnusedIndex
            }
      mR <- runMaybeT $ allocInBlock (firstUnusedIndex, block)
      case mR of
        Nothing -> error "MemoryPool internal error 88838123"
        Just (r1, r2, r3) -> do
          return (pool', r1, r2, r3)
    Just (r1, r2, r3) -> do
      let pool' = pool { _mp_freeHint = r1 }
      return (pool', r1, r2, r3)
 where
  allocBlock :: IO PoolBlock
  allocBlock = do
    let metaWidth = Bits.shift 1 $ fromIntegral $ _mp_blockSizeExp pool
    let metaLength = metaWidth*2 - 1
    let dataLength = _mp_blockSize pool
    blockPtr <- -- trace ("allocating " ++ show dataLength ++ " bytes via mallocBytes") $
      Foreign.Marshal.Alloc.mallocBytes $ fromIntegral dataLength
    metaPtr  <- -- trace ("address is " ++ show (Ptr.ptrToIntPtr blockPtr)) $
      newForeignPtr Foreign.Marshal.Alloc.finalizerFree
        =<< Foreign.Marshal.Array.mallocArray (fromIntegral $ metaLength)
    let vect = VectorSM.unsafeFromForeignPtr0 metaPtr (fromIntegral $ metaLength)
    let go _ 0 = return ()
        go n f = do
          VectorSM.write vect n f
          go (2*n+1) (f `div` 2)
          go (2*n+2) (f `div` 2)
    go 0 metaWidth
    return $ PoolBlock vect blockPtr
  opts = allocFromHint <|> allocFromAnyBlock
  allocFromHint :: MaybeT IO (Int, Int, Ptr Void)
  allocFromHint = case IntMap.lookup (_mp_freeHint pool) (_mp_poolBlocks pool) of
    Just x  -> allocInBlock (_mp_freeHint pool, x)
    Nothing -> mzero
  allocFromAnyBlock :: MaybeT IO (Int, Int, Ptr Void)
  allocFromAnyBlock = do
    msum $ fmap allocInBlock $ IntMap.toList $ _mp_poolBlocks pool
  allocInBlock
    :: (Int, PoolBlock)
    -> MaybeT IO (Int, Int, Ptr Void)
  allocInBlock (poolInd, PoolBlock meta rawPtr) = MaybeT $ do
    -- TODO: why the **** does Ptr.plusPtr not take CUIntPtr as parameter.
    r <- go 0 (Bits.shift 1 $ fromIntegral $ _mp_blockSizeExp pool) 0
    case r of
      Nothing -> return Nothing
      Just (metaInd, offset) -> do
        return
          $ return
          $ ( poolInd
            , metaInd
            , -- (\x -> traceShow (Ptr.ptrToIntPtr x, Ptr.ptrToIntPtr rawPtr, offset) x) $
                Ptr.plusPtr
                rawPtr
                (fromIntegral(offset*_mp_baseSize pool))
            )
   where
      -- we pass/handle the offsets directly here, because calculating them
      -- afterwards is cumbersome (if possible, given the max depth).
      -- 0
      -- 1       2
      -- 3   4   5   6
      -- 7 8 9 0¹1¹2¹3¹4 (leafs)
      -- 0 1 2 3 4 5 6 7 (offsets)
    go :: Int -> CSize -> CSize -> IO (Maybe (Int, CSize))
    go ind depthFact offset = do
      x <- VectorSM.read meta ind
      case x of
        i | i<sizeFact ->
          return Nothing
        i | i==depthFact && sizeFact==depthFact -> do
          VectorSM.write meta ind 0
          return $ Just (ind, offset)
        1 | depthFact==1 -> do
          VectorSM.write meta ind 0
          return $ Just (ind, offset)
        _ -> do
          -- we are not at depth 1 because of the above clause.
          -- consequently we unconditionally need to do the re-calculation of
          -- the current node's value below.
          leftR <- go (2*ind+1) (depthFact `div` 2) offset
          res <- case leftR of
            Nothing ->
              go (2*ind+2) (depthFact `div` 2) (offset + depthFact `div` 2)
            Just{} -> do
              return leftR
          do
            l <- VectorSM.read meta (2*ind+1)
            r <- VectorSM.read meta (2*ind+2)
            VectorSM.write meta ind (max l r)
          return res


-- | Return a visual representation of allocation inside the pool. Both
-- distribution of blocks and fragmentation inside each block is displayed.
debugShowPoolFillsData :: PoolData -> String
debugShowPoolFillsData pool
  = unlines
  $ fmap (\(i, s) -> show i ++ " " ++ s)
  $ fmap (second h)
  $ IntMap.toList
  $ _mp_poolBlocks pool
  where
    metaWidth = Bits.shift 1 $ fromIntegral $ _mp_blockSizeExp pool
    h :: PoolBlock -> String
    h (PoolBlock meta _) = Unsafe.performIO $ g meta 0 metaWidth
    g meta ind f = do
      x <- VectorSM.read meta ind
      case x of
        0 | f==1 -> return $ "#"
        1 | f==1 -> return $ " "
        0 -> do
          a <- VectorSM.read meta (2*ind+1)
          b <- VectorSM.read meta (2*ind+2)
          if a==0 || b==0
            then do
              l <- g meta (2*ind+1) (f `div` 2)
              r <- g meta (2*ind+2) (f `div` 2)
              return $ l++r
            else return $ '#' : replicate (f-1) '+'
        _ -> do
          l <- g meta (2*ind+1) (f `div` 2)
          r <- g meta (2*ind+2) (f `div` 2)
          return $ l++r

-- | Prints a visual representation of allocation inside the pool to stderr.
-- Both distribution of blocks and fragmentation inside each block is
-- displayed.
debugTracePoolFills :: Pool -> IO ()
debugTracePoolFills (Pool poolMVar) = do
  poolData <- takeMVar poolMVar
  trace ("\n" ++ debugShowPoolFillsData poolData) $ putMVar poolMVar poolData

-- | if Ptr is not allocated in this pool, returns Nothing.
-- Otherwise returns a rather rough estimate of the usage for the block that
-- the pointer is allocated in. For example if it returns (Just 0.75), at least
-- 25% of the block is free (the other bound should be.. 50% i think. But the
-- error depends in a nontrivial fashion on the value.
-- Use 'unsafeGetPoolDataSnapshot' to obtain the first argument.
getPtrFragmentation :: PoolData -> Ptr a -> Maybe Float
getPtrFragmentation poolData ptr
  = msum
  $ fmap (\(PoolBlock meta blkptr) ->
            [ Unsafe.performIO $ go meta
            | ptrV>=blkptr && ptrV<(Ptr.plusPtr blkptr bLen)
            ])
  $ IntMap.elems
  $ _mp_poolBlocks poolData
 where
  metaWidth :: Int
  metaWidth = Bits.shift 1 $ fromIntegral $ _mp_blockSizeExp poolData
  bLen :: Int
  bLen = fromIntegral (_mp_blockSize poolData)
  ptrV = Ptr.castPtr ptr
  go :: VectorSM.IOVector CSize -> IO Float
  go meta = do
    x <- VectorSM.read meta 0
    y <- VectorSM.read meta 1
    z <- VectorSM.read meta 2
    return $ if
      | x==0 -> 1.0
      | True -> 1.0 - fromIntegral (y + z) / fromIntegral metaWidth

-- | Retrieve a snapshot of the internal data of a pool. This currently exists
-- soly as an argument to 'getPtrFragmentation'.
unsafeGetPoolDataSnapshot :: Pool -> IO PoolData
unsafeGetPoolDataSnapshot (Pool mvar) = readMVar mvar
