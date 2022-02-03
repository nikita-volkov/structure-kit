module StructureKit.Prelude
  ( module Exports,
    guarded,
    mapCoercible,
    mapCoercible2,
    traverseCoercible,
    showBin,
    showBinFinite,

    -- *
    BVec,
    UVec,
  )
where

import Control.Applicative as Exports hiding (WrappedArrow (..))
import Control.Arrow as Exports hiding (first, second)
import Control.Category as Exports
import Control.Concurrent as Exports
import Control.DeepSeq as Exports
import Control.Exception as Exports
import Control.Monad as Exports hiding (fail, forM, forM_, mapM, mapM_, msum, sequence, sequence_)
import Control.Monad.Fail as Exports
import Control.Monad.Fix as Exports hiding (fix)
import Control.Monad.IO.Class as Exports
import Control.Monad.Primitive as Exports
import Control.Monad.ST as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Cont as Exports hiding (callCC, shift)
import Control.Monad.Trans.Except as Exports (Except, ExceptT (ExceptT), catchE, except, mapExcept, mapExceptT, runExcept, runExceptT, throwE, withExcept, withExceptT)
import Control.Monad.Trans.Maybe as Exports
import Control.Monad.Trans.Reader as Exports (Reader, ReaderT (ReaderT), ask, mapReader, mapReaderT, runReader, runReaderT, withReader, withReaderT)
import Control.Monad.Trans.State.Strict as Exports (State, StateT (StateT), evalState, evalStateT, execState, execStateT, mapState, mapStateT, runState, runStateT, withState, withStateT)
import Control.Monad.Trans.Writer.Strict as Exports (Writer, WriterT (..), execWriter, execWriterT, mapWriter, mapWriterT, runWriter)
import Data.Bifunctor as Exports
import Data.Bits as Exports
import Data.Bool as Exports
import Data.ByteString as Exports (ByteString)
import Data.Char as Exports
import Data.Coerce as Exports
import Data.Complex as Exports
import Data.Data as Exports
import Data.Dynamic as Exports
import Data.Either as Exports
import Data.Fixed as Exports
import Data.Foldable as Exports hiding (toList)
import Data.Function as Exports hiding (id, (.))
import Data.Functor as Exports
import Data.Functor.Compose as Exports
import Data.Functor.Identity as Exports
import Data.Group as Exports
import Data.Hashable as Exports (Hashable (hash, hashWithSalt))
import Data.IORef as Exports
import Data.Int as Exports
import Data.IntMap.Strict as Exports (IntMap)
import Data.IntSet as Exports (IntSet)
import Data.Ix as Exports
import Data.List as Exports hiding (all, and, any, concat, concatMap, elem, find, foldl, foldl', foldl1, foldr, foldr1, isSubsequenceOf, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, or, product, sortOn, sum, uncons)
import Data.List.NonEmpty as Exports (NonEmpty (..))
import Data.Maybe as Exports
import Data.Monoid as Exports hiding (Alt)
import Data.Ord as Exports
import Data.Primitive as Exports hiding (alignment, sizeOf)
import Data.Proxy as Exports
import Data.Ratio as Exports
import Data.STRef as Exports
import Data.Semigroup as Exports hiding (First (..), Last (..))
import Data.String as Exports
import Data.String.ToString as Exports (ToString (..))
import Data.Text as Exports (Text)
import Data.Traversable as Exports
import Data.Tuple as Exports
import Data.Unique as Exports
import qualified Data.Vector as BVec
import qualified Data.Vector.Unboxed as UVec
import Data.Version as Exports
import Data.Void as Exports
import Data.Word as Exports
import Debug.Trace as Exports
import DeferredFolds.Unfoldr as Exports (Unfoldr (..))
import Deque.Strict as Exports (Deque)
import Foreign.ForeignPtr as Exports
import Foreign.Ptr as Exports
import Foreign.StablePtr as Exports
import Foreign.Storable as Exports
import GHC.Conc as Exports hiding (orElse, threadWaitRead, threadWaitReadSTM, threadWaitWrite, threadWaitWriteSTM, withMVar)
import GHC.Exts as Exports (IsList (..), groupWith, inline, lazy, sortWith)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import GHC.OverloadedLabels as Exports
import Numeric as Exports
import System.Environment as Exports
import System.Exit as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import System.IO.Unsafe as Exports
import System.Mem as Exports
import System.Mem.StableName as Exports
import System.Timeout as Exports
import Text.ParserCombinators.ReadP as Exports (ReadP, readP_to_S, readS_to_P)
import Text.ParserCombinators.ReadPrec as Exports (ReadPrec, readP_to_Prec, readPrec_to_P, readPrec_to_S, readS_to_Prec)
import Text.Printf as Exports (hPrintf, printf)
import Text.Read as Exports (Read (..), readEither, readMaybe)
import Unsafe.Coerce as Exports
import Prelude as Exports hiding (all, and, any, concat, concatMap, elem, fail, foldl, foldl1, foldr, foldr1, id, mapM, mapM_, maximum, minimum, notElem, or, product, sequence, sequence_, sum, (.))

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded pred x = if pred x then pure x else empty

mapCoercible :: Coercible a b => (a -> a) -> b -> b
mapCoercible mapper =
  coerce . mapper . coerce

mapCoercible2 :: Coercible f g => (g a -> g b) -> f a -> f b
mapCoercible2 mapper =
  coerce . mapper . coerce

traverseCoercible :: (Coercible a b, Functor f) => (a -> f a) -> b -> f b
traverseCoercible f =
  fmap coerce . f . coerce

showBin :: (Integral a, Show a) => a -> String
showBin a = showIntAtBase 2 intToDigit a ""

showBinFinite :: (Integral a, Show a, FiniteBits a) => a -> String
showBinFinite a =
  let str = showBin a
      len = length str
      rem = finiteBitSize a - len
      pad = replicate rem '0'
   in pad <> str

type BVec = BVec.Vector

type UVec = UVec.Vector
