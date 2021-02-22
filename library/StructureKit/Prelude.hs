module StructureKit.Prelude
( 
  module Exports,
  mapCoercible,
  mapCoercible2,
  traverseCoercible,
)
where

-- base
-------------------------
import Control.Applicative as Exports hiding (WrappedArrow(..))
import Control.Arrow as Exports hiding (first, second)
import Control.Category as Exports
import Control.Concurrent as Exports
import Control.Exception as Exports
import Control.Monad as Exports hiding (fail, mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.IO.Class as Exports
import Control.Monad.Fail as Exports
import Control.Monad.Fix as Exports hiding (fix)
import Control.Monad.ST as Exports
import Data.Bifunctor as Exports
import Data.Bits as Exports
import Data.Bool as Exports
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
import Data.Int as Exports
import Data.IORef as Exports
import Data.Ix as Exports
import Data.List as Exports hiding (sortOn, isSubsequenceOf, uncons, concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
import Data.List.NonEmpty as Exports (NonEmpty(..))
import Data.Maybe as Exports
import Data.Monoid as Exports hiding (Alt)
import Data.Ord as Exports
import Data.Proxy as Exports
import Data.Ratio as Exports
import Data.Semigroup as Exports hiding (First(..), Last(..))
import Data.STRef as Exports
import Data.String as Exports
import Data.Traversable as Exports
import Data.Tuple as Exports
import Data.Unique as Exports
import Data.Version as Exports
import Data.Void as Exports
import Data.Word as Exports
import Debug.Trace as Exports
import Foreign.ForeignPtr as Exports
import Foreign.Ptr as Exports
import Foreign.StablePtr as Exports
import Foreign.Storable as Exports
import GHC.Conc as Exports hiding (orElse, withMVar, threadWaitWriteSTM, threadWaitWrite, threadWaitReadSTM, threadWaitRead)
import GHC.Exts as Exports (IsList(..), lazy, inline, sortWith, groupWith)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import GHC.OverloadedLabels as Exports
import Numeric as Exports
import Prelude as Exports hiding (fail, concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, id, (.))
import System.Environment as Exports
import System.Exit as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import System.IO.Unsafe as Exports
import System.Mem as Exports
import System.Mem.StableName as Exports
import System.Timeout as Exports
import Text.ParserCombinators.ReadP as Exports (ReadP, readP_to_S, readS_to_P)
import Text.ParserCombinators.ReadPrec as Exports (ReadPrec, readPrec_to_P, readP_to_Prec, readPrec_to_S, readS_to_Prec)
import Text.Printf as Exports (printf, hPrintf)
import Text.Read as Exports (Read(..), readMaybe, readEither)
import Unsafe.Coerce as Exports

-- containers
-------------------------
import Data.IntMap.Strict as Exports (IntMap)
import Data.IntSet as Exports (IntSet)

-- deque
-------------------------
import Deque.Strict as Exports (Deque)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Cont as Exports hiding (shift, callCC)
import Control.Monad.Trans.Except as Exports (ExceptT(ExceptT), Except, except, runExcept, runExceptT, mapExcept, mapExceptT, withExcept, withExceptT, throwE, catchE)
import Control.Monad.Trans.Maybe as Exports
import Control.Monad.Trans.Reader as Exports (Reader, ask, runReader, mapReader, withReader, ReaderT(ReaderT), runReaderT, mapReaderT, withReaderT)
import Control.Monad.Trans.State.Strict as Exports (State, runState, evalState, execState, mapState, withState, StateT(StateT), runStateT, evalStateT, execStateT, mapStateT, withStateT)
import Control.Monad.Trans.Writer.Strict as Exports (Writer, runWriter, execWriter, mapWriter, WriterT(..), execWriterT, mapWriterT)
import Data.Functor.Compose as Exports
import Data.Functor.Identity as Exports

-- text
-------------------------
import Data.Text as Exports (Text)

-- deferred-folds
-------------------------
import DeferredFolds.Unfoldr as Exports (Unfoldr)

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable(hash, hashWithSalt))


mapCoercible :: Coercible a b => (a -> a) -> b -> b
mapCoercible mapper =
  coerce . mapper . coerce

mapCoercible2 :: Coercible f g => (g a -> g b) -> f a -> f b
mapCoercible2 mapper =
  coerce . mapper . coerce

traverseCoercible :: (Coercible a b, Functor f) => (a -> f a) -> b -> f b
traverseCoercible f =
  fmap coerce . f . coerce
