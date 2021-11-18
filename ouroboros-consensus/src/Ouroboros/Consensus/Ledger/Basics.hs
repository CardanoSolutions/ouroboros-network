{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Definition is 'IsLedger'
--
-- Normally this is imported from "Ouroboros.Consensus.Ledger.Abstract". We
-- pull this out to avoid circular module dependencies.
module Ouroboros.Consensus.Ledger.Basics (
    -- * GetTip
    GetTip (..)
  , getTipHash
  , getTipSlot
    -- * Ledger Events
  , LedgerResult (..)
  , VoidLedgerEvent
  , castLedgerResult
  , embedLedgerResult
  , pureLedgerResult
    -- * Definition of a ledger independent of a choice of block
  , IsLedger (..)
  , LedgerCfg
  , applyChainTick
    -- * Link block to its ledger
  , LedgerConfig
  , LedgerError
  , LedgerState
  , LedgerStateKind
  , TickedLedgerState
    -- * UTxO HD
  , ApplyMapKind (..)
  , DiskLedgerView
  , FootprintL (..)
  , MapKind (..)
  , SMapKind (..)
  , TableKeySets
  , TableStuff (..)
  , emptyAppliedMK
  , mapValuesAppliedMK
  , toSMapKind
    -- * Misc
  , ShowLedgerState (..)
  ) where

import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks (..), OnlyCheckWhnfNamed (..))

import           Ouroboros.Network.Protocol.LocalStateQuery.Type (FootprintL (..))

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util ((..:))

{-------------------------------------------------------------------------------
  Tip
-------------------------------------------------------------------------------}

class GetTip l where
  -- | Point of the most recently applied block
  --
  -- Should be 'genesisPoint' when no blocks have been applied yet
  getTip :: l -> Point l

type instance HeaderHash (Ticked  l)                   = HeaderHash l
type instance HeaderHash (Ticked1 (l :: k -> Type))    = HeaderHash l
type instance HeaderHash (Ticked1 (l :: k -> Type) mk) = HeaderHash l

getTipHash :: GetTip l => l -> ChainHash l
getTipHash = pointHash . getTip

getTipSlot :: GetTip l => l -> WithOrigin SlotNo
getTipSlot = pointSlot . getTip

{-------------------------------------------------------------------------------
  Events directly from the ledger
-------------------------------------------------------------------------------}

-- | A 'Data.Void.Void' isomorph for explicitly declaring that some ledger has
-- no events
data VoidLedgerEvent l

-- | The result of invoke a ledger function that does validation
--
-- Note: we do not instantiate 'Applicative' or 'Monad' for this type because
-- those interfaces would typically incur space leaks. We encourage you to
-- process the events each time you invoke a ledger function.
data LedgerResult l a = LedgerResult
  { lrEvents :: [AuxLedgerEvent l]
  , lrResult :: !a
  }
  deriving (Foldable, Functor, Traversable)

castLedgerResult ::
     (AuxLedgerEvent l ~ AuxLedgerEvent l')
  => LedgerResult l  a
  -> LedgerResult l' a
castLedgerResult (LedgerResult x0 x1) = LedgerResult x0 x1

embedLedgerResult ::
     (AuxLedgerEvent l -> AuxLedgerEvent l')
  -> LedgerResult l  a
  -> LedgerResult l' a
embedLedgerResult inj lr = lr{lrEvents = inj `map` lrEvents lr}

pureLedgerResult :: a -> LedgerResult l a
pureLedgerResult a = LedgerResult {
    lrEvents = mempty
  , lrResult = a
  }

{-------------------------------------------------------------------------------
  Basic LedgerState classes
-------------------------------------------------------------------------------}

class ShowLedgerState (l :: LedgerStateKind) where
  showsLedgerState :: SMapKind mk -> l mk -> ShowS   -- TODO someway to show the mk values

{-------------------------------------------------------------------------------
  Definition of a ledger independent of a choice of block
-------------------------------------------------------------------------------}

-- | Static environment required for the ledger
type family LedgerCfg (l :: LedgerStateKind) :: Type

class ( -- Requirements on the ledger state itself
        ShowLedgerState                     l
      , forall mk. Eq                      (l mk)
      , forall mk. Typeable mk => NoThunks (l mk)
        -- Requirements on 'LedgerCfg'
      , NoThunks (LedgerCfg l)
        -- Requirements on 'LedgerErr'
      , Show     (LedgerErr l)
      , Eq       (LedgerErr l)
      , NoThunks (LedgerErr l)
        -- Get the tip
        --
        -- See comment for 'applyChainTickLedgerResult' about the tip of the
        -- ticked ledger.
      , forall mk. GetTip         (l mk)
      , forall mk. GetTip (Ticked1 l mk)
      , HeaderHash (l EmptyMK) ~ HeaderHash l
      , HeaderHash (l ValuesMK) ~ HeaderHash l
      , HeaderHash (l DiffMK) ~ HeaderHash l
      , HeaderHash (l TrackingMK) ~ HeaderHash l
      ) => IsLedger (l :: LedgerStateKind) where
  -- | Errors that can arise when updating the ledger
  --
  -- This is defined here rather than in 'ApplyBlock', since the /type/ of
  -- these errors does not depend on the type of the block.
  type family LedgerErr l :: Type

  -- | Event emitted by the ledger
  --
  -- TODO we call this 'AuxLedgerEvent' to differentiate from 'LedgerEvent' in
  -- 'InspectLedger'. When that module is rewritten to make use of ledger
  -- derived events, we may rename this type.
  type family AuxLedgerEvent l :: Type

  -- | Apply "slot based" state transformations
  --
  -- When a block is applied to the ledger state, a number of things happen
  -- purely based on the slot number of that block. For example:
  --
  -- * In Byron, scheduled updates are applied, and the update system state is
  --   updated.
  -- * In Shelley, delegation state is updated (on epoch boundaries).
  --
  -- The consensus layer must be able to apply such a "chain tick" function,
  -- primarily when validating transactions in the mempool (which, conceptually,
  -- live in "some block in the future") or when extracting valid transactions
  -- from the mempool to insert into a new block to be produced.
  --
  -- This is not allowed to throw any errors. After all, if this could fail,
  -- it would mean a /previous/ block set up the ledger state in such a way
  -- that as soon as a certain slot was reached, /any/ block would be invalid.
  --
  -- PRECONDITION: The slot number must be strictly greater than the slot at
  -- the tip of the ledger (except for EBBs, obviously..).
  --
  -- NOTE: 'applyChainTickLedgerResult' should /not/ change the tip of the
  -- underlying ledger state, which should still refer to the most recent
  -- applied /block/. In other words, we should have
  --
  -- >    ledgerTipPoint (applyChainTick cfg slot st)
  -- > == ledgerTipPoint st
  applyChainTickLedgerResult ::
       LedgerCfg l
    -> SlotNo
    -> l ValuesMK
    -> LedgerResult l (Ticked1 l TrackingMK)


-- This can't be in IsLedger because we have a compositional IsLedger instance
-- for LedgerState HardForkBlock but we will not (at least ast first) have a
-- compositional LedgerTables instance for HardForkBlock.
class ShowLedgerState (LedgerTables l) => TableStuff (l :: LedgerStateKind) where

  data family LedgerTables l :: LedgerStateKind

  forgetTickedLedgerStateTracking :: Ticked1 l TrackingMK -> Ticked1 l ValuesMK
  forgetLedgerStateTracking       ::         l TrackingMK ->         l ValuesMK

  forgetLedgerStateTables :: l any -> l EmptyMK

  -- TODO change first argument's mk to DiffMK
  prependLedgerStateTracking :: Ticked1 l TrackingMK -> l TrackingMK -> l TrackingMK

  projectLedgerTables :: l mk -> LedgerTables l mk

  -- | Overwrite the tables in some ledger state.
  --
  -- The contents of the tables should not be /younger/ than the content of the
  -- ledger state. In particular, for a
  -- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' ledger, the
  -- tables argument should not contain any data from eras that succeed the
  -- current era of the ledger state argument.
  withLedgerTables :: HasCallStack => l any -> LedgerTables l mk -> l mk

-- | 'lrResult' after 'applyChainTickLedgerResult'
applyChainTick :: IsLedger l => LedgerCfg l -> SlotNo -> l ValuesMK -> Ticked1 l TrackingMK
applyChainTick = lrResult ..: applyChainTickLedgerResult

{-------------------------------------------------------------------------------
  Link block to its ledger
-------------------------------------------------------------------------------}

type LedgerStateKind = MapKind -> Type

data MapKind = EmptyMK | KeysMK | ValuesMK | TrackingMK | DiffMK {- | AnnMK Type MapKind -}

data ApplyMapKind :: MapKind -> Type -> Type -> Type where
  ApplyEmptyMK    ::                                 ApplyMapKind EmptyMK      k v
  ApplyKeysMK     :: Set k                        -> ApplyMapKind KeysMK       k v
  ApplyValuesMK   :: Map k v                      -> ApplyMapKind ValuesMK     k v
  ApplyTrackingMK :: {- TODO -}                      ApplyMapKind TrackingMK   k v
  ApplyDiffMK     :: {- TODO -}                      ApplyMapKind DiffMK       k v
--  ApplyAnnMK      :: !a -> !(ApplyMapKind mk k v) -> ApplyMapKind (AnnMK a mk) k v

emptyAppliedMK :: SMapKind mk -> ApplyMapKind mk k v
emptyAppliedMK = \case
  SEmptyMK    -> ApplyEmptyMK
  SKeysMK     -> ApplyKeysMK Set.empty
  SValuesMK   -> ApplyValuesMK Map.empty
  STrackingMK -> ApplyTrackingMK
  SDiffMK     -> ApplyDiffMK

toSMapKind :: ApplyMapKind mk k v -> SMapKind mk
toSMapKind = \case
  ApplyEmptyMK    -> SEmptyMK
  ApplyKeysMK   _ -> SKeysMK
  ApplyValuesMK _ -> SValuesMK
  ApplyTrackingMK -> STrackingMK
  ApplyDiffMK     -> SDiffMK

mapValuesAppliedMK :: (v -> v') -> ApplyMapKind mk k v ->  ApplyMapKind mk k v'
mapValuesAppliedMK f = \case
  ApplyEmptyMK     -> ApplyEmptyMK
  ApplyKeysMK ks   -> ApplyKeysMK ks
  ApplyValuesMK vs -> ApplyValuesMK (f <$> vs)
  ApplyTrackingMK  -> ApplyTrackingMK
  ApplyDiffMK      -> ApplyDiffMK

instance (Ord k, Eq v) => Eq (ApplyMapKind mk k v) where
  ApplyEmptyMK    == _               = True
  ApplyKeysMK   l == ApplyKeysMK   r = l == r
  ApplyValuesMK l == ApplyValuesMK r = l == r
  ApplyTrackingMK == _               = True
  ApplyDiffMK     == _               = True

instance (Ord k, NoThunks k, NoThunks v) => NoThunks (ApplyMapKind mk k v) where
  wNoThunks    = error "wNoThunks @ApplyMapKind"
  showTypeOf _ = "ApplyMapKind"

  -- TODO methods

data SMapKind :: MapKind -> Type where
  SEmptyMK    :: SMapKind EmptyMK
  SKeysMK     :: SMapKind KeysMK
  SValuesMK   :: SMapKind ValuesMK
  STrackingMK :: SMapKind TrackingMK
  SDiffMK     :: SMapKind DiffMK

instance Eq (SMapKind mk) where
  l == _ = case l of
    SEmptyMK    -> True
    SKeysMK     -> True
    SValuesMK   -> True
    STrackingMK -> True
    SDiffMK     -> True

instance Show (SMapKind mk) where
  show = \case
    SEmptyMK    -> "SEmptyMK"
    SKeysMK     -> "SKeysMK"
    SValuesMK   -> "SValuesMK"
    STrackingMK -> "STrackingMK"
    SDiffMK     -> "SDiffMK"

deriving via OnlyCheckWhnfNamed "SMapKind" (SMapKind mk) instance NoThunks (SMapKind mk)

-- | Ledger state associated with a block
data family LedgerState blk :: LedgerStateKind

type instance HeaderHash (LedgerState blk)    = HeaderHash blk
type instance HeaderHash (LedgerState blk mk) = HeaderHash blk

type LedgerConfig      blk    = LedgerCfg (LedgerState blk)
type LedgerError       blk    = LedgerErr (LedgerState blk)
type TickedLedgerState blk mk = Ticked1   (LedgerState blk) mk

{-------------------------------------------------------------------------------
  UTxO HD stubs
-------------------------------------------------------------------------------}

-- | Monadic functions used to query this this block type's 'LargeL' ledger
-- states, which typically involve accessing disk.
data family DiskLedgerView blk :: (Type -> Type) -> Type

type TableKeySets l = LedgerTables l KeysMK
