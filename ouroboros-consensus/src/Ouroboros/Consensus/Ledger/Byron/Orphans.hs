{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Ledger.Byron.Orphans () where

import           Codec.Serialise (Serialise, decode, encode)
import           Control.Monad (void)
import           Data.ByteString (ByteString)
import           Data.Coerce
import           Data.Text (unpack)
import           Formatting

import           Cardano.Prelude (NoUnexpectedThunks, UseIsNormalForm (..))

import qualified Cardano.Binary
import           Cardano.Crypto (shortHashF)
import qualified Cardano.Crypto

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Delegation as CC
import qualified Cardano.Chain.MempoolPayload as CC
import qualified Cardano.Chain.Update as CC
import qualified Cardano.Chain.UTxO as CC

import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Serialise
-------------------------------------------------------------------------------}

instance Serialise CC.ChainValidationState where
  encode = Cardano.Binary.toCBOR
  decode = Cardano.Binary.fromCBOR

instance Serialise CC.KeyHash where
  encode = Cardano.Binary.toCBOR
  decode = Cardano.Binary.fromCBOR

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance Condense CC.HeaderHash where
  condense = formatToString CC.headerHashF

instance Condense CC.Block where
  condense = unpack
           . sformat build
           . CC.txpTxs
           . CC.bodyTxPayload
           . CC.blockBody

instance Condense CC.Header where
  condense hdr = mconcat [
        "( hash: "         <> unpack condensedHash
      , ", previousHash: " <> unpack condensedPrevHash
      , ", slot: "         <> unpack condensedSlot
      , ", issuer: "       <> condense issuer
      , ", delegate: "     <> condense delegate
      , ")"
      ]
    where
      psigCert = CC.delegationCertificate $ CC.headerSignature hdr
      issuer   = CC.issuerVK   psigCert
      delegate = CC.delegateVK psigCert
      hdrHash  = CC.hashHeader hdr

      condensedHash     = sformat CC.headerHashF $ hdrHash
      condensedPrevHash = sformat CC.headerHashF $ CC.headerPrevHash hdr
      condensedSlot     = sformat build $ CC.headerSlot hdr

instance Condense CC.BoundaryBlock where
  condense = condense . CC.boundaryHeader

instance Condense CC.BlockOrBoundary where
  condense (CC.BOBBlock blk) = mconcat [
        "( header: " <> condense (CC.blockHeader blk)
      , ", body: "   <> condense blk
      , ")"
      ]
  condense (CC.BOBBoundary ebb) =
      condense ebb

instance Condense CC.BoundaryHeader where
  condense hdr = mconcat [
        "( ebb: true"
      , ", hash: "         <> condensedHash
      , ", previousHash: " <> condensedPrevHash
      , ")"
      ]
    where
      condensedHash = error "wat: @mhueschen: Condense CC.BoundaryHeader"
            -- unpack
          -- . sformat CC.headerHashF
          -- . coerce
          -- . Cardano.Crypto.hashDecoded . fmap CC.wrapBoundaryBytes
          -- $ hdr

      condensedPrevHash =
          unpack $ case CC.boundaryPrevHash hdr of
            Left _  -> "Genesis"
            Right h -> sformat CC.headerHashF h

instance Condense CC.TxId where
  condense hash = "txid:" <> unpack (sformat shortHashF hash)

instance Condense CC.UpId where
  condense hash = "upid:" <> unpack (sformat shortHashF hash)

instance Condense CC.CertificateId where
  condense hash = "certificateid: " <> unpack (sformat shortHashF hash)

instance Condense CC.VoteId where
  condense hash = "voteid: " <> unpack (sformat shortHashF hash)

instance Condense CC.MempoolPayload where
  condense = error "wat: @mhueschen: Condense CC.MempoolPayload"
    -- condense (CC.MempoolTx tx) =
    --   "tx: " <> unpack (sformat build (void tx))
    -- condense (CC.MempoolDlg cert) =
    --   "dlg: " <> unpack (sformat build (void cert))
    -- condense (CC.MempoolUpdateProposal p) =
    --   "updateproposal: " <> unpack (sformat build (void p))
    -- condense (CC.MempoolUpdateVote vote) =
    --   "updatevote: " <> unpack (sformat build (void vote))

{-------------------------------------------------------------------------------
  NoUnexpectedThunks
-------------------------------------------------------------------------------}

-- TODO <https://github.com/input-output-hk/cardano-ledger/issues/685>
--
-- Cardano.Chain.Delegation.Validation.Registration.TooLarge is not exported,
-- but occurs somewhere in CC.ChainValidationError, so we use
-- 'UseIsNormalForm' instead of deriving one using Generics.
deriving via UseIsNormalForm CC.ChainValidationError
  instance NoUnexpectedThunks CC.ChainValidationError
