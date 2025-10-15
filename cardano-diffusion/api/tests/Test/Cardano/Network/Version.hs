{-# LANGUAGE ScopedTypeVariables #-}

-- | Test `NodeToNodeVersion` and `NodeToClientVersion` codecs.
--
module Test.Cardano.Network.Version (tests) where

import Cardano.Network.NodeToClient.Version (NodeToClientVersion (..),
           nodeToClientVersionCodec)
import Cardano.Network.NodeToNode.Version (NodeToNodeVersion (..),
           nodeToNodeVersionCodec)
import Ouroboros.Network.CodecCBORTerm

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit


tests :: TestTree
tests =
  testGroup "Cardano.Network.Protocol.Handshake.Version"
    [ testGroup "NodeToClientVersion"
      [ testCase "NodeToClientVersion round-trip codec property"
                 (roundTripPropAll nodeToClientVersionCodec)
      , testCase "NodeToClientVersion should not deserialise as NodeToNode"
                 (crossFailurePropAll
                   nodeToClientVersionCodec
                   nodeToNodeVersionCodec
                   ([minBound .. maxBound] :: [NodeToClientVersion]))
      ]
    , testGroup "NodeToNodeVersion"
      [ testCase "NodeToNodeVersion round-trip codec property"
                 (roundTripPropAll nodeToNodeVersionCodec)
      , testCase "NodeToNodeVersion should not deserialise as NodeToClient"
                 (crossFailurePropAll
                   nodeToNodeVersionCodec
                   nodeToClientVersionCodec
                   ([minBound .. maxBound] :: [NodeToNodeVersion]))
      ]
    ]


roundTripProp :: ( Eq a
                 , Show a
                 , Eq failure
                 , Show failure
                 )
              => CodecCBORTerm failure a
              -> a -> Assertion
roundTripProp codec a =
    Right a @=? decodeTerm codec (encodeTerm codec a)


-- Using `Monoid` instance of `IO ()`
roundTripPropAll
    :: forall failure a.
       ( Eq a
       , Enum a
       , Bounded a
       , Show a
       , Show failure
       , Eq failure
       )
    => CodecCBORTerm failure a -> Assertion
roundTripPropAll codec =
    foldMap (roundTripProp codec) ([minBound..maxBound] :: [a])


crossFailureProp
    :: forall failure a b.
       ( Show a
       , Show b
       )
    => CodecCBORTerm failure a
    -> CodecCBORTerm failure b
    -> a
    -> Assertion
crossFailureProp codecA codecB a =
    case decodeTerm codecB (encodeTerm codecA a) of
      Right b -> assertFailure (show a ++ "should not deserialise as " ++ show b)
      Left  _ -> pure ()


crossFailurePropAll
    :: forall failure a b.
       ( Show a
       , Show b
       )
    => CodecCBORTerm failure a
    -> CodecCBORTerm failure b
    -> [a]
    -> Assertion
crossFailurePropAll codecA codecB = foldMap (crossFailureProp codecA codecB)

