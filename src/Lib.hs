{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( someFunc
    ) where

import Data.List (find, maximumBy)
import Data.Set (Set(..))
import Data.Aeson.TH
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Crypto.Hash  (hash, SHA256 (..), Digest)

data Transaction = Transaction
  { sender :: String
  , recipient :: String
  , amount :: Integer
  } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions 'Transaction)


data Block = Block
  { index :: Integer
  , timestamp :: Integer
  , transactions :: [Transaction]
  , proof :: Integer
  , previousHash :: String
  } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions 'Block)


data Blockchain = Blockchain
  { nodes :: [String]
  , chain :: [Block]
  , currentTransactions :: [Transaction]
  } deriving (Show,Read,Eq)

initialBlock :: Blockchain
initialBlock = newBlock (Blockchain [] [] []) 0 100

sampleTransaction :: Transaction
sampleTransaction =
  Transaction
  { sender = "Alice"
  , recipient = "Bob"
  , amount = 1000
  }

sampleInvalidChain :: [Block]
sampleInvalidChain = [
      Block {
        index = 2,
        timestamp = 2,
        transactions = [sampleTransaction],
        proof = 12345,
        previousHash = "HOGE"
      },
      Block {
        index = 1,
        timestamp = 1,
        transactions = [sampleTransaction],
        proof = 12345,
        previousHash = "HOGE"
      }
    ]


newBlock :: Blockchain -> Integer -> Integer -> Blockchain
newBlock bc now proof =
  Blockchain
  { nodes = nodes bc
  , chain = block : chain bc
  , currentTransactions = []
  }
  where
    block = Block { index = (fromIntegral (length (chain bc))) + 1
                  , timestamp = now
                  , transactions = currentTransactions bc
                  , proof = proof
                  , previousHash = myHash (chain bc)
                  }
    myHash [] = "INITIAL_HASH"
    myHash (x:_) = toHash x

newTransaction :: Blockchain -> Transaction -> (Blockchain, Integer)
newTransaction bc t =
  ( Blockchain
    { nodes = nodes bc
    , chain = chain bc
    , currentTransactions = t : currentTransactions bc
    }
  , index (lastBlock bc) + 1
  )

class ToHash a where
  toHash :: a -> String

instance ToHash String where
  toHash str =
    let bs = BC.pack str
        digest :: Digest SHA256
        digest = hash $  bs
    in show (digest :: Digest SHA256)

instance ToHash Block where
  toHash block =
    let bs = encode block
        digest :: Digest SHA256
        digest = hash $ BL.toStrict bs
    in show (digest :: Digest SHA256)

lastBlock :: Blockchain -> Block
lastBlock bc = head (chain bc)

addNode :: Blockchain -> String -> Blockchain
addNode bc node = bc { nodes = node : nodes bc}


-- | verifyChain
--
-- >>> verifyChain sampleInvalidChain
-- False
-- >>> (b,i) = newTransaction initialBlock sampleTransaction
-- >>> Just validProof = proofOfWork (proof (lastBlock b)) 7400 10000
-- >>> validProof
-- 7474
-- >>> b2 = newBlock b 2 validProof
-- >>> verifyChain $ chain b2
-- True
verifyChain :: [Block] -> Bool
verifyChain [] = False
verifyChain (c:chain) = loop chain c
  where
    loop [] _ = True
    loop (c:cx) lastBlock =
      if previousHash lastBlock /= toHash c
      then False
      else if not (verifyProof (proof c) (proof lastBlock))
           then False
           else loop cx c


-- | verifyChain
--
-- >>> verifyChain sampleInvalidChain
-- False
-- >>> (b,i) = newTransaction initialBlock sampleTransaction
-- >>> Just validProof = proofOfWork (proof (lastBlock b)) 7400 10000
-- >>> validProof
-- 7474
-- >>> b2 = newBlock b 2 validProof
-- >>> Just validProof = proofOfWork (proof (lastBlock b2)) 0 10000
-- >>> b3 = newBlock b2 3 validProof
-- >>> Just validProof = proofOfWork (proof (lastBlock b3)) 0 10000
-- >>> b4 = newBlock b3 3 validProof
-- >>> Just bb = resolveConflicts b [chain b2,chain b3,chain b4]
-- >>> length (chain bb) == length (chain b4)
-- True
-- >>> verifyChain $ chain bb
-- True
-- >>> verifyChain $ chain b4
-- True
resolveConflicts :: Blockchain -> [[Block]] -> Maybe Blockchain
resolveConflicts bc chains =
  if length validChains == 0
  then Nothing
  else Just (bc {chain = longestChain})
  where
    validChains = filter (\c -> verifyChain c && (length (chain bc)) < (length c)) chains
    longestChain = maximumBy (\a b -> compare (length a) (length b)) validChains

verifyProof :: Integer -> Integer -> Bool
verifyProof lastProof proofCandidate =
  let guess = show lastProof ++ show proofCandidate
      hash = toHash guess
  in take 3 (reverse hash) == "000"


-- | proofOfWork
--
-- >>> proofOfWork 100 7400 10000
-- Just 7474
-- >>> take 3 $ reverse $ toHash "1007474"
-- "000"
proofOfWork :: Integer -> Integer -> Integer -> Maybe Integer
proofOfWork lastProof proofCandidate numTry =
  find (verifyProof lastProof) $ map (+ proofCandidate) [0..numTry]


-- | Test Transaction
--
-- >>> length $ chain initialBlock
-- 1
-- >>> length $ currentTransactions initialBlock
-- 0
-- >>> (b,i) = newTransaction initialBlock sampleTransaction
-- >>> length $ currentTransactions b
-- 1
-- >>> length $ chain b
-- 1
-- >>> b2 = newBlock b 1 12345
-- >>> length $ chain b2
-- 2
-- >>> index $ lastBlock b2
-- 2
-- >>> transactions (lastBlock b2) == [sampleTransaction]
-- True
-- >>> length (nodes b2)
-- 0
-- >>> b3 = addNode b2 "node1"
-- >>> length (nodes b3)
-- 1
someFunc :: IO ()
someFunc = putStrLn "someFunc"


