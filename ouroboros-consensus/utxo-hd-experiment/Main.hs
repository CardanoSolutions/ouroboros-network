module Main where

import Data.Foldable
import Data.Traversable
import Data.Word
import System.Directory
import System.Environment

-- import qualified Database.LMDB.Raw as LMDB.Raw
import qualified Database.LMDB.Simple as LMDB.Simple
import qualified Database.LMDB.Simple.Extra as LMDB.Extra
import qualified Database.LMDB.Simple.Internal as LMDB.Internal

-- Hard-coded size of a @Word64@: @size == 64 / 8@.
sizeBytesWord64 :: Int
sizeBytesWord64 = 8

-- | Simple program that reads some commands from a file and
-- performs them on an @LMDB@ database. Used for profiling
-- @LMDB@ performance under different @`LMDB.Simple.mapSize`@
-- parameters.
main :: IO ()
main = do
  putStrLn "> Starting..."

  [arg1, arg2] <- getArgs

  let
    cmdsFilePath :: FilePath
    cmdsFilePath = arg1
    mapSizeFactors :: [Int]
    mapSizeFactors = read arg2

  b <- doesFileExist cmdsFilePath
  if b
    then pure ()
    else error "Input commands filepath does not exist!"
  fileContents <- readFile cmdsFilePath

  let
    cmds :: [Cmd]
    cmds = readCommands fileContents
    limits :: LMDB.Simple.Limits
    limits = LMDB.Simple.defaultLimits {
        LMDB.Simple.mapSize = product mapSizeFactors
      }
    dbFilePath :: String
    dbFilePath = "./db"

  removeDirectoryRecursive dbFilePath
  createDirectory dbFilePath

  dbEnv <-
    LMDB.Simple.openReadWriteEnvironment
    dbFilePath
    limits
  db <-
    LMDB.Simple.readOnlyTransaction dbEnv $
      LMDB.Simple.getDatabase Nothing :: IO (LMDB.Internal.Database Word64 Word64)

  mapM_ (cmdDo dbEnv db) cmds

  LMDB.Simple.closeEnvironment dbEnv

  putStrLn "> Stopping..."
  putStrLn ""

data Cmd
  = Puts !Word64 !Word64
  | Gets !Word64 !Word64
  | GetSize
  deriving (Show, Read)

readCommands :: String -> [Cmd]
readCommands = map read . lines

cmdDo ::
     LMDB.Internal.Environment LMDB.Internal.ReadWrite
  -> LMDB.Internal.Database Word64 Word64
  -> Cmd
  -> IO ()
cmdDo dbEnv db cmd = do
  putStrLn $ "> " ++ show cmd

  case cmd of
    Puts k1 k2 -> cmdPuts dbEnv db k1 k2
    Gets k1 k2 -> cmdGets dbEnv db k1 k2
    GetSize -> cmdGetSize dbEnv db

cmdPuts ::
     LMDB.Internal.Environment LMDB.Internal.ReadWrite
  -> LMDB.Internal.Database Word64 Word64
  -> Word64
  -> Word64
  -> IO ()
cmdPuts dbEnv db k1 k2 = do
  LMDB.Simple.readWriteTransaction dbEnv $
    puts db [(k, Just v) | k <- [k1 .. k2], v <- [k]]

cmdGets ::
     LMDB.Internal.Environment LMDB.Internal.ReadWrite
  -> LMDB.Internal.Database Word64 Word64
  -> Word64
  -> Word64
  -> IO ()
cmdGets dbEnv db k1 k2 = do
  _vs <- LMDB.Simple.readOnlyTransaction dbEnv $
    gets db [k | k <- [k1 .. k2]]
  pure ()

cmdGetSize ::
     LMDB.Internal.Environment LMDB.Internal.ReadWrite
  -> LMDB.Internal.Database Word64 Word64
  -> IO ()
cmdGetSize dbEnv db = do
  dbSize <- LMDB.Simple.readOnlyTransaction dbEnv $
    LMDB.Extra.size db

  putStrLn $ "# Database size: " ++ show dbSize

puts ::
     (LMDB.Internal.Serialise k, LMDB.Internal.Serialise v)
  => LMDB.Internal.Database k v
  -> [(k, Maybe v)]
  -> LMDB.Internal.Transaction LMDB.Internal.ReadWrite ()
puts db kvs = forM_ kvs $ uncurry (LMDB.Simple.put db)

gets ::
     (LMDB.Internal.Serialise k, LMDB.Internal.Serialise v)
  => LMDB.Internal.Database k v
  -> [k]
  -> LMDB.Internal.Transaction LMDB.Internal.ReadOnly [Maybe v]
gets db ks = forM ks $ LMDB.Simple.get db
