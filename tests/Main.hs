{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
module Main where

import Codec.Binary.UTF8.String (encode)
import Control.Applicative ((<$>))
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr, ord)
import Data.List (isSuffixOf)
import Data.ListLike as ListLike (ListLike(length, concat, null, groupBy, head, toList, dropWhile, takeWhile, drop, length), ListLikeIO(readFile))
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(fromString))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Prelude hiding (length, readFile, head)
import GHC.IO.Exception
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFileStatus, fileMode, setFileMode, unionFileModes, ownerExecuteMode, groupExecuteMode, otherExecuteMode)
import System.Process (CreateProcess, proc, ProcessHandle, shell, readProcessWithExitCode)
import System.Process.ByteString as B
import System.Process.ByteString.Lazy as LB
import System.Process.String as S
import System.Process.Text as T
import System.Process.Text.Lazy as LT
import qualified System.Process.Chunks as C
import qualified System.Process.ChunkE as E
import System.Process.ListLike.Classes (ListLikeLazyIO, ProcessOutput)
import System.Process.ListLike.Instances
import System.Process.ListLike.StrictString () -- the lazy one is the same as lazy text, the strict one is more interesting to test
import System.Process.ListLike.Read as LL
import System.Timeout
import Test.HUnit hiding (path)
import Text.Regex.Posix ((=~))
import Text.Printf

instance Show ProcessHandle where
    show _ = "<processhandle>"

instance Eq ProcessHandle where
    _ == _ = False

instance Eq SomeException where
    _ == _ = False

deriving instance Show a => Show (C.Chunk a)
deriving instance Show a => Show (E.Chunk a)

deriving instance Eq a => Eq (C.Chunk a)
deriving instance Eq a => Eq (E.Chunk a)

instance Monoid Test where
    mempty = TestList []
    mappend (TestList a) (TestList b) = TestList (a <> b)
    mappend (TestList a) b = TestList (a <> [b])
    mappend a (TestList b) = TestList ([a] <> b)
    mappend a b = TestList [a, b]

type MkTest = forall a c. (Show a, ListLikeLazyIO a c, IsString a, Eq a, Enum c) => String -> a -> Test

testInstances :: MkTest -> Test
testInstances mkTest = mappend (testCharInstances mkTest) (testWord8Instances mkTest)

testStrictInstances :: MkTest -> Test
testStrictInstances mkTest = mappend (testStrictCharInstances mkTest) (testStrictWord8Instances mkTest)

testLazyInstances :: MkTest -> Test
testLazyInstances mkTest = mappend (testLazyCharInstances mkTest) (testLazyWord8Instances mkTest)

testCharInstances :: MkTest -> Test
testCharInstances mkTest = mappend (testLazyCharInstances mkTest) (testStrictCharInstances mkTest)

testLazyCharInstances :: MkTest -> Test
testLazyCharInstances mkTest = mkTest "Lazy Text" LT.empty

testStrictCharInstances :: MkTest -> Test
testStrictCharInstances mkTest = mappend (mkTest "Strict Text" T.empty) (mkTest "String" ("" :: String))

testWord8Instances :: MkTest -> Test
testWord8Instances mkTest = mappend (testLazyWord8Instances mkTest) (testStrictWord8Instances mkTest)

testLazyWord8Instances :: MkTest -> Test
testLazyWord8Instances mkTest = mkTest "Lazy ByteString" LB.empty

testStrictWord8Instances :: MkTest -> Test
testStrictWord8Instances mkTest = mkTest "Strict ByteString" B.empty

main :: IO ()
main =
    do chmod "tests/script1.hs"
       chmod "tests/script4.hs"
       (c,st) <- runTestText putTextToShowS test1 -- (TestList (versionTests ++ sourcesListTests ++ dependencyTests ++ changesTests))
       putStrLn (st "")
       case (failures c) + (errors c) of
         0 -> return ()
         _ -> exitFailure

chmod :: FilePath -> IO ()
chmod path =
    getFileStatus path >>= \ status ->
    setFileMode path (foldr unionFileModes (fileMode status) [ownerExecuteMode, groupExecuteMode, otherExecuteMode])

cps :: [CreateProcess]
cps = [ proc "true" []
      , proc "false" []
      , shell "foo"
      , proc "foo" []
      , shell "yes | cat -n | head 100"
      , shell "yes | cat -n"
      , proc "cat" ["tests/text"]
      , proc "cat" ["tests/houseisclean.jpg"]
      , proc "tests/script1.hs" []
      ]

test1 :: Test
test1 =
    TestLabel "test1"
      (TestList
       [ TestLabel "[Output]" $
         TestList
         [
         -- This demonstrates lazy generation of process output.  If
         -- you try this with a strict instance the call to
         -- readCreateProcessChunks will block indefinitely.
         , let expected "Lazy Text" = [E.Stderr "..",E.Stderr "..",E.Stderr "..",E.Stderr "..",E.Stderr "..",E.Stderr "..",E.Stderr "..",E.Stderr "..",E.Stderr ".."]
               expected "Lazy ByteString" = [E.Stderr "................................",E.Stderr ".................................",
                                             E.Stderr ".................................",E.Stderr ".................................",
                                             E.Stderr "................................",E.Stderr ".................................",
                                             E.Stderr ".................................",E.Stderr ".................................",
                                             E.Stderr "................................"]
               expected _ = error "unexpected" in
           testLazyInstances
           (\ s i -> TestLabel s $ TestCase $ do
                       (E.ProcessHandle _ : chunks) <- LL.readCreateProcess (proc "bash" ["-c", "yes | cat -n"]) i >>= return . take 10 . E.dotifyChunks 1000 (head (i <> "."))
                       assertEqual "ten chunks" (expected s) chunks)
         , testInstances
           (\ s i -> TestLabel s $ TestCase $ do
                       (E.ProcessHandle _ : chunks) <- LL.readCreateProcess (proc "tests/script2.hs" []) i
                       assertEqual
                         "exception chunk" [E.Stderr "Here comes an exception\nscript2.hs: user error (This is a GHC.IO.Exception.UserError)\n",
                                            E.Result (ExitFailure 1)] (E.fuseChunks chunks))
         ]
       ])

expected :: String -> Double
expected "String" =             23000000.0
expected "Strict ByteString" = 760000000.0
expected "Lazy ByteString" =   760000000.0
expected "Strict Text" =        44000000.0
expected "Lazy Text" =          47000000.0
expected s = error $ "Unexpected instance: " <> show s

percentMessage v expected =
    case v / expected of
      r | r < 1.0 -> printf "%01.2f pct slower than usual" (100.0 - 100.0 * r)
      r -> printf "%01.2f pct faster than usual" (100.0 * r - 100.0)

about :: Double -> Double -> String
about v expected =
    case v / expected of
      r | r >= 0.7 && r <= 1.3 -> "Within 30% of usual speed"
      _ -> percentMessage v expected

parseTimeOutput :: forall a c. (ListLike a c, Enum c) => a -> Double
parseTimeOutput a =
    case s =~ ("^(.*)system ([0-9]*):([0-9]*).([0-9]*)elapsed(.*)$" :: String) :: (String, String, String, [String]) of
      (_, _, _, [_, m, s, h, _]) -> (read m :: Double) * 60.0 + (read s :: Double) + (read ("0." <> h) :: Double)
    where
      s = map (chr . fromEnum) $ toList a
