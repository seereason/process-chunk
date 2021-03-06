-- | Like "System.Process.Chunk", but catches all exceptions and
-- returns them using the 'Exception' constructor.
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module System.Process.ChunkE
    ( Chunk(..)
    , readCreateProcessChunks
    , discardEmptyChunks
    , fuseChunks
    , collectProcessTriple
    , collectProcessResult
    , collectProcessOutput
    , indentChunks
    , dotifyChunks
    , putChunk
    , putMappedChunks
    , putIndented
    , putIndentedShowCommand
    , putDots
    , putDotsLn
    , insertStart
    , insertResult
    , insertNewline
    , insertCommandStart
    , insertCommandResult
    , insertCommandDisplay
    , showCreateProcessForUser
    , showCmdSpecForUser
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException)
import Control.Monad.State (StateT, evalState, evalStateT, get, put)
import Control.Monad.Trans (lift)
import Data.ListLike (ListLike(..), ListLikeIO(hPutStr, putStr))
import Data.Monoid (Monoid(mempty, mappend), (<>))
import Data.String (IsString(fromString))
import Prelude hiding (mapM, putStr, null, tail, break, sequence, length, replicate, rem, head)
import System.Exit (ExitCode(ExitFailure))
import System.IO (stderr)
import System.Process (ProcessHandle, CreateProcess(cmdspec, cwd), CmdSpec(..), showCommandForUser)
import System.Process.ListLike.Classes (ListLikeLazyIO, ProcessOutput(pidf, outf, errf, intf, codef))
import System.Process.ListLike.Read (readCreateProcess)

-- | A concrete representation of the methods in ProcessOutput.
data Chunk a
    = ProcessHandle ProcessHandle -- ^ This will always come first
    | Stdout a
    | Stderr a
    | Exception SomeException
    | Result ExitCode

instance ListLikeLazyIO a c => ProcessOutput a [Chunk a] where
    pidf p = [ProcessHandle p]
    outf x = [Stdout x]
    errf x = [Stderr x]
    intf e = [Exception e]
    codef c = [Result c]

instance ListLikeLazyIO a c => ProcessOutput a (ExitCode, [Chunk a]) where
    pidf p = (mempty, [ProcessHandle p])
    codef c = (c, mempty)
    outf x = (mempty, [Stdout x])
    errf x = (mempty, [Stderr x])
    intf e = (mempty, [Exception e])

-- | A concrete use of 'readCreateProcess' - build a list containing
-- chunks of process output, any exceptions that get thrown, and
-- finally an exit code.  If a is a lazy type the returned list will
-- be lazy.
readCreateProcessChunks :: (ListLikeLazyIO a c) => CreateProcess -> a -> IO [Chunk a]
readCreateProcessChunks = readCreateProcess

-- | Eliminate empty Stdout or Stderr chunks.
discardEmptyChunks :: ListLikeLazyIO a c => [Chunk a] -> [Chunk a]
discardEmptyChunks [] = []
discardEmptyChunks (Stdout a : more) | null a = discardEmptyChunks more
discardEmptyChunks (Stderr a : more) | null a = discardEmptyChunks more
discardEmptyChunks (a : more) = a : discardEmptyChunks more

-- | Merge adjacent Stdout or Stderr chunks.  This may be undesirable
-- if you want to get your input as soon as it becomes available, it
-- has the effect of making the result "less lazy".
fuseChunks :: ListLikeLazyIO a c => [Chunk a] -> [Chunk a]
fuseChunks [] = []
fuseChunks (Stdout a : Stdout b : more) = fuseChunks (Stdout (a <> b) : more)
fuseChunks (Stderr a : Stderr b : more) = fuseChunks (Stderr (a <> b) : more)
fuseChunks (Stdout a : more) | null a = fuseChunks more
fuseChunks (Stderr a : more) | null a = fuseChunks more
fuseChunks (a : more) = a : fuseChunks more

collectProcessTriple :: Monoid a => [Chunk a] -> (Either SomeException ExitCode, a, a)
collectProcessTriple [] = mempty
collectProcessTriple (Result x : xs) = (Right x, mempty, mempty) <> collectProcessTriple xs
collectProcessTriple (Exception x : xs) = (Left x, mempty, mempty) <> collectProcessTriple xs
collectProcessTriple (Stdout x : xs) = (mempty, x, mempty) <> collectProcessTriple xs
collectProcessTriple (Stderr x : xs) = (mempty, mempty, x) <> collectProcessTriple xs
collectProcessTriple (_ : xs) = collectProcessTriple xs

collectProcessResult :: [Chunk a] -> (Either SomeException ExitCode, [Chunk a])
collectProcessResult [] = mempty
collectProcessResult (Result x : xs) = (Right x, mempty) <> collectProcessResult xs
collectProcessResult (Exception x : xs) = (Left x, mempty) <> collectProcessResult xs
collectProcessResult (x : xs) = (mempty, [x]) <> collectProcessResult xs

collectProcessOutput :: Monoid a => [Chunk a] -> (Either SomeException ExitCode, a)
collectProcessOutput [] = mempty
collectProcessOutput (Result x : xs) = (Right x, mempty) <> collectProcessOutput xs
collectProcessOutput (Exception x : xs) = (Left x, mempty) <> collectProcessOutput xs
collectProcessOutput (Stdout x : xs) = (mempty, x) <> collectProcessOutput xs
collectProcessOutput (Stderr x : xs) = (mempty, x) <> collectProcessOutput xs
collectProcessOutput (_ : xs) = mempty <> collectProcessOutput xs

-- | Pure function to indent the text of a chunk list.
indentChunks :: forall a c. (ListLikeLazyIO a c, Eq c, IsString a) => String -> String -> [Chunk a] -> [Chunk a]
indentChunks outp errp chunks =
    evalState (Prelude.concat <$> mapM (indentChunk nl (fromString outp) (fromString errp)) chunks) BOL
    where
      nl :: c
      nl = Data.ListLike.head (fromString "\n" :: a)

-- | The monad state, are we at the beginning of a line or the middle?
data BOL = BOL | MOL deriving (Eq)

-- | Indent the text of a chunk with the prefixes given for stdout and
-- stderr.  The state monad keeps track of whether we are at the
-- beginning of a line - when we are and more text comes we insert one
-- of the prefixes.
indentChunk :: forall a c m. (Monad m, Functor m, ListLikeLazyIO a c, Eq c) => c -> a -> a -> Chunk a -> StateT BOL m [Chunk a]
indentChunk nl outp errp chunk =
    case chunk of
      Stdout x -> doText Stdout outp x
      Stderr x -> doText Stderr errp x
      _ -> return [chunk]
    where
      doText :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doText con pre x = do
        let (hd, tl) = break (== nl) x
        (<>) <$> doHead con pre hd <*> doTail con pre tl
      doHead :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doHead _ _ x | null x = return []
      doHead con pre x = do
        bol <- get
        case bol of
          BOL -> put MOL >> return [con (pre <> x)]
          MOL -> return [con x]
      doTail :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doTail _ _ x | null x = return []
      doTail con pre x = do
        bol <- get
        put BOL
        tl <- doText con pre (tail x)
        return $ (if bol == BOL then [con pre] else []) <> [con (singleton nl)] <> tl

dotifyChunks :: forall a c. (ListLikeLazyIO a c, IsString a) => Int -> [Chunk a] -> [Chunk a]
dotifyChunks charsPerDot chunks =
    evalState (Prelude.concat <$> mapM (dotifyChunk charsPerDot dot) chunks) 0
    where
      dot :: c
      dot = Data.ListLike.head (fromString "." :: a)

-- | dotifyChunk charsPerDot dot chunk - Replaces every charsPerDot
-- characters in the Stdout and Stderr chunks with one dot.  Runs in
-- the state monad to keep track of how many characters had been seen
-- when the previous chunk finished.  chunks.
dotifyChunk :: forall a c m. (Monad m, Functor m, ListLikeLazyIO a c) => Int -> c -> Chunk a -> StateT Int m [Chunk a]
dotifyChunk charsPerDot dot chunk =
    case chunk of
      Stdout x -> doChars (length x)
      Stderr x -> doChars (length x)
      _ -> return [chunk]
    where
      doChars count = do
        rem <- get
        let (count', rem') = divMod (rem + count) (fromIntegral charsPerDot)
        put rem'
        if (count' > 0) then return [Stderr (replicate count' dot)] else return []

-- | Write the Stdout chunks to stdout and the Stderr chunks to stderr.
putChunk :: ListLikeLazyIO a c => Chunk a -> IO ()
putChunk (Stdout x) = putStr x
putChunk (Stderr x) = hPutStr stderr x
putChunk _ = return ()

-- | Apply a function to the chunk list and output the result,
-- return the original (unmodified) chunk list.
putMappedChunks :: ListLikeLazyIO a c => ([Chunk a] -> [Chunk a]) -> [Chunk a] -> IO [Chunk a]
putMappedChunks f chunks = mapM_ putChunk (f chunks) >> return chunks

-- | Output the indented text of a chunk list, but return the original
-- unindented list.
putIndented :: (ListLikeLazyIO a c, Eq c, IsString a) => String -> String -> [Chunk a] -> IO [Chunk a]
putIndented outp errp chunks = putMappedChunks (indentChunks outp errp) chunks

putIndentedShowCommand :: (ListLikeLazyIO a c, Eq c, IsString a) =>
                          CreateProcess -> String -> String -> [Chunk a] -> IO [Chunk a]
putIndentedShowCommand p outp errp chunks =
    putMappedChunks (insertCommandDisplay p . indentChunks outp errp) chunks

-- | Output the dotified text of a chunk list. Returns the original
-- (undotified) list.
putDots :: (ListLikeLazyIO a c) => Int -> c -> [Chunk a] -> IO [Chunk a]
putDots charsPerDot dot chunks =
    evalStateT (mapM (\ x -> dotifyChunk charsPerDot dot x >>= mapM_ (lift . putChunk) >> return x) chunks) 0

-- | Output the dotified text of a chunk list with a newline at EOF.
-- Returns the original list.
putDotsLn :: forall a c. (IsString a, ListLikeLazyIO a c) => Int -> c -> [Chunk a] -> IO [Chunk a]
putDotsLn cpd dot chunks = putDots cpd dot chunks >>= \ r -> hPutStr stderr (fromString "\n" :: a) >> return r

-- | Insert a chunk displaying the command and its arguments at the
-- beginning of the chunk list.
insertStart :: (IsString a, ListLikeLazyIO a c, Eq c) => CreateProcess -> [Chunk a] -> [Chunk a]
insertStart p chunks = [Stderr (fromString (" -> " ++ showCreateProcessForUser p))] <> chunks

-- | Insert a chunk displaying the command and its arguments at the
-- beginning of the chunk list.
insertNewline :: (IsString a, ListLikeLazyIO a c, Eq c) => [Chunk a] -> [Chunk a]
insertNewline chunks = [Stderr (fromString "\n")] <> chunks

-- | Insert a chunk displaying the exit code or exception that terminates
-- the process.
insertResult :: (IsString a, ListLikeLazyIO a c, Eq c) => [Chunk a] -> [Chunk a]
insertResult [] = []
insertResult (x@(Result code) : xs) = Stderr (fromString (" -> " ++ show code ++ "\n")) : x : xs
insertResult (x@(Exception e) : xs) = Stderr (fromString (" -> " ++ show e ++ "\n")) : x : xs
insertResult (x : xs) = x : insertResult xs

-- | Insert a chunk displaying the command and its arguments at the
-- beginning of the chunk list.
insertCommandStart :: (IsString a, ListLikeLazyIO a c, Eq c) =>
                      CreateProcess -> [Chunk a] -> [Chunk a]
insertCommandStart p chunks = insertStart p $ insertNewline $ chunks

-- | Insert a chunk displaying the command and the result code.
insertCommandResult :: (IsString a, ListLikeLazyIO a c, Eq c) =>
                       CreateProcess -> [Chunk a] -> [Chunk a]
insertCommandResult _ [] = []
insertCommandResult p (Result code : xs) =
    Stderr (fromString (" <- " ++ show code ++ " <- " ++ showCmdSpecForUser (cmdspec p) ++ "\n")) :
    Result code :
    xs
insertCommandResult p (x : xs) = x : insertCommandResult p xs

insertCommandDisplay :: (IsString a, ListLikeLazyIO a c, Eq c) => CreateProcess -> [Chunk a] -> [Chunk a]
insertCommandDisplay p = insertCommandResult p . insertCommandStart p

showCreateProcessForUser :: CreateProcess -> String
showCreateProcessForUser p =
    showCmdSpecForUser (cmdspec p) ++ maybe "" (\ d -> " (in " ++ d ++ ")") (cwd p)

showCmdSpecForUser :: CmdSpec -> String
showCmdSpecForUser (ShellCommand s) = s
showCmdSpecForUser (RawCommand p args) = showCommandForUser p args
