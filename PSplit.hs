{-# LANGUAGE OverloadedStrings #-}

module PSplit (getChunk, paragraphLine, fileChunks, chunkWrites) where

import System.IO
import Control.Monad ((<=<), (>=>), zipWithM_)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as T
import qualified Data.Text.Format as F

-- | An action that writes a list of lines to a specified file.
chunkWrite :: FilePath -> [Text] -> IO ()
chunkWrite p = T.writeFile p . T.unlines

type InputState = [Text]

-- | The input is converted into (nextInput, chunk); that is, the
-- input list is split into two lists. And the chunk is a combination of
-- `n` from the input plus some extra taken from the leftovers.
--
-- A bit of cleanup is done, too: empty lines are dropped from the start of
-- the chunk. I probably shouldn't bother.
getChunk :: InputState -- ^ The input text lines
         -> Int        -- ^ Minimum size for a chunk
         -> (InputState, [Text])
getChunk input n = (nextInput, chunk)
  where
    input' = dropWhile (not . paragraphLine) input
    (before, after) = splitAt (n - 1) input'
    (extra, nextInput) = span paragraphLine after
    chunk = before ++ extra

-- | A convenience exported solely for use in tests. May become smarter later,
-- however.
paragraphLine :: Text -- ^ Line to be checked
              -> Bool -- ^ Is line not empty?
paragraphLine = not . T.null

-- | The action that loops the getChunk action while the input state is not empty,
-- accumulating and returning the results.
fileChunks :: InputState
           -> Int
           -> [[Text]]
fileChunks [] _ = []
fileChunks ls n = let (ls', chunk) = getChunk ls n in
    chunk : fileChunks ls' n

-- | To keep the args to chunkWrites at bay
type SufLen = Int

-- | Lazy list of filenames e.g. [x01, x02, ...]
filenames :: Text -- ^ filename prefix
          -> SufLen  -- ^ suffix length
          -> [FilePath]
filenames pfx m =
    map (T.unpack . F.format "{}{}" . (,) pfx . F.left m '0') ([1..] :: [Int])

-- | An action that writes a file out into chunks of some minimum length,
-- broken on paragraph boundaries.
chunkWrites :: String   -- ^ output prefix; String to interface with CmdArgs.
            -> SufLen   -- ^ suffix length
            -> Int      -- ^ min lines for each chunk
            -> FilePath -- ^ input
            -> IO ()
chunkWrites pfx m n = let pfx' = T.pack pfx in
    zipWithM_ chunkWrite (filenames pfx' m) . flip fileChunks n . T.lines
    <=< T.readFile
