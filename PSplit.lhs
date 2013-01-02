
> {-# LANGUAGE OverloadedStrings #-}

> module PSplit (getChunk, paragraphLine, fileChunks, main) where

> import System.IO
> import Control.Monad (sequence_, (<=<), (>=>), zipWithM_)
> import qualified Data.Text.Lazy as T
> import Data.Text.Lazy (Text)
> import Data.Text.Lazy.IO as T
> import qualified Data.Text.Format as F
> import Data.Monoid ((<>))
> import Options.Applicative

"An action that writes a file out into chunks broken on paragraph boundaries."

"A sequence of actions that each write a chunk from a file into a different
output file."

>--  psplit :: IO ()
>--  psplit = sequence_ chunkWrites

Ok, now let's drill down on the chunk writing action

"An action that writes a certain number of lines from a [handle/proxy/file?], to
a [handle/proxy/file?] plus however much extra is needed to stop at a paragraph
boundary."

Nope, that's still wonky.

> -- | An action that writes a list of lines to a specified file.
> chunkWrite :: FilePath -> [Text] -> IO ()
> chunkWrite p = T.writeFile p . T.unlines

So, what is the list of lines?

Break it down a bit. A thing that has to happen is the input plus numLines is
converted into (rest, chunk); the input list is split into two lists. And the
chunk is a combination of numLines plus some extra taken from the rest. So like,

> type InputState = [Text]

> getChunk :: InputState -> Int -> (InputState, [Text])
> getChunk input numLines = (nextInput, chunk)
>   where
>     (before, after) = splitAt (numLines - 1) input'
>     (extra, nextInput) = span paragraphLine after
>     input' = dropWhile (not . paragraphLine) input
>     chunk = before ++ extra

And then

> paragraphLine :: Text -> Bool
> paragraphLine = not . T.null

Damn it's about time for some quickchecks. Next time!

... and done!

So now I want to run getChunk on the input, threading the state, until the input
is done. I want to accumulate the chunks, too.

The action that loops the getChunk action while the input state is not empty,
accumulating the result of the getChunk action and returning the final
accumulation.

> fileChunks :: InputState -> Int -> [[Text]]
> fileChunks [] _ = []
> fileChunks ls n = let (ls', chunk) = getChunk ls n in
>     chunk : fileChunks ls' n

I'm still holding out on making this an explicit State monad. It will happen, however.

What may make it happen is the next part. The next part is creating chunkWrites,
the list of writing actions. This needs to create the list of output files. The
list of output files is the prefix plus numbers. Let's be lazy.

> filenames :: Text -> Int -> [FilePath]
> filenames pfx n =
>     map (T.unpack . F.format "{}{}" . (,) pfx . F.left n '0') ([1..] :: [Int])

Eeasy.

Now we'd need to (lazily) make those into handles. That isn't worth a standalone
function; it's "map (flip openFile WriteMode) filenames".

Now what? Now we need to get the initial input state. No we don't. Let's wrap
this shit together.

> chunkWrites :: Text -> Int -> Int -> FilePath -> IO ()
> chunkWrites pfx a n =
>     zipWithM_ chunkWrite (filenames pfx a) . flip fileChunks n . T.lines
>     <=< T.readFile

Ok, now command line arguments. This is a Thing all to itself. But since
chunkWrites is the action of the program, we see immediately what it needs from
the user:

> data PSplitArgs = Args
>     { suffixLength :: Int
>     , lines :: Int
>     , input :: FilePath
>     , prefix :: String
>     }

> args :: Parser PSplitArgs
> args = Args
>     <$> option
>         (long "suffix-length"
>         <> short 'a'
>         <> help "use suffixes of length N (default 2)"
>         <> metavar "N"
>         <> value 2)
>     <*> option
>         (long "lines"
>         <> short 'l'
>         <> metavar "NUMBER"
>         <> help "put at least NUMBER lines per output file")
>     <*> argument' str
>         (metavar "INPUT"
>         <> value "-")
>     <*> argument str
>         (metavar "PREFIX"
>         <> value "x")

TODO: Make this look more like split's options. Also, add a 'number' option.

> main = execParser opts >>= \ (Args a n fp pfx) -> chunkWrites (T.pack pfx) a n fp
>   where
>    opts = info (helper <*> args)
>        (fullDesc
>        <> progDesc "Split a file into a number of other files")
