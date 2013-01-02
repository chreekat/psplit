Ok, now command line arguments. This is a Thing all to itself. But since
chunkWrites is the action of the program, we see immediately what it needs from
the user:

>  import PSplit (chunkWrites)
>  import Options.Applicative
>  import Data.Monoid ((<>))

>  data PSplitArgs = Args
>      { suffixLength :: Int
>      , lines :: Int
>      , input :: FilePath
>      , prefix :: String
>      }

>  args :: Parser PSplitArgs
>  args = Args
>      <$> option
>          (long "suffix-length"
>          <> short 'a'
>          <> help "use suffixes of length N (default 2)"
>          <> metavar "N"
>          <> value 2)
>      <*> option
>          (long "lines"
>          <> short 'l'
>          <> metavar "NUMBER"
>          <> help "put at least NUMBER lines per output file")
>      <*> argument' str
>          (metavar "INPUT"
>          <> value "-")
>      <*> argument str
>          (metavar "PREFIX"
>          <> value "x")

TODO: Make this look more like split's options. Also, add a 'number' option.

>  main = execParser opts >>= \ (Args a n fp pfx) -> chunkWrites pfx a n fp
>    where
>     opts = info (helper <*> args)
>         (fullDesc
>         <> progDesc "Split a file into a number of other files")

