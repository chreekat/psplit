import PSplit (chunkWrites)
import Options.Applicative

data PSplitArgs = Args
    { suffixLength :: Int
    , lines :: Int
    , input :: FilePath
    , prefix :: String
    }

-- TODO: Make this look more like split's options. Also, add a 'number' option.

args :: Parser PSplitArgs
args = Args
    <$> option
        (long "suffix-length"
        <> short 'a'
        <> help "use suffixes of length N (default 2)"
        <> metavar "N"
        <> value 2)
    <*> option
        (long "lines"
        <> short 'l'
        <> metavar "NUMBER"
        <> help "put at least NUMBER lines per output file")
    <*> argument' str
        (metavar "INPUT"
        <> value "-")
    <*> argument str
        (metavar "PREFIX"
        <> value "x")

main = execParser opts >>= \(Args a l fp pfx) -> chunkWrites pfx a l fp
  where
    opts = info (helper <*> args) $
        fullDesc
        <> progDesc "Split a file into a number of other files"
