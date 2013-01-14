> {-# LANGUAGE TemplateHaskell #-}

> import PSplit
> import Data.Text.Lazy (Text)
> import qualified Data.Text.Lazy as T
> import Test.QuickCheck
> import Test.QuickCheck.All
> import Safe
> import Data.Function (on)
> import Control.Applicative ((<$>))

> instance Arbitrary Text where
>    arbitrary = T.pack <$> arbitrary

{-
    Arbitrary prose is an arbitrary number of paragraphs comprised of arbitrary
    numbers of lines, separated by blank lines (the paragraph boundary).

    That is, arbitrary prose is arbitrary, nonempty [Text] separated by
    ppBoundary.

    That is, arbitrary prose is arbitrary [Text]. Hm. Heh. No instance-writing
    required!

    To the tests, then.
-}

> prop_sameText ls n =
>     sameText  ls (chunk ++ nextInput)
>   where
>     (nextInput, chunk) = getChunk ls n
>     sameText = ((==) `on` filter paragraphLine)

> prop_splitsAtBoundary ls n =
>     not . paragraphLine $ headDef T.empty nextInput
>   where (nextInput, chunk) = getChunk ls n

> prop_consumesAllInput ls n =
>     sameLength ls (concat chunks)
>   where
>     chunks = fileChunks ls n
>     sameLength = ((==) `on` (length . filter paragraphLine))

> main = $quickCheckAll
