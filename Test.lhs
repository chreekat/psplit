> {-# LANGUAGE TemplateHaskell #-}

> import PSplit
> import Data.Text (Text)
> import qualified Data.Text as T
> import Test.QuickCheck
> import Test.QuickCheck.All
> import Safe

> (<$>) = fmap
> infixr 4 <$>

> instance Arbitrary Text where
>     arbitrary = T.pack <$> arbitrary

Arbitrary prose is an arbitrary number of paragraphs comprised of arbitrary
numbers of lines, separated by blank lines (the paragraph boundary)

> -- instance Arbitrary Text where
> --     arbitrary = T.pack <$> arbitrary

That is, arbitrary prose is arbitrary, nonempty [Text] separated by ppBoundary.

That is, arbitrary prose is arbitrary [Text]. Hm. Heh.

Here's where I'll think up some properties for the functions I've written so
far. First a quick dump:

getChunk :: InputState -> Int -> (InputState, [Text])
-------

1. With original state `input`, new state `rest`, and result `chunk`, then the
input is more or less equal to the output:

    input (== `on` (filter paragraphLine)) chunk ++ rest

2. The rest of the input must not exist or must be a paragraph boundary.

    headDef T.empty rest == T.empty

paragraphLine :: Text -> Bool
---------

1. Nonempty lines are paragraph lines. This is just rewriting the function
though, so I don't know if it's useful.

    (length line) > 0 && paragraphLine line == True

Ok, that's fine. Now to actually build some tests.

> on :: (b -> b -> r) -> (a -> b) -> a -> a -> r
> on cmp g l r = cmp (g l) (g r)

> prop_sameText input numLines =
>     ((==) `on` (filter paragraphLine)) input (chunk ++ nextInput)
>   where (nextInput, chunk) = getChunk input numLines

> prop_splitsAtBoundary input numLines =
>     not . paragraphLine $ headDef T.empty nextInput
>   where (nextInput, chunk) = getChunk input numLines

> prop_consumesAllInput ls n =
>     ((==) `on` (length . filter paragraphLine)) ls (concat chunks)
>   where
>     chunks = fileChunks ls n

> main = $quickCheckAll
