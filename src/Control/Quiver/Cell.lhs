> {-# LANGUAGE BangPatterns, RankNTypes #-}

> -- | Module:    Control.Quiver.Cell
> -- Description: Convertions between cell-based and row-based representations of tabular data
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- Quiver processors converting between cellular and traditional tabular data.

> module Control.Quiver.Cell (
>   toRows, fromRows,
>   -- * Bounded row lengths
>   BoundedFailure(..), toBoundedRows, toBoundedVectors,
> ) where

> import Data.Cell
> import Control.Arrow (first, second)
> import Control.Quiver.SP
> import qualified Data.DList as D
> import Data.Vector (Vector, fromListN)

> import qualified Data.Foldable as F

> -- | A simple Quiver processor that converts a stream of cells into a stream of rows,
> --   with each row represented by a non-empty list of cell values.

> toRows :: (Monoid a, Functor f) => SP (Cell a) [a] f e
> toRows = loop0
>  where
>   loop0 = consume () (loop2 D.empty D.empty) (deliver SPComplete)
>
>   loop1 row cell = consume () (loop2 row cell) (spemit (assembleComplete row cell))
>
>   loop2 row cell (Cell part d) =
>     case d of
>       EOP -> loop1 row cell'
>       EOC -> loop1 (row `D.snoc` mconcat (D.toList cell')) D.empty
>       _   -> assembleComplete row cell' >:> loop0
>    where
>     cell' = cell `D.snoc` part
>   assemble row cell = row `D.snoc` mconcat (D.toList cell)
>   assembleComplete row = D.toList . assemble row

> -- | Possible failure cases when constructing bounded rows.

> data BoundedFailure = TooShort Int Int
>                       -- ^ The required minimum length and actual
>                       -- length.
>                     | TooLong Int Int
>                       -- ^ The required maximum length and length
>                       -- found so far.
>                     deriving (Eq, Ord, Show, Read)

> -- | A variant of 'toRows' which provides support for stating a
> --   minimum and maximum length of each row.

> toBoundedRows :: (Monoid a, Functor f) => Int -> Int -> SP (Cell a) (Int, [a]) f BoundedFailure
> toBoundedRows lb ub = loop0
>  where
>   loop0 = consume () (loop2 (0, D.empty) D.empty) (deliver SPComplete)
>
>   loop1 rowC cell = consume () (loop2 rowC cell) (spemit (assembleComplete (first succ rowC) cell))
>
>   loop2 rowC@(!c,row) cell (Cell part d) =
>     case d of
>       EOP -> loop1 rowC cell'
>       EOC | c' > ub   -> deliver . SPFailed $ TooLong ub c'
>           | otherwise -> loop1 (assemble (c',row) cell') D.empty
>       _   | c' < lb   -> deliver . SPFailed $ TooShort lb c'
>           | c' >= ub  -> deliver . SPFailed $ TooLong ub c'
>           | otherwise -> assembleComplete (c',row) cell' >:> loop0
>    where
>     cell' = cell `D.snoc` part
>     c' = c + 1
>
>   assembleComplete rowC = second D.toList . assemble rowC
>
>   assemble rowC cell = second (`D.snoc` mconcat (D.toList cell)) rowC

> -- | A variant of 'toBoundedRows' that returns a 'Vector'.

> toBoundedVectors :: (Monoid a, Functor f) => Int -> Int -> SP (Cell a) (Vector a) f BoundedFailure
> toBoundedVectors lb ub = toBoundedRows lb ub >->> sppure (uncurry fromListN) >&> fst

> -- | A simple Quiver processor that converts a stream of rows to a stream of cells.
> --   In this version, the final cell in the table is not marked with @EOT@ to avoid
> --   the need for row lookahead.  Empty input rows are ignored.

> fromRows :: (F.Foldable t) => SP (t a) (Cell a) f e
> fromRows = loop0
>  where
>   loop0 = consume () loop1 (deliver SPComplete)
>
>   loop1 = snd . F.foldr (\x (d,act) -> (EOC, Cell x d >:> act)) (EOR, loop0)
