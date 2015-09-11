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
> ) where

> import Data.Cell
> import Control.Quiver.SP
> import qualified Data.DList as D

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

> -- | A simple Quiver processor that converts a stream of rows to a stream of cells.
> --   In this version, the final cell in the table is not marked with @EOT@ to avoid
> --   the need for row lookahead.  Empty input rows are ignored.

> fromRows :: SP [a] (Cell a) f e
> fromRows = loop0
>  where
>   loop0 = consume () loop1 (deliver SPComplete)
>   loop1 (x:xs) = Cell x (if null xs then EOR else EOC) >:> loop1 xs
>   loop1 [] = loop0
