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

> import qualified Data.Foldable as F

> -- | A simple Quiver processor that converts a stream of cells into a stream of rows,
> --   with each row represented by a non-empty list of cell values.

> toRows :: Monoid a => SP (Cell a) [a] f e
> toRows = loop0
>  where
>   loop0 = consume () (loop2 [] []) (deliver SPComplete)
>   loop1 row cell = consume () (loop2 row cell) (spemit (assemble row cell))
>   loop2 row cell (Cell part d) =
>     case d of
>       EOP -> loop1 row cell'
>       EOC -> loop1 (mconcat (reverse cell') : row) []
>       _   -> assemble row cell' >:> loop0
>    where
>     cell' = part:cell
>   assemble row cell = reverse (mconcat (reverse cell) : row)

> -- | A simple Quiver processor that converts a stream of rows to a stream of cells.
> --   In this version, the final cell in the table is not marked with @EOT@ to avoid
> --   the need for row lookahead.  Empty input rows are ignored.

> fromRows :: (F.Foldable t) => SP (t a) (Cell a) f e
> fromRows = loop0
>  where
>   loop0 = consume () loop1 (deliver SPComplete)
>
>   loop1 = snd . F.foldr (\x (d,act) -> (EOC, Cell x d >:> act)) (EOR, loop0)
