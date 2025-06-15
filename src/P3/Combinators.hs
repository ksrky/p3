module P3.Combinators
    ( eof
    ) where

import Control.Lens.Combinators
import Control.Monad
import P3.Monad

-- | Check if there's no more token.
eof :: ParserM t ()
eof = do
    toks <- use tokens
    case toks of
        [] -> return ()
        _  -> mzero
