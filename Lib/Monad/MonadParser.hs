{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--      Polymorphic monadic consumer based parser.

module Lib.Monad.MonadParser(MonadParser(..)) where

import Control.Monad hiding (guard)
-- import Lib.Monad.MonadT

------------------------------------------------------------------------------

class MonadPlus m => MonadParser tok m where
    item :: m tok

--instance (MonadPlus (t m),MonadParser tok m,MonadT t m) => MonadParser tok (t m) where
--    item = up item

