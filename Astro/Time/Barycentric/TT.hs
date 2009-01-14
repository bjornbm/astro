{-# LANGUAGE MultiParamTypeClasses #-}

{- |
   Copyright  : Copyright (C) 2008 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : experimental
   Portability: GHC only?

An trivial implementation of TDB conversions assuming that @TDB = TT@
for applications with low accuracy requirements. The maximum
conversion error is 1.7 milliseconds.

This module exports no data types or functions, it only provides additional
'Astro.Time.Convert' instances.
-}
module Astro.Time.Barycentric.TT (ttToTDB, tdbToTT) where

import Astro.Time

-- | Convert a TT epoch into a TDB epoch.
ttToTDB :: E TT -> E TDB
ttToTDB (E t) = E t

-- | Convert a TDB epoch into a TT epoch.
tdbToTT :: E TDB -> E TT
tdbToTT (E t) = E t

