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
module Astro.Time.TDB.TT () where

import Astro.Time

-- | Convert a TT epoch into a TDB epoch.
ttToTDB :: E TT -> E TDB
ttToTDB (E t) = E t

-- | Convert a TDB epoch into a TT epoch.
tdbToTT :: E TDB -> E TT
tdbToTT (E t) = E t


-- Additional Convert instances
-- ============================

instance Convert TAI TDB where convert = convert . (convert :: E TAI -> E TT)
instance Convert TAI TCB where convert = convert . (convert :: E TAI -> E TT)

instance Convert TT  TDB where convert =  ttToTDB
instance Convert TT  TCB where convert =  convert . ttToTDB

instance Convert TCG TDB where convert = convert . (convert :: E TCG -> E TT)
instance Convert TCG TCB where convert = convert . (convert :: E TCG -> E TT)

instance Convert TDB TAI where convert = convert . tdbToTT
instance Convert TDB TT  where convert = tdbToTT
instance Convert TDB TCG where convert = convert . tdbToTT

instance Convert TCB TAI where convert = convert . (convert :: E TCB -> E TDB)
instance Convert TCB TT  where convert = convert . (convert :: E TCB -> E TDB)
instance Convert TCB TCG where convert = convert . (convert :: E TCB -> E TDB)

