{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.Text.Internal
-- Copyright   : (c) 2012-2013, 2016 Duncan Coutts,
--               (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Portability : GHC
--
-- A module containing private 'ShortText' internals. This exposes the
-- 'ShortText' representation and low level construction functions.
-- Modules which extend the 'Text' system may need to use this module.
--
-- You should not use this module unless you are determined to monkey
-- with the internals, as the functions here do just about nothing to
-- preserve data invariants.  You have been warned!

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Data.Text.Short.Internal
    (
    -- * Type
    -- $internals
      ShortText(..)
    ) where

import Data.Typeable (Typeable)
import qualified Data.Text.Array as A
--import Data.Text.Internal (Text(..), safe)
import Prelude ()

-- | A space efficient Unicode text type.
--
-- It has a lower memory overhead than 'Text'. It can be converted to or from
-- 'Text' (at the cost of copying and converting the string data). It supports
-- very few other operations.
--
-- It is suitable for use as an internal representation for code that needs
-- to keep many short strings in memory, but it /should not/ be used as an
-- interchange type. That is, it should not generally be used in public APIs.
-- The 'Text' type is usually more suitable for use in interfaces; it is more
-- flexible and it supports a wide range of operations.
--
newtype ShortText = ShortText A.Array  -- payload (Word8 UTF8 elements)
    deriving (Typeable)


-- $internals
--
-- Internally, the 'ShortText' type is represented as an array of 'Word8'
-- UTF-8 code units.
--
-- Invariants that all functions must maintain:
--
-- * Valid UTF-8 encoding (shortest encoding invariant)
--
-- * Since 'Text' cannot represent code points in the reserved surrogate
--   range U+D800 to U+DFFF and because we do unchecked conversions between
--   'ShortText' and 'Text' then we also avoid the the reserved surrogate range
--   in 'ShortText'. To maintain this invariant, the 'safe' function maps
--   'Char' values in this range to the replacement character (U+FFFD,
--   \'&#xfffd;\').

