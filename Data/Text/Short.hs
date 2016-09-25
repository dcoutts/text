{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Text.Internal
-- Copyright   : (c) 2012-2013, 2016 Duncan Coutts,
--               (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Portability : GHC
--

-- A compact representation suitable for storing short Unicode strings in
-- memory.
--
-- In typical use cases it can be imported alongside "Data.Text", e.g.
--
-- > import qualified Data.Text       as T
-- > import qualified Data.Text.Short as T
-- >          (ShortText, toShort, fromShort)
--
-- Other 'ShortText' operations clash with "Data.Text" or "Prelude"
-- functions however, so they should be imported @qualified@ with a different
-- alias e.g.
--
-- > import qualified Data.Text.Short as T.Short
--

module Data.Text.Short
    (
    -- * Type
      ShortText
    -- ** Memory overhead
    -- $memory

    -- * Conversions
    , toShort
    , fromShort
    , pack
    , unpack

    -- * Other operations
    , empty
    , null
    , length
    ) where

import Data.Text.Short.Internal
import qualified Data.Text.Array    as A
import qualified Data.Text.Internal as T
import           Data.Text.Internal (Text)

-- $memory
--
-- With GHC, the memory overheads are as follows, expressed in words and
-- in bytes (words are 4 and 8 bytes on 32 or 64bit machines respectively).
--
-- * 'Text' unshared: 6 words; 24 or 48 bytes.
--
-- * 'Text' shared substring: 4 words; 16 or 32 bytes.
--
-- * 'ShortText': 4 words; 16 or 32 bytes.
--
-- For the string data itself 'ShortText' uses UTF-8 while 'Text' uses UTF-16
-- so the memory required depends on the mix of code points used. For the
-- special case of ASCII this is one byte per code point for UTF-8 and two
-- bytes for UTF-16, rounded up to the nearest word.
--
-- For example, including the overheads, a length 10 ASCII 'ShortText' would
-- take @16 + 12 = 28@ bytes on a 32bit platform and @32 + 16 = 48@ bytes on
-- a 64bit platform. An equivalent length 10 ASCII 'Text' would take
-- @24 + 20 = 44@ bytes on a 32bit platform and @48 + 24 = 72@ bytes on a
-- 64bit platform.
--
-- These overheads can all be reduced by 1 word (4 or 8 bytes) when the
-- 'ShortText' or 'Text' is unpacked into another constructor.
--
-- For example:
--
-- > data ThingId = ThingId {-# UNPACK #-} !Int
-- >                        {-# UNPACK #-} !ShortText
--
-- This will take @1 + 1 + 3@ words (the @ThingId@ constructor +
-- unpacked @Int@ + unpacked @ShortText@), plus the words for the
-- string data.

instance Eq ShortText where
    (==)    = equateShortText
    {-# INLINE (==) #-}

instance Ord ShortText where
    compare = compareShortText

instance Show ShortText where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read ShortText where
    readsPrec p str = [(pack x,y) | (x,y) <- readsPrec p str]

#if MIN_VERSION_base(4,9,0)
-- Semigroup orphan instances for older GHCs are provided by
-- 'semigroups` package

instance Semigroup ShortText where
    (<>) = append
#endif

instance Monoid ShortText where
    mempty  = empty
#if MIN_VERSION_base(4,9,0)
    mappend = (<>) -- future-proof definition
#else
    mappend = append
#endif
    mconcat = concat

instance IsString ShortText where
    fromString = pack

#if __GLASGOW_HASKELL__ >= 708
instance Exts.IsList ShortText where
    type Item Text = Char
    fromList       = pack
    toList         = unpack
#endif

#if defined(HAVE_DEEPSEQ)
instance NFData Text where rnf !_ = ()
#endif

instance Binary Text where
  put = undefined
  get = undefined

------------------------------------------------------------------------
-- Simple operations

-- | /O(1)/ The empty 'ShortText'.
empty :: ShortText
empty = ShortText A.empty
{-# INLINE [1] empty #-}

-- | /O(n)/ Returns the number of characters in a 'Text'.
-- Subject to fusion.
length :: ShortText -> Int
length = undefined


------------------------------------------------------------------------
-- Conversion to and from Text

-- | /O(n)/. Convert a 'Text' into a 'ShortText'.
--
-- This makes a copy, so does not retain the input string.
--
toShort :: Text -> ShortText
toShort !_ = undefined

-- | /O(n)/. Convert a 'ShortText' into a 'Text'.
--
fromShort :: ShortText -> Text
fromShort !_ = undefined


------------------------------------------------------------------------
-- Packing and unpacking from lists

-- | /O(n)/ Convert a 'String' into a 'ShortText'.
-- Performs replacement on invalid scalar values.
pack :: String -> ShortText
pack = packChars

-- | /O(n)/. Convert a 'ShortText' into a 'String'.
unpack :: ShortText -> String
unpack = unpackChars


------------------------------------------------------------------------
-- Eq and Ord implementations

-- The UTF-8 encoding is such that lexical comparison of the bytes of the
-- encoding is the same as lexical comparison of the corresponding code points.
-- Thus we can do these operations in terms of bytes.

equateBytes :: ShortText -> ShortText -> Bool
equateBytes = undefined

compareBytes :: ShortText -> ShortText -> Ordering
compareBytes = undefined

------------------------------------------------------------------------
-- Appending and concatenation

append :: ShortText -> ShortText -> ShortText
append = undefined

concat :: [ShortText] -> ShortText
concat = undefined

