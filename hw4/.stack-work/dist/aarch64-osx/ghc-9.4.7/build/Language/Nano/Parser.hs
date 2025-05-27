{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Nano.Lexer
import Language.Nano.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

newtype HappyAbsSyn t4 t5 t6 t7 t8 t9 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: t4 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: t5 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: t6 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: t7 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: t8 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: t9 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyInTok :: (Token) -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x2e\x07\x80\x02\x00\x40\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\x07\x00\x18\x06\x00\x05\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\x1c\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x80\xcb\x01\xa0\x00\x70\x39\x00\x34\x00\x00\xe0\x7f\x08\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\x17\x00\x00\x10\x00\x00\x80\xcb\x01\xa0\x00\x00\x02\xff\x03\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\xb8\x1c\x00\x0a\x00\x97\x03\x40\x01\xe0\x72\x00\x28\x00\x5c\x0e\x00\x05\x80\xcb\x01\xa0\x00\x70\x39\x00\x14\x00\x2e\x07\x80\x02\xc0\xe5\x00\x50\x00\xb8\x1c\x00\x0a\x00\x97\x03\x40\x01\x00\x00\x3e\x04\x00\x00\xc0\x87\x00\x00\x00\xf8\x10\x00\x00\x00\x1f\x02\x00\x00\xe0\x43\x00\x00\x00\x1c\x00\x00\x00\x80\x03\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x10\x00\x00\x00\x2e\x07\x80\x02\xc0\xe5\x00\x50\x00\x00\x80\xff\x01\x00\x97\x03\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\xcb\x01\xa0\x00\x00\x00\x00\x00\x00\x00\xe0\x43\x00\x00\x10\xfc\x0f\x00\x40\x80\xff\x01\x00\x00\x00\x00\x00\xe0\x72\x00\x28\x00\x20\xc0\xff\x00\x80\xcb\x01\xa0\x00\x70\x39\x00\x14\x00\x00\xe0\x43\x00\x00\x00\xfc\x0f\x00\xb8\x1c\x00\x0a\x00\x00\xf0\x3f\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_top","Top","Expr","Axpr","Bxpr","Exprs","Ids","let","true","false","in","if","then","else","TNUM","ID","'\\\\'","'->'","'='","'+'","'-'","'*'","'&&'","'||'","'=='","'/='","'<'","'<='","':'","'('","')'","'['","']'","','","%eof"]
        bit_start = st Prelude.* 37
        bit_end = (st Prelude.+ 1) Prelude.* 37
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..36]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x0b\x00\xf8\xff\xfb\xff\xf3\xff\x84\x00\x14\x00\x00\x00\x08\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x06\x00\x10\x00\x1e\x00\x01\x00\x69\x00\x04\x00\x00\x00\x00\x00\x78\x00\x1a\x00\x1e\x00\x4f\x00\xfc\xff\x00\x00\x1e\x00\x1e\x00\x1e\x00\x1e\x00\x1e\x00\x1e\x00\x1e\x00\x1e\x00\x1e\x00\x1e\x00\xb6\x00\x8e\x00\x8e\x00\x8e\x00\x8e\x00\x49\x00\x49\x00\x00\x00\x1b\x00\x1b\x00\x1d\x00\x23\x00\x1e\x00\x1e\x00\x98\x00\x1e\x00\x00\x00\x00\x00\x1e\x00\x00\x00\xb6\x00\x5f\x00\x2b\x00\x00\x00\x1e\x00\x3e\x00\x1e\x00\x1e\x00\xa2\x00\xac\x00\x1e\x00\xac\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x58\x00\x00\x00\x00\x00\x00\x00\x66\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x29\x00\x00\x00\xc7\x00\xcc\x00\xcf\x00\xd2\x00\xd5\x00\xd8\x00\xdb\x00\xde\x00\xe1\x00\xe4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x00\xe7\x00\xea\x00\x00\x00\xed\x00\x00\x00\x00\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x00\x00\x00\xf3\x00\xf6\x00\x00\x00\x00\x00\xf9\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\xee\xff\xec\xff\x00\x00\xea\xff\xe9\xff\x00\x00\xeb\xff\xe7\xff\x00\x00\x00\x00\x00\x00\xe4\xff\x00\x00\xe7\xff\xe6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xf6\xff\xf7\xff\xf8\xff\xf9\xff\xfa\xff\xfb\xff\xf3\xff\xf4\xff\xf5\xff\x00\x00\xe2\xff\x00\x00\x00\x00\xfe\xff\x00\x00\xe8\xff\xe5\xff\x00\x00\xe3\xff\xf1\xff\x00\x00\x00\x00\xe1\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\xf0\xff\x00\x00\xef\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x09\x00\x01\x00\x02\x00\x03\x00\x09\x00\x05\x00\x0c\x00\x0c\x00\x08\x00\x09\x00\x0a\x00\x01\x00\x02\x00\x03\x00\x1c\x00\x05\x00\x09\x00\x0c\x00\x08\x00\x09\x00\x0a\x00\x02\x00\x03\x00\x17\x00\x09\x00\x19\x00\x1a\x00\x08\x00\x09\x00\x1a\x00\x01\x00\x02\x00\x03\x00\x17\x00\x05\x00\x19\x00\x0b\x00\x08\x00\x09\x00\x0a\x00\x0c\x00\x0f\x00\x17\x00\x09\x00\x19\x00\x05\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x03\x00\x17\x00\xff\xff\x19\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x04\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x06\x00\x0d\x00\x0e\x00\x0f\x00\x01\x00\x02\x00\x03\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x07\x00\x01\x00\x02\x00\x03\x00\x05\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x01\x00\x02\x00\x03\x00\xff\xff\x1b\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\xff\xff\x18\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x01\x00\x02\x00\x03\x00\xff\xff\x16\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x03\x00\x08\x00\x09\x00\x0a\x00\x30\x00\x0b\x00\x17\x00\x31\x00\x0c\x00\x13\x00\x0e\x00\x08\x00\x09\x00\x0a\x00\xff\xff\x0b\x00\x19\x00\x17\x00\x0c\x00\x0d\x00\x0e\x00\x09\x00\x0a\x00\x0f\x00\x16\x00\x10\x00\x14\x00\x0c\x00\x13\x00\x36\x00\x08\x00\x09\x00\x0a\x00\x0f\x00\x0b\x00\x10\x00\x34\x00\x0c\x00\x13\x00\x0e\x00\x3d\x00\x1d\x00\x0f\x00\x30\x00\x10\x00\x2e\x00\x3f\x00\x03\x00\x04\x00\x05\x00\x06\x00\x19\x00\x0f\x00\x00\x00\x10\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x43\x00\x10\x00\x05\x00\x06\x00\x11\x00\x10\x00\x05\x00\x06\x00\x37\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x32\x00\x1b\x00\x1c\x00\x1d\x00\x17\x00\x05\x00\x06\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x40\x00\x14\x00\x05\x00\x06\x00\x3b\x00\x00\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x32\x00\x05\x00\x06\x00\x00\x00\x37\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x00\x00\x35\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x2d\x00\x05\x00\x06\x00\x00\x00\x24\x00\x2c\x00\x05\x00\x06\x00\x2b\x00\x05\x00\x06\x00\x2a\x00\x05\x00\x06\x00\x29\x00\x05\x00\x06\x00\x28\x00\x05\x00\x06\x00\x27\x00\x05\x00\x06\x00\x26\x00\x05\x00\x06\x00\x25\x00\x05\x00\x06\x00\x24\x00\x05\x00\x06\x00\x3a\x00\x05\x00\x06\x00\x39\x00\x05\x00\x06\x00\x38\x00\x05\x00\x06\x00\x3d\x00\x05\x00\x06\x00\x41\x00\x05\x00\x06\x00\x40\x00\x05\x00\x06\x00\x43\x00\x05\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 30) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30)
	]

happy_n_terms = 29 :: Prelude.Int
happy_n_nonterms = 6 :: Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happySpecReduce_3  0# happyReduction_1
happyReduction_1 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (happy_var_3
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happySpecReduce_1  0# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn4
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happySpecReduce_3  1# happyReduction_3
happyReduction_3 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (EBin Cons  happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happySpecReduce_3  1# happyReduction_4
happyReduction_4 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (EBin And   happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happySpecReduce_3  1# happyReduction_5
happyReduction_5 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (EBin Or    happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happySpecReduce_3  1# happyReduction_6
happyReduction_6 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (EBin Eq    happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happySpecReduce_3  1# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (EBin Ne    happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happySpecReduce_3  1# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (EBin Lt    happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_3  1# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (EBin Le    happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_3  1# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (EBin Plus  happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happySpecReduce_3  1# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (EBin Minus happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happySpecReduce_3  1# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (EBin Mul   happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happyReduce 6# 1# happyReduction_13
happyReduction_13 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	case happyOut5 happy_x_6 of { happy_var_6 -> 
	happyIn5
		 (EIf  happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happyReduce 4# 1# happyReduction_14
happyReduction_14 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (ID _ happy_var_2) -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	happyIn5
		 (ELam happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happyReduce 6# 1# happyReduction_15
happyReduction_15 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (ID _ happy_var_2) -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	case happyOut5 happy_x_6 of { happy_var_6 -> 
	happyIn5
		 (ELet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happyReduce 7# 1# happyReduction_16
happyReduction_16 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (ID _ happy_var_2) -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	case happyOut5 happy_x_5 of { happy_var_5 -> 
	case happyOut5 happy_x_7 of { happy_var_7 -> 
	happyIn5
		 (ELet happy_var_2 (mkLam happy_var_3 happy_var_5) happy_var_7
	) `HappyStk` happyRest}}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_1  1# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_2  2# happyReduction_18
happyReduction_18 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (EApp happy_var_1 happy_var_2
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_1  2# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_1  3# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOutTok happy_x_1 of { (NUM _ happy_var_1) -> 
	happyIn7
		 (EInt happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_1  3# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn7
		 (EBool True
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happySpecReduce_1  3# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn7
		 (EBool False
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happySpecReduce_3  3# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happySpecReduce_1  3# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { (ID _ happy_var_1) -> 
	happyIn7
		 (EVar happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happySpecReduce_2  3# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  happyIn7
		 (ENil
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_26 = happySpecReduce_3  3# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (exprList happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_27 = happySpecReduce_1  4# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ([happy_var_1]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_28 = happySpecReduce_3  4# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (happy_var_1 : happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_29 = happySpecReduce_1  5# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOutTok happy_x_1 of { (ID _ happy_var_1) -> 
	happyIn9
		 ([happy_var_1]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_30 = happySpecReduce_2  5# happyReduction_30
happyReduction_30 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (ID _ happy_var_1) -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (happy_var_1 : happy_var_2
	)}}

happyNewToken action sts stk [] =
	happyDoAction 28# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	LET _ -> cont 1#;
	TRUE _ -> cont 2#;
	FALSE _ -> cont 3#;
	IN _ -> cont 4#;
	IF _ -> cont 5#;
	THEN _ -> cont 6#;
	ELSE _ -> cont 7#;
	NUM _ happy_dollar_dollar -> cont 8#;
	ID _ happy_dollar_dollar -> cont 9#;
	LAM _ -> cont 10#;
	ARROW _ -> cont 11#;
	EQB _ -> cont 12#;
	PLUS _ -> cont 13#;
	MINUS _ -> cont 14#;
	MUL _ -> cont 15#;
	AND _ -> cont 16#;
	OR  _ -> cont 17#;
	EQL _ -> cont 18#;
	NEQ _ -> cont 19#;
	LESS _ -> cont 20#;
	LEQ _ -> cont 21#;
	COLON _ -> cont 22#;
	LPAREN _ -> cont 23#;
	RPAREN _ -> cont 24#;
	LBRAC _ -> cont 25#;
	RBRAC _ -> cont 26#;
	COMMA _ -> cont 27#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 28# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Except String a -> (a -> Except String b) -> Except String b
happyThen = ((>>=))
happyReturn :: () => a -> Except String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Except String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Except String a
happyError' = (\(tokens, _) -> parseError tokens)
top tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {x' = happyOut4 x} in x'))

happySeq = happyDontSeq


mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
