{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParMini where
import AbsMini
import LexMini
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn32 :: (Ident) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (Ident)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Integer) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Integer)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (Double) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (Double)
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (String) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (String)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (Program) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (Program)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (Type) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (Type)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Stm) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Stm)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ([Stm]) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ([Stm])
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (Param) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (Param)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ([Param]) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> ([Param])
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Assign) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Assign)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: ([Assign]) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> ([Assign])
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Var) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Var)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ([Var]) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> ([Var])
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (Value) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (Value)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Exp) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Exp)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (Exp) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (Exp)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (Exp) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (Exp)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (Exp) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (Exp)
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: (Exp) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> (Exp)
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (Exp) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (Exp)
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (Exp) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (Exp)
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: (Exp) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> (Exp)
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: (Exp) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> (Exp)
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (Exp) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (Exp)
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (Exp) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (Exp)
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: ([Exp]) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> ([Exp])
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (Exp) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> (Exp)
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: (Exp) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> (Exp)
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: (Exp) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> (Exp)
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: (Exp) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> (Exp)
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: (Exp) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> (Exp)
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyIn64 :: (Exp) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> (Exp)
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x0f\x05\x7f\x00\x00\x00\x0f\x05\x0f\x05\x34\x01\x34\x01\x34\x01\x34\x01\xb0\x00\xc0\x00\xc0\x00\xb6\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\x34\x01\x00\x00\x51\x01\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x00\x81\x00\x2c\x01\xac\x00\x2f\x01\xb6\x00\x2f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\x01\x05\x00\x2a\x01\x00\x00\x2a\x01\x00\x00\x2a\x01\x00\x00\x00\x00\x5a\x00\x4b\x01\x22\x01\x00\x00\x10\x01\x00\x00\x10\x01\x00\x00\x10\x01\x2d\x01\x0d\x01\x1e\x00\xff\xff\x01\x00\xfa\xff\x04\x00\xfe\xff\x0d\x01\x0d\x01\x00\x00\x0d\x01\x00\x00\x0d\x01\x00\x00\x00\x00\x00\x00\x0d\x01\x29\x01\xfb\x00\xfb\x00\x0c\x01\xef\x00\x03\x01\xda\x00\xee\x00\xec\x00\xcd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcd\x00\x15\x00\xf8\xff\xcd\x00\xe5\x00\xd1\x00\x00\x00\xd4\x00\xac\x00\xa2\x00\xca\x00\x00\x00\xa5\x00\xa5\x00\x7f\x00\x00\x00\x5c\x00\xac\x00\xbe\x00\x00\x00\xbd\x00\xac\x00\x00\x00\x00\x00\xb9\x00\xb5\x00\x0a\x00\xb3\x00\x0f\x05\x00\x00\xac\x00\x92\x00\x92\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\xac\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa9\x00\xac\x00\xa4\x00\x00\x00\x00\x00\x9d\x00\x05\x00\xa3\x01\xa3\x01\x81\x00\x81\x00\x81\x00\x81\x00\x12\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x05\x88\x00\x00\x00\x00\x00\x78\x00\x00\x00\x7f\x00\x7f\x00\x55\x00\x00\x00\x27\x00\xf0\xff\x00\x00\x7f\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x63\x00\x0f\x00\x8b\x01\x2f\x00\x46\x00\x27\x01\x35\x00\xa6\x00\x94\x00\x60\x00\x0d\x00\x1c\x05\x0b\x05\x05\x05\xf2\x04\xd0\x04\x9f\x04\x20\x04\xdd\x03\x69\x03\x38\x03\x17\x03\xee\x01\xd5\x02\xf6\x02\x81\x03\x98\x03\xaf\x03\x36\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x02\x85\x00\xde\x00\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x62\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x01\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x93\x02\x72\x02\x00\x00\x1f\x00\x00\x00\x00\x00\x6a\x01\x00\x00\x6a\x01\x51\x02\x00\x00\x00\x00\x00\x00\x30\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x01\x00\x00\x0f\x02\x83\x00\x16\x00\xeb\x04\xd8\x04\xbc\x04\xb4\x04\x8a\x04\x75\x04\x60\x04\x4b\x04\x0a\x04\xf4\x03\xc6\x03\x51\x03\xcd\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x01\x28\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x01\x0c\x00\xe6\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xcb\xff\x00\x00\x00\x00\xcb\xff\x00\x00\xc8\xff\x00\x00\xc4\xff\x00\x00\xc0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x96\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe2\xff\xc1\xff\xba\xff\xb9\xff\xb8\xff\xb5\xff\xb2\xff\xad\xff\xab\xff\xa8\xff\xa5\xff\x8e\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\xff\xb7\xff\xe1\xff\xe0\xff\xdf\xff\x9d\xff\x8f\xff\x00\x00\xa0\xff\x00\x00\x90\xff\x00\x00\x91\xff\x98\xff\xb5\xff\x99\xff\x97\xff\x92\xff\x00\x00\x9b\xff\x00\x00\x93\xff\x00\x00\x95\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb5\xff\x00\x00\xc1\xff\x00\x00\xbd\xff\xbc\xff\xbb\xff\x00\x00\xbf\xff\x00\x00\x00\x00\xc3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc7\xff\x00\x00\xda\xff\xdd\xff\xdc\xff\xdb\xff\xd9\xff\x00\x00\x00\x00\xc0\xff\x00\x00\x00\x00\x00\x00\xce\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcb\xff\x00\x00\x00\x00\xde\xff\xca\xff\x00\x00\x00\x00\x00\x00\xd3\xff\x00\x00\x00\x00\xcf\xff\xd5\xff\xc1\xff\x00\x00\xbf\xff\x00\x00\xc8\xff\xc9\xff\x00\x00\xc4\xff\xc0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x96\xff\xb1\xff\xb0\xff\xae\xff\xac\xff\xaf\xff\x00\x00\x96\xff\x00\x00\xb4\xff\x94\xff\x9a\xff\x9c\xff\x9f\xff\x9e\xff\xa1\xff\xa2\xff\xa3\xff\xa4\xff\xa6\xff\xa7\xff\xa9\xff\xaa\xff\xbe\xff\xc2\xff\xc5\xff\xc6\xff\xd7\xff\xd8\xff\xc8\xff\x00\x00\xd6\xff\xd4\xff\x00\x00\xcd\xff\x00\x00\x00\x00\x00\x00\xb3\xff\x00\x00\xd1\xff\xd2\xff\x00\x00\xcb\xff\x00\x00\xd0\xff\xcc\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x01\x00\x05\x00\x0c\x00\x15\x00\x01\x00\x0d\x00\x0e\x00\x0b\x00\x06\x00\x11\x00\x12\x00\x09\x00\x01\x00\x02\x00\x03\x00\x10\x00\x08\x00\x07\x00\x05\x00\x10\x00\x00\x00\x05\x00\x03\x00\x0f\x00\x22\x00\x0e\x00\x07\x00\x0b\x00\x09\x00\x0a\x00\x26\x00\x0c\x00\x0c\x00\x0d\x00\x26\x00\x26\x00\x07\x00\x26\x00\x13\x00\x14\x00\x26\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x07\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x03\x00\x00\x00\x20\x00\x0a\x00\x07\x00\x0c\x00\x09\x00\x0a\x00\x26\x00\x0c\x00\x1f\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x05\x00\x13\x00\x14\x00\x08\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x04\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x03\x00\x00\x00\x07\x00\x00\x00\x07\x00\x0a\x00\x09\x00\x0a\x00\x04\x00\x0c\x00\x0f\x00\x07\x00\x05\x00\x0c\x00\x0d\x00\x0c\x00\x13\x00\x14\x00\x0b\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x04\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x03\x00\x00\x00\x00\x00\x00\x00\x07\x00\x06\x00\x09\x00\x0a\x00\x09\x00\x0c\x00\x04\x00\x0a\x00\x0b\x00\x0c\x00\x0c\x00\x0c\x00\x13\x00\x14\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x02\x00\x0c\x00\x22\x00\x23\x00\x24\x00\x25\x00\x03\x00\x00\x00\x07\x00\x04\x00\x07\x00\x0a\x00\x09\x00\x0a\x00\x04\x00\x0c\x00\x03\x00\x0a\x00\x0b\x00\x0c\x00\x07\x00\x22\x00\x09\x00\x0a\x00\x05\x00\x16\x00\x03\x00\x08\x00\x09\x00\x03\x00\x07\x00\x1c\x00\x0c\x00\x0a\x00\x0c\x00\x16\x00\x03\x00\x22\x00\x23\x00\x24\x00\x25\x00\x1c\x00\x0c\x00\x0c\x00\x26\x00\x16\x00\x03\x00\x22\x00\x23\x00\x24\x00\x25\x00\x1c\x00\x23\x00\x24\x00\x25\x00\x16\x00\x03\x00\x22\x00\x23\x00\x24\x00\x25\x00\x1c\x00\x0c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0c\x00\x05\x00\x06\x00\x0f\x00\x10\x00\x11\x00\x0a\x00\x0c\x00\x0c\x00\x26\x00\x08\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x26\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x05\x00\x05\x00\x06\x00\x08\x00\x09\x00\x22\x00\x0a\x00\x0f\x00\x0c\x00\x08\x00\x26\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x26\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x05\x00\x05\x00\x06\x00\x08\x00\x09\x00\x08\x00\x0a\x00\x26\x00\x0c\x00\x08\x00\x26\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x20\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x02\x00\x05\x00\x06\x00\x26\x00\x22\x00\x26\x00\x0a\x00\x03\x00\x0c\x00\x22\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0d\x00\x0e\x00\xff\xff\xff\xff\x11\x00\x12\x00\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\xff\xff\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x1d\x00\x1e\x00\x1f\x00\x20\x00\xff\xff\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x1d\x00\x1e\x00\x1f\x00\x20\x00\xff\xff\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x1f\x00\x20\x00\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x1e\x00\x1f\x00\x20\x00\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1f\x00\x20\x00\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x0c\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x13\x00\x14\x00\xff\xff\xff\xff\xff\xff\x18\x00\x0c\x00\xff\xff\x1b\x00\x0f\x00\x1d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x93\x00\x91\x00\x89\x00\xc0\xff\xbf\x00\x91\x00\x8d\x00\x8e\x00\x8a\x00\x8b\x00\x8f\x00\x90\x00\x8c\x00\x54\x00\x55\x00\x56\x00\x92\x00\x88\x00\xc0\x00\x73\x00\x92\x00\x52\x00\x89\x00\x2c\x00\x86\x00\x1f\x00\x57\x00\x2d\x00\x8a\x00\x2e\x00\x2f\x00\xff\xff\x6e\x00\x58\x00\xab\x00\xff\xff\xff\xff\x77\x00\xff\xff\x63\x00\x64\x00\xff\xff\x30\x00\x6f\x00\x65\x00\x70\x00\x71\x00\x66\x00\x31\x00\x67\x00\x72\x00\x73\x00\x52\x00\x68\x00\x1f\x00\x32\x00\x33\x00\x34\x00\xff\xff\x2c\x00\x7f\x00\x94\x00\x5e\x00\x2d\x00\x5d\x00\x2e\x00\x2f\x00\xff\xff\x6e\x00\xc0\x00\x5b\x00\x80\x00\x81\x00\x82\x00\x5f\x00\x63\x00\x64\x00\x67\x00\x30\x00\x6f\x00\x65\x00\x70\x00\x71\x00\x66\x00\x31\x00\x67\x00\x72\x00\x73\x00\xbc\x00\xc3\x00\x1f\x00\x32\x00\x33\x00\x34\x00\x2c\x00\x52\x00\x96\x00\x52\x00\x2d\x00\x97\x00\x2e\x00\x2f\x00\x74\x00\x6e\x00\x86\x00\x75\x00\x89\x00\x58\x00\x59\x00\x84\x00\x63\x00\x64\x00\x8a\x00\x30\x00\x6f\x00\x65\x00\x70\x00\x71\x00\x66\x00\x31\x00\x67\x00\x72\x00\x73\x00\xb8\x00\xb7\x00\x1f\x00\x32\x00\x33\x00\x34\x00\x2c\x00\x52\x00\x52\x00\x52\x00\x2d\x00\x8b\x00\x2e\x00\x2f\x00\x8c\x00\x6e\x00\xb9\x00\x5b\x00\xac\x00\x5d\x00\x97\x00\x99\x00\x63\x00\x64\x00\x52\x00\x30\x00\x6f\x00\x65\x00\x70\x00\x71\x00\x66\x00\x31\x00\x67\x00\x72\x00\x73\x00\x93\x00\x5a\x00\x1f\x00\x32\x00\x33\x00\x34\x00\x2c\x00\x52\x00\x96\x00\xbb\x00\x2d\x00\x97\x00\x2e\x00\x2f\x00\x9e\x00\x7b\x00\x2c\x00\x5b\x00\x5c\x00\x5d\x00\x2d\x00\x1f\x00\x2e\x00\x2f\x00\x5f\x00\x30\x00\x2c\x00\x60\x00\xb9\x00\xb2\x00\x2d\x00\x31\x00\xb0\x00\x2f\x00\xb1\x00\x30\x00\x2c\x00\x1f\x00\x32\x00\x33\x00\x34\x00\x31\x00\xb4\x00\xb5\x00\xff\xff\x30\x00\x79\x00\x1f\x00\x32\x00\x33\x00\x34\x00\x31\x00\x32\x00\x33\x00\x34\x00\x30\x00\x7d\x00\x1f\x00\x32\x00\x33\x00\x34\x00\x31\x00\x7e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x1f\x00\x32\x00\x33\x00\x34\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x69\x00\x76\x00\x24\x00\x25\x00\x98\x00\x6b\x00\x7f\x00\x3d\x00\xff\xff\x84\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x6c\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x5f\x00\x69\x00\xc1\x00\x60\x00\xae\x00\x1f\x00\x6b\x00\x86\x00\x3d\x00\x87\x00\xff\xff\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x6c\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x5f\x00\x69\x00\xbc\x00\x60\x00\x61\x00\x88\x00\x6b\x00\xff\xff\x3d\x00\x95\x00\xff\xff\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x94\x00\x6c\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x93\x00\x69\x00\xbd\x00\xff\xff\x1f\x00\xff\xff\x6b\x00\x9c\x00\x3d\x00\x1f\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\x6c\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x69\x00\x76\x00\x00\x00\x00\x00\x00\x00\x6b\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\x6c\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x69\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x6b\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\x6c\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x8d\x00\x8e\x00\x00\x00\x00\x00\x8f\x00\x90\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x9c\x00\x46\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x9e\x00\x46\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x45\x00\x46\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\xad\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\xb2\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\xb5\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\x79\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\x7b\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\x9a\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\x43\x00\x44\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x00\x00\x00\x00\x41\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x47\x00\x00\x00\x00\x00\x00\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x3e\x00\x48\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x9f\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x49\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x42\x00\x3b\x00\x39\x00\x37\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x3a\x00\x3b\x00\x39\x00\x37\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x38\x00\x39\x00\x37\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x35\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x00\x37\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\xa0\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x34\x00\x4a\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\xa1\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\xa2\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x4b\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\xa3\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\xa4\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\xa5\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\xa6\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x4c\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\xa7\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\xa8\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x27\x00\x4d\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\xa9\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\xaa\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x26\x00\x4e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x4f\x00\x50\x00\x00\x00\x00\x00\x24\x00\x51\x00\x52\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x63\x00\x64\x00\x00\x00\x00\x00\x00\x00\x65\x00\x50\x00\x00\x00\x66\x00\x53\x00\x67\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (29, 113) [
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113)
	]

happy_n_terms = 39 :: Int
happy_n_nonterms = 33 :: Int

happyReduce_29 = happySpecReduce_1  0# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn32
		 (Ident happy_var_1
	)}

happyReduce_30 = happySpecReduce_1  1# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn33
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_31 = happySpecReduce_1  2# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn34
		 ((read ( happy_var_1)) :: Double
	)}

happyReduce_32 = happySpecReduce_1  3# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_33 = happySpecReduce_1  4# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (AbsMini.PStms (reverse happy_var_1)
	)}

happyReduce_34 = happySpecReduce_1  5# happyReduction_34
happyReduction_34 happy_x_1
	 =  happyIn37
		 (AbsMini.Type_double
	)

happyReduce_35 = happySpecReduce_1  5# happyReduction_35
happyReduction_35 happy_x_1
	 =  happyIn37
		 (AbsMini.Type_int
	)

happyReduce_36 = happySpecReduce_1  5# happyReduction_36
happyReduction_36 happy_x_1
	 =  happyIn37
		 (AbsMini.Type_string
	)

happyReduce_37 = happySpecReduce_1  5# happyReduction_37
happyReduction_37 happy_x_1
	 =  happyIn37
		 (AbsMini.Type_bool
	)

happyReduce_38 = happySpecReduce_1  5# happyReduction_38
happyReduction_38 happy_x_1
	 =  happyIn37
		 (AbsMini.Type_void
	)

happyReduce_39 = happySpecReduce_3  6# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 (AbsMini.SDefi happy_var_1 happy_var_2
	)}}

happyReduce_40 = happySpecReduce_3  6# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 (AbsMini.SDecl happy_var_1 happy_var_2
	)}}

happyReduce_41 = happySpecReduce_3  6# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 (AbsMini.SPrint happy_var_2
	)}

happyReduce_42 = happySpecReduce_2  6# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (AbsMini.SAssign happy_var_1
	)}

happyReduce_43 = happySpecReduce_3  6# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 (AbsMini.SRet happy_var_2
	)}

happyReduce_44 = happySpecReduce_2  6# happyReduction_44
happyReduction_44 happy_x_2
	happy_x_1
	 =  happyIn38
		 (AbsMini.SVRet
	)

happyReduce_45 = happyReduce 5# 6# happyReduction_45
happyReduction_45 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut59 happy_x_3 of { happy_var_3 -> 
	case happyOut38 happy_x_5 of { happy_var_5 -> 
	happyIn38
		 (AbsMini.SWhile happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_46 = happyReduce 5# 6# happyReduction_46
happyReduction_46 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut59 happy_x_3 of { happy_var_3 -> 
	case happyOut38 happy_x_5 of { happy_var_5 -> 
	happyIn38
		 (AbsMini.SIf happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_47 = happyReduce 7# 6# happyReduction_47
happyReduction_47 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut59 happy_x_3 of { happy_var_3 -> 
	case happyOut38 happy_x_5 of { happy_var_5 -> 
	case happyOut38 happy_x_7 of { happy_var_7 -> 
	happyIn38
		 (AbsMini.SIfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_48 = happySpecReduce_2  6# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (AbsMini.SExp happy_var_1
	)}

happyReduce_49 = happySpecReduce_1  6# happyReduction_49
happyReduction_49 happy_x_1
	 =  happyIn38
		 (AbsMini.SEmpty
	)

happyReduce_50 = happySpecReduce_3  6# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 (AbsMini.SBlock (reverse happy_var_2)
	)}

happyReduce_51 = happyReduce 8# 6# happyReduction_51
happyReduction_51 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	case happyOut41 happy_x_4 of { happy_var_4 -> 
	case happyOut39 happy_x_7 of { happy_var_7 -> 
	happyIn38
		 (AbsMini.SFunDfn happy_var_1 happy_var_2 happy_var_4 (reverse happy_var_7)
	) `HappyStk` happyRest}}}}

happyReduce_52 = happySpecReduce_0  7# happyReduction_52
happyReduction_52  =  happyIn39
		 ([]
	)

happyReduce_53 = happySpecReduce_2  7# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_2 of { happy_var_2 -> 
	happyIn39
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_54 = happySpecReduce_2  8# happyReduction_54
happyReduction_54 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (AbsMini.Param happy_var_1 happy_var_2
	)}}

happyReduce_55 = happySpecReduce_0  9# happyReduction_55
happyReduction_55  =  happyIn41
		 ([]
	)

happyReduce_56 = happySpecReduce_1  9# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 ((:[]) happy_var_1
	)}

happyReduce_57 = happySpecReduce_3  9# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_58 = happySpecReduce_3  10# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut59 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (AbsMini.Assign happy_var_1 happy_var_3
	)}}

happyReduce_59 = happySpecReduce_0  11# happyReduction_59
happyReduction_59  =  happyIn43
		 ([]
	)

happyReduce_60 = happySpecReduce_1  11# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 ((:[]) happy_var_1
	)}

happyReduce_61 = happySpecReduce_3  11# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_62 = happySpecReduce_1  12# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (AbsMini.Var happy_var_1
	)}

happyReduce_63 = happySpecReduce_0  13# happyReduction_63
happyReduction_63  =  happyIn45
		 ([]
	)

happyReduce_64 = happySpecReduce_1  13# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ((:[]) happy_var_1
	)}

happyReduce_65 = happySpecReduce_3  13# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_66 = happySpecReduce_1  14# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (AbsMini.VInt happy_var_1
	)}

happyReduce_67 = happySpecReduce_1  14# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (AbsMini.VFlo happy_var_1
	)}

happyReduce_68 = happySpecReduce_1  14# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (AbsMini.VStr happy_var_1
	)}

happyReduce_69 = happySpecReduce_1  15# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (AbsMini.EILit happy_var_1
	)}

happyReduce_70 = happySpecReduce_1  15# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (AbsMini.EFLit happy_var_1
	)}

happyReduce_71 = happySpecReduce_1  15# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (AbsMini.ESLit happy_var_1
	)}

happyReduce_72 = happySpecReduce_1  15# happyReduction_72
happyReduction_72 happy_x_1
	 =  happyIn47
		 (AbsMini.EBTLit
	)

happyReduce_73 = happySpecReduce_1  15# happyReduction_73
happyReduction_73 happy_x_1
	 =  happyIn47
		 (AbsMini.EBFLit
	)

happyReduce_74 = happySpecReduce_1  15# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (AbsMini.EVar happy_var_1
	)}

happyReduce_75 = happySpecReduce_3  15# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (happy_var_2
	)}

happyReduce_76 = happyReduce 4# 16# happyReduction_76
happyReduction_76 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_1 of { happy_var_1 -> 
	case happyOut58 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (AbsMini.EFunInv happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_77 = happySpecReduce_1  16# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (happy_var_1
	)}

happyReduce_78 = happySpecReduce_2  17# happyReduction_78
happyReduction_78 happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (AbsMini.EPoInc happy_var_1
	)}

happyReduce_79 = happySpecReduce_2  17# happyReduction_79
happyReduction_79 happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (AbsMini.EPoDec happy_var_1
	)}

happyReduce_80 = happySpecReduce_2  17# happyReduction_80
happyReduction_80 happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn49
		 (AbsMini.EPrInc happy_var_2
	)}

happyReduce_81 = happySpecReduce_2  17# happyReduction_81
happyReduction_81 happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn49
		 (AbsMini.EPrDec happy_var_2
	)}

happyReduce_82 = happySpecReduce_1  17# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (happy_var_1
	)}

happyReduce_83 = happySpecReduce_2  18# happyReduction_83
happyReduction_83 happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_2 of { happy_var_2 -> 
	happyIn50
		 (AbsMini.ENegate happy_var_2
	)}

happyReduce_84 = happySpecReduce_1  18# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_85 = happySpecReduce_3  19# happyReduction_85
happyReduction_85 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 (AbsMini.EMul happy_var_1 happy_var_3
	)}}

happyReduce_86 = happySpecReduce_3  19# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 (AbsMini.EDiv happy_var_1 happy_var_3
	)}}

happyReduce_87 = happySpecReduce_1  19# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (happy_var_1
	)}

happyReduce_88 = happySpecReduce_3  20# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (AbsMini.EAdd happy_var_1 happy_var_3
	)}}

happyReduce_89 = happySpecReduce_3  20# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (AbsMini.ESub happy_var_1 happy_var_3
	)}}

happyReduce_90 = happySpecReduce_1  20# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (happy_var_1
	)}

happyReduce_91 = happySpecReduce_3  21# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (AbsMini.EOrdLT happy_var_1 happy_var_3
	)}}

happyReduce_92 = happySpecReduce_3  21# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (AbsMini.EOrdLE happy_var_1 happy_var_3
	)}}

happyReduce_93 = happySpecReduce_3  21# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (AbsMini.EOrdGT happy_var_1 happy_var_3
	)}}

happyReduce_94 = happySpecReduce_3  21# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (AbsMini.EOrdGE happy_var_1 happy_var_3
	)}}

happyReduce_95 = happySpecReduce_1  21# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (happy_var_1
	)}

happyReduce_96 = happySpecReduce_3  22# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (AbsMini.EOrdEQ happy_var_1 happy_var_3
	)}}

happyReduce_97 = happySpecReduce_3  22# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (AbsMini.EOrdNE happy_var_1 happy_var_3
	)}}

happyReduce_98 = happySpecReduce_1  22# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (happy_var_1
	)}

happyReduce_99 = happySpecReduce_3  23# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn55
		 (AbsMini.EConj happy_var_1 happy_var_3
	)}}

happyReduce_100 = happySpecReduce_1  23# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut61 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 (happy_var_1
	)}

happyReduce_101 = happySpecReduce_3  24# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (AbsMini.EDisj happy_var_1 happy_var_3
	)}}

happyReduce_102 = happySpecReduce_1  24# happyReduction_102
happyReduction_102 happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (happy_var_1
	)}

happyReduce_103 = happySpecReduce_1  25# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (AbsMini.EAssign happy_var_1
	)}

happyReduce_104 = happySpecReduce_1  25# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (happy_var_1
	)}

happyReduce_105 = happySpecReduce_0  26# happyReduction_105
happyReduction_105  =  happyIn58
		 ([]
	)

happyReduce_106 = happySpecReduce_1  26# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 ((:[]) happy_var_1
	)}

happyReduce_107 = happySpecReduce_3  26# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	case happyOut58 happy_x_3 of { happy_var_3 -> 
	happyIn58
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_108 = happySpecReduce_1  27# happyReduction_108
happyReduction_108 happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	happyIn59
		 (happy_var_1
	)}

happyReduce_109 = happySpecReduce_1  28# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (happy_var_1
	)}

happyReduce_110 = happySpecReduce_1  29# happyReduction_110
happyReduction_110 happy_x_1
	 =  case happyOut62 happy_x_1 of { happy_var_1 -> 
	happyIn61
		 (happy_var_1
	)}

happyReduce_111 = happySpecReduce_1  30# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOut63 happy_x_1 of { happy_var_1 -> 
	happyIn62
		 (happy_var_1
	)}

happyReduce_112 = happySpecReduce_1  31# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn63
		 (happy_var_1
	)}

happyReduce_113 = happySpecReduce_1  32# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn64
		 (happy_var_1
	)}

happyNewToken action sts stk [] =
	happyDoAction 38# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TV happy_dollar_dollar) -> cont 34#;
	PT _ (TI happy_dollar_dollar) -> cont 35#;
	PT _ (TD happy_dollar_dollar) -> cont 36#;
	PT _ (TL happy_dollar_dollar) -> cont 37#;
	_ -> happyError' (tk:tks)
	}

happyError_ 38# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut36 x))

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut37 x))

pStm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut38 x))

pListStm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut39 x))

pParam tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut40 x))

pListParam tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut41 x))

pAssign tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut42 x))

pListAssign tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut43 x))

pVar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut44 x))

pListVar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut45 x))

pValue tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut46 x))

pExp16 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut47 x))

pExp15 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut48 x))

pExp14 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut49 x))

pExp13 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut50 x))

pExp12 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut51 x))

pExp11 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut52 x))

pExp9 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut53 x))

pExp8 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut54 x))

pExp4 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut55 x))

pExp3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut56 x))

pExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (happyOut57 x))

pListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (happyOut58 x))

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (happyOut59 x))

pExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (happyOut60 x))

pExp5 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (happyOut61 x))

pExp6 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (happyOut62 x))

pExp7 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (happyOut63 x))

pExp10 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (happyOut64 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc-8.0.1/include/ghcversion.h" #-}

















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc670_0/ghc_2.h" #-}


































































































































































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
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
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

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
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
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
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

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
happyDoSeq   a b = a `seq` b
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
