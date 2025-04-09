{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
-- Cabeçalho do módulo e importação do Lexer
module Parser where
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.0.2

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29
        = HappyTerminal (Token)
        | HappyErrorToken Prelude.Int
        | HappyAbsSyn4 t4
        | HappyAbsSyn5 t5
        | HappyAbsSyn6 t6
        | HappyAbsSyn7 t7
        | HappyAbsSyn8 t8
        | HappyAbsSyn9 t9
        | HappyAbsSyn10 t10
        | HappyAbsSyn11 t11
        | HappyAbsSyn12 t12
        | HappyAbsSyn13 t13
        | HappyAbsSyn14 t14
        | HappyAbsSyn15 t15
        | HappyAbsSyn16 t16
        | HappyAbsSyn17 t17
        | HappyAbsSyn18 t18
        | HappyAbsSyn19 t19
        | HappyAbsSyn20 t20
        | HappyAbsSyn21 t21
        | HappyAbsSyn22 t22
        | HappyAbsSyn23 t23
        | HappyAbsSyn24 t24
        | HappyAbsSyn25 t25
        | HappyAbsSyn26 t26
        | HappyAbsSyn27 t27
        | HappyAbsSyn28 t28
        | HappyAbsSyn29 t29

happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x40\x04\x00\x00\x00\x00\x00\x00\x30\x2a\x03\x60\x00\x00\x00\x00\x00\x10\x80\x7f\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\xc0\xa8\x0c\x80\x09\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\xfe\x21\x00\x00\x00\x00\x00\x00\x00\x80\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\x32\x00\x06\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x8c\xca\x00\x18\x00\x00\x00\x00\x80\x51\x19\x00\x03\x00\x00\x00\x00\x30\x2a\x03\x60\x00\x00\x00\x00\x00\x46\x65\x00\x0c\x00\x00\x00\x00\xc0\xa8\x0c\x80\x01\x00\x00\x00\x00\x18\x95\x01\x30\x00\x00\x00\x00\x00\xa3\x32\x00\x06\x00\x00\x00\x00\x60\x54\x06\xc0\x00\x00\x00\x00\x00\x8c\xca\x00\x18\x00\x00\x00\x00\x00\x04\xe0\x3f\x02\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x10\x00\x40\x08\x00\x00\x00\x00\x00\x02\xf0\x0b\x01\x00\x00\x00\x00\x40\x00\x00\x21\x00\x00\x00\x00\x00\x08\x00\x20\x04\x00\x00\x00\x00\x00\x01\x80\x85\x00\x00\x00\x00\x00\x20\x00\xb0\x10\x00\x00\x00\x00\x00\x04\x00\x16\x02\x00\x00\x00\x00\x80\x00\xc0\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x00\x00\x18\x95\x01\x30\x00\x00\x00\x00\x00\xa3\x32\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x80\x51\x19\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x12\x57\x00\x08\x5c\x00\x00\x00\x00\x40\x00\x00\x00\x04\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x02\xf0\x0f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xc0\x3f\x04\x00\x00\x00\x00\x00\x00\x00\x10\x08\x00\x00\x00\x00\x8c\xca\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x65\x00\x0c\x00\x00\x00\x00\x00\x02\xf0\x0f\x09\x00\x00\x00\x00\x40\x00\xfe\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x02\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x01\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x5c\x01\x20\x71\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x20\x00\x00\x00\x00\x80\x51\x19\x00\x07\x00\x00\x00\x00\x30\x2a\x03\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\xc0\xa8\x0c\x80\x01\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\xa3\x32\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x51\x19\x00\x03\x00\x00\x00\x00\x00\x00\x00\x08\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\xf0\x0f\x21\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x08\xc0\x3f\x06\x00\x00\x00\x00\x00\x01\xf8\x87\x00\x00\x00\x00\x00\x12\x57\x00\x08\x5c\x00\x00\x00\x80\x51\x19\x00\x03\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x46\x65\x00\x0c\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x51\x19\x00\x03\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x46\x65\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xc0\x3f\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\xff\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x81\x00\x00\x00\x00\x00\x20\x00\x00\x00\x02\x00\x00\x00\x40\x00\xfe\x21\x02\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\xfe\x21\x02\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x10\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\xe2\x0a\x00\x81\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x04\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","ArgumentList","Assignment","Block","Declaration","DeclarationList","Expression","ExpressionList","FieldAssignment","FieldList","IfCondition","IndexContent","LValue","MatchedStmt","NewType","OptionalSemicolon","ParamList","ParamListWithTypes","ParamWithOrWithoutType","Program","ReturnTy","Statement","StatementList","Type","TypeList","UnmatchedStmt","WhileStmt","ASSIGN","BREAK","BOOL","CHAR","DATA","DOT","ELSE","FLOAT","FUN","ID","IF","INT","ITERATE","MAIN","NEW","NULL","OP_GE","OP_LE","LT","RT","PLUS","MINUS","AND","OP","P_COLON","P_COMMA","P_DOUBLECOLON","P_LBRACE","P_LBRACKET","P_LPAREN","P_RBRACE","P_RBRACKET","P_RPAREN","P_SEMICOLON","PRINT","READ","RETURN","TYPEID","WHILE","%eof"]
        bit_start = st               Prelude.* 69
        bit_end   = (st Prelude.+ 1) Prelude.* 69
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..68]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x85\x00\x00\x00\xb2\x00\x00\x00\x09\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\xff\xff\x00\x00\x00\x00\xf9\xff\xff\xff\x00\x00\x00\x00\x13\x00\x00\x00\x3d\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x17\x00\x00\x00\x38\x00\x00\x00\x5f\x00\x00\x00\x54\x00\x00\x00\x00\x00\x00\x00\x09\x01\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x68\x00\x00\x00\x79\x00\x00\x00\x7b\x00\x00\x00\x86\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\xa1\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\xe2\x00\x00\x00\x94\x00\x00\x00\x0d\x00\x00\x00\x1c\x01\x00\x00\x0d\x00\x00\x00\x0d\x00\x00\x00\x55\x00\x00\x00\x55\x00\x00\x00\x55\x00\x00\x00\x55\x00\x00\x00\x00\x00\x00\x00\x3e\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\xad\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\x98\x00\x00\x00\xa3\x00\x00\x00\xaa\x00\x00\x00\xac\x00\x00\x00\xc3\x00\x00\x00\x0b\x00\x00\x00\xfb\xff\xff\xff\xce\x00\x00\x00\xca\x00\x00\x00\x09\x01\x00\x00\x00\x00\x00\x00\x09\x01\x00\x00\x63\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x88\x00\x00\x00\x09\x01\x00\x00\x00\x00\x00\x00\xd2\x00\x00\x00\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x00\x00\x00\xe0\x00\x00\x00\xcd\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xde\x00\x00\x00\xe6\x00\x00\x00\xf4\x00\x00\x00\x0f\x00\x00\x00\x33\x00\x00\x00\xb2\x00\x00\x00\xdb\x00\x00\x00\xb2\x00\x00\x00\xf8\x00\x00\x00\xb2\x00\x00\x00\xe5\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\xed\xff\xff\xff\x00\x00\x00\x00\x4f\x00\x00\x00\xe7\x00\x00\x00\xf6\x00\x00\x00\x09\x01\x00\x00\x0b\x00\x00\x00\xb2\x00\x00\x00\xeb\x00\x00\x00\xb2\x00\x00\x00\xfc\x00\x00\x00\xf5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x06\x01\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\xf7\x00\x00\x00\xf9\x00\x00\x00\x00\x00\x00\x00\xfa\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x9b\x00\x00\x00\x00\x00\x00\x00\x09\x01\x00\x00\x00\x00\x00\x00\xfd\x00\x00\x00\x64\x00\x00\x00\xfb\xff\xff\xff\x62\x00\x00\x00\x0e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x00\x00\x00\x08\x01\x00\x00\x76\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x00\x00\x03\x01\x00\x00\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x01\x00\x00\x0c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x0e\x00\x00\x00\x13\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x01\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x6a\x00\x00\x00\x9f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x96\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x24\x01\x00\x00\x25\x01\x00\x00\x32\x01\x00\x00\x33\x01\x00\x00\x35\x01\x00\x00\x36\x01\x00\x00\x37\x01\x00\x00\x38\x01\x00\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x00\x00\x00\x39\x01\x00\x00\x00\x00\x00\x00\xa2\x00\x00\x00\x3a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3e\x01\x00\x00\xc0\x00\x00\x00\xbe\x00\x00\x00\x2b\x01\x00\x00\xbb\x00\x00\x00\x30\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x01\x00\x00\xc9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x00\x00\x00\x8c\x00\x00\x00\x42\x01\x00\x00\x00\x00\x00\x00\x43\x01\x00\x00\x00\x00\x00\x00\xdc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x45\x01\x00\x00\x3b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x01\x00\x00\x00\x00\x00\x00\xd9\x00\x00\x00\x47\x01\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\xd6\x00\x00\x00\x4b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x01\x00\x00\x00\x00\x00\x00\x4c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x51\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x01\x00\x00\x00\x00\x00\x00\x52\x01\x00\x00\x00\x00\x00\x00\xd5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x54\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xff\xf2\xff\xff\xff\xf3\xff\xff\xff\xf4\xff\xff\xff\xf6\xff\xff\xff\xf5\xff\xff\xff\x00\x00\x00\x00\xf1\xff\xff\xff\xda\xff\xff\xff\x00\x00\x00\x00\xf8\xff\xff\xff\xb8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\xff\xff\xf7\xff\xff\xff\xdf\xff\xff\xff\x00\x00\x00\x00\xe0\xff\xff\xff\xdc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe7\xff\xff\xff\xc5\xff\xff\xff\xc6\xff\xff\xff\xfc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\xff\xff\xff\x00\x00\x00\x00\xee\xff\xff\xff\xe9\xff\xff\xff\xef\xff\xff\xff\xf0\xff\xff\xff\xea\xff\xff\xff\xeb\xff\xff\xff\xed\xff\xff\xff\xec\xff\xff\xff\xe5\xff\xff\xff\x00\x00\x00\x00\xfc\xff\xff\xff\x00\x00\x00\x00\xe2\xff\xff\xff\xda\xff\xff\xff\x00\x00\x00\x00\xe1\xff\xff\xff\x00\x00\x00\x00\xbe\xff\xff\xff\xbc\xff\xff\xff\x00\x00\x00\x00\xb9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\xff\xff\xb6\xff\xff\xff\xde\xff\xff\xff\xdb\xff\xff\xff\xdd\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\xff\xff\xe3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\xff\xff\xe6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbd\xff\xff\xff\xbb\xff\xff\xff\xaf\xff\xff\xff\xb0\xff\xff\xff\x00\x00\x00\x00\xc9\xff\xff\xff\x00\x00\x00\x00\xb4\xff\xff\xff\xb2\xff\xff\xff\x00\x00\x00\x00\xb3\xff\xff\xff\xb5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\xff\xff\xff\x00\x00\x00\x00\xc3\xff\xff\xff\xcc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xff\xff\xbf\xff\xff\xff\x00\x00\x00\x00\xc7\xff\xff\xff\xb1\xff\xff\xff\xfa\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xac\xff\xff\xff\xb7\xff\xff\xff\xf9\xff\xff\xff\xe4\xff\xff\xff\x00\x00\x00\x00\xad\xff\xff\xff\xae\xff\xff\xff\x00\x00\x00\x00\xd5\xff\xff\xff\xfb\xff\xff\xff\xce\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\xff\xff\xff\xaa\xff\xff\xff\xcd\xff\xff\xff\xc0\xff\xff\xff\xd3\xff\xff\xff\xd1\xff\xff\xff\xc4\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\xff\xff\xff\x00\x00\x00\x00\xd9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\xff\xff\xff\xab\xff\xff\xff\x00\x00\x00\x00\xc8\xff\xff\xff\xcf\xff\xff\xff\xd2\xff\xff\xff\xa9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa8\xff\xff\xff\xcb\xff\xff\xff\xc1\xff\xff\xff\xd0\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x02\x00\x00\x00\x1e\x00\x00\x00\x0a\x00\x00\x00\x05\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x05\x00\x00\x00\x0d\x00\x00\x00\x02\x00\x00\x00\x0f\x00\x00\x00\x22\x00\x00\x00\x05\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x06\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0d\x00\x00\x00\x0a\x00\x00\x00\x0f\x00\x00\x00\x1c\x00\x00\x00\x06\x00\x00\x00\x0a\x00\x00\x00\x1f\x00\x00\x00\x26\x00\x00\x00\x12\x00\x00\x00\x26\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x18\x00\x00\x00\x27\x00\x00\x00\x1c\x00\x00\x00\x19\x00\x00\x00\x0a\x00\x00\x00\x1d\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x1e\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x02\x00\x00\x00\x27\x00\x00\x00\x1d\x00\x00\x00\x05\x00\x00\x00\x1e\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x08\x00\x00\x00\x0d\x00\x00\x00\x0a\x00\x00\x00\x0f\x00\x00\x00\x0c\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x1a\x00\x00\x00\x08\x00\x00\x00\x1a\x00\x00\x00\x0a\x00\x00\x00\x1c\x00\x00\x00\x0c\x00\x00\x00\x20\x00\x00\x00\x1c\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x28\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x06\x00\x00\x00\x1e\x00\x00\x00\x05\x00\x00\x00\x1a\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x06\x00\x00\x00\x0a\x00\x00\x00\x20\x00\x00\x00\x0a\x00\x00\x00\x21\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x1d\x00\x00\x00\x18\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x22\x00\x00\x00\x1d\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x1a\x00\x00\x00\x1d\x00\x00\x00\x21\x00\x00\x00\x05\x00\x00\x00\x1a\x00\x00\x00\x21\x00\x00\x00\x21\x00\x00\x00\x21\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x06\x00\x00\x00\x0a\x00\x00\x00\x1a\x00\x00\x00\x05\x00\x00\x00\x1d\x00\x00\x00\x0e\x00\x00\x00\x19\x00\x00\x00\x09\x00\x00\x00\x21\x00\x00\x00\x21\x00\x00\x00\x1f\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x06\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x1e\x00\x00\x00\x1d\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x20\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x0a\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x20\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x00\x00\x1d\x00\x00\x00\x21\x00\x00\x00\x08\x00\x00\x00\x20\x00\x00\x00\x0a\x00\x00\x00\x1a\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x1b\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x1c\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x0a\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x0a\x00\x00\x00\x14\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x1d\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x19\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x06\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x14\x00\x00\x00\x1c\x00\x00\x00\x22\x00\x00\x00\x0a\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x06\x00\x00\x00\x1d\x00\x00\x00\x0a\x00\x00\x00\x1d\x00\x00\x00\x22\x00\x00\x00\x26\x00\x00\x00\x0a\x00\x00\x00\x1e\x00\x00\x00\x19\x00\x00\x00\x1e\x00\x00\x00\x0a\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x06\x00\x00\x00\x0a\x00\x00\x00\x1c\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1a\x00\x00\x00\x07\x00\x00\x00\x19\x00\x00\x00\x20\x00\x00\x00\x05\x00\x00\x00\x20\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x06\x00\x00\x00\x0a\x00\x00\x00\x1c\x00\x00\x00\x22\x00\x00\x00\x1d\x00\x00\x00\x0a\x00\x00\x00\x1c\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0d\x00\x00\x00\x18\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x1d\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x16\x00\x00\x00\x18\x00\x00\x00\x13\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x0e\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\xff\xff\xff\xff\x05\x00\x00\x00\x0f\x00\x00\x00\x05\x00\x00\x00\x16\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x5d\x00\x00\x00\x1e\x00\x00\x00\x1c\x00\x00\x00\x5e\x00\x00\x00\x53\x00\x00\x00\x33\x00\x00\x00\x39\x00\x00\x00\x5f\x00\x00\x00\x60\x00\x00\x00\x61\x00\x00\x00\x02\x00\x00\x00\x62\x00\x00\x00\x5d\x00\x00\x00\x63\x00\x00\x00\x94\x00\x00\x00\x5e\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x1f\x00\x00\x00\x5f\x00\x00\x00\x60\x00\x00\x00\x61\x00\x00\x00\x79\x00\x00\x00\x62\x00\x00\x00\x3d\x00\x00\x00\x63\x00\x00\x00\x40\x00\x00\x00\x7a\x00\x00\x00\x1a\x00\x00\x00\x78\x00\x00\x00\x1d\x00\x00\x00\x0e\x00\x00\x00\x54\x00\x00\x00\x64\x00\x00\x00\x65\x00\x00\x00\x66\x00\x00\x00\x27\x00\x00\x00\x67\x00\x00\x00\x40\x00\x00\x00\xbf\xff\xff\xff\x10\x00\x00\x00\x28\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x73\x00\x00\x00\x64\x00\x00\x00\x65\x00\x00\x00\x66\x00\x00\x00\x5d\x00\x00\x00\x67\x00\x00\x00\x7b\x00\x00\x00\x5e\x00\x00\x00\x13\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x5f\x00\x00\x00\x60\x00\x00\x00\x61\x00\x00\x00\x06\x00\x00\x00\x62\x00\x00\x00\x07\x00\x00\x00\x63\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x39\x00\x00\x00\x06\x00\x00\x00\x83\x00\x00\x00\x07\x00\x00\x00\x40\x00\x00\x00\x08\x00\x00\x00\x3a\x00\x00\x00\x40\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\xff\xff\xff\xff\x0b\x00\x00\x00\x0c\x00\x00\x00\x71\x00\x00\x00\x64\x00\x00\x00\x65\x00\x00\x00\x66\x00\x00\x00\x1f\x00\x00\x00\x12\x00\x00\x00\x28\x00\x00\x00\x48\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x1f\x00\x00\x00\x29\x00\x00\x00\x17\x00\x00\x00\x3d\x00\x00\x00\x49\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1f\x00\x00\x00\x46\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x28\x00\x00\x00\x27\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x92\x00\x00\x00\x28\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1f\x00\x00\x00\x8a\x00\x00\x00\x48\x00\x00\x00\x48\x00\x00\x00\x28\x00\x00\x00\x3e\x00\x00\x00\x02\x00\x00\x00\x38\x00\x00\x00\x9a\x00\x00\x00\x4e\x00\x00\x00\x9c\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1f\x00\x00\x00\x10\x00\x00\x00\xa5\x00\x00\x00\x6e\x00\x00\x00\x28\x00\x00\x00\x11\x00\x00\x00\x36\x00\x00\x00\x6f\x00\x00\x00\x96\x00\x00\x00\xa6\x00\x00\x00\x37\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1f\x00\x00\x00\x0c\x00\x00\x00\x13\x00\x00\x00\x35\x00\x00\x00\x28\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x82\x00\x00\x00\x17\x00\x00\x00\x44\x00\x00\x00\x33\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x4a\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1a\x00\x00\x00\x28\x00\x00\x00\x43\x00\x00\x00\x06\x00\x00\x00\x9e\x00\x00\x00\x07\x00\x00\x00\x42\x00\x00\x00\x08\x00\x00\x00\x54\x00\x00\x00\x55\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0c\x00\x00\x00\x67\x00\x00\x00\x41\x00\x00\x00\x54\x00\x00\x00\x55\x00\x00\x00\x40\x00\x00\x00\x56\x00\x00\x00\x57\x00\x00\x00\x50\x00\x00\x00\x3b\x00\x00\x00\x10\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x56\x00\x00\x00\x57\x00\x00\x00\x58\x00\x00\x00\x59\x00\x00\x00\x71\x00\x00\x00\x3b\x00\x00\x00\x5a\x00\x00\x00\x5b\x00\x00\x00\x3d\x00\x00\x00\x76\x00\x00\x00\x54\x00\x00\x00\x55\x00\x00\x00\x7e\x00\x00\x00\x5a\x00\x00\x00\x5b\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x00\x00\x14\x00\x00\x00\x69\x00\x00\x00\x50\x00\x00\x00\x56\x00\x00\x00\x8d\x00\x00\x00\x89\x00\x00\x00\x3b\x00\x00\x00\x1f\x00\x00\x00\x7e\x00\x00\x00\x9a\x00\x00\x00\x7e\x00\x00\x00\x9f\x00\x00\x00\x8e\x00\x00\x00\x40\x00\x00\x00\x7c\x00\x00\x00\x75\x00\x00\x00\x5a\x00\x00\x00\x5b\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x4b\x00\x00\x00\x1f\x00\x00\x00\x7d\x00\x00\x00\x74\x00\x00\x00\x28\x00\x00\x00\x76\x00\x00\x00\x6d\x00\x00\x00\x6b\x00\x00\x00\x69\x00\x00\x00\x8c\x00\x00\x00\x91\x00\x00\x00\x3d\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1f\x00\x00\x00\x87\x00\x00\x00\x40\x00\x00\x00\x40\x00\x00\x00\x28\x00\x00\x00\x83\x00\x00\x00\x99\x00\x00\x00\x9d\x00\x00\x00\x85\x00\x00\x00\x02\x00\x00\x00\x84\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x1f\x00\x00\x00\x98\x00\x00\x00\x40\x00\x00\x00\xa1\x00\x00\x00\x28\x00\x00\x00\xa9\x00\x00\x00\x40\x00\x00\x00\x31\x00\x00\x00\x30\x00\x00\x00\x54\x00\x00\x00\x55\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x1a\x00\x00\x00\x27\x00\x00\x00\x56\x00\x00\x00\xa2\x00\x00\x00\x2f\x00\x00\x00\x2e\x00\x00\x00\x28\x00\x00\x00\x2d\x00\x00\x00\x2c\x00\x00\x00\x2b\x00\x00\x00\x2a\x00\x00\x00\x45\x00\x00\x00\x43\x00\x00\x00\x3e\x00\x00\x00\x51\x00\x00\x00\xa3\x00\x00\x00\x4e\x00\x00\x00\x4c\x00\x00\x00\x4b\x00\x00\x00\x80\x00\x00\x00\x6d\x00\x00\x00\x6b\x00\x00\x00\x92\x00\x00\x00\x94\x00\x00\x00\x8f\x00\x00\x00\x8c\x00\x00\x00\x88\x00\x00\x00\x00\x00\x00\x00\x87\x00\x00\x00\x96\x00\x00\x00\x85\x00\x00\x00\x9e\x00\x00\x00\xa6\x00\x00\x00\xa1\x00\x00\x00\xa9\x00\x00\x00\xa7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 87) [
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
        (87 , happyReduce_87)
        ]

happy_n_terms = 41 :: Prelude.Int
happy_n_nonterms = 26 :: Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn4
                 ([happy_var_1]
        )
happyReduction_1 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happySpecReduce_3  0# happyReduction_2
happyReduction_2 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn4  happy_var_1)
         =  HappyAbsSyn4
                 (happy_var_1 ++ [happy_var_3]
        )
happyReduction_2 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happySpecReduce_0  0# happyReduction_3
happyReduction_3  =  HappyAbsSyn4
                 ([]
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happySpecReduce_3  1# happyReduction_4
happyReduction_4 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn15  happy_var_1)
         =  HappyAbsSyn5
                 (LValueAssignment happy_var_1 happy_var_3
        )
happyReduction_4 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happySpecReduce_3  2# happyReduction_5
happyReduction_5 _
        (HappyAbsSyn25  happy_var_2)
        _
         =  HappyAbsSyn6
                 (BlockStmt happy_var_2
        )
happyReduction_5 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happyReduce 6# 3# happyReduction_6
happyReduction_6 ((HappyAbsSyn6  happy_var_6) `HappyStk`
        (HappyAbsSyn23  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn20  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (ID happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn7
                 (FunStmt happy_var_1 happy_var_3 happy_var_5 happy_var_6
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happySpecReduce_1  4# happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn8
                 ([happy_var_1]
        )
happyReduction_7 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happySpecReduce_2  4# happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
        (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn8
                 (happy_var_1 : happy_var_2
        )
happyReduction_8 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_1  5# happyReduction_9
happyReduction_9 (HappyTerminal (ID happy_var_1))
         =  HappyAbsSyn9
                 (VarExpr happy_var_1
        )
happyReduction_9 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_1  5# happyReduction_10
happyReduction_10 (HappyTerminal (INT happy_var_1))
         =  HappyAbsSyn9
                 (IntExpr happy_var_1
        )
happyReduction_10 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happySpecReduce_1  5# happyReduction_11
happyReduction_11 (HappyTerminal (FLOAT happy_var_1))
         =  HappyAbsSyn9
                 (FloatExpr happy_var_1
        )
happyReduction_11 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happySpecReduce_1  5# happyReduction_12
happyReduction_12 (HappyTerminal (CHAR happy_var_1))
         =  HappyAbsSyn9
                 (CharExpr happy_var_1
        )
happyReduction_12 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happySpecReduce_1  5# happyReduction_13
happyReduction_13 (HappyTerminal (BOOL happy_var_1))
         =  HappyAbsSyn9
                 (BoolExpr happy_var_1
        )
happyReduction_13 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happySpecReduce_1  5# happyReduction_14
happyReduction_14 _
         =  HappyAbsSyn9
                 (NullExpr
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happySpecReduce_3  5# happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (OpExpr "+" happy_var_1 happy_var_3
        )
happyReduction_15 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happySpecReduce_3  5# happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (OpExpr "-" happy_var_1 happy_var_3
        )
happyReduction_16 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_3  5# happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_3)
        (HappyTerminal (OP happy_var_2))
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (OpExpr happy_var_2 happy_var_1 happy_var_3
        )
happyReduction_17 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_3  5# happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (OpExpr "<=" happy_var_1 happy_var_3
        )
happyReduction_18 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_3  5# happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (OpExpr ">=" happy_var_1 happy_var_3
        )
happyReduction_19 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_3  5# happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (OpExpr "<" happy_var_1 happy_var_3
        )
happyReduction_20 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_3  5# happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (OpExpr ">" happy_var_1 happy_var_3
        )
happyReduction_21 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happySpecReduce_3  5# happyReduction_22
happyReduction_22 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (OpExpr "&&" happy_var_1 happy_var_3
        )
happyReduction_22 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happyReduce 4# 5# happyReduction_23
happyReduction_23 (_ `HappyStk`
        (HappyAbsSyn4  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (ID happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (CallExpr happy_var_1 happy_var_3
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happySpecReduce_2  5# happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_2)
        _
         =  HappyAbsSyn9
                 (NewExpr happy_var_2
        )
happyReduction_24 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happyReduce 5# 5# happyReduction_25
happyReduction_25 (_ `HappyStk`
        (HappyAbsSyn4  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn17  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (NewExprArgs happy_var_2 happy_var_4
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_26 = happySpecReduce_3  5# happyReduction_26
happyReduction_26 (HappyTerminal (ID happy_var_3))
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (FieldAccessExpr happy_var_1 happy_var_3
        )
happyReduction_26 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_27 = happyReduce 6# 5# happyReduction_27
happyReduction_27 (_ `HappyStk`
        (HappyAbsSyn9  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn9  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn9  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (SliceExpr happy_var_1 happy_var_3 happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_28 = happyReduce 4# 5# happyReduction_28
happyReduction_28 (_ `HappyStk`
        (HappyAbsSyn14  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn9  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (IndexExpr happy_var_1 happy_var_3
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_29 = happySpecReduce_3  5# happyReduction_29
happyReduction_29 _
        (HappyAbsSyn12  happy_var_2)
        _
         =  HappyAbsSyn9
                 (RecordExpr happy_var_2
        )
happyReduction_29 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_30 = happySpecReduce_3  5# happyReduction_30
happyReduction_30 _
        (HappyAbsSyn10  happy_var_2)
        _
         =  HappyAbsSyn9
                 (ArrayLiteral happy_var_2
        )
happyReduction_30 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_31 = happySpecReduce_2  5# happyReduction_31
happyReduction_31 _
        _
         =  HappyAbsSyn9
                 (ArrayLiteral []
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_32 = happySpecReduce_1  6# happyReduction_32
happyReduction_32 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn10
                 ([happy_var_1]
        )
happyReduction_32 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_33 = happySpecReduce_3  6# happyReduction_33
happyReduction_33 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn10  happy_var_1)
         =  HappyAbsSyn10
                 (happy_var_1 ++ [happy_var_3]
        )
happyReduction_33 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_34 = happySpecReduce_3  7# happyReduction_34
happyReduction_34 (HappyAbsSyn9  happy_var_3)
        _
        (HappyTerminal (ID happy_var_1))
         =  HappyAbsSyn11
                 ((happy_var_1, happy_var_3)
        )
happyReduction_34 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_35 = happySpecReduce_1  8# happyReduction_35
happyReduction_35 (HappyAbsSyn11  happy_var_1)
         =  HappyAbsSyn12
                 ([happy_var_1]
        )
happyReduction_35 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_36 = happySpecReduce_3  8# happyReduction_36
happyReduction_36 (HappyAbsSyn12  happy_var_3)
        _
        (HappyAbsSyn11  happy_var_1)
         =  HappyAbsSyn12
                 (happy_var_1 : happy_var_3
        )
happyReduction_36 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_37 = happySpecReduce_0  8# happyReduction_37
happyReduction_37  =  HappyAbsSyn12
                 ([]
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_38 = happySpecReduce_3  9# happyReduction_38
happyReduction_38 _
        (HappyAbsSyn9  happy_var_2)
        _
         =  HappyAbsSyn13
                 (happy_var_2
        )
happyReduction_38 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_39 = happySpecReduce_1  9# happyReduction_39
happyReduction_39 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn13
                 (happy_var_1
        )
happyReduction_39 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_40 = happySpecReduce_1  10# happyReduction_40
happyReduction_40 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn14
                 (happy_var_1
        )
happyReduction_40 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_41 = happySpecReduce_1  11# happyReduction_41
happyReduction_41 (HappyTerminal (ID happy_var_1))
         =  HappyAbsSyn15
                 (VarExpr happy_var_1
        )
happyReduction_41 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_42 = happySpecReduce_3  11# happyReduction_42
happyReduction_42 (HappyTerminal (ID happy_var_3))
        _
        (HappyAbsSyn15  happy_var_1)
         =  HappyAbsSyn15
                 (FieldAccessExpr happy_var_1 happy_var_3
        )
happyReduction_42 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_43 = happyReduce 4# 11# happyReduction_43
happyReduction_43 (_ `HappyStk`
        (HappyAbsSyn9  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn15  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn15
                 (IndexExpr happy_var_1 happy_var_3
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_44 = happySpecReduce_3  12# happyReduction_44
happyReduction_44 _
        (HappyAbsSyn9  happy_var_2)
        _
         =  HappyAbsSyn16
                 (PrintStmt happy_var_2
        )
happyReduction_44 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_45 = happyReduce 5# 12# happyReduction_45
happyReduction_45 ((HappyAbsSyn16  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn16  happy_var_3) `HappyStk`
        (HappyAbsSyn13  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (IfStmt happy_var_2 happy_var_3 happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_46 = happySpecReduce_3  12# happyReduction_46
happyReduction_46 _
        (HappyAbsSyn10  happy_var_2)
        _
         =  HappyAbsSyn16
                 (ReturnStmt happy_var_2
        )
happyReduction_46 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_47 = happyReduce 6# 12# happyReduction_47
happyReduction_47 ((HappyAbsSyn6  happy_var_6) `HappyStk`
        (HappyAbsSyn27  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn20  happy_var_3) `HappyStk`
        (HappyTerminal (ID happy_var_2)) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (FunStmt happy_var_2 happy_var_3 happy_var_5 happy_var_6
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_48 = happyReduce 5# 12# happyReduction_48
happyReduction_48 ((HappyAbsSyn6  happy_var_5) `HappyStk`
        (HappyAbsSyn27  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn20  happy_var_2) `HappyStk`
        (HappyTerminal (ID happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (FunStmt happy_var_1 happy_var_2 happy_var_4 happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_49 = happySpecReduce_3  12# happyReduction_49
happyReduction_49 (HappyAbsSyn6  happy_var_3)
        (HappyTerminal (ID happy_var_2))
        _
         =  HappyAbsSyn16
                 (DataStmt happy_var_2 happy_var_3
        )
happyReduction_49 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_50 = happySpecReduce_3  12# happyReduction_50
happyReduction_50 (HappyAbsSyn6  happy_var_3)
        (HappyAbsSyn9  happy_var_2)
        _
         =  HappyAbsSyn16
                 (IterateStmt happy_var_2 happy_var_3
        )
happyReduction_50 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_51 = happySpecReduce_2  12# happyReduction_51
happyReduction_51 (HappyTerminal (ID happy_var_2))
        _
         =  HappyAbsSyn16
                 (ReadStmt happy_var_2
        )
happyReduction_51 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_52 = happyReduce 6# 12# happyReduction_52
happyReduction_52 ((HappyAbsSyn6  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (TYPEID happy_var_2)) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (NewStmt happy_var_2 happy_var_4 happy_var_6
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_53 = happySpecReduce_2  12# happyReduction_53
happyReduction_53 _
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn16
                 (AssignmentStmt happy_var_1
        )
happyReduction_53 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_54 = happySpecReduce_1  12# happyReduction_54
happyReduction_54 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn16
                 (happy_var_1
        )
happyReduction_54 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_55 = happyReduce 5# 12# happyReduction_55
happyReduction_55 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn4  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (ID happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (CallStmt happy_var_1 happy_var_3
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_56 = happySpecReduce_2  12# happyReduction_56
happyReduction_56 _
        _
         =  HappyAbsSyn16
                 (BreakStmt
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_57 = happySpecReduce_1  13# happyReduction_57
happyReduction_57 (HappyTerminal (TYPEID happy_var_1))
         =  HappyAbsSyn17
                 (happy_var_1
        )
happyReduction_57 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_58 = happySpecReduce_1  13# happyReduction_58
happyReduction_58 (HappyTerminal (ID happy_var_1))
         =  HappyAbsSyn17
                 (happy_var_1
        )
happyReduction_58 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_59 = happySpecReduce_1  14# happyReduction_59
happyReduction_59 _
         =  HappyAbsSyn18
                 (()
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_60 = happySpecReduce_0  14# happyReduction_60
happyReduction_60  =  HappyAbsSyn18
                 (()
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_61 = happySpecReduce_1  15# happyReduction_61
happyReduction_61 (HappyTerminal (ID happy_var_1))
         =  HappyAbsSyn19
                 ([happy_var_1]
        )
happyReduction_61 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_62 = happySpecReduce_3  15# happyReduction_62
happyReduction_62 (HappyTerminal (ID happy_var_3))
        _
        (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn19
                 (happy_var_1 ++ [happy_var_3]
        )
happyReduction_62 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_63 = happySpecReduce_0  15# happyReduction_63
happyReduction_63  =  HappyAbsSyn19
                 ([]
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_64 = happySpecReduce_0  16# happyReduction_64
happyReduction_64  =  HappyAbsSyn20
                 ([]
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_65 = happySpecReduce_1  16# happyReduction_65
happyReduction_65 (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn20
                 ([happy_var_1]
        )
happyReduction_65 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_66 = happySpecReduce_3  16# happyReduction_66
happyReduction_66 (HappyAbsSyn20  happy_var_3)
        _
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn20
                 (happy_var_1 : happy_var_3
        )
happyReduction_66 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_67 = happySpecReduce_1  17# happyReduction_67
happyReduction_67 (HappyTerminal (ID happy_var_1))
         =  HappyAbsSyn21
                 (( happy_var_1, "" )
        )
happyReduction_67 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_68 = happySpecReduce_3  17# happyReduction_68
happyReduction_68 (HappyAbsSyn26  happy_var_3)
        _
        (HappyTerminal (ID happy_var_1))
         =  HappyAbsSyn21
                 (( happy_var_1, happy_var_3 )
        )
happyReduction_68 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_69 = happyReduce 5# 18# happyReduction_69
happyReduction_69 ((HappyAbsSyn8  happy_var_5) `HappyStk`
        (HappyAbsSyn6  happy_var_4) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn22
                 (MainWithDecls happy_var_4 happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_70 = happyReduce 4# 18# happyReduction_70
happyReduction_70 ((HappyAbsSyn6  happy_var_4) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn22
                 (MainProgram happy_var_4
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_71 = happySpecReduce_1  18# happyReduction_71
happyReduction_71 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn22
                 (ProgramStmtList happy_var_1
        )
happyReduction_71 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_72 = happySpecReduce_2  19# happyReduction_72
happyReduction_72 (HappyAbsSyn27  happy_var_2)
        _
         =  HappyAbsSyn23
                 (happy_var_2
        )
happyReduction_72 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_73 = happySpecReduce_0  19# happyReduction_73
happyReduction_73  =  HappyAbsSyn23
                 ([]
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_74 = happySpecReduce_1  20# happyReduction_74
happyReduction_74 (HappyAbsSyn29  happy_var_1)
         =  HappyAbsSyn24
                 (happy_var_1
        )
happyReduction_74 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_75 = happySpecReduce_1  20# happyReduction_75
happyReduction_75 (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn24
                 (happy_var_1
        )
happyReduction_75 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_76 = happySpecReduce_1  20# happyReduction_76
happyReduction_76 (HappyAbsSyn28  happy_var_1)
         =  HappyAbsSyn24
                 (happy_var_1
        )
happyReduction_76 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_77 = happySpecReduce_1  21# happyReduction_77
happyReduction_77 (HappyAbsSyn24  happy_var_1)
         =  HappyAbsSyn25
                 ([happy_var_1]
        )
happyReduction_77 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_78 = happySpecReduce_2  21# happyReduction_78
happyReduction_78 (HappyAbsSyn24  happy_var_2)
        (HappyAbsSyn25  happy_var_1)
         =  HappyAbsSyn25
                 (happy_var_1 ++ [happy_var_2]
        )
happyReduction_78 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_79 = happySpecReduce_1  22# happyReduction_79
happyReduction_79 (HappyTerminal (TYPEID happy_var_1))
         =  HappyAbsSyn26
                 (happy_var_1
        )
happyReduction_79 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_80 = happySpecReduce_1  22# happyReduction_80
happyReduction_80 (HappyTerminal (ID happy_var_1))
         =  HappyAbsSyn26
                 (happy_var_1
        )
happyReduction_80 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_81 = happySpecReduce_3  22# happyReduction_81
happyReduction_81 _
        _
        (HappyTerminal (TYPEID happy_var_1))
         =  HappyAbsSyn26
                 (happy_var_1 ++ "[]"
        )
happyReduction_81 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_82 = happySpecReduce_3  22# happyReduction_82
happyReduction_82 _
        _
        (HappyTerminal (ID happy_var_1))
         =  HappyAbsSyn26
                 (happy_var_1 ++ "[]"
        )
happyReduction_82 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_83 = happySpecReduce_1  23# happyReduction_83
happyReduction_83 (HappyAbsSyn26  happy_var_1)
         =  HappyAbsSyn27
                 ([happy_var_1]
        )
happyReduction_83 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_84 = happySpecReduce_3  23# happyReduction_84
happyReduction_84 (HappyAbsSyn26  happy_var_3)
        _
        (HappyAbsSyn27  happy_var_1)
         =  HappyAbsSyn27
                 (happy_var_1 ++ [happy_var_3]
        )
happyReduction_84 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_85 = happySpecReduce_3  24# happyReduction_85
happyReduction_85 (HappyAbsSyn24  happy_var_3)
        (HappyAbsSyn13  happy_var_2)
        _
         =  HappyAbsSyn28
                 (IfStmt happy_var_2 happy_var_3 (BlockStmt [])
        )
happyReduction_85 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_86 = happyReduce 5# 24# happyReduction_86
happyReduction_86 ((HappyAbsSyn28  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn16  happy_var_3) `HappyStk`
        (HappyAbsSyn13  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn28
                 (IfStmt happy_var_2 happy_var_3 happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_87 = happyReduce 5# 25# happyReduction_87
happyReduction_87 ((HappyAbsSyn6  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn9  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn29
                 (WhileStmt happy_var_3 happy_var_5
        ) `HappyStk` happyRest

happyNewToken action sts stk [] =
        happyDoAction 40# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
        let cont i = happyDoAction i tk action sts stk tks in
        case tk of {
        ASSIGN -> cont 1#;
        BREAK -> cont 2#;
        BOOL happy_dollar_dollar -> cont 3#;
        CHAR happy_dollar_dollar -> cont 4#;
        DATA -> cont 5#;
        PUNC "." -> cont 6#;
        ELSE -> cont 7#;
        FLOAT happy_dollar_dollar -> cont 8#;
        FUN -> cont 9#;
        ID happy_dollar_dollar -> cont 10#;
        IF -> cont 11#;
        INT happy_dollar_dollar -> cont 12#;
        ITERATE -> cont 13#;
        MAIN -> cont 14#;
        NEW -> cont 15#;
        NULL -> cont 16#;
        OP ">=" -> cont 17#;
        OP "<=" -> cont 18#;
        OP "<" -> cont 19#;
        OP ">" -> cont 20#;
        OP "+" -> cont 21#;
        OP "-" -> cont 22#;
        OP "&&" -> cont 23#;
        OP happy_dollar_dollar -> cont 24#;
        PUNC ":" -> cont 25#;
        PUNC "," -> cont 26#;
        PUNC "::" -> cont 27#;
        PUNC "{" -> cont 28#;
        PUNC "[" -> cont 29#;
        PUNC "(" -> cont 30#;
        PUNC "}" -> cont 31#;
        PUNC "]" -> cont 32#;
        PUNC ")" -> cont 33#;
        PUNC ";" -> cont 34#;
        PRINT -> cont 35#;
        READ -> cont 36#;
        RETURN -> cont 37#;
        TYPEID happy_dollar_dollar -> cont 38#;
        WHILE -> cont 39#;
        _ -> happyError' ((tk:tks), [])
        }

happyError_ explist 40# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- Definição da Árvore de Sintaxe Abstrata (AST)
-- Construtores organizados em ordem alfabética.
data AST
    = ArrayLiteral [AST]                              -- Lista literal de elementos
    | AssignmentStmt AST                              -- Instrução de atribuição
    | BlockStmt [AST]                                 -- Bloco de instruções
    | BoolExpr Bool                                   -- Expressão booleana
    | BreakStmt                                       -- Instrução break
    | CallExpr String [AST]                           -- Chamada de função (expressão)
    | CallStmt String [AST]                           -- Chamada de função (instrução)
    | CharExpr Char                                   -- Expressão de caractere
    | DataStmt String AST                             -- Declaração de dado
    | FloatExpr Float                                 -- Expressão de ponto flutuante
    | FieldAccessExpr AST String                      -- Acesso a campo de registro
    | FunStmt String [(String, String)] [String] AST   -- Declaração de função
    | IfStmt AST AST AST                              -- Instrução if (condição, then, else)
    | IndexExpr AST AST                               -- Expressão de indexação
    | IntExpr Int                                     -- Expressão inteira
    | IterateStmt AST AST                             -- Instrução iterate
    | LValueAssignment AST AST                        -- Atribuição via lvalue
    | MainProgram AST                                 -- Programa principal sem declarações
    | MainWithDecls AST [AST]                         -- Programa principal com declarações
    | MultipleAssignment String String                -- Atribuição múltipla
    | NewExpr String                                  -- Criação de objeto (sem argumentos)
    | NewExprArgs String [AST]                        -- Criação de objeto (com argumentos)
    | NewStmt String [String] AST                     -- Declaração "new" (tipo, parâmetros, bloco)
    | NullExpr                                        -- Expressão null
    | OpExpr String AST AST                           -- Expressão com operador binário
    | PrintStmt AST                                   -- Instrução de impressão
    | ProgramStmtList [AST]                           -- Lista de instruções do programa
    | ReadStmt String                                 -- Instrução de leitura
    | RecordExpr [(String, AST)]                      -- Expressão de registro
    | ReturnStmt [AST]                                -- Instrução return
    | SingleAssignment String AST                     -- Atribuição simples com identificador
    | SliceExpr AST AST AST                           -- Expressão de slicing (sub-array)
    | VarExpr String                                  -- Expressão variável
    | WhileStmt AST AST                               -- Instrução while
    deriving (Show)

-- Função de erro para análise sintática
parseError :: [Token] -> a
parseError tokens = error $ "Erro de análise sintática. Tokens restantes: " ++ show tokens

-- Função para gerar uma representação "bonita" da AST
prettyPrintAST :: AST -> String
prettyPrintAST ast = go 0 ast
  where
    indent n = replicate (n * 4) ' '
    
    go n (ArrayLiteral elems) =
      indent n ++ "└─ ArrayLiteral\n" ++ concatMap (go (n+1)) elems
    go n (AssignmentStmt asgn) =
      indent n ++ "└─ AssignmentStmt\n" ++ go (n+1) asgn
    go n (BlockStmt stmts) =
      indent n ++ "└─ BlockStmt\n" ++ concatMap (go (n+1)) stmts
    go n (BoolExpr b) =
      indent n ++ "└─ BoolExpr [BOOL: " ++ show b ++ "]\n"
    go n BreakStmt =
      indent n ++ "└─ BreakStmt\n"
    go n (CallExpr name args) =
      indent n ++ "└─ CallExpr\n" ++
      indent (n+1) ++ "├─ Name: [ID: " ++ name ++ "]\n" ++
      indent (n+1) ++ "└─ Args:\n" ++ concatMap (go (n+2)) args
    go n (CallStmt name args) =
      indent n ++ "└─ CallStmt\n" ++
      indent (n+1) ++ "├─ Name: [ID: " ++ name ++ "]\n" ++
      indent (n+1) ++ "└─ Args:\n" ++ concatMap (go (n+2)) args
    go n (CharExpr c) =
      indent n ++ "└─ CharExpr [CHAR: " ++ show c ++ "]\n"
    go n (DataStmt name body) =
      indent n ++ "└─ DataStmt [ID: " ++ name ++ "]\n" ++ go (n+1) body
    go n (FloatExpr f) =
      indent n ++ "└─ FloatExpr [FLOAT: " ++ show f ++ "]\n"
    go n (FieldAccessExpr expr field) =
      indent n ++ "└─ FieldAccessExpr\n" ++
      indent (n+1) ++ "├─ Expression:\n" ++ go (n+2) expr ++
      indent (n+1) ++ "└─ Field: [ID: " ++ field ++ "]\n"
    go n (FunStmt name params types body) =
      indent n ++ "└─ FunStmt\n" ++
      indent (n+1) ++ "├─ Name: [ID: " ++ name ++ "]\n" ++
      indent (n+1) ++ "├─ Params: " ++ showParams params ++ "\n" ++
      indent (n+1) ++ "└─ ReturnTypes: " ++ show types ++ "\n" ++
      go (n+1) body
    go n (IfStmt cond thenStmt elseStmt) =
      indent n ++ "└─ IfStmt\n" ++
      indent (n+1) ++ "├─ Condition:\n" ++ go (n+2) cond ++
      indent (n+1) ++ "├─ Then:\n" ++ go (n+2) thenStmt ++
      indent (n+1) ++ "└─ Else:\n" ++ go (n+2) elseStmt
    go n (IndexExpr expr idx) =
      indent n ++ "└─ IndexExpr\n" ++
      indent (n+1) ++ "├─ Array:\n" ++ go (n+2) expr ++
      indent (n+1) ++ "└─ Index:\n" ++ go (n+2) idx
    go n (IntExpr i) =
      indent n ++ "└─ IntExpr [INT: " ++ show i ++ "]\n"
    go n (IterateStmt expr body) =
      indent n ++ "└─ IterateStmt\n" ++ go (n+1) expr ++ go (n+1) body
    go n (LValueAssignment lval expr) =
      indent n ++ "└─ LValueAssignment\n" ++
      indent (n+1) ++ "├─ LValue:\n" ++ go (n+2) lval ++
      indent (n+1) ++ "└─ Expr:\n" ++ go (n+2) expr
    go n (MainProgram a) =
      indent n ++ "└─ MainProgram\n" ++ go (n+1) a
    go n (MainWithDecls a decls) =
      indent n ++ "└─ MainWithDecls\n" ++ 
      indent (n+1) ++ "├─ Main:\n" ++ go (n+2) a ++
      indent (n+1) ++ "└─ Declarações:\n" ++ concatMap (go (n+2)) decls
    go n (MultipleAssignment n1 n2) =
      indent n ++ "└─ MultipleAssignment\n" ++
      indent (n+1) ++ "└─ Names: [ID: " ++ n1 ++ "], [ID: " ++ n2 ++ "]\n"
    go n (NewExpr typeName) =
      indent n ++ "└─ NewExpr [TYPEID: " ++ typeName ++ "]\n"
    go n (NewExprArgs typeName args) =
      indent n ++ "└─ NewExprArgs [TYPEID: " ++ typeName ++ "]\n" ++
      indent (n+1) ++ "└─ Args:\n" ++ concatMap (go (n+2)) args
    go n (NewStmt typeName params body) =
      indent n ++ "└─ NewStmt\n" ++
      indent (n+1) ++ "├─ Type: [TYPEID: " ++ typeName ++ "]\n" ++
      indent (n+1) ++ "└─ Params: " ++ show params ++ "\n" ++
      go (n+1) body
    go n NullExpr =
      indent n ++ "└─ NullExpr [NULL]\n"
    go n (OpExpr op left right) =
      indent n ++ "└─ OpExpr [OP: " ++ op ++ "]\n" ++
      indent (n+1) ++ "├─ Left:\n" ++ go (n+2) left ++
      indent (n+1) ++ "└─ Right:\n" ++ go (n+2) right
    go n (PrintStmt expr) =
      indent n ++ "└─ PrintStmt\n" ++ go (n+1) expr
    go n (ProgramStmtList stmts) =
      indent n ++ "└─ ProgramStmtList\n" ++ concatMap (go (n+1)) stmts
    go n (ReadStmt name) =
      indent n ++ "└─ ReadStmt [ID: " ++ name ++ "]\n"
    go n (RecordExpr fields) =
      indent n ++ "└─ RecordExpr\n" ++
      concatMap (\(name, expr) -> indent (n+1) ++ "├─ " ++ name ++ ": \n" ++ go (n+2) expr) fields
    go n (ReturnStmt exprs) =
      indent n ++ "└─ ReturnStmt\n" ++ concatMap (go (n+1)) exprs
    go n (SingleAssignment name expr) =
      indent n ++ "└─ SingleAssignment\n" ++
      indent (n+1) ++ "├─ Name: [ID: " ++ name ++ "]\n" ++
      indent (n+1) ++ "└─ Expr:\n" ++ go (n+2) expr
    go n (SliceExpr arr start end) =
      indent n ++ "└─ SliceExpr\n" ++
      indent (n+1) ++ "├─ Array:\n" ++ go (n+2) arr ++
      indent (n+1) ++ "├─ Start:\n" ++ go (n+2) start ++
      indent (n+1) ++ "└─ End:\n" ++ go (n+2) end
    go n (VarExpr s) =
      indent n ++ "└─ VarExpr [ID: " ++ s ++ "]\n"
    go n (WhileStmt cond stmt) =
      indent n ++ "└─ WhileStmt\n" ++
      indent (n+1) ++ "├─ Condition:\n" ++ go (n+2) cond ++
      indent (n+1) ++ "└─ Body:\n" ++ go (n+2) stmt

    showParams [] = "[]"
    showParams ps = concatMap (\(n,t) -> "[ID: " ++ n ++ " :: " ++ t ++ "] ") ps
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#  define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#  define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#  define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#  define LT(n,m) (n Happy_GHC_Exts.<# m)
#  define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#  define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define ERROR_TOK 0#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

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
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " ++ show (Happy_GHC_Exts.I# st) ++
              ",\ttoken: " ++ show (Happy_GHC_Exts.I# i) ++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail (happyExpListPerState (Happy_GHC_Exts.I# st)) i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " ++ show (Happy_GHC_Exts.I# rule) ++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Just (Happy_GHC_Exts.I# act) -> act
  Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  = Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | otherwise
  = Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | otherwise         = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

{-# INLINE happyLt #-}
happyLt x y = LT(x,y)

readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (happyIndexOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 5#))) (bit `Prelude.mod` 32)
  where unbox_int (Happy_GHC_Exts.I# x) = x

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
-- trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st sts stk
     = happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk)
                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyAdjustOffset (happyIndexOffAddr happyGotoOffsets st1)
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            happyThen1 (fn stk tk)
                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist ERROR_TOK tk old_st _ stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--      trace "failing" $
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st (HappyCons action sts)
                               (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction ERROR_TOK tk action sts (saved_tok`HappyStk`stk)
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk action sts stk =
-- trace "entering error recovery" $
        happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
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
