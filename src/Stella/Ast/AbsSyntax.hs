-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Syntax.

module Stella.Ast.AbsSyntax where

import Prelude (Integer)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

import qualified Data.Text
import qualified Data.Data    as C (Data, Typeable)
import qualified GHC.Generics as C (Generic)

data Program = AProgram LanguageDecl [Extension] [Decl]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data LanguageDecl = LanguageCore
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Extension = AnExtension [ExtensionName]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Decl
    = DeclFun [Annotation] StellaIdent [ParamDecl] ReturnType ThrowType [Decl] Expr
    | DeclFunGeneric [Annotation] StellaIdent [StellaIdent] [ParamDecl] ReturnType ThrowType [Decl] Expr
    | DeclTypeAlias StellaIdent Type
    | DeclExceptionType Type
    | DeclExceptionVariant StellaIdent Type
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data LocalDecl = ALocalDecl Decl
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Annotation = InlineAnnotation
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ParamDecl = AParamDecl StellaIdent Type
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ReturnType = NoReturnType | SomeReturnType Type
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ThrowType = NoThrowType | SomeThrowType [Type]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Type
    = TypeFun [Type] Type
    | TypeForAll [StellaIdent] Type
    | TypeRec StellaIdent Type
    | TypeSum Type Type
    | TypeTuple [Type]
    | TypeRecord [RecordFieldType]
    | TypeVariant [VariantFieldType]
    | TypeList Type
    | TypeBool
    | TypeNat
    | TypeUnit
    | TypeTop
    | TypeBottom
    | TypeRef Type
    | TypeVar StellaIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data MatchCase = AMatchCase Pattern Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data OptionalTyping = NoTyping | SomeTyping Type
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data PatternData = NoPatternData | SomePatternData Pattern
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ExprData = NoExprData | SomeExprData Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Pattern
    = PatternVariant StellaIdent PatternData
    | PatternInl Pattern
    | PatternInr Pattern
    | PatternTuple [Pattern]
    | PatternRecord [LabelledPattern]
    | PatternList [Pattern]
    | PatternCons Pattern Pattern
    | PatternFalse
    | PatternTrue
    | PatternUnit
    | PatternInt Integer
    | PatternSucc Pattern
    | PatternVar StellaIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data LabelledPattern = ALabelledPattern StellaIdent Pattern
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Binding = ABinding StellaIdent Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Expr
    = Sequence Expr Expr
    | Assign Expr Expr
    | If Expr Expr Expr
    | Let [PatternBinding] Expr
    | LetRec [PatternBinding] Expr
    | TypeAbstraction [StellaIdent] Expr
    | LessThan Expr Expr
    | LessThanOrEqual Expr Expr
    | GreaterThan Expr Expr
    | GreaterThanOrEqual Expr Expr
    | Equal Expr Expr
    | NotEqual Expr Expr
    | TypeAsc Expr Type
    | TypeCast Expr Type
    | Abstraction [ParamDecl] Expr
    | Variant StellaIdent ExprData
    | Match Expr [MatchCase]
    | List [Expr]
    | Add Expr Expr
    | Subtract Expr Expr
    | LogicOr Expr Expr
    | Multiply Expr Expr
    | Divide Expr Expr
    | LogicAnd Expr Expr
    | Ref Expr
    | Deref Expr
    | Application Expr [Expr]
    | TypeApplication Expr [Type]
    | DotRecord Expr StellaIdent
    | DotTuple Expr Integer
    | Tuple [Expr]
    | Record [Binding]
    | ConsList Expr Expr
    | Head Expr
    | IsEmpty Expr
    | Tail Expr
    | Panic
    | Throw Expr
    | TryCatch Expr Pattern Expr
    | TryWith Expr Expr
    | Inl Expr
    | Inr Expr
    | Succ Expr
    | LogicNot Expr
    | Pred Expr
    | IsZero Expr
    | Fix Expr
    | NatRec Expr Expr Expr
    | Fold Type Expr
    | Unfold Type Expr
    | ConstTrue
    | ConstFalse
    | ConstUnit
    | ConstInt Integer
    | ConstMemory MemoryAddress
    | Var StellaIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data PatternBinding = APatternBinding Pattern Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data VariantFieldType
    = AVariantFieldType StellaIdent OptionalTyping
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data RecordFieldType = ARecordFieldType StellaIdent Type
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Typing = ATyping Expr Type
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

newtype StellaIdent = StellaIdent Data.Text.Text
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

newtype ExtensionName = ExtensionName Data.Text.Text
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

newtype MemoryAddress = MemoryAddress Data.Text.Text
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

