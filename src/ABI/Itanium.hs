{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module ABI.Itanium (
  DecodedName(..),
  CVQualifier(..),
  CXXType(..),
  Name(..),
  Prefix(..),
  UnqualifiedName(..),
  CtorDtor(..),
  Operator(..),
  Expression(..),
  CallOffset(..),
  Substitution(..),
  demangleName,
  mangleName,
  cxxNameToString,
  cxxNameToText
  ) where

import Prelude hiding ( (.) )
import Control.Category ( (.) )
import Text.Boomerang
import Text.Boomerang.String
import Text.Boomerang.TH

import ABI.Itanium.Pretty
import ABI.Itanium.Types

$(derivePrinterParsers ''DecodedName)
$(derivePrinterParsers ''Name)
$(derivePrinterParsers ''CVQualifier)
$(derivePrinterParsers ''CXXType)
$(derivePrinterParsers ''Operator)
$(derivePrinterParsers ''CtorDtor)
$(derivePrinterParsers ''UnqualifiedName)
$(derivePrinterParsers ''Prefix)
$(derivePrinterParsers ''CallOffset)
$(derivePrinterParsers ''Substitution)

demangleName :: String -> Either String DecodedName
demangleName s =
  case parseString itaniumName s of
    Right n -> Right n
    Left e -> Left (show e)

mangleName :: DecodedName -> Maybe String
mangleName = unparseString itaniumName

itaniumName :: StringPrinterParser () (DecodedName :- ())
itaniumName = lit "_Z" . topLevelEntity

topLevelEntity :: PrinterParser StringError String a (DecodedName :- a)
topLevelEntity =
  ( rVirtualTable . lit "TV" . cxxType <>
    rVTTStructure . lit "TT" . cxxType <>
    rTypeInfo . lit "TI" . cxxType <>
    rTypeInfoName . lit "TS" . cxxType <>
    rGuardVariable . lit "GV" . name <>
    rOverrideThunk . lit "T" . callOffset . topLevelEntity <>
    rOverrideThunkCovariant . lit "Tc" . callOffset . callOffset . topLevelEntity <>
    rFunction . name . bareFunctionType <>
    rData . name
  )


-- | Equivalent to the <type> production in the Itanium ABI spec
--
-- FIXME: Add support for unnamed types and decltypes
--
-- <unnamed-type-name> ::= Ut [ <nonnegative number> ] _
-- <decltype>  ::= Dt <expression> E  # decltype of an id-expression or class member access (C++0x)
--             ::= DT <expression> E  # decltype of an expression (C++0x)
cxxType :: PrinterParser StringError String a (CXXType :- a)
cxxType = ( rQualifiedType . rList1 cvQualifier . cxxType <>
            rPointerToType . lit "P" . cxxType <>
            rReferenceToType . lit "R" . cxxType <>
            rRValueReferenceToType . lit "O" . cxxType <>
            rComplexPairType . lit "C" . cxxType <> -- C99
            rImaginaryType . lit "G" . cxxType <> -- C99
            rParameterPack . lit "Dp" . cxxType  <> -- C++11 parameter pack
            rVendorTypeQualifier . lit "U" . sourceName . cxxType <>
            rVoidType . lit "v" <>
            rWchar_tType . lit "w" <>
            rBoolType . lit "b" <>
            rCharType . lit "c" <>
            rSignedCharType . lit "a" <>
            rUnsignedCharType . lit "h" <>
            rShortType . lit "s" <>
            rUnsignedShortType . lit "t" <>
            rIntType . lit "i" <>
            rUnsignedIntType . lit "j" <>
            rLongType . lit "k" <>
            rUnsignedLongType . lit "m" <>
            rLongLongType . lit "x" <>
            rUnsignedLongLongType . lit "y" <>
            rInt128Type . lit "n" <>
            rUnsignedInt128Type . lit "o" <>
            rFloatType . lit "f" <>
            rDoubleType . lit "d" <>
            rLongDoubleType . lit "e" <>
            rFloat128Type . lit "g" <>
            rEllipsisType . lit "z" <>
            rChar32Type . lit "Di" <>
            rChar16Type . lit "Ds" <>
            rAutoType . lit "Da" <>
            rNullPtrType . lit "Dn" <>
            rVendorBuiltinType . lit "u" . sourceName <>
            rExternCFunctionType . lit "FY" . bareFunctionType . lit "E" <>
            rFunctionType . lit "F" . bareFunctionType . lit "E" <>
            rArrayTypeN . lit "A" . rMaybe int . lit "_" . cxxType <>
            rPtrToMemberType . lit "M" . cxxType . cxxType <>
            rSubstitutionType . substitution <>
            rClassEnumType . name
            -- Still need: array-type (E), decltype, substitution
          )

bareFunctionType :: PrinterParser StringError String a ([CXXType] :- a)
bareFunctionType = rList1 cxxType

callOffset :: PrinterParser StringError String a (CallOffset :- a)
callOffset = ( rVirtualOffset . lit "v" . abiInt . lit "_" . abiInt . lit "_" <>
               rNonVirtualOffset . lit "h" . abiInt . lit "_"
             )

cvQualifier :: PrinterParser StringError String a (CVQualifier :- a)
cvQualifier = ( rRestrict . lit "r" <>
                rVolatile . lit "V" <>
                rConst . lit "K"
              )

operator :: PrinterParser StringError String a (Operator :- a)
operator = ( rOpNew . lit "nw" <>
             rOpNewArray . lit "na" <>
             rOpDelete . lit "dl" <>
             rOpDeleteArray . lit "da" <>
             rOpUPlus . lit "ps" <>
             rOpUMinus . lit "ng" <>
             rOpAddressOf . lit "ad" <>
             rOpDeref . lit "de" <>
             rOpBitNot . lit "co" <>
             rOpPlus . lit "pl" <>
             rOpMinus . lit "mi" <>
             rOpMul . lit "ml" <>
             rOpDiv . lit "dv" <>
             rOpMod . lit "rm" <>
             rOpBitAnd . lit "an" <>
             rOpBitOr . lit "or" <>
             rOpBitXor . lit "eo" <>
             rOpAssign . lit "aS" <>
             rOpPlusAssign . lit "pL" <>
             rOpMinusAssign . lit "mI" <>
             rOpMulAssign . lit "mL" <>
             rOpDivAssign . lit "dV" <>
             rOpModAssign . lit "rM" <>
             rOpAndAssign . lit "aN" <>
             rOpOrAssign . lit "oR" <>
             rOpXorAssign . lit "eO" <>
             rOpShl . lit "ls" <>
             rOpShr . lit "rs" <>
             rOpShlAssign . lit "lS" <>
             rOpShrAssign . lit "rS" <>
             rOpEquals . lit "eq" <>
             rOpNotEquals . lit "ne" <>
             rOpLt . lit "lt" <>
             rOpGt . lit "gt" <>
             rOpLte . lit "le" <>
             rOpGte . lit "ge" <>
             rOpNot . lit "nt" <>
             rOpAnd . lit "aa" <>
             rOpOr . lit "oo" <>
             rOpPlusPlus . lit "pp" <>
             rOpMinusMinus . lit "mm" <>
             rOpComma . lit "cm" <>
             rOpArrowStar . lit "pm" <>
             rOpArrow . lit "pt" <>
             rOpCall . lit "cl" <>
             rOpIndex . lit "ix" <>
             rOpQuestion . lit "qu" <>
             rOpSizeofType . lit "st" <>
             rOpSizeofExpr . lit "sz" <>
             rOpAlignofType . lit "at" <>
             rOpAlignofExpr . lit "az" <>
             rOpCast . lit "cv" . cxxType <>
             rOpVendor . lit "v" . abiInt . sourceName
           )

ctorDtor :: PrinterParser StringError String a (CtorDtor :- a)
ctorDtor = ( rC1 . lit "C1" <>
             rC2 . lit "C2" <>
             rC3 . lit "C3" <>
             rD0 . lit "D0" <>
             rD1 . lit "D1" <>
             rD2 . lit "D2"
           )

unqualifiedName :: PrinterParser StringError String a (UnqualifiedName :- a)
unqualifiedName = ( rOperatorName . operator <>
                    rCtorDtorName . ctorDtor <>
                    rSourceName . sourceName
                  )

prefix :: PrinterParser StringError String a (Prefix :- a)
prefix = ( rUnqualifiedPrefix . unqualifiedName <>
           rDataMemberPrefix . sourceName . lit "M" <>
           rSubstitutionPrefix . substitution
         )

name :: PrinterParser StringError String a (Name :- a)
name = ( rNestedName . lit "N" . rList cvQualifier . rList prefix . unqualifiedName . lit "E" <>
         -- Standard qualified names do not require the N..E
         -- delimiters, so this case is *not* redundant
         rUnscopedStdName . lit "St" . unqualifiedName <>
         rUnscopedName . unqualifiedName
       )

substitution :: PrinterParser StringError String a (Substitution :- a)
substitution = ( rSubstitution . lit "S" . rMaybe (rList1 (satisfy (/='_'))) . lit "_" <>
                 rSubStdNamespace . lit "St" <>
                 rSubStdAllocator . lit "Sa" <>
                 rSubBasicString . lit "Sb" <>
                 rSubBasicStringArgs . lit "Ss" <>
                 rSubBasicIstream . lit "Si" <>
                 rSubBasicOstream . lit "So" <>
                 rSubBasicIostream . lit "Sd"
               )

-- | Parse a length-prefixed string (does not handle newlines)
sourceName :: PrinterParser (ParserError MajorMinorPos) String a (String :- a)
sourceName = val pf sf
  where
    pf = Parser $ \tok pos ->
      case tok of
        [] -> mkParserError pos [EOI "input", Expect "number"]
        _ ->
          case parseInt tok of
            Nothing -> mkParserError pos [Expect "length-prefixed string"]
            Just (len, rest1) ->
              let (s, rest2) = splitAt len rest1
                  pos' = incMinor (length (show len) + length s) pos
              in case length s == len of
                True -> [Right ((s, rest2), pos')]
                False -> mkParserError pos [EOI "input", Expect "length-prefixed string"]
    sf b = [ (\string -> concat [ show (length b), b, string ]) ]

parseInt :: String -> Maybe (Int, String)
parseInt s =
  case reads s of
    [(i, rest)] -> Just (i, rest)
    _ -> Nothing

-- | In the Itanium ABI, negative numbers are prefixed by 'n' instead
-- of a negative sign.  This is an alternate parser to be used instead
-- of the 'int' parser that comes with boomerang.
abiInt :: PrinterParser (ParserError MajorMinorPos) String a (Int :- a)
abiInt = val pf sf
  where
    pf = Parser $ \tok pos ->
      case tok of
        [] -> mkParserError pos [EOI "input", Expect "abi number"]
        'n' : rest1 ->
          case parseInt rest1 of
            Nothing -> mkParserError pos [Expect "abi number"]
            Just (num, rest2) ->
              let pos' = incMinor (length (show num) + 1) pos
              in [Right ((negate num, rest2), pos')]
        _ ->
          case parseInt tok of
            Nothing -> mkParserError pos [Expect "abi number"]
            Just (num, rest2) ->
              let pos' = incMinor (length (show num)) pos
              in [Right ((num, rest2), pos')]
    sf b | b >= 0 = [ (\string -> concat [ show b, string ]) ]
         | otherwise = [ (\string -> concat [ "n", show b, string ]) ]
