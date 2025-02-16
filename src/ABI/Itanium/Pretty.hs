{-# LANGUAGE LambdaCase #-}

module ABI.Itanium.Pretty (
  cxxNameToString,
  cxxNameToText,
  -- * Exceptions thrown
  MissingSubstitution,
  CtorDtorFallthru,
  UnqualCtorDtor,
  NonPointerFunctionType,
  BarePtrToMember,
  EmptyFunctionType
  ) where

import Control.Monad ( foldM, unless, void )
import Control.Monad.Catch ( Exception, MonadThrow, throwM )
import Control.Monad.Trans.State.Strict
import Data.Char ( ord )
import Data.List ( intersperse )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Maybe ( catMaybes )
import Data.Text.Lazy ( Text, unpack, unsnoc )
import Data.Text.Lazy.Builder

import ABI.Itanium.Types


-- | The Store maintains the substitution table used for emitting a
-- demangled names.  For compression, demangling allows the use of
-- "substitutions" to refer to previously emitted portions of the
-- name.  Most portions of the name that are emitted are recorded
-- (with compositional buildup: @foo::bar@ will record both @foo@ and
-- @foo::bar@).  The only exceptions to recording are function names
-- and builtin types.  As a name is demangled, each portion will be
-- added to this table for use in subsequent substitutions.

data Store = Store { substitutions :: HashMap Int Builder
                   , templateArgs :: [Maybe Builder]
                   }

emptyStore :: Store
emptyStore = Store mempty mempty

-- | The Pretty type is used as the main State object for performing
-- conversion to pretty output with the Store as the internal state.

type Pretty = StateT Store

----------------------------------------------------------------------

-- | Records a substitution component in the current table, skipping
-- any duplications.
--
-- For convenience, it returns the same item it recorded to allow this
-- to be the last statement for a Pretty Builder operation that should
-- record and return the generated output.
recordSubstitution :: Monad m => Builder -> Pretty m Builder
recordSubstitution b = do
  store <- get
  let s = substitutions store
  case b `elem` HM.elems s of
    False -> do
      let n = HM.size s
      let store' = store { substitutions = HM.insert n b s }
      put $! store'
      return b
    True -> return b

-- | Called for special cases where a substitution should be recorded,
-- even if it is a duplicate of another already-recorded substitution.
recordSubstitutionAlways :: Monad m => Builder -> Pretty m Builder
recordSubstitutionAlways b = do
  store <- get
  let s = substitutions store
  let n = HM.size s
  let store' = store { substitutions = HM.insert n b s }
  put $! store'
  return b

-- | This is a convenience wrapper for 'recordSubstitution' that will
-- return a void value; this can be used where the results of the
-- recording are not needed and the compiler would warn about an
-- unused return value.
recordSubstitution' :: Monad m => Builder -> Pretty m ()
recordSubstitution' = void . recordSubstitution

-- | Function names are not recorded, but their prefixes are.  For example:
--
--    Input                          | Records
--    -------------------------------|--------------------------------
--    @foo(int)@                     | (nothing)
--                                   |
--    @bar::foo(int)@                | @bar@
--                                   |
--    @bar::cow<moo>::boo::foo(int)@ | @bar@ @bar::cow@ @bar::cow<moo>@
--                                   | @bar::cow<moo>::boo@
--
-- To support this, the various parts of the function call are
-- recorded as normal (using recursive, shared pretty printing) and
-- then this function is called to drop the last element recorded,
-- which is the actual function name.
dropLastSubstitution :: Monad m => Pretty m ()
dropLastSubstitution = modify $ \store ->
  let s = substitutions store
      s' = HM.delete ((HM.size s) - 1) s
  in store { substitutions = s' }

-- | Lookup a recorded substitution and return it.  A lookup failure
-- means either a malformed mangled name (unlikely) or a logic error
-- below that did not properly record a substitution component.
getSubstitution :: (Monad m, MonadThrow m) => Maybe String -> Pretty m Builder
getSubstitution s = do
  st <- gets substitutions
  case s of
    Nothing -> lookupError 0 st
    -- This case always adds 1 from the number because the
    -- Nothing case is index zero
    Just ix ->
      -- seq ID is base 36
      case numberValue 36 ix of
        Just n -> lookupError (n+1) st
        Nothing -> errMsg
  where
    errMsg = throwM $ MissingSubstitution s
    lookupError k m = maybe errMsg return (HM.lookup k m)

-- | The MissingSubstitution exception is thrown when the mangled name
-- requests a substitution that cannot be found.  This indicates
-- either an invalid mangled name or else an internal logic error in
-- this Pretty implementation.

data MissingSubstitution = MissingSubstitution (Maybe String)
instance Exception MissingSubstitution
instance Show MissingSubstitution where
  show (MissingSubstitution s) = "No substitution found for " ++ show s



----------------------------------------------------------------------
-- * Template Argument handling
--
-- Template arguments are different than substitutions in that they
-- are numbered when the template opening character '<' is
-- seen/emitted, but the actual argument itself cannot be stored until
-- the closing character '>' is reached.  This means that if the
-- template argument itself contains template arguments, the arguments
-- processed during the recursion must follow the current argument.
--
--  Example:
--
--   foo<std::basic_string<char, std::char_traits<char>, std::allocator<char> > >
--       ^                 ^     ^                ^                     ^
--       |                 |     |                `-dup of T1, ignored--'
--       |                 T1    T2--------------------  T3------------------
--       T0--------------------------------------------------------------------
--
-- Further sophistication is needed to handle the case where
-- duplicates are observed.  For example, after T1 above, the '<'
-- following the char_traits reserves another template argument
-- position, but then when the '>' is reached, it can determine that
-- it's a duplicate and doesn't need to be recorded.  This is fine if
-- there were no other args recorded after the '<' and before the '>',
-- but consider the case where T0 might be discovered to be a
-- duplicate.  To resolve this, the template argument reservation adds
-- a Nothing to the reservation array, but lookups by index ignore
-- Nothing entries.

newtype ReservedTemplateArgument = RTA Int

-- | Reserves a template argument location
reserveTemplateArgument :: Monad m => Pretty m ReservedTemplateArgument
reserveTemplateArgument = do
  store <- get
  let tas = templateArgs store
      nta = length tas
  put $! store { templateArgs = templateArgs store <> [ Nothing ] }
  return $! RTA nta


-- | Records a template argument in the current table, skipping any
-- duplications.  Returns the input as a convenience pass-through.
recordTemplateArgument :: Monad m
                       => ReservedTemplateArgument -> Builder -> Pretty m Builder
recordTemplateArgument (RTA i) b = do
  store <- get
  let tas = templateArgs store
  unless (i < length tas) $
    error "INVALID TEMPLATE ARG RESERVATION: CODING ERROR"
  let (pre,_:post) = splitAt i tas
  if Just b `elem` pre
    then return $! b
    else do let store' = store { templateArgs = pre <> [ Just b ] <> post }
            put $! store'
            return $! b


-- | Lookup a recorded template argument and return it.  A lookup failure
-- means either a malformed mangled name (unlikely) or a logic error
-- below that did not properly record a substitution component.
getTemplateArgument :: (Monad m, MonadThrow m)
                    => Maybe String -> Pretty m Builder
getTemplateArgument s = do
  st <- catMaybes <$> gets templateArgs
  case s of
    Nothing -> lookupError 0 st
    -- This case always adds 1 from the number because the
    -- Nothing case is index zero
    Just ix ->
      -- seq ID is base 36
      case numberValue 36 ix of
        Just n -> lookupError (n+1) st
        Nothing -> errMsg
  where
    errMsg = throwM $ MissingTemplateArgument s
    lookupError k m = if k < length m
                      then return $ m !! k
                      else errMsg

-- | The MissingTemplateArgument exception is thrown when the mangled name
-- requests a template argument that cannot be found.  This indicates
-- either an invalid mangled name or else an internal logic error in
-- this Pretty implementation.

data MissingTemplateArgument = MissingTemplateArgument (Maybe String)
instance Exception MissingTemplateArgument
instance Show MissingTemplateArgument where
  show (MissingTemplateArgument s) = "No template argument found for " ++ show s


----------------------------------------------------------------------

-- | Primary interface to get the pretty version of a parsed mangled
-- name in Text form.  This is a monadic operation to support throwing
-- an exception in that outer monad when there is a pretty-printing
-- conversion error.

cxxNameToText :: (Monad m, MonadThrow m) => DecodedName -> m Text
cxxNameToText n = toLazyText <$> evalStateT (dispatchTopLevel n) emptyStore

-- | Primary interface to get the pretty version of a parsed mangled
-- name in String form.

cxxNameToString :: (Monad m, MonadThrow m) => DecodedName -> m String
cxxNameToString = fmap unpack . cxxNameToText

dispatchTopLevel :: (Monad m, MonadThrow m) => DecodedName -> Pretty m Builder
dispatchTopLevel n =
  case n of
    Function fname argTypes -> showFunction fname argTypes
    ConstStructData varName -> showUnqualifiedName varName
    Data varName -> do nm <- showName varName
                       qual <- showNameQualifiers nm varName
                       return $! mconcat [nm, qual]
    VirtualTable t -> do
      tb <- showType t
      return $! mconcat [ fromString "vtable for ", tb ]
    VTTStructure t -> do
      tb <- showType t
      return $! mconcat [ fromString "<vttstruct for ", tb, singleton '>' ]
    TypeInfo t -> do
      tb <- showType t
      return $! mconcat [ fromString "typeinfo for ", tb ]
    TypeInfoName t -> do
      tb <- showType t
      return $! mconcat [ fromString "typeinfo name for ", tb ]
    GuardVariable vname -> do
      vn <- showName vname
      vq <- showNameQualifiers vn vname
      return $! mconcat [ fromString "guard variable for ", vn, vq ]
    OverrideThunk _ target -> do
      tn <- dispatchTopLevel target
      return $! mconcat [ fromString "non-virtual thunk to ", tn ]
    OverrideThunkCovariant _ _ target -> do
      tn <- dispatchTopLevel target
      return $! mconcat [ fromString "virtual thunk to ", tn ]

-- | Show a Function and its arguments.  Function representation has
-- several rules:
--
--   1. template functions have return types with some exceptions
--
--   2. function types which are not part of a function name
--      mangling have return types with some exceptions
--
--   3. non-template function names do not have return types
--
-- The exceptions are that constructors, destructors, and conversion
-- operators do not have return types.
showFunction :: (Monad m, MonadThrow m)
             => Name -> [CXXType] -> Pretty m Builder
showFunction fname args =
  let (retType:retArgTypes) = args
      argTypes = if hasRetType fname then retArgTypes else args
  in do nameBuilder <- showName fname
        dropLastSubstitution
        retSpec <- if hasRetType fname
                   then do p <- case args of
                                  [] -> return $ fromString "void"
                                  _ -> showType retType
                           return $! mconcat [ p, singleton ' ' ]
                   else return $! mempty
        argBuilders <- case argTypes of
          [VoidType] -> return mempty
          _ -> mapM showType argTypes
        let argSpec = mconcat
                      $ intersperse (fromString ", ") argBuilders
        quals <- showNameQualifiers mempty fname
        return $! mconcat [ retSpec
                          , nameBuilder
                          , singleton '(' , argSpec , singleton ')'
                          , quals
                          ]

hasRetType :: Name -> Bool
hasRetType = \case
  NestedTemplateName {} -> True
  UnscopedTemplateName{} -> True
  _ -> False

-- -- | There is a specific parse rule in older C++ that two
-- -- consecutive template closure brackets have to be separated by a
-- -- space.  This was changed in C++14, but the c++filt (i.e. the
-- -- reference standard) still emits the space separators.  This
-- -- function adds the brackets around template arguments, adding the
-- -- space separator for compatibility with c++filt and older C++.

templateBracket :: Builder -> Builder
templateBracket tmpltArgs =
  let lastIsTemplateClosure = maybe False (('>' ==) . snd) . unsnoc . toLazyText
  in singleton '<' `mappend`
     if lastIsTemplateClosure tmpltArgs
     then tmpltArgs `mappend` fromString " >"
     else tmpltArgs `mappend` singleton '>'

showName :: (Monad m, MonadThrow m) => Name -> Pretty m Builder
showName n =
  case n of
    NestedName _ pfxs uname -> do
      pn <- showPrefixedName pfxs uname
      recordSubstitution pn
    UnscopedName uname -> showUName uname
    NestedTemplateName _ pfxs targs -> do
      p <- showPrefixes pfxs
      t <- templateBracket <$> showTArgs targs
      recordSubstitution $! mappend p t
    UnscopedTemplateName uname targs -> do
      un <- showUName uname
      recordSubstitution' un
      tns <- showTArgs targs
      recordSubstitution $! un `mappend` templateBracket tns
    UnscopedTemplateSubstitution s targs -> do
      ss <- showSubstitution s
      tns <- showTArgs targs
      return $! ss `mappend` templateBracket tns

showUName :: (Monad m, MonadThrow m) => UName -> Pretty m Builder
showUName u =
  case u of
    UName uname -> showUnqualifiedName uname
    UStdName uname -> do
      un <- showUnqualifiedName uname
      return (fromString "std::" `mappend` un)

showNameQualifiers :: (Monad m)
                   => Builder -> Name -> Pretty m Builder
showNameQualifiers pn = \case
  NestedName qs@(_:_) _ _ ->
    mappend (mconcat [ pn, singleton ' ' ]) <$> showQualifiers pn qs
  NestedTemplateName qs@(_:_) _ _ ->
    mappend (mconcat [ pn, singleton ' ' ]) <$> showQualifiers pn qs
  _ -> return mempty

showTArgs :: (Monad m, MonadThrow m) => [TemplateArg] -> Pretty m Builder
showTArgs targs = do
  tns <- mapM showTArg targs
  return $! mconcat $! intersperse (fromString ", ") tns

showTArg :: (Monad m, MonadThrow m) => TemplateArg -> Pretty m Builder
showTArg ta =
  case ta of
    TypeTemplateArg t -> do tnum <- reserveTemplateArgument
                            tt <- showType t
                            void $ recordTemplateArgument tnum tt
                            return tt
    ExprPrimaryTemplateArg ep -> showExprPrimary ep

showExprPrimary :: (Monad m, MonadThrow m) => ExprPrimary -> Pretty m Builder
showExprPrimary =
  let parenShowType ty = do sty <- showType ty
                            return $ mconcat [singleton '(', sty, singleton ')']
  in \case
    ExprIntLit ty intval ->
      case ty of
        BoolType
          | intval == 0 -> return $! fromString "false"
          | intval == 1 -> return $! fromString "true"
        _ -> do sty <- parenShowType ty
                return $! mconcat [ sty
                                  , fromString $ show intval ]

-- pass the current prefix builder down so that it can be added to and
-- stored for substitutions
showPrefixedName :: (Monad m, MonadThrow m)
                 => [Prefix] -> UnqualifiedName -> Pretty m Builder
showPrefixedName = go mempty
  where
    go acc pfxs uname =
      case (pfxs, uname) of
        ([], SourceName n) -> do
          recordSubstitution $! mconcat [ acc, fromString "::", fromString n ]
        ([], OperatorName op) -> do
          ob <- showOperator op
          recordSubstitution' ob
          case acc == mempty of
            False -> return $! mconcat [ acc, fromString "::operator", ob ]
            True -> return $! mconcat [ fromString "operator", ob ]
        -- We need to handle constructors and destructors specially
        -- because we won't have enough information to build the right
        -- name if we do a fully depth-first traversal in isolation
        -- without context.  We recognize them here and short circuit
        -- some of the traversal to make things easier.
        ([UnqualifiedPrefix (SourceName className)], CtorDtorName cd) -> do
          let curPfx =
                case acc == mempty of
                  False -> acc `mappend` fromString "::"
                  True -> mempty
              inFix = case isDestructor cd of
                False -> fromString "::"
                True -> fromString "::~"
              sub = curPfx `mappend` fromString className
          recordSubstitution' sub
          return $! mconcat [ curPfx, fromString className, inFix, fromString className ]
        ([UnqualifiedPrefix (SourceName className), tmplPfx@(TemplateArgsPrefix{})], CtorDtorName cd) -> do
          let prevPfx here = case acc == mempty of
                               True -> here
                               False -> mconcat [ acc, fromString "::", here ]
          nextAcc <- showPrefix (prevPfx $ fromString className) tmplPfx
          let inFix = case isDestructor cd of
                False -> fromString "::"
                True -> fromString "::~"
          return $! mconcat [ nextAcc, inFix, fromString className ]
        (outerPfx : innerPfxs, _) -> do
          nextAcc <- showPrefix acc outerPfx
          go nextAcc innerPfxs uname
        ([], CtorDtorName _) -> throwM CtorDtorFallthru

-- | The CtorDtorFallthru exception is thrown when a Constructor or
-- Destructor is declared without declaring the object type that it is
-- the Constructor or Destructor for.  This indicates either an
-- invalid mangled name or else an internal logic error in prefix
-- evaluation.

data CtorDtorFallthru = CtorDtorFallthru
instance Exception CtorDtorFallthru
instance Show CtorDtorFallthru where
  show _ = "Illegal fallthrough in constructor/destructor case"

isDestructor :: CtorDtor -> Bool
isDestructor cd =
  case cd of
    D0 -> True
    D1 -> True
    D2 -> True
    _ -> False

showQualifiers :: Monad m => Builder -> [CVQualifier] -> Pretty m Builder
showQualifiers qualifies qs =
  case null qs of
    True -> return mempty
    False -> snd <$> foldM showQualifier (qualifies,mempty) qs

showQualifier :: Monad m
              => (Builder, Builder) -> CVQualifier
              -> Pretty m (Builder, Builder)
showQualifier (accum,res) q = do
  -- accum is the accumulated name with the base name, used for
  -- recording subtitutions.  res is the accumulated name but not
  -- including the base name which is previously emitted.  res is
  -- ultimately returned.
  let qual = case q of
              Restrict -> fromString "restrict"
              Volatile -> fromString "volatile"
              Const -> fromString "const"
      acc' = mconcat [ accum, singleton ' ', qual ]
      res' = case res == mempty of
               True -> qual
               False -> mconcat [ res, singleton ' ', qual ]
  recordSubstitution' acc'
  return $! (acc', res')


showPrefixes :: (Monad m, MonadThrow m) => [Prefix] -> Pretty m Builder
showPrefixes = foldM showPrefix mempty

-- | These are outer namespace/class name qualifiers, so convert them
-- to strings followed by ::
showPrefix :: (Monad m, MonadThrow m) => Builder -> Prefix -> Pretty m Builder
showPrefix prior pfx =
  let addPrior doRecord toThis = do
        let ret = case prior == mempty of
                    False -> mconcat [ prior, fromString "::", toThis ]
                    True -> toThis
        if doRecord then recordSubstitution ret else return ret
  in case pfx of
       DataMemberPrefix s -> addPrior True $ fromString s
       UnqualifiedPrefix uname -> addPrior True =<< showUnqualifiedName uname
       SubstitutionPrefix s -> addPrior False =<< showSubstitution s
       TemplateArgsPrefix args ->
         case prior == mempty of
           True -> (templateBracket <$> showTArgs args) >>= recordSubstitution
           False -> do recordSubstitution' prior
                       targs <- showTArgs args
                       let this = mconcat [ prior, templateBracket targs ]
                       recordSubstitution this

showUnqualifiedName :: (Monad m, MonadThrow m)
                    => UnqualifiedName -> Pretty m Builder
showUnqualifiedName uname =
  case uname of
    OperatorName op -> do
      ob <- showOperator op
      return (fromString "operator" `mappend` ob)
    CtorDtorName _ -> throwM UnqualCtorDtor
    SourceName s -> return (fromString s) -- KWQ: add Substitution?  "C" extern func?

-- | The UnqualCtorDtor exception is thrown when a attempting to
-- generate a Constructor or Destructor name for an Unqualified name.
-- Although this is allowed by the specification, in this
-- implementation it represents a logic issue since there is no known
-- object to declare the Constructor or Destructor for.

data UnqualCtorDtor = UnqualCtorDtor
instance Exception UnqualCtorDtor
instance Show UnqualCtorDtor where
  show _ = "showUnqualifiedName shouldn't reach the ctor/dtor case?"

showOperator :: (Monad m, MonadThrow m) => Operator -> Pretty m Builder
showOperator op =
  case op of
    OpNew -> return $! fromString " new"
    OpNewArray -> return $! fromString " new[]"
    OpDelete -> return $! fromString " delete"
    OpDeleteArray -> return $! fromString " delete[]"
    OpUPlus -> return $! singleton '+'
    OpUMinus -> return $! singleton '-'
    OpAddressOf -> return $! singleton '&'
    OpDeref -> return $! singleton '*'
    OpBitNot -> return $! singleton '~'
    OpPlus -> return $! singleton '+'
    OpMinus -> return $! singleton '-'
    OpMul -> return $! singleton '*'
    OpDiv -> return $! singleton '/'
    OpMod -> return $! singleton '%'
    OpBitAnd -> return $! singleton '&'
    OpBitOr -> return $! singleton '|'
    OpBitXor -> return $! singleton '^'
    OpAssign -> return $! singleton '='
    OpPlusAssign -> return $! fromString "+="
    OpMinusAssign -> return $! fromString "-="
    OpMulAssign -> return $! fromString "*="
    OpDivAssign -> return $! fromString "/="
    OpModAssign -> return $! fromString "%="
    OpAndAssign -> return $! fromString "&="
    OpOrAssign -> return $! fromString "|="
    OpXorAssign -> return $! fromString "^="
    OpShl -> return $! fromString "<<"
    OpShr -> return $! fromString ">>"
    OpShlAssign -> return $! fromString "<<="
    OpShrAssign -> return $! fromString ">>="
    OpEquals -> return $! fromString "=="
    OpNotEquals -> return $! fromString "!="
    OpLt -> return $! singleton '<'
    OpGt -> return $! singleton '>'
    OpLte -> return $! fromString "<="
    OpGte -> return $! fromString ">="
    OpNot -> return $! singleton '!'
    OpAnd -> return $! fromString "&&"
    OpOr -> return $! fromString "||"
    OpPlusPlus -> return $! fromString "++"
    OpMinusMinus -> return $! fromString "--"
    OpComma -> return $! singleton ','
    OpArrowStar -> return $! fromString "->*"
    OpArrow -> return $! fromString "->"
    OpCall -> return $! fromString "()"
    OpIndex -> return $! fromString "[]"
    OpQuestion -> return $! singleton '?'
    OpSizeofType -> return $! fromString " sizeof"
    OpSizeofExpr -> return $! fromString " sizeof"
    OpAlignofType -> return $! fromString " alignof"
    OpAlignofExpr -> return $! fromString " alignof"
    OpCast t -> do
      tb <- showType t
      return $! singleton ' ' `mappend` tb
    OpVendor n oper -> return $! fromString ("vendor" ++ show n ++ oper) -- ??

showType :: (Monad m, MonadThrow m) => CXXType -> Pretty m Builder
showType t =
  case t of
    QualifiedType qs t' -> do
      tb <- showType t'
      quals <- showQualifiers tb qs
      return $! mconcat [ tb, singleton ' ' ] `mappend` quals
    PointerToType (FunctionType ts) -> do
      -- Since we don't explicitly descend the FunctionType here, we
      -- need to create a stub entry in the substitution table for it
      -- (otherwise the pointer to the type will be off by one).  The
      -- stub will never be referenced because function types aren't
      -- first-class
      ts' <- mapM showType ts
      case ts' of
        [] ->
          return()
        ty:tys ->
          recordSubstitution'
          $ mconcat [ ty
                    , fromString " ("
                    , mconcat $ intersperse (fromString ", ") tys
                    , singleton ')'
                    ]
      r <- showFunctionType ts
      recordSubstitution $! r
    PointerToType t' -> do
      tb <- showType t'
      let r = tb `mappend` singleton '*'
      recordSubstitution $! r
    ReferenceToType t' -> do
      tb <- showType t'
      let r = tb `mappend` singleton '&'
      recordSubstitution $! r
    RValueReferenceToType t' -> do
      tb <- showType t'
      let r = tb `mappend` fromString "&&"
      recordSubstitution $! r
    ComplexPairType t' -> do
      tb <- showType t'
      let r = tb `mappend` fromString " complex"
      return $! r
    ImaginaryType t' -> do
      tb <- showType t'
      let r = tb `mappend` fromString " imaginary"
      return $! r
    ParameterPack _ -> undefined
    VendorTypeQualifier q t' -> do
      tb <- showType t'
      let r = mconcat [ fromString q, singleton ' ', tb ]
      return $! r
    VoidType -> return $! fromString "void"
    Wchar_tType -> return $! fromString "wchar_t"
    BoolType -> return $! fromString "bool"
    CharType -> return $! fromString "char"
    SignedCharType -> return $! fromString "signed char"
    UnsignedCharType -> return $! fromString "unsigned char"
    ShortType -> return $! fromString "short"
    UnsignedShortType -> return $! fromString "unsigned short"
    IntType -> return $! fromString "int"
    UnsignedIntType -> return $! fromString "unsigned int"
    LongType -> return $! fromString "long"
    UnsignedLongType -> return $! fromString "unsigned long"
    LongLongType -> return $! fromString "long long"
    UnsignedLongLongType -> return $! fromString "unsigned long long"
    Int128Type -> return $! fromString "__int128"
    UnsignedInt128Type -> return $! fromString "unsigned __int128"
    FloatType -> return $! fromString "float"
    DoubleType -> return $! fromString "double"
    LongDoubleType -> return $! fromString "long double"
    Float128Type -> return $! fromString "__float128"
    EllipsisType -> return $! fromString "..."
    Char32Type -> return $! fromString "char32_t"
    Char16Type -> return $! fromString "char16_t"
    AutoType -> return $! fromString "auto"
    NullPtrType -> return $! fromString "std::nullptr_t"
    VendorBuiltinType s -> return $! fromString s
    FunctionType _ -> throwM NonPointerFunctionType
    ExternCFunctionType ts -> do
      tb <- showFunctionType ts
      let r = fromString "extern \"C\" " `mappend` tb
      return $! r
    ArrayTypeN (Just n) t' -> do
      tb <- showType t'
      let r = mconcat [ tb, singleton '[', fromString (show n), singleton ']' ]
      return $! r
    ArrayTypeN Nothing t' -> do
      tb <- showType t'
      let r = tb `mappend` fromString "[]"
      return $! r
    ClassEnumType n -> do
      r <- showName n
      q <- showNameQualifiers r n
      recordSubstitution $! mconcat [r, q]
    PtrToMemberType c m -> do
      r <- showPtrToMember c m
      return $! r
    SubstitutionType s -> showSubstitution s
    TemplateParamType tt -> showTemplateParam tt

-- | The NonPointerFunctionType exception is thrown when attempting to
-- pretty-print a function type that is not a pointer.  First-class
-- function types are not supported at this time.  This is a logic
-- error in the library?

data NonPointerFunctionType = NonPointerFunctionType
instance Exception NonPointerFunctionType
instance Show NonPointerFunctionType where
    show _ = "Only pointers to function types are supported"

showSubstitution :: (Monad m, MonadThrow m) => Substitution -> Pretty m Builder
showSubstitution s =
  case s of
    Substitution ss -> getSubstitution ss
    SubStdNamespace -> return $! fromString "std"
    SubStdAllocator -> return $! fromString "std::allocator"
    SubBasicString -> return $! fromString "std::basic_string"
    SubBasicStringArgs -> return $! fromString "std::basic_string<char, std::char_traits<char>, std::allocator<char> >"
    SubBasicIstream -> return $! fromString "std::basic_istream<char, std::char_traits<char> >"
    SubBasicOstream -> return $! fromString "std::basic_ostream<char, std::char_traits<char> >"
    SubBasicIostream -> return $! fromString "std::basic_iostream<char, std::char_traits<char> >"

showTemplateParam :: (Monad m, MonadThrow m) => TemplateParam -> Pretty m Builder
showTemplateParam (TemplateParam t) = do r <- getTemplateArgument t
                                         recordSubstitutionAlways r

showPtrToMember :: (Monad m, MonadThrow m)
                => CXXType -> CXXType -> Pretty m Builder
showPtrToMember (ClassEnumType n) (FunctionType (rt:argts)) = do
  rt' <- showType rt
  argts' <- mapM showType argts
  nb <- showName n
  q <- showNameQualifiers nb n
  return $! mconcat [ rt', fromString " (", nb, q , fromString "::*)("
                    , mconcat (intersperse (fromString ", ") argts')
                    , singleton ')'
                    ]
showPtrToMember _ _ = throwM BarePtrToMember

-- | The BarePtrToMember exception is thrown when there is not enough
-- information to determine what the pointer should point to.  This is
-- either a bad mangled name or an internal logic error.

data BarePtrToMember = BarePtrToMember
instance Exception BarePtrToMember
instance Show BarePtrToMember where
  show _ = "Expected a ClassEnumType and FunctionType pair for PtrToMemberType"

showFunctionType :: (Monad m, MonadThrow m) => [CXXType] -> Pretty m Builder
showFunctionType ts =
  case ts of
    [] -> throwM EmptyFunctionType
    [rtype, VoidType] -> do
      rt' <- showType rtype
      return $! mconcat [ rt', fromString " (*)()" ]
    rtype:rest -> do
      tb <- showType rtype
      rbs <- mapM showType rest
      let arglist = mconcat $ intersperse (fromString ", ") rbs
      return $! mconcat [ tb, fromString " (*)(", arglist, singleton ')' ]

-- | The EmptyFunctionType exception is thrown when there is no
-- argument specification for the function.  This is either a bad
-- mangled name or an internal logic error.

data EmptyFunctionType = EmptyFunctionType
instance Exception EmptyFunctionType
instance Show EmptyFunctionType where
  show _ = "Empty type list in function type"

-- Helpers

-- Taken from parsec-numbers
numberValue :: Integral i => Int -> String -> Maybe i
numberValue base =
  let seqIdToNum seqId | seqId >= ord 'A' && seqId <= ord 'Z' = Just $ seqId - ord 'A' + 10
                       | seqId >= ord '0' && seqId <= ord '9' = Just $ seqId - ord '0'
                       | otherwise = Nothing
  in
    foldM (\ x -> fmap (((fromIntegral base * x) +) . fromIntegral) . seqIdToNum . ord) 0
