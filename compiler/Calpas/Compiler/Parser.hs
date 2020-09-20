module Calpas.Compiler.Parser where

import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Data.CaseInsensitive (CI, mk)
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.Functor (void)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text, cons)
import Data.Void (Void)
import Prelude hiding (some)
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text


--------------------------------------------------------------------------------
-- AST data types
--------------------------------------------------------------------------------

newtype Identifier = Identifier { unIdentifier :: CI Text }
  deriving stock (Show, Eq)

newtype UnsignedInteger = UnsignedInteger Integer
  deriving stock (Show, Eq)

data Sign = Positive | Negative
  deriving stock (Show, Eq)

newtype UnsignedReal = UnsignedReal Scientific
  deriving stock (Show, Eq)

newtype PString = PString Text
  deriving stock (Show, Eq)

data UnsignedConstant = UCReal UnsignedReal
                      | UCInteger UnsignedInteger
                      | UCIdent Identifier
                      | UCString PString
                      | UCNil
                      deriving stock (Show, Eq)

data Constant = ConstReal (Maybe Sign) UnsignedReal
              | ConstInteger (Maybe Sign) UnsignedInteger
              | ConstIdent (Maybe Sign) Identifier
              | ConstString PString
              deriving stock (Show, Eq)

data ConstDefn = ConstDefn
                 { cdIdent :: Identifier
                 , cdConst :: Constant
                 }
                 deriving stock (Show, Eq)

data TypeDefn = TypeDefn
                { tdIdent :: Identifier
                , tdType  :: PType
                }
                deriving stock (Show, Eq)

data PType = PTSimple SimpleType
           | PTStructured StructuredType
           | PTPointer PointerType
           deriving stock (Show, Eq)

data SimpleType = STScalar ScalarType
                | STSubRange SubRangeType
                | STTypeIdent Identifier
                deriving stock (Show, Eq)

newtype ScalarType = ScalarType { unScalarType :: NonEmpty Identifier }
  deriving stock (Show, Eq)

data SubRangeType = SubRangeType
                    { srtLower :: Constant
                    , srtUpper :: Constant
                    }
                    deriving stock (Show, Eq)

data StructuredType = StructuredType (Maybe Packed) UnpackedStructuredType
  deriving stock (Show, Eq)

data Packed = Packed
  deriving stock (Show, Eq)

data UnpackedStructuredType = USTArray ArrayType
                            | USTRecord RecordType
                            | USTSet SetType
                            | USTFile FileType
                            deriving stock (Show, Eq)

data ArrayType = ArrayType
                 { atIndexTypes    :: NonEmpty SimpleType
                 , atComponentType :: PType
                 }
                 deriving stock (Show, Eq)

newtype RecordType = RecordType { rtFieldList :: FieldList }
  deriving stock (Show, Eq)

data FieldList = FLFixed { flFixedPart :: NonEmpty RecordSection }
               | FLVariant { flVariantPart :: VariantPart }
               | FLFixedAndVariant
                 { flFixedPart   :: NonEmpty RecordSection
                 , flVariantPart :: VariantPart
                 }
               deriving stock (Show, Eq)

data RecordSection = RecordSection
                     { rsIdents :: NonEmpty Identifier
                     , rsType   :: PType
                     }
                     deriving stock (Show, Eq)

data VariantPart = VariantPart
                   { vpTagField  :: Identifier
                   , vpTypeIdent :: Identifier
                   , vpVariants  :: NonEmpty Variant
                   }
                   deriving stock (Show, Eq)

data Variant = Variant
               { vCaseLabels :: NonEmpty Constant
               , vFields     :: [FieldList]
               }
               deriving stock (Show, Eq)

newtype SetType = SetType { unSetType :: SimpleType }
  deriving stock (Show, Eq)

newtype FileType = FileType { unFileType :: PType }
  deriving stock (Show, Eq)

newtype PointerType = PointerType { unPointerType :: Identifier }
  deriving stock (Show, Eq)

data VarDecl = VarDecl
               { vdIdents :: NonEmpty Identifier
               , vdType   :: PType
               }
               deriving stock (Show, Eq)

data Variable = VEntire Identifier
              | VComponent ComponentVariable
              | VReferenced Variable
              deriving stock (Show, Eq)

data ComponentVariable = CVIndexed IndexedVariable
                       | CVFieldDesig FieldDesignator
                       | CVFileBuffer Variable
                       deriving stock (Show, Eq)

data IndexedVariable = IndexedVariable
                       { ivArrayVariable :: Variable
                       , ivIndexes       :: NonEmpty Expression
                       }
                       deriving stock (Show, Eq)

data FieldDesignator = FieldDesignator
                       { fdRecordVariable :: Variable
                       , fdFieldIdent     :: Identifier
                       }
                       deriving stock (Show, Eq)

data Factor = FVar Variable
            | FUnsignedConstant UnsignedConstant
            | VFunDesig FunctionDesignator
            | VSet [Expression]
            | VParenExpr Expression
            | VNotExp Expression
            deriving stock (Show, Eq)

data Term = TFactor Factor
          | TMul Term MulOpr Term
          deriving stock (Show, Eq)

data SimpleExpression = SETerm Term
                      | SEAdd SimpleExpression AddOpr Term
                      | SEUnary AddOpr Term
                      deriving stock (Show, Eq)

data Expression = ESimple SimpleExpression
                | ERel SimpleExpression RelOpr SimpleExpression
                deriving stock (Show, Eq)

data MulOpr = OprMult | OprIntDiv | OprDiv | OprMod | OprAnd
  deriving stock (Show, Eq)

data AddOpr = OprAdd | OprSub | OprOr
  deriving stock (Show, Eq)

data RelOpr = OprEq | OprNotEq | OprLT | OprGT | OprLE | OprGE | OprIn
  deriving stock (Show, Eq)

data FunctionDesignator = FDIdent
                          { fdIdent  :: Identifier
                          , fdParams :: [ActualParameter]
                          }
                          deriving stock (Show, Eq)

data Statement = Statement
                 { stmtLabel      :: Maybe Label
                 , stmtUnlabelled :: UnlabelledStatement
                 }
                 deriving stock (Show, Eq)

data UnlabelledStatement = USSimple SimpleStatement
                         | USStructured StructuredStatement
                         deriving stock (Show, Eq)

data Label = Label { unLabel :: UnsignedInteger }
  deriving stock (Show, Eq)

data SimpleStatement = SSAssign AssignmentStatement
                     | SSProc ProcedureStatement
                     | SSGoto GotoStatement
                     | SSEmpty
                     deriving stock (Show, Eq)

data AssignmentStatement = AssignmentStatement
                           { asVariable   :: Variable
                           , asExpression :: Expression
                           }
                           deriving stock (Show, Eq)

data ProcedureStatement = ProcedureStatement
                          { psIdent  :: Identifier
                          , psParams :: [ActualParameter]
                          }
                          deriving stock (Show, Eq)

data ActualParameter = ActualParameter { unActualParameter :: Expression }
  deriving stock (Show, Eq)

data GotoStatement = GotoStatement Label
  deriving stock (Show, Eq)

data StructuredStatement = SSCompound CompoundStatement
                         | SSConditional ConditionalStatement
                         | SSRepeat RepetitiveStatement
                         | SSWith WithStatement
                         deriving stock (Show, Eq)

newtype CompoundStatement = CompoundStatement { unCompoundStatement :: NonEmpty Statement }
  deriving stock (Show, Eq)

data ConditionalStatement = CSIf IfStatement
                          | CSCase CaseStatement
                          deriving stock (Show, Eq)

data IfStatement = IfThen
                   { ifCondition :: Expression
                   , thenStmt    :: Statement
                   }
                   | IfThenElse
                   { ifCondition :: Expression
                   , thenStmt    :: Statement
                   , elseStmt    :: Statement
                   }
                   deriving stock (Show, Eq)

data CaseStatement = CaseStatement
                     { csExpr    :: Expression
                     , csClauses :: NonEmpty CaseListElement
                     }
                     deriving stock (Show, Eq)

data CaseListElement = CaseListElement
                       { cleLabels :: NonEmpty Constant
                       , cleStmt   :: Statement
                       }
                       deriving stock (Show, Eq)

data RepetitiveStatement = RSWhile WhileStatement
                         | RSRepeat RepeatStatement
                         | RSFor ForStatement
                         deriving stock (Show, Eq)

data WhileStatement = WhileStatement
                      { wsExpr :: Expression
                      , wsStmt :: Statement
                      }
                      deriving stock (Show, Eq)

data RepeatStatement = RepeatStatement
                       { rsStmt :: Statement
                       , rsExpr :: Expression
                       }
                       deriving stock (Show, Eq)

data ForStatement = ForStatement
                    { fsControlVar :: Identifier
                    , fsList       :: ForList
                    , fsStmt       :: Statement
                    }
                    deriving stock (Show, Eq)

data ForList = FLUp { flInitial :: Expression, flFinal :: Expression }
             | FLDown { flInitial :: Expression, flFinal :: Expression }
             deriving stock (Show, Eq)

data WithStatement = WithStatement
                     { withVars :: NonEmpty Variable
                     , withStmt :: Statement
                     }
                     deriving stock (Show, Eq)

data ProcDecl = ProcDecl
                { pdHeading     :: ProcHeading
                , pdLabels      :: Maybe LabelDeclPart
                , pdConsts      :: Maybe ConstDefnPart
                , pdTypes       :: Maybe TypeDefnPart
                , pdVars        :: Maybe VarDeclPart
                , pdProcAndFuns :: [ProcOrFunDecl]
                , pdStatement   :: CompoundStatement
                }
                deriving stock (Show, Eq)

data ProcHeading = ProcHeading
                   { phName   :: Identifier
                   , phParams :: [FormalParameterSection]
                   }
                   deriving stock (Show, Eq)

data FormalParameterSection = FPSVal ParameterGroup
                            | FPSVar ParameterGroup
                            | FPSFun ParameterGroup
                            | FPSProc (NonEmpty Identifier)
                            deriving stock (Show, Eq)

data ParameterGroup = ParameterGroup
                      { pgIdents :: NonEmpty Identifier
                      , pgType   :: Identifier
                      }
                      deriving stock (Show, Eq)

data LabelDeclPart = LabelDeclPart { ldpLabels :: NonEmpty Label }
  deriving stock (Show, Eq)

data ConstDefnPart = ConstDefnPart { cdpConstDefns :: NonEmpty ConstDefn }
  deriving stock (Show, Eq)

data TypeDefnPart = TypeDefnPart { tdpTypeDefns :: NonEmpty TypeDefn }
  deriving stock (Show, Eq)

data VarDeclPart = VarDeclPart { vdpVarDecls :: NonEmpty VarDecl }
  deriving stock (Show, Eq)

data ProcOrFunDecl = POFDProc ProcDecl
                   | POFDFun FunDecl
                   deriving stock (Show, Eq)

data FunDecl = FunDecl
               { fdHeading     :: FunHeading
               , fdLabels      :: Maybe LabelDeclPart
               , fdConsts      :: Maybe ConstDefnPart
               , fdTypes       :: Maybe TypeDefnPart
               , fdVars        :: Maybe VarDeclPart
               , fdProcAndFuns :: [ProcOrFunDecl]
               , fdStatement   :: CompoundStatement
               }
               deriving stock (Show, Eq)

data FunHeading = FunHeading
                   { fhName       :: Identifier
                   , fhParams     :: [FormalParameterSection]
                   , fhResultType :: Identifier
                   }
                   deriving stock (Show, Eq)

data Program = Program
  { prgHeading     :: Maybe ProgramHeading
  , prgLabels      :: Maybe LabelDeclPart
  , prgConsts      :: Maybe ConstDefnPart
  , prgTypes       :: Maybe TypeDefnPart
  , prgVars        :: Maybe VarDeclPart
  , prgProcAndFuns :: [ProcOrFunDecl]
  , prgStatement   :: CompoundStatement
  } deriving stock (Show, Eq)

data ProgramHeading = ProgramHeading
                      { phName   :: Identifier
                      , phParams :: [Identifier]
                      }
                      deriving stock (Show, Eq)


--------------------------------------------------------------------------------
-- Parser util functions
--------------------------------------------------------------------------------

spaces :: Parser ()
spaces = L.space space1 lineComment (L.skipBlockComment "{" "}")
  where
    lineComment = void $ takeWhile1P Nothing (const False)

symbol :: Text -> Parser Text
symbol = L.symbol' spaces

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

sepAndEndBy1 :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepAndEndBy1 p sep = sepBy1 p sep <* sep


--------------------------------------------------------------------------------
-- Grammar
--------------------------------------------------------------------------------

identifier :: Parser Identifier
identifier = Identifier . mk <$> do
  c <- satisfy (\c -> isAscii c && isAlpha c) <?> "letter"
  cs <- takeWhileP (Just "letter or digit") $ (\ch -> isAscii ch && isAlphaNum ch)
  pure $ c `cons` cs

unsignedInteger :: Parser UnsignedInteger
unsignedInteger = UnsignedInteger <$> lexeme L.decimal

unsignedReal :: Parser UnsignedReal
unsignedReal = try $ do
  l <- lexeme L.decimal
  right <- optional $ symbol "." *> lexeme L.decimal
  exponent <- optional $ symbol "e" *> scaleFactor
  case (right, exponent) of
    (Nothing, Nothing) -> empty
    (Just r, Nothing)  -> mkNumber l r 0
    (Nothing, Just e)  -> mkNumber l 0 e
    (Just r, Just e)   -> mkNumber l r e
  where
    mkNumber :: Integer -> Integer -> Int -> Parser UnsignedReal
    mkNumber l r e = pure . UnsignedReal $ scientific (combine l r) e

    combine :: Integer -> Integer -> Integer
    combine l r = (l * shiftOf r) + r

    shiftOf :: Integer -> Integer
    shiftOf 0 = 1
    shiftOf x = 10 * shiftOf (x `div` 10)

scaleFactor :: Parser Int
scaleFactor = do
  positive <- lexeme $ (('+' ==) <$> oneOf ['+', '-']) <|> pure True
  n <- lexeme L.decimal
  let n' = if positive then n else negate n

  when (n' < fromIntegral @Int minBound || n' > fromIntegral @Int maxBound) $
    fancyFailure (one $ ErrorFail $ "scale factor is out of bounds")

  pure $ fromInteger n'

sign :: Parser Sign
sign = symbol "+" $> Positive
       <|> symbol "-" $> Negative

pstring :: Parser PString
pstring = PString . fromString <$> between (char '\'') (char '\'') (some stringChar)
  where
    stringChar :: Parser Char
    stringChar = try (chunk "''" $> '\'') <|> satisfy (/= '\'')

constant :: Parser Constant
constant = try (ConstReal <$> optional sign <*> unsignedReal)
           <|> try (ConstInteger <$> optional sign <*> unsignedInteger)
           <|> ConstString <$> pstring
           <|> try (ConstIdent <$> optional sign <*> identifier)

constDefn :: Parser ConstDefn
constDefn = ConstDefn
            <$> identifier
            <*  symbol "="
            <*> constant



{-
program :: Parser Program
program = Program
          <$> optional programHeading
          <*> optional labelDeclPart
          <*> optional constDefnPart
          <*> optional typeDefnPart
          <*> optional varDeclPart
          <*> optional procAndFunDeclPart
          <*> stmtPart
          <*  symbol "."
          <*  eof

programHeading :: Parser ProgramHeading
programHeading = do
  symbol "program"
  ident <- identifier
  params <- optional $ between (symbol "(") (symbol ")") identifierList
  symbol ";"
  pure $ ProgramHeading ident params

labelDeclPart :: Parser LabelDeclarationPart
labelDeclPart = LabelDeclarationPart <$> (symbol "label" *> sepBy1 label (symbol ",") <* symbol ";")

constDefnPart :: Parser ConstantDefinitionPart
constDefnPart = ConstantDefinitionPart <$> (symbol "const" *> sepAndEndBy1 constDefn (symbol ";"))

typeDefnPart :: Parser TypeDefinitionPart
typeDefnPart = TypeDefinitionPart <$> (symbol "type" *> sepAndEndBy1 typeDefn (symbol ";"))

varDeclPart :: Parser VarDeclarationPart
varDeclPart = VarDeclarationPart <$> (symbol "var" *> sepAndEndBy1 varDecl (symbol ";"))
-}
