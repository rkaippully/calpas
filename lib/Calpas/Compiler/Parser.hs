module Calpas.Compiler.Parser where

import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Data.CaseInsensitive (CI, mk)
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.Scientific (Scientific, scientific)
import Data.Text (cons)
import Prelude hiding (many, some)
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

data FieldList = FieldList
                 { flFixedPart   :: Maybe (NonEmpty RecordSection)
                 , flVariantPart :: Maybe VariantPart
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
               , vFields     :: FieldList
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
            | FFunDesig FunctionDesignator
            | FSet [Expression]
            | FParenExp Expression
            | FNotExp Factor
            deriving stock (Show, Eq)

data Term = Term
            { trmFactor :: Factor
            , trmTrail  :: [(MulOpr, Factor)]
            }
            deriving stock (Show, Eq)

data SimpleExpression = SimpleExpression
                        { seSign  :: Maybe Sign
                        , seTerm  :: Term
                        , seTrail :: [(AddOpr, Term)]
                        }
                        deriving stock (Show, Eq)

data Expression = Expression
                  { expSimple :: SimpleExpression
                  , expTrail  :: [(RelOpr, SimpleExpression)]
                  }
                  deriving stock (Show, Eq)

data MulOpr = OprMult | OprIntDiv | OprDiv | OprMod | OprAnd
  deriving stock (Show, Eq)

data AddOpr = OprAdd | OprSub | OprOr
  deriving stock (Show, Eq)

data RelOpr = OprEq | OprNotEq | OprLT | OprGT | OprLE | OprGE | OprIn
  deriving stock (Show, Eq)

data FunctionDesignator = FunctionDesignator
                          { fdIdent  :: Identifier
                          , fdParams :: Maybe (NonEmpty ActualParameter)
                          }
                          deriving stock (Show, Eq)

data Statement = Statement
                 { stmtLabel      :: Maybe PLabel
                 , stmtUnlabelled :: UnlabelledStatement
                 }
                 deriving stock (Show, Eq)

data UnlabelledStatement = USSimple SimpleStatement
                         | USStructured StructuredStatement
                         deriving stock (Show, Eq)

newtype PLabel = PLabel { unLabel :: UnsignedInteger }
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
                          , psParams :: ProcedureParams
                          }
                          deriving stock (Show, Eq)

data ProcedureParams = PPActual (Maybe (NonEmpty ActualParameter))
                     | PPWrite (NonEmpty WriteParameter)
                     deriving stock (Show, Eq)

data WriteParameter = WP1 Expression
                    | WP2 Expression Expression
                    | WP3 Expression Expression Expression
                    deriving stock (Show, Eq)

newtype ActualParameter = ActualParameter { unActualParameter :: Expression }
  deriving stock (Show, Eq)

newtype GotoStatement = GotoStatement PLabel
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

data IfStatement = IfStatement
                   { ifCondition :: Expression
                   , thenStmt    :: Statement
                   , elseStmt    :: Maybe Statement
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
                       { rsStmts :: NonEmpty Statement
                       , rsExpr  :: Expression
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
                   , phParams :: Maybe (NonEmpty FormalParameterSection)
                   }
                   deriving stock (Show, Eq)

data FormalParameterSection = FPSVal ParameterGroup
                            | FPSVar ParameterGroup
                            -- | FPSFun ParameterGroup            -- Not supported
                            -- | FPSProc (NonEmpty Identifier)    -- Not supported
                            deriving stock (Show, Eq)

data ParameterGroup = ParameterGroup
                      { pgIdents :: NonEmpty Identifier
                      , pgType   :: Identifier
                      }
                      deriving stock (Show, Eq)

newtype LabelDeclPart = LabelDeclPart { ldpLabels :: NonEmpty PLabel }
  deriving stock (Show, Eq)

newtype ConstDefnPart = ConstDefnPart { cdpConstDefns :: NonEmpty ConstDefn }
  deriving stock (Show, Eq)

newtype TypeDefnPart = TypeDefnPart { tdpTypeDefns :: NonEmpty TypeDefn }
  deriving stock (Show, Eq)

newtype VarDeclPart = VarDeclPart { vdpVarDecls :: NonEmpty VarDecl }
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
                   , fhParams     :: Maybe (NonEmpty FormalParameterSection)
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
                      , phParams :: Maybe (NonEmpty Identifier)
                      }
                      deriving stock (Show, Eq)


--------------------------------------------------------------------------------
-- Parser util functions
--------------------------------------------------------------------------------

spaces :: Parser ()
spaces = L.space space1 lineComment blockComment
  where
    lineComment = void $ takeWhile1P Nothing (const False)
    blockComment = (string "{" <|> string "(*" <|> string "/*")
                   *> void (manyTill anySingle (string "}" <|> string "*)" <|> string "*/"))

symbol :: Text -> Parser Text
symbol = L.symbol' spaces

wordSymbol :: Text -> Parser Text
wordSymbol s = lexeme $ try $ string' s <* notFollowedBy (satisfy isAlphaNum)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

sepAndEndBy1 :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepAndEndBy1 p sep = liftA2 (:|) p' (many p')
  where
    p' = p <* sep


--------------------------------------------------------------------------------
-- Grammar
--------------------------------------------------------------------------------

reservedWords :: HashSet (CI Text)
reservedWords = [ "div", "mod", "nil", "in", "if", "then", "else", "case", "of"
                , "repeat", "until", "while", "do", "for", "to", "downto"
                , "begin", "end", "with", "goto", "const", "var", "type"
                , "array", "record", "set", "file", "function", "procedure"
                , "label", "packed", "param"
                ]

identifier :: Parser Identifier
identifier = label "identifier" $ try $
  Identifier <$> do
    c <- satisfy (\c -> isAscii c && isAlpha c) <?> "letter"
    cs <- takeWhileP (Just "letter or digit") (\ch -> isAscii ch && isAlphaNum ch)

    let name = mk $ c `cons` cs
    when (name `member` reservedWords) empty

    spaces $> name

unsignedInteger :: Parser UnsignedInteger
unsignedInteger = UnsignedInteger <$> lexeme L.decimal

sign :: Parser Sign
sign = symbol "+" $> Positive
       <|> symbol "-" $> Negative

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
  positive <- lexeme $ (('+' ==) <$> oneOf ("+-" :: String)) <|> pure True
  n <- lexeme L.decimal
  let n' = if positive then n else negate n

  when (n' < fromIntegral @Int minBound || n' > fromIntegral @Int maxBound) $
    fancyFailure (one $ ErrorFail "scale factor is out of bounds")

  pure $ fromInteger n'

pstring :: Parser PString
pstring = PString . fromString <$> between (char '\'') (char '\'') (some stringChar) <* spaces
  where
    stringChar :: Parser Char
    stringChar = try (chunk "''" $> '\'') <|> satisfy (/= '\'')

unsignedConstant :: Parser UnsignedConstant
unsignedConstant = try (UCReal <$> unsignedReal)
           <|> UCInteger <$> unsignedInteger
           <|> UCNil <$ wordSymbol "nil"
           <|> UCIdent <$> identifier
           <|> UCString <$> pstring

constant :: Parser Constant
constant = try (ConstReal <$> optional sign <*> unsignedReal)
           <|> try (ConstInteger <$> optional sign <*> unsignedInteger)
           <|> ConstIdent <$> optional sign <*> identifier
           <|> ConstString <$> pstring

constDefn :: Parser ConstDefn
constDefn = ConstDefn
            <$> identifier
            <*  symbol "="
            <*> constant

typeDefn :: Parser TypeDefn
typeDefn = TypeDefn
            <$> identifier
            <*  symbol "="
            <*> ptype

ptype :: Parser PType
ptype = PTSimple <$> simpleType
        <|> PTStructured <$> structuredType
        <|> PTPointer <$> pointerType

simpleType :: Parser SimpleType
simpleType = STScalar <$> scalarType
             <|> STSubRange <$> subRangeType
             <|> STTypeIdent <$> identifier

scalarType :: Parser ScalarType
scalarType = ScalarType <$> between (symbol "(") (symbol ")") identifierList

identifierList :: Parser (NonEmpty Identifier)
identifierList = sepBy1 identifier (symbol ",")

subRangeType :: Parser SubRangeType
subRangeType = try $
  SubRangeType
  <$> constant
  <*  symbol ".."
  <*> constant

structuredType :: Parser StructuredType
structuredType = StructuredType
                 <$> optional packed
                 <*> unpackedStructuredType

packed :: Parser Packed
packed = Packed <$ wordSymbol "packed"

unpackedStructuredType :: Parser UnpackedStructuredType
unpackedStructuredType = USTArray <$> arrayType
                         <|> USTRecord <$> recordType
                         <|> USTSet <$> setType
                         <|> USTFile <$> fileType

arrayType :: Parser ArrayType
arrayType = ArrayType
            <$  wordSymbol "array"
            <*> between openSquareBracket closeSquareBracket (sepBy1 simpleType (symbol ","))
            <*  wordSymbol "of"
            <*> ptype

openSquareBracket :: Parser Text
openSquareBracket = symbol "[" <|> symbol "(."

closeSquareBracket :: Parser Text
closeSquareBracket = symbol "]" <|> symbol ".)"

recordType :: Parser RecordType
recordType = RecordType
             <$  wordSymbol "record"
             <*> fieldList
             <*  wordSymbol "end"

fieldList :: Parser FieldList
fieldList = optional fixedPart >>= \case
  Nothing   -> optional variantPart >>= \case
    Nothing -> pure $ FieldList Nothing Nothing
    var     -> optional (symbol ";") $> FieldList Nothing var
  fixed     -> optional (try (symbol ";" *> variantPart)) >>= \case
    Nothing -> optional (symbol ";") $> FieldList fixed Nothing
    var     -> optional (symbol ";") $> FieldList Nothing var

fixedPart :: Parser (NonEmpty RecordSection)
fixedPart = liftA2 (:|) recordSection (many $ try $ symbol ";" *> recordSection)  -- can't use sepBy1

recordSection :: Parser RecordSection
recordSection = RecordSection
                <$> identifierList
                <*  symbol ":"
                <*> ptype

variantPart :: Parser VariantPart
variantPart = VariantPart
              <$  wordSymbol "case"
              <*> identifier
              <*  symbol ":"
              <*> identifier
              <*  wordSymbol "of"
              <*> sepBy1 variant (symbol ";")

variant :: Parser Variant
variant = Variant
          <$> sepBy1 constant (symbol ",")
          <*  symbol ":"
          <*> between (symbol "(") (symbol ")") fieldList

setType :: Parser SetType
setType = SetType
          <$  wordSymbol "set"
          <*  wordSymbol "of"
          <*> simpleType

fileType :: Parser FileType
fileType = FileType
           <$  wordSymbol "file"
           <*  wordSymbol "of"
           <*> ptype

pointerType :: Parser PointerType
pointerType = PointerType
              <$  pointerSymbol
              <*> identifier

pointerSymbol :: Parser Text
pointerSymbol = symbol "^" <|> symbol "@"

varDecl :: Parser VarDecl
varDecl = VarDecl
          <$> identifierList
          <*  symbol ":"
          <*> ptype

variable :: Parser Variable
variable = identifier >>= rest . VEntire
  where
    rest :: Variable -> Parser Variable
    rest v = (pointerSymbol $> VReferenced v >>= rest)
             <|> ((VComponent <$> componentVariable v) >>= rest)
             <|> pure v

componentVariable :: Variable -> Parser ComponentVariable
componentVariable v = CVIndexed <$> indexedVariable v
                      <|> CVFieldDesig <$> fieldDesignator v
                      -- <|> (pointerSymbol $> CVFileBuffer v)   -- this never matches because VReferenced is identical

indexedVariable :: Variable -> Parser IndexedVariable
indexedVariable v = IndexedVariable v
                    <$> between openSquareBracket closeSquareBracket (sepBy1 expression (symbol ","))

fieldDesignator :: Variable -> Parser FieldDesignator
fieldDesignator v = FieldDesignator v
                    <$  symbol "."
                    <*> identifier

factor :: Parser Factor
factor = FNotExp <$ wordSymbol "not" <*> factor
         <|> FSet <$> between openSquareBracket closeSquareBracket (sepBy expression (symbol ","))
         <|> FParenExp <$> between (symbol "(") (symbol ")") expression
         <|> try (FFunDesig <$> functionDesignator)
         <|> FVar <$> variable
         <|> FUnsignedConstant <$> unsignedConstant

term :: Parser Term
term = Term
       <$> factor
       <*> many ((,) <$> mulOpr <*> factor)

simpleExpression :: Parser SimpleExpression
simpleExpression = SimpleExpression
                   <$> optional sign
                   <*> term
                   <*> many ((,) <$> addOpr <*> term)

expression :: Parser Expression
expression = Expression
             <$> simpleExpression
             <*> many ((,) <$> relOpr <*> simpleExpression)

mulOpr :: Parser MulOpr
mulOpr = OprMult <$ symbol "*"
         <|> OprIntDiv <$ wordSymbol "div"
         <|> OprDiv <$ symbol "/"
         <|> OprMod <$ wordSymbol "mod"
         <|> OprAnd <$ wordSymbol "and"

addOpr :: Parser AddOpr
addOpr = OprAdd <$ symbol "+"
         <|> OprSub <$ symbol "-"
         <|> OprOr <$ wordSymbol "or"

relOpr :: Parser RelOpr
relOpr = OprEq <$ symbol "="
         <|> OprNotEq <$ symbol "<>"
         <|> OprLE <$ symbol "<="
         <|> OprGE <$ symbol ">="
         <|> OprLT <$ symbol "<"
         <|> OprGT <$ symbol ">"
         <|> OprIn <$ wordSymbol "in"

functionDesignator :: Parser FunctionDesignator
functionDesignator = FunctionDesignator
                     <$> identifier
                     <*> (Just <$> actualParameterList)

actualParameterList :: Parser (NonEmpty ActualParameter)
actualParameterList = between (symbol "(") (symbol ")") $ sepBy1 actualParameter (symbol ",")

actualParameter :: Parser ActualParameter
actualParameter = ActualParameter <$> expression

statement :: Parser Statement
statement = Statement
            <$> optional (plabel <* symbol ":")
            <*> unlabelledStatement

unlabelledStatement :: Parser UnlabelledStatement
unlabelledStatement = USStructured <$> structuredStatement
                      <|> USSimple <$> simpleStatement

plabel :: Parser PLabel
plabel = PLabel <$> unsignedInteger

simpleStatement :: Parser SimpleStatement
simpleStatement = SSGoto <$> gotoStatement
                  -- TODO: Remove these trys for better error messages
                  <|> try (SSAssign <$> assignmentStatement)
                  <|> try (SSProc <$> procedureStatement)
                  <|> pure SSEmpty

assignmentStatement :: Parser AssignmentStatement
assignmentStatement = AssignmentStatement
                      <$> variable
                      <*  symbol ":="
                      <*> expression

procedureStatement :: Parser ProcedureStatement
procedureStatement = ProcedureStatement
                     <$> identifier
                     <*> procedureParams

procedureParams :: Parser ProcedureParams
procedureParams = try (PPWrite <$> between (symbol "(") (symbol ")") writeParameterList)
                  <|> pure (PPActual Nothing)
                  -- all regular params will be detected as a writeParameter during parsing
                  -- the semantic analysis will fix it to ActualParameters.

writeParameterList :: Parser (NonEmpty WriteParameter)
writeParameterList = sepBy1 writeParameter (symbol ",")

writeParameter :: Parser WriteParameter
writeParameter = try (WP3 <$> expression <* symbol ":" <*> expression <* symbol ":" <*> expression)
                 <|> try (WP2 <$> expression <* symbol ":" <*> expression)
                 <|> WP1 <$> expression

gotoStatement :: Parser GotoStatement
gotoStatement = GotoStatement <$ wordSymbol "goto" <*> plabel

structuredStatement :: Parser StructuredStatement
structuredStatement = SSCompound <$> compoundStatement
                      <|> SSConditional <$> conditionalStatement
                      <|> SSRepeat <$> repetitiveStatement
                      <|> SSWith <$> withStatement

compoundStatement :: Parser CompoundStatement
compoundStatement = CompoundStatement
                    <$> between (wordSymbol "begin") (wordSymbol "end") (sepBy1 statement (symbol ";"))

conditionalStatement :: Parser ConditionalStatement
conditionalStatement = CSIf <$> ifStatement
                       <|> CSCase <$> caseStatement

ifStatement :: Parser IfStatement
ifStatement = IfStatement
              <$  wordSymbol "if"
              <*> expression
              <*  wordSymbol "then"
              <*> statement
              <*> optional (wordSymbol "else" *> statement)

caseStatement :: Parser CaseStatement
caseStatement = CaseStatement
                <$  wordSymbol "case"
                <*> expression
                <*  wordSymbol "of"
                <*> caseLists
                <*  optional (symbol ";")
                <*  wordSymbol "end"
  where
    caseLists = liftA2 (:|) caseListElement (many $ try $ symbol ";" *> caseListElement)  -- can't use sepBy1

caseListElement :: Parser CaseListElement
caseListElement = CaseListElement
                  <$> sepBy1 constant (symbol ",")
                  <*  symbol ":"
                  <*> statement

repetitiveStatement :: Parser RepetitiveStatement
repetitiveStatement = RSWhile <$> whileStatement
                      <|> RSRepeat <$> repeatStatement
                      <|> RSFor <$> forStatement

whileStatement :: Parser WhileStatement
whileStatement = WhileStatement
                 <$  wordSymbol "while"
                 <*> expression
                 <*  wordSymbol "do"
                 <*> statement

repeatStatement :: Parser RepeatStatement
repeatStatement = RepeatStatement
                 <$  wordSymbol "repeat"
                 <*> sepBy1 statement (symbol ";")
                 <*  wordSymbol "until"
                 <*> expression

forStatement :: Parser ForStatement
forStatement = ForStatement
               <$  wordSymbol "for"
               <*> identifier
               <*  symbol ":="
               <*> forList
               <*  wordSymbol "do"
               <*> statement

forList :: Parser ForList
forList = expression
          <**> (try (FLUp <$ wordSymbol "to") <|> (FLDown <$ wordSymbol "downto"))
          <*>  expression

withStatement :: Parser WithStatement
withStatement = WithStatement
                <$  wordSymbol "with"
                <*> sepBy1 variable (symbol ",")
                <*  wordSymbol "do"
                <*> statement

procDecl :: Parser ProcDecl
procDecl = ProcDecl
          <$> procHeading
          <*> optional labelDeclPart
          <*> optional constDefnPart
          <*> optional typeDefnPart
          <*> optional varDeclPart
          <*> procAndFunDeclPart
          <*> compoundStatement

procHeading :: Parser ProcHeading
procHeading = ProcHeading
              <$  wordSymbol "procedure"
              <*> identifier
              <*> optional formalParams
              <*  symbol ";"

formalParams :: Parser (NonEmpty FormalParameterSection)
formalParams = between (symbol "(") (symbol ")") (sepBy1 formalParameterSection (symbol ";"))

formalParameterSection :: Parser FormalParameterSection
formalParameterSection = try (FPSVar <$ wordSymbol "var" <*> parameterGroup)
                       -- <|> try (FPSFun <$ wordSymbol "function" <*> parameterGroup)    -- Not supported
                       -- <|> try (FPSProc <$ wordSymbol "procedure" <*> identifierList)  -- Not supported
                       <|> FPSVal <$> parameterGroup

parameterGroup :: Parser ParameterGroup
parameterGroup = ParameterGroup
                 <$> identifierList
                 <*  symbol ":"
                 <*> identifier

labelDeclPart :: Parser LabelDeclPart
labelDeclPart = LabelDeclPart <$> (wordSymbol "label" *> sepBy1 plabel (symbol ",") <* symbol ";")

constDefnPart :: Parser ConstDefnPart
constDefnPart = ConstDefnPart <$> (wordSymbol "const" *> sepAndEndBy1 constDefn (symbol ";"))

typeDefnPart :: Parser TypeDefnPart
typeDefnPart = TypeDefnPart <$> (wordSymbol "type" *> sepAndEndBy1 typeDefn (symbol ";"))

varDeclPart :: Parser VarDeclPart
varDeclPart = VarDeclPart <$> (wordSymbol "var" *> sepAndEndBy1 varDecl (symbol ";"))

procAndFunDeclPart :: Parser [ProcOrFunDecl]
procAndFunDeclPart = many $ procOrFunDecl <* symbol ";"

procOrFunDecl :: Parser ProcOrFunDecl
procOrFunDecl = POFDProc <$> procDecl
                <|> POFDFun <$> funDecl

funDecl :: Parser FunDecl
funDecl = FunDecl
          <$> funHeading
          <*> optional labelDeclPart
          <*> optional constDefnPart
          <*> optional typeDefnPart
          <*> optional varDeclPart
          <*> procAndFunDeclPart
          <*> compoundStatement

funHeading :: Parser FunHeading
funHeading = FunHeading
              <$  wordSymbol "function"
              <*> identifier
              <*> optional formalParams
              <*  symbol ":"
              <*> identifier
              <*  symbol ";"

program :: Parser Program
program = Program
          <$  spaces
          <*> optional programHeading
          <*> optional labelDeclPart
          <*> optional constDefnPart
          <*> optional typeDefnPart
          <*> optional varDeclPart
          <*> procAndFunDeclPart
          <*> compoundStatement
          <*  symbol "."
          <*  eof

programHeading :: Parser ProgramHeading
programHeading = do
  ident <- wordSymbol "program" *> identifier
  params <- optional $ between (symbol "(") (symbol ")") identifierList
  symbol ";" $> ProgramHeading ident params

parseFile :: FilePath -> IO (Either LText Program)
parseFile f = do
  s <- readFileText f
  pure $ first (fromString . errorBundlePretty) $ parse program f s
