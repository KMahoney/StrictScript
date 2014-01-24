-- | The parsec parser and AST types. This parser reads from the lexer
-- 'Token' stream defined in 'Lex'.
module Parse (Expr(..), Stmt(..), Block(..), parseBlockString, parseExprString, parseFile) where


import Prelude hiding (lex)
import qualified Test.QuickCheck as Check

import Lex
import TypeCheck.Types (Type(..))

import Data.List (intercalate)
import Control.Applicative ((<*), (*>), (<*>), (<$>), liftA2)

import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Text.Parsec.Pos (initialPos)

import qualified Text.Parsec as P
import qualified Text.Parsec.String as PS


type Parser a = Parsec [(SourcePos, Token)] () a


-- AST, including the current source position which comes in handy
-- when displaying error messages.

data Expr = LNumber SourcePos String           -- 123
          | LString SourcePos String           -- "string"
          | LObject SourcePos [(String, Expr)] -- {field: expr, ...}
          | EVar    SourcePos String           -- x
          | EApp    SourcePos Expr [Expr]      -- fn(arg1, arg2)
          | EFn     SourcePos [String] Block   -- fn (a, b) { stmts }
          | EField  SourcePos Expr String      -- expr.field

data Stmt = SExpr   SourcePos Expr
          | SBind   SourcePos String Expr      -- var x = expr
          | SReturn SourcePos Expr             -- return expr

type Block = [Stmt]


-- Turn an AST back into source. Ignores the source position.

showSepList sep = intercalate sep . map show

instance Show Expr where
   show (LNumber _ s) = s
   show (LString _ s) = "\"" ++ s ++ "\""
   show (LObject _ fields) = "{" ++ intercalate "," (map showField fields) ++ "}"
     where showField (s, a) = s ++ ":" ++ show a
   show (EVar _ s) = s
   show (EApp _ a args) = show a ++ "(" ++ showSepList "," args ++ ")"
   show (EFn _ args a) = "fn (" ++ intercalate "," args ++ ") {" ++ showBlock a ++ "}"
     where showBlock = showSepList " "
   show (EField _ e f) = show e ++ "." ++ f
   
instance Show Stmt where
   show (SExpr _ a) = show a
   show (SBind _ id a) = "var " ++ id ++ " = " ++ show a
   show (SReturn _ e) = "return " ++ show e


-- Some simple building blocks
  
-- | Shorthand for current position.
pos = getPosition

-- | Match a comma separated list.
commaSep = flip sepBy (assertToken TSeparator)

-- | Match between paranthesese.
parans :: Parser a -> Parser a
parans = between (assertToken TOpen) (assertToken TClose)

-- | Match between curly braces.
curlies :: Parser a -> Parser a
curlies = between (assertToken TCurlyOpen) (assertToken TCurlyClose)

-- | Get the next identifier or fail.
identifier = lexerToken $ \token -> case token of
  TIdentifer id -> Just id
  _ -> Nothing

-- | Match an operator or fail.
operator op = lexerToken $ \token -> case token of
  TOperator a -> if op == a then Just a else Nothing
  _ -> Nothing

-- | Match an identifier or fail.
keyword id = assertToken $ TIdentifer id


-- Literals

literal = do
  pos <- getPosition
  lexerToken $ \token -> case token of
    TNumber s -> Just (LNumber pos s)
    TString s -> Just (LString pos s)
    _ -> Nothing

litObject = LObject <$> getPosition <*> (curlies $ commaSep field)
  where field = (,) <$> identifier <* assertToken TColon <*> expression


-- Simple expressions

var = EVar <$> pos <*> identifier

fn = EFn <$> pos <*> (keyword "fn" *> parans (commaSep identifier)) <*> curlies block

atom = parans expression <|> fn <|> literal <|> litObject <|> var

arguments = parans (commaSep expression)

-- | Parse repeated field lookups and function applications.
postfixApplication = do
  -- Type signatures included here for clarity.
  a <- atom :: Parser Expr
  
  -- Parse repeated field lookups and function applications as a list
  -- of functions that take the left side of the expression (that
  -- would evaluate to either a function or an object) as an argument.
  p <- many $ choice [field, app] :: Parser [(Expr -> Expr)]
  
  -- Fold the list of functions into a single 'Expr' by applying them
  -- in sequence to the original atom.
  return $ foldl (\x y -> y x) a p
  
  where
    field = do
      operator "."
      p <- pos
      id <- identifier
      return $ \expr -> EField p expr id
    app = do
      p <- pos
      args <- arguments
      return $ \expr -> EApp p expr args



-- Infix/Postfix Expressions
-- Careful of partial matches: put longest operators first.

table = [ [Prefix negate]
        , leftOps ["*", "/"]
        , leftOps ["+", "-"]
        , leftOps ["<=", ">=", "==", "!=", "<", ">"]
        ]
  where binaryApplication pos op a b = EApp pos (EVar pos op) [a, b]
        binary assoc op = Infix (binaryApplication <$> pos <*> operator op) assoc
        leftOps = map (binary AssocLeft)
        negate = do
          pos <- getPosition
          operator "-"
          return $ \expr -> EApp pos (EVar pos "-") [expr]


-- Top level expression

expression = buildExpressionParser table postfixApplication


-- Statement blocks

bindStmt = keyword "var" *> varIdentifier <* operator "=" <*> expression
  where varIdentifier = SBind <$> pos <*> identifier

returnStmt = SReturn <$> pos <*> (keyword "return" *> expression)

exprStmt = SExpr <$> pos <*> postfixApplication

block = many (bindStmt <|> returnStmt <|> try exprStmt)


-- Top level

parseBlockString :: String -> String -> Either ParseError Block
parseBlockString sourceName source = lex sourceName source >>= P.parse (block <* eof) sourceName

parseExprString :: String -> String -> Either ParseError Expr
parseExprString sourceName source = lex sourceName source >>= P.parse (expression <* eof) sourceName

parseFile filename = fmap (parseBlockString filename) $ readFile filename


-- QuickCheck testing

-- | Generate a random expression using a dummy intial source position.
instance Check.Arbitrary Expr where
  arbitrary = Check.choose (0, 3) >>= a_expr
    where
      dummyPos = initialPos "Arbitrary"
      idChars = concat [['a'..'z'], ['A'..'Z'], "_$"]
      -- TODO: handle escaped characters
      strChars = concat [['a'..'z'], ['A'..'Z'], " \n\r\t?!.,"]
      a_id = Check.listOf1 (Check.elements idChars)
      a_var = EVar dummyPos <$> a_id
      -- TODO: negative numbers
      -- TODO: exponents
      a_number = LNumber dummyPos <$> show <$> abs <$> Check.arbitrarySizedIntegral
      a_string = LString dummyPos <$> Check.listOf (Check.elements strChars)
      a_field depth = liftA2 (,) a_id (a_expr depth)
      a_object depth = LObject dummyPos <$> Check.listOf (a_field depth)
      a_app depth = liftA2 (EApp dummyPos) (a_expr depth) (Check.listOf (a_expr depth))
      -- TODO: fn
      a_expr :: Int -> Check.Gen Expr
      a_expr 0 = Check.oneof [a_var, a_number, a_string]
      a_expr depth = Check.oneof [a_var, a_number, a_string, a_object (depth - 1), a_app (depth - 1)]
      
      
-- | Check if an arbitrary 'Expr' can be turned in to a string and
-- re-parsed. Match each 'Expr' by ignoring the source position, which
-- is not generated by the 'Arbitrary' instance.
prop_parseable_expr :: Expr -> Bool
prop_parseable_expr ast = let source = show ast
                     in case parseExprString "quickcheck" source of 
                       Right ast' -> matchExpr ast ast'
                       Left _ -> False
  where matchExpr (LNumber _ s1) (LNumber _ s2)           = s1 == s2
        matchExpr (LString _ s1) (LString _ s2)           = s1 == s2
        matchExpr (LObject _ fields1) (LObject _ fields2) = matchFields fields1 fields2
        matchExpr (EVar _ s1) (EVar _ s2)                 = s1 == s2
        matchExpr (EApp _ e1 args1) (EApp _ e2 args2)     = matchExpr e1 e2 && matchArgs args1 args2
        matchExpr (EField _ e1 s1) (EField _ e2 s2)       = matchExpr e1 e2 && s1 == s2
        matchExpr _ _ = False
        
        matchFields [] [] = True
        matchFields [] _ = False
        matchFields _ [] = False
        matchFields ((f1, e1) : r1) ((f2, e2) : r2) = f1 == f2 && matchExpr e1 e2 && matchFields r1 r2
        
        matchArgs [] [] = True
        matchArgs [] _ = False
        matchArgs _ [] = False
        matchArgs (e1 : r1) (e2 : r2) = matchExpr e1 e2 && matchArgs r1 r2


check = Check.quickCheck prop_parseable_expr
