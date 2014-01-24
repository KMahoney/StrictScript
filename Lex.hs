-- | The parsec lexer and Token types.
module Lex (Token(..), lex, lexer, lexerToken, assertToken) where

import Prelude hiding (lex)
import Control.Applicative ((<*), (*>), (<*>), (<$>), liftA2)

import Text.Parsec
import Text.Parsec.Expr


data Token = TIdentifer String
           | TNumber String
           | TOperator String
           | TString String
           | TOpen
           | TClose
           | TCurlyOpen
           | TCurlyClose
           | TSeparator
           | TColon
           | TDoubleColon
           deriving (Show, Eq)
                    

type Lexer a = Parsec String () a

                    
operator = many1 $ oneOf "+-*/?<>=."

identifierChar = letter <|> oneOf "_$"
identifier = (:) <$> identifierChar <*> many (identifierChar <|> digit)

litNumber = (++) <$> digits <*> option "" fraction
  where digits = many1 digit
        fraction = (:) <$> char '.' <*> digits
        
litString = char '"' *> manyTill anyChar (char '"')

lexemes :: [Lexer Token]
lexemes = [ TIdentifer <$> identifier
          , TNumber <$> litNumber
          , TOperator <$> operator
          , TString <$> litString
          , single '(' TOpen
          , single ')' TClose
          , single '{' TCurlyOpen
          , single '}' TCurlyClose
          , single ',' TSeparator
          , try (string "::") >> return TDoubleColon
          , single ':' TColon
          ]
  where single c t = char c >> return t          

lexeme :: Lexer (SourcePos, Token)
lexeme = choice (map withPos lexemes)
  where withPos = liftA2 (,) getPosition

lexer = spaces >> many (lexeme <* spaces) <* eof

-- | Parse a stream of tokens
lex = parse lexer

-- | Match a token
lexerToken test = token (show . snd) fst (test . snd)

-- | Fail unless the next token is 't'
assertToken t = lexerToken (\x -> if t == x then Just t else Nothing)
