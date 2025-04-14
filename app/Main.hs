{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
module Main where

import System.Environment           ( getArgs )

import Text.Megaparsec hiding       ( State )
import Text.Megaparsec.Char
import Text.Printf                  ( printf )

import Control.Applicative          ()

import Data.Void
import Data.Text                    ( Text )
import Data.Char                    ( isPrint )
import Data.List                    (  sortBy )
import Data.Functor                 ( (<&>) )
import Data.Time.Clock              ( getCurrentTime, diffUTCTime )


import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec.Char.Lexer as L


{- 
Haskell-To-C is a project for translating Haskell source code into C source code, 
Taking from Template Haskell, the meta-programming extention of haskell.

Future ambitions for this project is a general "Haskell-To-..." project,
capable of tranlating Haskell code into any other for which parse blocks have been defined
-}


main :: IO ()
main = do
  (source : target : _) <- getArgs
  timeStart <- getCurrentTime

  putStrLn $ "parsing file " <> source <> " into " <> target

  preparsed <- TIO.readFile source <&> preparse
  let eDeclarations = map (runParser parseDeclaration source) preparsed
  
  case sequence eDeclarations of
    Left e             -> error (show e)
    Right declarations -> do
      let translated = translate (sortDeclarations declarations)
      
      TIO.writeFile target translated
      
      timeEnd <- getCurrentTime
      printf "Finished in %s \nWrote %d Bytes" (show (diffUTCTime timeEnd timeStart)) (T.length translated)

      

-- !! TEMPORARY DEFINITIONS !!
--
-- there are many intricacies going into this
-- in haskell the type declaration and the body declaration are two seperate things
-- in C however its one and the same, 
-- as such we have to translate functions in pairs of type and body declarations
translate :: [Declaration] -> Text
translate = undefined




translateFunction :: Declaration -> Declaration -> Text
translateFunction (SigD name typ) (FunctionD _ [Clause ps (NormalB expr) _]) = 
  T.pack (show (hTypeToC (last ts))) <> name <> "(" <> variables (init ts) ps <> ") {\n"
  <> translateExpression expr <> "\n}"
  where
    ts = typeToList typ
translateFunction _ _ = error "translateFunction unexpected input"

translateDeclaration :: Declaration -> Text
translateDeclaration = \case
  ImportD "Prelude" -> "#include <stdio.h>"
  ModuleD name      -> "// module " <> name
  _ -> undefined

translateExpression :: Expression -> Text
translateExpression = \case
  DoE sts                 -> T.concat $ map (\s -> translateStatement s <> "\n") sts
  ApplyE (VarE "putStr") (LitE (StringL xs)) -> "printf(" <> xs <> ");"
  _ -> undefined

translateStatement :: Statement -> Text
translateStatement = \case
  NoBindS expr -> translateExpression expr
  _ -> undefined

variables :: [Text] -> [Pattern] -> Text
variables ts ps = T.concat $ map (\(t, VarP n) -> t <> " " <> n <> " ") (zip ts ps)

typeToList :: Type -> [Text]
typeToList (VarT t) = [t]
typeToList (ApplyT (VarT t) rest) = t : typeToList rest
typeToList _ = undefined

sortDeclarations :: [Declaration] -> [Declaration]
sortDeclarations = sortBy (\x y -> compare (declarationPos x) (declarationPos y))


hTypeToC :: Text -> Text
hTypeToC = \case
  "Int"     -> "int"
  "Integer" -> "int"
  "Double"  -> "double"
  "Bool"    -> "bool"
  "IO"      -> "int"
  _ -> undefined

declarationPos :: Declaration -> Int
declarationPos = \case 
  ModuleD _           -> 0
  ImportD _           -> 1
  ClassD {}           -> 2
  DataD  {}           -> 3
  NewtypeD {}         -> 4
  TypeD {}            -> 5
  InstanceD {}        -> 6
  SigD "main" _       -> 100
  FunctionD "main" _  -> 110
  SigD {}             -> 50
  FunctionD {}        -> 60


parseDeclaration :: Parser Declaration
parseDeclaration = choice
  [ try parseFunctionD
  , try parseSigD
  , try parseTypeD
  , try parseImportD
  , parseModuleD
  -- TODO
  ]

parseFunctionD :: Parser Declaration
parseFunctionD = do
  name <- lowerValid
  hspace
  FunctionD name . (: []) <$> parseClause

parseClause :: Parser Clause
parseClause = do
  patterns <- many $ do
    hspace
    pattern <- parsePattern
    hspace
    pure pattern

  body <- parseBody  
  
  -- TODO: add the 'where' declarations 
  pure $ Clause patterns body []

-- TODO
parsePattern :: Parser Pattern
parsePattern = choice
  [ try parseTupleP
  , try parseListP
  , try parseLitP
  -- try parseConP TODO
  ,     parseVarP
  ]

parseTupleP :: Parser Pattern
parseTupleP = do
  char '(' >> hspace
  patterns <- parsePattern `sepBy` (hspace >> char ',' >> hspace)
  _ <- hspace >> char ')'

  pure $ TupleP patterns

parseListP :: Parser Pattern
parseListP = do
  char '[' >> hspace
  patterns <- parsePattern `sepBy` (hspace >> char ',' >> hspace)
  _ <- hspace >> char ']'

  pure $ ListP patterns

parseLitP :: Parser Pattern
parseLitP = LitP <$> choice
  [ try parseCharL
  , try parseStringL
  , try parseFloatingL
  , parseIntegerL
  ]

parseVarP :: Parser Pattern
parseVarP = VarP . T.pack <$> some validChar


parseBody :: Parser Body
parseBody = choice
  [ try parseBodyNormal
  , parseBodyGuarded
  ]

parseBodyNormal :: Parser Body
parseBodyNormal = do
  hspace >> char '=' >> hspace
  NormalB <$> parseExpression

parseBodyGuarded :: Parser Body
parseBodyGuarded = undefined  <$> string "TEMPORARY" -- TODO

-- TODO:
parseExpression :: Parser Expression
parseExpression = choice
  [ try parseInfixApplyE
  , try parseApplyE
  , try parseRangeE
  , try parseComphE
  , try parseCastE
  , try parseDoE
  , try parseListE
  , try parseParenE
  , try parseLitE

  ,     parseVarE -- parsing variables has to always be at the very end
  ]

parseApplyE :: Parser Expression
parseApplyE = do
  l <- parseAtomicExpression
  hspace
  
  args <- some $ do 
    hspace
    parseExpression

  hspace

  pure $ foldl ApplyE l args

parseInfixApplyE :: Parser Expression
parseInfixApplyE = do
  l <- optional parseAtomicExpression
  hspace
  o <- parseInfix
  hspace
  r <- optional parseExpression
  hspace

  pure $ InfixApplyE l o r

parseLambdaE :: Parser Expression
parseLambdaE = do
  _ <- char '(' >> hspace >> char '\\'

  patterns <- some $ do
    hspace
    pattern <- parsePattern
    hspace
    pure pattern
    
  hspace >> string "->" >> hspace
  
  expr <- parseExpression
  
  _ <- hspace >> char ')'

  pure $ LambdaE patterns expr

parseAtomicExpression :: Parser Expression
parseAtomicExpression = choice
  [ try parseParenE
  , try parseListE
  , try parseLitE
  , try parseLambdaE
  , parseVarE
  ]

parseParenE :: Parser Expression
parseParenE = do
  char '(' >> hspace
  expr <- parseExpression
  _ <- hspace >> char ')'
  
  hspace
  pure $ ParensE expr


parseInfix :: Parser Expression
parseInfix = try $ between (char '`') (char '`') parseVarE
         <|> VarE <$> choice ["==", "/=", "++", "!!", "-", "*", "/", "$", ". ", "<>", "+", ": "]

parseRangeE :: Parser Expression
parseRangeE = RangeE <$> parseRangeR

parseRangeR :: Parser Range
parseRangeR = choice
  [ try parseRangeFromR
  , try parseRangeFromThenR
  , try parseRangeFromToR
  , parseRangeFromThenToR
  ]

parseRangeFromR :: Parser Range
parseRangeFromR = do
  char '[' >> hspace
  from <- parseExpression  
  _ <- string ".."
  _ <- hspace >> char ']'
  
  hspace
  pure $ FromR from

parseRangeFromThenR :: Parser Range
parseRangeFromThenR = do
  char '[' >> hspace
  from <- parseExpression
  hspace >> char ',' >> hspace
  thn <- parseExpression
  _ <- string ".."
  _ <- hspace >> char ']'

  pure $ FromThenR from thn

parseRangeFromToR :: Parser Range
parseRangeFromToR = do
  char '[' >> hspace
  from <- parseExpression
  hspace >> string ".." >> hspace
  to <- parseExpression
  _ <- hspace >> char ']'

  pure $ FromToR from to

parseRangeFromThenToR :: Parser Range
parseRangeFromThenToR = do
  char '[' >> hspace
  from <- parseExpression
  hspace >> char ',' >> hspace
  thn <- parseExpression
  hspace >> string ".." >> hspace
  to <- parseExpression
  _ <- hspace >> char ']'

  pure $ FromThenToR from thn to

-- TODO
parseComphE :: Parser Expression
parseComphE = undefined <$> string "TEMPORARY"

parseDoE :: Parser Expression
parseDoE = do
  string "do" >> space
  statements <- some $ do
    hspace
    s <- parseStatement
    space
    pure s

  pure $ DoE statements

parseStatement :: Parser Statement
parseStatement = choice
  [ try parseBindS
  , try parseLetS
  , parseNoBindS
  ]

parseBindS :: Parser Statement
parseBindS = do
  pat  <- parsePattern
  hspace >> string "<-" >> hspace
  
  BindS pat <$> parseExpression

parseLetS :: Parser Statement
parseLetS = do
  hspace >> string "let" >> hspace
  LetS . (: []) <$> parseDeclaration

parseNoBindS :: Parser Statement
parseNoBindS = NoBindS <$> (hspace *> parseExpression)



parseListE :: Parser Expression
parseListE = do
  char '[' >> hspace
  expressions <- parseExpression `sepBy` (hspace >> char ',' >> hspace)
  hspace
  _ <- char ']'
  pure $ ListE expressions

parseCastE :: Parser Expression
parseCastE = do
  expr <- parseAtomicExpression
  hspace >> string "::" >> hspace
  
  CastE expr <$> parseTypeT

parseVarE :: Parser Expression
parseVarE = VarE . T.pack <$> some validChar

parseLitE :: Parser Expression
parseLitE = LitE <$> choice
  [ try parseCharL
  , try parseStringL
  , try parseFloatingL
  , parseIntegerL
  ]



parseCharL :: Parser Literal
parseCharL = CharL <$> between (char '\'') (char '\'') letterChar

parseStringL :: Parser Literal
parseStringL = StringL <$> between (char '"') (char '"') ( T.pack <$> many (satisfy (\t -> isPrint t && t /= '"')) )

parseFloatingL :: Parser Literal
parseFloatingL = do
  l <- some numberChar
  _ <- char '.'
  r <- some numberChar
  pure $ FloatingL (read (l <> "." <> r))

parseIntegerL :: Parser Literal
parseIntegerL = do
  n <- some numberChar
  pure $ IntegerL (read n)



parseSigD :: Parser Declaration
parseSigD = do
  name <- lowerValid
  hspace >> string "::" >> hspace

  SigD name <$> parseTypeT

parseDataD :: Parser Declaration
parseDataD = undefined -- TODO

parseNewtypeD :: Parser Declaration
parseNewtypeD = undefined -- TODO

parseTypeD :: Parser Declaration
parseTypeD = do
  string "type" >> hspace
  name <- upperValid
  hspace >> string "=" >> hspace
  
  TypeD name <$> parseTypeT

parseClassD :: Parser Declaration
parseClassD = undefined -- TODO

parseInstanceD :: Parser Declaration
parseInstanceD = undefined -- TODO

parseImportD :: Parser Declaration
parseImportD = do
  string "import" >> hspace
  name <- T.pack <$> some (alphaNumChar <|> char '.')

  pure $ ImportD name

parseModuleD :: Parser Declaration
parseModuleD = do
  string "module" >> hspace
  name <- T.pack <$> some validChar
  _ <- hspace >> string "where"

  pure $ ModuleD name



parseTypeT :: Parser Type
parseTypeT = try parseApplyT <|> parseNApplyT

parseApplyT :: Parser Type
parseApplyT = do
  mndt <- parseNApplyT
  _ <- between hspace hspace (string "->")

  ApplyT mndt <$> parseTypeT

parseNApplyT :: Parser Type
parseNApplyT = choice
  [ try parseListType
  , try parseParenType
  , parseVarT
  ]

parseListType :: Parser Type
parseListType = do
  typ <- between (char '[') (char ']') parseTypeT

  pure $ ListT typ

parseParenType :: Parser Type
parseParenType = do
  typ <- between (char '(') (char ')') parseTypeT

  pure $ ParenT typ

parseVarT :: Parser Type
parseVarT = (T.pack <$> some validChar) <&> VarT



lowerValid :: Parser Text
lowerValid = T.pack <$> ((:) <$> lowerChar <*> many validChar)

upperValid :: Parser Text
upperValid = T.pack <$> ((:) <$> upperChar <*> many validChar)

validChar :: Parser Char
validChar = alphaNumChar <|> char '\'' <|> char '_'

-- the preparse
preparse :: Text -> [Text]
preparse = blocks . filter (\line -> T.strip line /= "" && not (T.isPrefixOf "--" (T.strip line))) . T.lines

-- we parse the text into blocks for further parsing
blocks :: [Text] -> [Text]
blocks [] = []
blocks ts = x : blocks xs
  where
    (x, xs) = block ts

block :: [Text] -> (Text, [Text])
block []     = ("", [])
block (x:xs) = (T.unlines (x : takeWhile isIndented xs), dropWhile isIndented xs)


isWhiteSpace :: Text -> Bool
isWhiteSpace = T.all (`T.elem` " \t\n")

isIndented :: Text -> Bool
isIndented ts
  | T.null ts = False
  | otherwise = T.head ts `T.elem` " \t"

splice :: Text -> Text -> Text
splice a b = a <> "\n" <> b

type Parser = Parsec Void Text


data Expression
  = VarE        Text                                              -- name
  | LitE        Literal
  | ApplyE      Expression Expression                             -- f x
  | InfixApplyE  (Maybe Expression) Expression (Maybe Expression)  -- infix apply, IE (1 + 2), (1 +) or (+ 1)
  | ParensE     Expression                                        -- (e)
  | LambdaE     [Pattern] Expression                              -- \p1 .. pn -> e
  | TupleE      [Maybe Expression]
  | ConditionE  Expression Expression Expression                  -- if e1 then e2 else e3
  | LetE        [Declaration] Expression                          -- let d = e
  | Case        Expression [Match]
  | DoE         [Statement]
  | ComphE      [Statement]                                       -- list comprehension, result of comprehension is the last Statement; a NoBind
  | ListE       [Expression]
  | RangeE      Range
  | CastE       Expression Type                                   -- e :: t
  deriving ( Show )

-- list ranges 
data Range
  = FromR       Expression                                        -- [n..]
  | FromThenR   Expression Expression                             -- [n1,n2..]
  | FromToR     Expression Expression                             -- [n1..n2]
  | FromThenToR Expression Expression Expression                  -- [n1,n2..n3]
  deriving ( Show )

data Declaration
  = FunctionD Text [Clause]                                           -- function definition
  | SigD      Text Type                                               -- function declaration
  | DataD     Context Text (Maybe Kind) [DataConstructor] [Derive]    -- data type definition
  | NewtypeD  Context Text (Maybe Kind) DataConstructor   [Derive]    -- netype definition
  | TypeD     Text Type
  | ClassD    Context Text [Declaration]
  | InstanceD Context Type   [Declaration]
  | ImportD   Text  -- im lazy
  | ModuleD   Text  -- im lazy 2: electric boogaloo
  deriving ( Show )

data Clause = Clause [Pattern] Body [Declaration]
  deriving ( Show )

data DataConstructor
  = NormalC Text [Type]
  | RecordC Text [(String, Type)]
  deriving ( Show )

data Pattern
  = VarP    Text
  | LitP    Literal
  | TupleP  [Pattern]
  | ListP   [Pattern]
  | ConP    Text [Type] [Pattern]
  deriving ( Show )

type Context    = [Type]
type Kind       = Type
type Derive     = Text

-- function body
data Body
  = GuardedB [(Guard, Expression)]
  | NormalB  Expression
  deriving ( Show )

data Guard
  = NormalG   Expression
  | PatternG  [Statement]
  deriving ( Show )

-- do notation statements
data Statement
  = BindS   Pattern Expression
  | LetS    [Declaration]
  | NoBindS Expression
  deriving ( Show )

data Match = Match Pattern Body [Declaration]
  deriving ( Show )

-- i am NOT going to support type holes arlight?
data Type
  = VarT    Text
  | ApplyT  Type Type
  | ListT   Type
  | ParenT  Type
  deriving ( Show )

-- Literals
data Literal
  = CharL     Char
  | StringL   Text
  | IntegerL  Integer
  | FloatingL Double
  deriving ( Show )

