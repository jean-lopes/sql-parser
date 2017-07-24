-- |
-- Module      : Language.Sql
-- Copyright   : Jean Carlo Giambastiani Lopes, 2017
-- License     : MIT
-- Stability   : experimental
--
-- A SQL parser implementation of the minimum required for a ODBC driver.
module Language.Sql.Parser
    (statementList) where
import Prelude hiding (not, and, or, drop)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.List (sort)
import Data.Char (toLower)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as Lexer
import qualified Language.Sql.AST as AST

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space (void spaceChar) line block
    where
        line = Lexer.skipLineComment "--"
        block = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme p = Lexer.lexeme spaceConsumer p

comma :: Parser ()
comma = lexeme $ void $ char ','

commaSeparated1 :: Parser a -> Parser (NonEmpty a)
commaSeparated1 p = lexeme $ sepBy1 p comma >>= return . fromList

closed :: Char -> Parser a -> Char -> Parser a
closed o p c = open *> lexeme p <* close
    where
        open = lexeme $ char' o
        close = lexeme $ char' c

reservedKeywords :: [String]
reservedKeywords = sort $ map (map toLower)
    [ "ABSOLUTE" , "IS" , "ACTION" , "ISOLATION" , "ADA" , "JOIN" , "ADD" 
    , "KEY", "ALL", "LANGUAGE", "ALLOCATE", "LAST", "ALTER", "LEADING", "AND"
    , "LEFT", "ANY", "LEVEL", "ARE", "LIKE", "AS", "LOCAL", "ASC", "LOWER"
    , "ASSERTION", "MATCH", "AT", "MAX", "AUTHORIZATION", "MIN", "AVG", "MINUTE"
    , "BEGIN", "MODULE", "BETWEEN", "MONTH", "BIT", "NAMES", "BIT_LENGTH"
    , "NATIONAL", "BOTH", "NATURAL", "BY", "NCHAR", "CASCADE", "NEXT"
    , "CASCADED", "NO", "CASE", "NONE", "CAST", "NOT", "CATALOG", "NULL", "CHAR"
    , "NULLIF", "CHAR_LENGTH", "NUMERIC", "CHARACTER", "OCTET_LENGTH"
    , "CHARACTER_LENGTH", "OF", "CHECK", "ON", "CLOSE", "ONLY", "COALESCE"
    , "OPEN", "COLLATE", "OPTION", "COLLATION", "OR", "COLUMN", "ORDER"
    , "COMMIT", "OUTER", "CONNECT", "OUTPUT", "CONNECTION", "OVERLAPS"
    , "CONSTRAINT", "PAD", "CONSTRAINTS", "PARTIAL", "CONTINUE", "PASCAL"
    , "CONVERT", "POSITION", "CORRESPONDING", "PRECISION", "COUNT", "PREPARE"
    , "CREATE", "PRESERVE", "CROSS", "PRIMARY", "CURRENT", "PRIOR"
    , "CURRENT_DATE", "PRIVILEGES", "CURRENT_TIME", "PROCEDURE"
    , "CURRENT_TIMESTAMP", "PUBLIC", "CURRENT_USER", "READ", "CURSOR", "REAL"
    , "DATE", "REFERENCES", "DAY", "RELATIVE", "DEALLOCATE", "RESTRICT", "DEC"
    , "REVOKE", "DECIMAL", "RIGHT", "DECLARE", "ROLLBACK", "DEFAULT", "ROWS"
    , "DEFERRABLE", "SCHEMA", "DEFERRED", "SCROLL", "DELETE", "SECOND", "DESC"
    , "SECTION", "DESCRIBE", "SELECT", "DESCRIPTOR", "SESSION", "DIAGNOSTICS"
    , "SESSION_USER", "DISCONNECT", "SET", "DISTINCT", "SIZE", "DOMAIN"
    , "SMALLINT", "DOUBLE", "SOME", "DROP", "SPACE", "ELSE", "SQL", "END"
    , "SQLCA", "END-EXEC", "SQLCODE", "ESCAPE", "SQLERROR", "EXCEPT", "SQLSTATE"
    , "EXCEPTION", "SQLWARNING", "EXEC", "SUBSTRING", "EXECUTE", "SUM", "EXISTS"
    , "SYSTEM_USER", "EXTERNAL", "TABLE", "EXTRACT", "TEMPORARY", "FALSE"
    , "THEN", "FETCH", "TIME", "FIRST", "TIMESTAMP", "FLOAT", "TIMEZONE_HOUR"
    , "FOR", "TIMEZONE_MINUTE", "FOREIGN", "TO", "FORTRAN", "TRAILING", "FOUND"
    , "TRANSACTION", "FROM", "TRANSLATE", "FULL", "TRANSLATION", "GET", "TRIM"
    , "GLOBAL", "TRUE", "GO", "UNION", "GOTO", "UNIQUE", "GRANT", "UNKNOWN"
    , "GROUP", "UPDATE", "HAVING", "UPPER", "HOUR", "USAGE", "IDENTITY", "USER"
    , "IMMEDIATE", "USING", "IN", "VALUE", "INCLUDE", "VALUES", "INDEX"
    , "VARCHAR", "INDICATOR", "VARYING", "INITIALLY", "VIEW", "INNER", "WHEN"
    , "INPUT", "WHENEVER", "INSENSITIVE", "WHERE", "INSERT", "WITH", "INT"
    , "WORK", "INTEGER", "WRITE", "INTERSECT", "YEAR", "INTERVAL", "ZONE"
    , "INTO"
    ]

isReserved :: String -> Bool
isReserved ident = scan reservedKeywords
    where
        scan [] = False
        scan (r:rs) = case (compare r ident) of
            LT -> scan rs
            EQ -> True
            GT -> False

reserved :: String -> Parser ()
reserved xs = lexeme $
    if isReserved xs
        then string' xs >> notFollowedBy alphaNumChar >> return ()
        else error $ xs ++ " is not a reserved keyword!"

reservedSeq :: [String] -> Parser ()
reservedSeq [] = return ()
reservedSeq (x:xs) = reserved x >> reservedSeq xs

stringToType :: [(String, a)] -> Parser a
stringToType = choice . map (\(t, x) -> string' t >> return x)

dynamicParameter :: Parser AST.DynamicParameter
dynamicParameter = lexeme $ char '?' >> return AST.DynamicParameter

comparisonOperator :: Parser AST.ComparisonOperator
comparisonOperator = lexeme $ stringToType
    [ (">=", AST.GreaterOrEqualTo)
    , ("<>", AST.Inequality)
    , ("<=", AST.LessOrEqualTo)
    , ("<" , AST.LessThan)
    , (">" , AST.GreaterThan)
    , ("=" , AST.Equality)   
    ]

unsignedInteger :: Parser AST.UnsignedInteger
unsignedInteger = lexeme $ label "unsigned integer" $ do
    x <- digitChar
    xs <- many $ digitChar
    return $ AST.UnsignedInteger $ read $ x:xs

quoteLiteral :: Parser Char
quoteLiteral = string "''" >> return '\''

literal :: Parser AST.Literal
literal = lexeme $ between (char '\'') (char '\'') $ do
    xs <- many $ quoteLiteral <|> (try $ noneOf' "'")
    return $ AST.Literal xs

userDefinedName :: Parser AST.UserDefinedName
userDefinedName = lexeme $ label "user defined name" $ try $ do
    x <- letterChar
    xs <- many $ alphaNumChar <|> char '_'
    let name = x:xs
    case isReserved name of
        True -> unexpected . Label $ fromList $ "reserved keyword: " ++ name
        False -> return $ AST.UserDefinedName name

tableIdentifier :: Parser AST.TableIdentifier
tableIdentifier = lexeme $ label "table identifier" $ 
    try userDefinedName >>= return . AST.TableIdentifier

baseTableIdentifier :: Parser AST.BaseTableIdentifier
baseTableIdentifier = lexeme $ label "base table identifier" $
    try userDefinedName >>= return . AST.BaseTableIdentifier

columnIdentifier :: Parser AST.ColumnIdentifier
columnIdentifier = lexeme $ label "column identifier" $
    try userDefinedName >>= return . AST.ColumnIdentifier

tableName :: Parser AST.TableName
tableName = lexeme $ label "table name" $
    try tableIdentifier >>= return . AST.TableName

columnName :: Parser AST.ColumnName
columnName = lexeme $ label "column name" $ do
    table <- optional $ try $ tableName >>= \x -> char '.' >> return x
    column <- columnIdentifier
    return $ AST.ColumnName table column

baseTableName :: Parser AST.BaseTableName
baseTableName = lexeme $ label "base table name" $
    try baseTableIdentifier >>= return . AST.BaseTableName

tableReference :: Parser AST.TableReference
tableReference = lexeme $ label "table reference" $
    try tableName >>= return . AST.TableReference

tableReferenceList :: Parser AST.TableReferenceList
tableReferenceList = lexeme $ label "table reference list" $ do
    tables <- try $ commaSeparated1 tableReference
    return $ AST.TableReferenceList tables

sortOrdering :: Parser AST.SortOrdering
sortOrdering = lexeme $ choice
    [ try $ reserved "asc" >> return AST.Ascending 
    , reserved "desc" >> return AST.Descending
    ]

sortSpecificationByPosition :: Parser AST.SortSpecification
sortSpecificationByPosition = lexeme $ do
    position <- unsignedInteger
    order <- optional sortOrdering
    return $ AST.SortByColumnPosition position order
   
sortSpecificationByName :: Parser AST.SortSpecification
sortSpecificationByName = lexeme $ do
    column <- columnName
    order <- optional sortOrdering
    return $ AST.SortByColumnName column order

sortSpecification :: Parser AST.SortSpecification
sortSpecification = lexeme $
    sortSpecificationByPosition <|> sortSpecificationByName
  
orderByClause :: Parser AST.OrderByClause
orderByClause = reservedSeq ["order", "by"] *>
    (AST.OrderByClause <$> commaSeparated1 sortSpecification)

dataType :: Parser AST.DataType
dataType = lexeme $ f <*> closed '(' unsignedInteger ')'
    where f = stringToType [ ("char", AST.Char), ("varchar", AST.VarChar) ]

multiplicationOperator :: Parser AST.MultiplicationOperator
multiplicationOperator = lexeme $ stringToType
    [ ("*", AST.Multiply)
    , ("/", AST.Divide)
    ]

sign :: Parser AST.Sign
sign = lexeme $ stringToType
    [ ("+", AST.Plus)
    , ("-", AST.Minus)
    ]

expressionOp :: Parser AST.ExpressionOp    
expressionOp = AST.ExpressionOp <$> sign <*> expression

expression :: Parser AST.Expression 
expression = AST.Expression <$> term <*> optional expressionOp

termOp :: Parser AST.TermOp
termOp = AST.TermOp <$> multiplicationOperator <*> term

term :: Parser AST.Term 
term = AST.Term <$> factor <*> optional termOp

factor :: Parser AST.Factor  
factor = AST.Factor <$> optional sign <*> primary

primaryColumnName :: Parser AST.Primary
primaryColumnName = AST.PrimaryColumnName <$> columnName

primaryDynamicParameter :: Parser AST.Primary
primaryDynamicParameter = AST.PrimaryDynamicParameter <$> dynamicParameter

primaryLiteral :: Parser AST.Primary
primaryLiteral = AST.PrimaryLiteral <$> literal

primaryExpression :: Parser AST.Primary
primaryExpression = AST.PrimaryExpression <$> closed '(' expression ')'

primary :: Parser AST.Primary
primary =   primaryColumnName
        <|> primaryDynamicParameter
        <|> primaryLiteral
        <|> primaryExpression

selectSubList :: Parser AST.SelectSubList
selectSubList = AST.SelectSubList <$> expression

asterisk :: Parser ()
asterisk = lexeme $ void $ char '*'

selectList :: Parser AST.SelectList
selectList =   try $ AST.SelectList <$> commaSeparated1 selectSubList
           <|> asterisk *> return AST.SelectAll 

-- -- > insert-value = dynamic-parameter | literal | 'NULL' | 'USER' ;
-- data InsertValue
--     = InsertDynamicParameter DynamicParameter
--     | InsertLiteral Literal
--     | InsertNull
--     | InsertUser
--     deriving Show

insertValue :: Parser AST.InsertValue
insertValue = choice
    [ AST.InsertDynamicParameter <$> dynamicParameter
    , AST.InsertLiteral <$> literal
    , reserved "null" >> return AST.InsertNull
    , reserved "user" >> return AST.InsertUser
    ]

comparisonPredicate :: Parser AST.ComparisonPredicate
comparisonPredicate =   AST.ComparisonPredicate 
                    <$> expression 
                    <*> comparisonOperator 
                    <*> expression

not :: Parser AST.Not
not = reserved "not" >> return AST.Not

and :: Parser ()
and = reserved "and"

or :: Parser ()
or = reserved "or"

booleanFactor :: Parser AST.BooleanFactor
booleanFactor = AST.BooleanFactor <$> optional not <*> booleanPrimary

booleanPrimary :: Parser AST.BooleanPrimary
booleanPrimary =   try $ AST.BooleanPrimarySearch <$> closed '(' searchCondition ')'
               <|> AST.BooleanPrimaryComparison <$> comparisonPredicate

booleanAnd :: Parser AST.BooleanAnd
booleanAnd = AST.BooleanAnd <$> try (and >> booleanTerm)

booleanTerm :: Parser AST.BooleanTerm
booleanTerm = AST.BooleanTerm <$> booleanFactor <*> optional booleanAnd

booleanOr :: Parser AST.BooleanOr
booleanOr = AST.BooleanOr <$> try (or >> searchCondition)

searchCondition :: Parser AST.SearchCondition
searchCondition = AST.SearchCondition <$> booleanTerm <*> optional booleanOr

distinct :: Parser AST.Distinct
distinct = reserved "distinct" >> return AST.Distinct

select :: Parser AST.Select
select =   AST.Select
       <$> (reserved "select" >> optional distinct)
       <*> selectList
       <*> (reserved "from" >> tableReferenceList)
       <*> try (optional $ reserved "where" >> searchCondition)
       <*> try (optional orderByClause)

columnAndType :: Parser AST.ColumnAndType
columnAndType = AST.ColumnAndType <$> columnIdentifier <*> dataType

createTable :: Parser AST.CreateTable
createTable =   AST.CreateTable
            <$> (reservedSeq ["create", "table"] >> baseTableName)
            <*> closed '(' (commaSeparated1 columnAndType) ')'

drop :: Parser AST.Drop
drop = AST.Drop <$> (reservedSeq ["drop", "table"] >> baseTableName)

delete :: Parser AST.Delete
delete =   AST.Delete
       <$> (reservedSeq ["delete", "from"] >> tableName)
       <*> optional (reserved "where" >> searchCondition)

insert :: Parser AST.Insert
insert =   AST.Insert
       <$> (reservedSeq ["insert", "into"] >> tableName)
       <*> optional (closed '(' (commaSeparated1 columnIdentifier) ')')
       <*> (reserved "values" >> closed '(' (commaSeparated1 insertValue) ')')

updateValue :: Parser AST.UpdateValue
updateValue =   try $ AST.UpdateValueExpression <$> expression
            <|> (reserved "null" >> return AST.UpdateValueNull)

updateColumn :: Parser AST.UpdateColumn
updateColumn =   AST.UpdateColumn
             <$> columnIdentifier 
             <*  lexeme (char '=')
             <*> updateValue

updateColumnList :: Parser AST.UpdateColumnList
updateColumnList = commaSeparated1 updateColumn

update :: Parser AST.Update
update =   AST.Update
       <$> (reserved "update" >> tableName)
       <*> (reserved "set" >> updateColumnList)
       <*> optional (reserved "where" >> searchCondition)

statement :: Parser AST.Statement
statement =   try (AST.CreateStatement <$> createTable)
          <|> try (AST.DeleteStatement <$> delete)
          <|> try (AST.DropStatement   <$> drop)
          <|> try (AST.InsertStatement <$> insert)
          <|> try (AST.SelectStatement <$> select)
          <|>      AST.UpdateStatement <$> update

statementList :: Parser AST.StatementList
statementList = AST.StatementList <$> (sepBy1 statement semi >>= return . fromList)
  where
    semi = lexeme $ char ';' 