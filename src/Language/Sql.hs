-- |
-- Module      : Language.Sql
-- Copyright   : Jean Carlo Giambastiani Lopes, 2017
-- License     : MIT
-- Stability   : experimental
--
-- A SQL parser implementation of the minimum required for a ODBC driver.
module Language.Sql(
    module Language.Sql.AST,
    fromSql,
    toSql,
    ast
) where
import Text.Megaparsec
import Language.Sql.AST
import Language.Sql.AST.Tree
import Language.Sql.Code
import Language.Sql.Parser

-- | Parses a SQL statement into an AST
fromSql :: String -> Either (ParseError Char Dec) StatementList
fromSql xs = parse statementList "(input)" xs

-- | Generate a SQL Statement from an AST
toSql :: AST a => a -> String
toSql x = code x

-- | Prints the AST generated from a SQL Statment
ast :: String -> IO ()
ast stmt = case fromSql stmt of
    (Left er) -> putStrLn $ parseErrorPretty er
    (Right x) -> view x
