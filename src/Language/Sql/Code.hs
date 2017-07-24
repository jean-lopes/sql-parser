{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Language.Sql
-- Copyright   : Jean Carlo Giambastiani Lopes, 2017
-- License     : MIT
-- Stability   : experimental
--
-- A SQL parser implementation of the minimum required for a ODBC driver.
module Language.Sql.Code
where
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import qualified Language.Sql.AST as AST

class Code a where
    code :: a -> String

    scode :: a -> String
    scode x = ' ' : code x

    codes :: a -> String
    codes x = code x ++ " "

    scodes :: a -> String
    scodes x = ' ' : code x ++ " "

sepList1 :: Code a => String -> NonEmpty a -> String
sepList1 sep xs = List.intercalate sep list    
  where
    list = NonEmpty.toList $ fmap code xs

commaSepList1 :: Code a => NonEmpty a -> String
commaSepList1 xs = sepList1 ", " xs

maybeCode :: Code a => Maybe a -> (String -> String) -> String
maybeCode x f = code $ fmap (f . code) x

whereClause :: Maybe AST.SearchCondition -> String
whereClause x = maybeCode x $ (++) " WHERE "

parens :: String -> String
parens xs ="(" ++ xs  ++ ")"

instance Code a => Code (Maybe a) where
    code Nothing = ""
    code (Just x) = code x

    scode x = code $ fmap scode x

    codes x = code $ fmap codes x

    scodes x = code $ fmap scodes x

instance a ~ Char => Code [a] where
    code xs = xs

instance Code AST.MultiplicationOperator where
    code AST.Multiply = "*"
    code AST.Divide = "/"

instance Code AST.Sign where
    code AST.Plus = "+"
    code AST.Minus = "-"

instance Code AST.DynamicParameter where
    code AST.DynamicParameter = "?"

instance Code AST.ComparisonOperator where
    code AST.LessThan = "<"
    code AST.GreaterThan = ">"
    code AST.LessOrEqualTo = "<="
    code AST.GreaterOrEqualTo = ">="
    code AST.Equality = "="
    code AST.Inequality = "<>"

instance Code AST.UnsignedInteger where
    code (AST.UnsignedInteger n) = show n

instance Code AST.Literal where
    code (AST.Literal str) = "'" ++ str ++ "'"

instance Code AST.UserDefinedName where
    code (AST.UserDefinedName xs) = xs

instance Code AST.TableIdentifier where
    code (AST.TableIdentifier u) = code u

instance Code AST.BaseTableIdentifier where
    code (AST.BaseTableIdentifier u) = code u

instance Code AST.ColumnIdentifier where
    code (AST.ColumnIdentifier u) = code u

instance Code AST.TableName where
    code (AST.TableName x) = code x

instance Code AST.ColumnName where
    code (AST.ColumnName table column) = maybeCode table (\xs -> xs ++ ".") ++ code column

instance Code AST.BaseTableName where
    code (AST.BaseTableName x) = code x

instance Code AST.TableReference where
    code (AST.TableReference x) = code x

instance Code AST.TableReferenceList where
    code (AST.TableReferenceList xs) = commaSepList1 xs

instance Code AST.SortOrdering where
    code AST.Ascending = "ASC"
    code AST.Descending = "DESC"

instance Code AST.SortSpecification where
    code (AST.SortByColumnPosition x s) = code x ++ scode s
    code (AST.SortByColumnName x s) = code x ++ scode s

instance Code AST.OrderByClause where
    code (AST.OrderByClause xs) = "ORDER BY " ++ commaSepList1 xs

instance Code AST.DataType where
    code (AST.Char x) = "CHAR(" ++ code x ++ ")"
    code (AST.VarChar x) = "VARCHAR(" ++ code x ++ ")"

instance Code AST.Expression where
    code (AST.Expression term exprOp) = code term ++ code exprOp

instance Code AST.ExpressionOp where
    code (AST.ExpressionOp sign expr) = scodes sign ++ code expr

instance Code AST.Factor where
    code (AST.Factor s p) = code s ++ code p

instance Code AST.Term where
    code (AST.Term factor termOp) = code factor ++ code termOp

instance Code AST.TermOp where
    code (AST.TermOp m term) = scode m ++ scode term

instance Code AST.Primary where
    code (AST.PrimaryColumnName x) = code x
    code (AST.PrimaryDynamicParameter x) = code x
    code (AST.PrimaryLiteral x) = code x
    code (AST.PrimaryExpression x) = "(" ++ code x ++ ")"

instance Code AST.SelectSubList where
    code (AST.SelectSubList x) = code x

instance Code AST.SelectList where
    code (AST.SelectAll) = "*"
    code (AST.SelectList xs) = commaSepList1 xs

instance Code AST.InsertValue where
    code (AST.InsertDynamicParameter x) = code x
    code (AST.InsertLiteral x) = code x
    code (AST.InsertNull) = "NULL"
    code (AST.InsertUser) = "USER"

instance Code AST.ComparisonPredicate where
    code (AST.ComparisonPredicate e1 op e2) = code e1 ++ scodes op ++ code e2

instance Code AST.Not where
    code (AST.Not) = "NOT"

instance Code AST.BooleanFactor where
    code (AST.BooleanFactor n p) = codes n ++ code p

instance Code AST.BooleanPrimary where
    code (AST.BooleanPrimaryComparison x) = code x
    code (AST.BooleanPrimarySearch x) = "(" ++ code x ++ ")"

instance Code AST.BooleanAnd where
    code (AST.BooleanAnd x) = "AND" ++ scode x

instance Code AST.BooleanTerm where
    code (AST.BooleanTerm f a) = code f ++ scode a

instance Code AST.BooleanOr where
    code (AST.BooleanOr x) = "OR" ++ scode x

instance Code AST.SearchCondition where
    code (AST.SearchCondition x y) = code x ++ scode y

instance Code AST.Distinct where
    code (AST.Distinct) = "DISTINCT"

instance Code AST.ColumnAndType where
    code (AST.ColumnAndType x y) = code x ++ scode y

instance Code AST.CreateTable where
    code (AST.CreateTable table fields) =  "CREATE TABLE" ++ scode table
                                        ++ "(" ++ commaSepList1 fields ++ ")"

instance Code AST.Drop where
    code (AST.Drop x) = "DROP TABLE" ++ scode x

instance Code AST.Select where
    code (AST.Select d fs ts ws os) =  "SELECT" ++ scode d ++ scodes fs
                                    ++ "FROM" ++ scode ts
                                    ++ whereClause ws
                                    ++ scode os

instance Code AST.Delete where
    code (AST.Delete table condition) =  "DELETE FROM" ++ scode table
                                      ++ whereClause condition

instance Code AST.Insert where
    code (AST.Insert table Nothing values)
        =  "INSERT INTO" ++ scodes table 
        ++ "VALUES" 
        ++ parens (commaSepList1 values)
    code (AST.Insert table (Just columns) values)
        = "INSERT INTO" ++ scode table 
        ++ parens (commaSepList1 columns)
        ++ " VALUES" 
        ++ parens (commaSepList1 values)

instance Code AST.UpdateValue where
    code (AST.UpdateValueExpression x) = code x
    code AST.UpdateValueNull = "NULL"

instance Code AST.UpdateColumn where
    code (AST.UpdateColumn x y) = codes x ++ "=" ++ scode y

instance Code AST.Update where
    code (AST.Update table fields condition)
        = "UPDATE" ++ scodes table 
        ++ "SET " ++ commaSepList1 fields 
        ++ whereClause condition 

instance Code AST.Statement where
    code (AST.CreateStatement x) = code x
    code (AST.DeleteStatement x) = code x
    code (AST.DropStatement x) = code x
    code (AST.InsertStatement x) = code x
    code (AST.SelectStatement x) = code x
    code (AST.UpdateStatement x) = code x

instance Code AST.StatementList where
    code (AST.StatementList xs) = sepList1 "; " xs