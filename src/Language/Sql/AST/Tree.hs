{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Language.Sql
-- Copyright   : Jean Carlo Giambastiani Lopes, 2017
-- License     : MIT
-- Stability   : experimental
--
-- A SQL parser implementation of the minimum required for a ODBC driver.
module Language.Sql.AST.Tree
where
import           Data.Tree
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Language.Sql.AST
import           Language.Sql.Code

add :: AST a => Tree String -> a -> Tree String
add (Node l xs) x = Node l $ xs ++ [ tree x ]

addM :: AST a => Tree String -> Maybe a -> Tree String
addM t Nothing = t
addM t (Just x) = add t x

addL :: AST a => Tree String -> NonEmpty a -> Tree String
addL t xs = foldl add t xs

addML :: AST a => Tree String -> Maybe (NonEmpty a) -> Tree String
addML t Nothing = t
addML t (Just xs) = addL t xs

class Code a => AST a where
    view :: a -> IO ()
    view = putStrLn . drawForest . forest

    forest :: a -> Forest String
    forest = pure . tree

    tree :: a -> Tree String

    leaf :: String -> a -> Tree String
    leaf str x = Node str [ pure $ code x ]

    node :: String -> a -> Tree String
    node str x = Node str [ tree x ]

    nodes :: String -> NonEmpty a -> Tree String
    nodes str xs = Node str $ map tree $ NonEmpty.toList xs
   
instance a ~ Char => AST [a] where
    tree xs = pure xs

instance AST MultiplicationOperator where
    tree x = leaf "multiplication-operator" x

instance AST Sign where
    tree x = leaf "sign" x

instance AST DynamicParameter where
    tree x = leaf "dynamic-parameter" x

instance AST ComparisonOperator where
    tree x = leaf "comparison-operator" x

instance AST UnsignedInteger where  
    tree x = leaf "unsigned-integer" x

instance AST Literal where
    tree x = leaf "literal" x

instance AST UserDefinedName where
    tree x = leaf "user-defined-name" x

instance AST TableIdentifier where
    tree (TableIdentifier x) = node "table-identifier" x

instance AST BaseTableIdentifier where
    tree (BaseTableIdentifier x) = node "base-table-identifier" x

instance AST ColumnIdentifier where
    tree (ColumnIdentifier x) = node "column-identifier" x

instance AST TableName where
    tree (TableName x) = node "table-name" x

instance AST ColumnName where
    tree (ColumnName t c) = pure "column-name" `addM` t `add` c

instance AST BaseTableName where
    tree (BaseTableName x) = node "base-table-name" x

instance AST TableReference where
    tree (TableReference x) = node "table-reference" x

instance AST TableReferenceList where
    tree (TableReferenceList xs) = nodes "table-reference-list" xs

instance AST SortOrdering where
    tree x = leaf "sort-ordering" x

instance AST SortSpecification where
    tree (SortByColumnPosition x y) = node "sort-specification" x `addM` y
    tree (SortByColumnName x y) = node "sort-specification" x `addM` y

instance AST OrderByClause where
    tree (OrderByClause xs) = nodes "order-by-clause" xs

instance AST DataType where
    tree (Char x) = Node "data-type" [ node "char" x ]
    tree (VarChar x) = Node "data-type" [ node "varchar" x ]

instance AST Expression where
    tree (Expression term exprOp) = node "expression" term `addM` exprOp

instance AST ExpressionOp where
    tree (ExpressionOp sign expr) = node "expression-op" sign `add` expr

instance AST Factor where
    tree (Factor s p) = pure "factor" `addM` s `add` p

instance AST Term where
    tree (Term factor termOp) = node "term" factor `addM` termOp

instance AST TermOp where
    tree (TermOp m term) = node "term-op" m `add` term

instance AST Primary where
    tree (PrimaryColumnName x) = node "primary" x
    tree (PrimaryDynamicParameter x) = node "primary" x
    tree (PrimaryLiteral x) = node "primary" x
    tree (PrimaryExpression x) = node "primary" x

instance AST SelectSubList where
    tree (SelectSubList x) = node "select-sublist" x

instance AST SelectList where
    tree (SelectAll) = pure "select-list" `add` "*"
    tree (SelectList xs) = nodes "select-list" xs

instance AST InsertValue where
    tree (InsertDynamicParameter x) = node "insert-value" x
    tree (InsertLiteral x) = node "insert-value" x
    tree (InsertNull) = pure "insert-value" `add` "NULL"
    tree (InsertUser) = pure "insert-value" `add` "USER"

instance AST ComparisonPredicate where
    tree (ComparisonPredicate e1 op e2) = node "comparison-predicate" e1 `add` op `add` e2

instance AST Not where
    tree (Not) = pure "not"

instance AST BooleanFactor where
    tree (BooleanFactor n p) = pure "boolean-factor" `addM` n `add` p

instance AST BooleanPrimary where
    tree (BooleanPrimaryComparison x) = node "boolean-primary" x
    tree (BooleanPrimarySearch x) = node "boolean-primary" x

instance AST BooleanAnd where
    tree (BooleanAnd x) = node "boolean-and" x

instance AST BooleanTerm where
    tree (BooleanTerm f a) = node "boolean-term" f `addM` a

instance AST BooleanOr where
    tree (BooleanOr x) = node "boolean-or" x

instance AST SearchCondition where
    tree (SearchCondition x y) = node "search-condition" x `addM` y

instance AST Distinct where
    tree (Distinct) = pure "distinct"

instance AST ColumnAndType where
    tree (ColumnAndType x y) = node "column-and-type" x `add` y

instance AST CreateTable where
    tree (CreateTable table fields) = node "create-table-statement" table `addL` fields

instance AST Drop where
    tree (Drop x) = node "drop-table-statement" x

instance AST Select where
    tree (Select d fs ts ws os) = pure "select-statement" `addM` d `add`  fs `add`  ts `addM` ws `addM` os

instance AST Delete where
    tree (Delete table condition) = node "delete-statement-searched" table `addM` condition

instance AST Insert where
    tree (Insert table columns values) = node "insert-statement" table `addML` columns `addL` values

instance AST UpdateValue where
    tree (UpdateValueExpression x) = node "update-value" x
    tree UpdateValueNull = pure "update-value" `add` "null"

instance AST UpdateColumn where
    tree (UpdateColumn x y) = node "update-column" x `add` y

instance AST Update where
    tree (Update table fields condition) = node "update-statement-searched" table `addL` fields `addM` condition

instance AST Statement where
    tree (CreateStatement x) = node "statement" x
    tree (DeleteStatement x) = node "statement" x
    tree (DropStatement x) = node "statement" x
    tree (InsertStatement x) = node "statement" x
    tree (SelectStatement x) = node "statement" x
    tree (UpdateStatement x) = node "statement" x

instance AST StatementList where
    tree (StatementList xs) = nodes "statement-list" xs