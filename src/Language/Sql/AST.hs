-- |
-- Module      : Language.Sql
-- Copyright   : Jean Carlo Giambastiani Lopes, 2017
-- License     : MIT
-- Stability   : experimental
--
-- A SQL parser implementation of the minimum required for a ODBC driver.
--
-- | /EBNF:/
--     
-- > 
-- > character = ? any character ?;
-- > 
-- > letter = lower-case-letter | upper-case-letter;
-- > 
-- > lower-case-letter = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' 
-- >                   | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' 
-- >                   | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' ;
-- > 
-- > upper-case-letter = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' 
-- >                   | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T'
-- >                   | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' ;
-- > 
-- > digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
module Language.Sql.AST
where
import Data.List.NonEmpty

-- | /EBNF:/
--
-- > multiplication-operator = '*' | '/' ;
data MultiplicationOperator
    = Multiply
    | Divide
    deriving Show

-- | /EBNF:/
--
-- > sign = '+' | '-' ;
data Sign
    = Plus
    | Minus
    deriving Show

-- | /EBNF:/
--
-- > dynamic-parameter = '?' ;
data DynamicParameter = DynamicParameter
    deriving Show

-- | /EBNF:/
--
-- > comparison-operator = '<' | '>' | '<=' | '>=' | '=' | '<>' ;
data ComparisonOperator
    = LessThan
    | GreaterThan
    | LessOrEqualTo
    | GreaterOrEqualTo
    | Equality
    | Inequality
    deriving Show

-- | /EBNF:/
--
-- > unsigned-integer = digit, { digit };
newtype UnsignedInteger = UnsignedInteger Int
    deriving Show

-- | /EBNF:/
--
-- > literal = "'", { character - "'" | "''" }, "'";
newtype Literal = Literal String
    deriving Show

-- | /EBNF:/
--
-- > user-defined-name = letter, { digit | letter | '_' };
newtype UserDefinedName = UserDefinedName String
    deriving Show

-- | /EBNF:/
--
-- > table-identifier = user-defined-name;
newtype TableIdentifier = TableIdentifier UserDefinedName
    deriving Show

-- | /EBNF:/
--
-- > base-table-identifier = user-defined-name;
newtype BaseTableIdentifier = BaseTableIdentifier UserDefinedName
    deriving Show

-- | /EBNF:/
--
-- > column-identifier = user-defined-name;
newtype ColumnIdentifier = ColumnIdentifier UserDefinedName
    deriving Show

-- | /EBNF:/
--
-- > table-name = table-identifier;
newtype TableName = TableName TableIdentifier
    deriving Show

-- | /EBNF:/
--
-- > column-name = [ table-name, '.' ], column-identifier;
data ColumnName = ColumnName (Maybe TableName) ColumnIdentifier
    deriving Show

-- | /EBNF:/
--
-- > base-table-name = base-table-identifier;
newtype BaseTableName = BaseTableName BaseTableIdentifier
    deriving Show

-- | /EBNF:/
--
-- > table-reference = table-name;
newtype TableReference = TableReference TableName
    deriving Show

-- | /EBNF:/
--
-- > table-reference-list = table-reference,  { ',', table-reference };    
newtype TableReferenceList = TableReferenceList (NonEmpty TableReference)
    deriving Show

-- | /EBNF:/
--
-- > sort-ordering = 'ASC' | 'DESC' ;
data SortOrdering
    = Ascending
    | Descending
    deriving Show

-- | /EBNF:/
--
-- > sort-specification = (unsigned-integer | column-name), [ sort-ordering ];
data SortSpecification
    = SortByColumnPosition UnsignedInteger (Maybe SortOrdering)
    | SortByColumnName ColumnName (Maybe SortOrdering)
    deriving Show

-- | /EBNF:/
--
-- > order-by-clause = 'ORDER', 'BY', sort-specification, { ',', sort-specification };
newtype OrderByClause = OrderByClause (NonEmpty SortSpecification)
    deriving Show

-- | /EBNF:/
--
-- > data-type = 'char', '(', unsigned-integer, ')'
-- >           | 'varchar', '(', unsigned-integer, ')'
-- >           ;    
data DataType
    = Char UnsignedInteger
    | VarChar UnsignedInteger
    deriving Show

-- | /EBNF:/
--
-- > expression = term, [ expression-op ];
data Expression = Expression Term (Maybe ExpressionOp)
    deriving Show

-- | /EBNF:/
--
-- > expression-op = sign, expression;
data ExpressionOp = ExpressionOp Sign Expression
    deriving Show

-- | /EBNF:/
--
-- > factor = [ sign ], primary;
data Factor = Factor (Maybe Sign) Primary
    deriving Show    

-- | /EBNF:/
--
-- > term = factor, [ term-op ]
data Term = Term Factor (Maybe TermOp)
    deriving Show

-- | /EBNF:/
--
-- > term-op = multiplication-operator, term
data TermOp = TermOp MultiplicationOperator Term
    deriving Show

-- | /EBNF:/
--
-- > primary = column-name | dynamic-parameter | literal | '(', expression, ')';    
data Primary
    = PrimaryColumnName ColumnName
    | PrimaryDynamicParameter DynamicParameter
    | PrimaryLiteral Literal
    | PrimaryExpression Expression
    deriving Show

-- | /EBNF:/
--
-- > select-sublist = expression;
newtype SelectSubList = SelectSubList Expression
    deriving Show

-- | /EBNF:/
--
-- > (* select-list cannot contain parameters *)
-- > select-list = '*'
-- >             | select-sublist, { ',', select-sublist }
-- >             ;
data SelectList
    = SelectAll
    | SelectList (NonEmpty SelectSubList)
    deriving Show

-- | /EBNF:/
--
-- > insert-value = dynamic-parameter | literal | 'NULL' | 'USER' ;
data InsertValue
    = InsertDynamicParameter DynamicParameter
    | InsertLiteral Literal
    | InsertNull
    | InsertUser
    deriving Show

-- | /EBNF:/
--
-- > comparison-predicate = expression, comparison-operator, expression;
data ComparisonPredicate
    = ComparisonPredicate Expression ComparisonOperator Expression
    deriving Show

-- | /EBNF:/
--
-- > not = 'not';
data Not = Not
    deriving Show

-- | /EBNF:/
--
-- > boolean-factor = [ not ], boolean-primary;
data BooleanFactor = BooleanFactor (Maybe Not) BooleanPrimary
    deriving Show

-- | /EBNF:/
--
-- > boolean-primary = comparison-predicate
-- >                 | '(', search-condition, ')'
-- >                 ;
data BooleanPrimary
    = BooleanPrimaryComparison ComparisonPredicate
    | BooleanPrimarySearch SearchCondition
    deriving Show

-- | /EBNF:/
--
-- > boolean-and = 'and', boolean-term;
data BooleanAnd = BooleanAnd BooleanTerm
    deriving Show

-- | /EBNF:/
--
-- > boolean-term = boolean-factor, [ boolean-and ];
data BooleanTerm
    = BooleanTerm BooleanFactor (Maybe BooleanAnd)
    deriving Show

-- | /EBNF:/
--
-- > boolean-or = 'or', search-condition;
data BooleanOr = BooleanOr SearchCondition
    deriving Show

-- | /EBNF:/
--
-- > search-condition = boolean-term, [ boolean-or ];
data SearchCondition = SearchCondition BooleanTerm (Maybe BooleanOr)
    deriving Show

-- | /EBNF:/
--
-- > distinct = 'DISTINCT';
data Distinct = Distinct
    deriving Show

-- | /EBNF:/
--
-- > column-and-type = column-identifier, data-type;
data ColumnAndType = ColumnAndType ColumnIdentifier DataType
    deriving Show

-- | /EBNF:/
--
-- > create-table-statement = 'CREATE', 'TABLE', base-table-name, '(',
-- >                              column-and-type, { ',', column-and-type }
-- >                          ')'
data CreateTable = CreateTable BaseTableName (NonEmpty ColumnAndType)
    deriving Show

-- | /EBNF:/
--
-- > drop-table-statement = 'DROP', 'TABLE', base-table-name;
data Drop = Drop BaseTableName
    deriving Show

-- | /EBNF:/
--
-- > select-statement = 'SELECT', [ distinct ], select-list,
-- >                    'FROM' table-reference-list,
-- >                    ['WHERE' search-condition],
-- >                    [order-by-clause];
data Select
    = Select (Maybe Distinct) SelectList
      TableReferenceList 
      (Maybe SearchCondition)
      (Maybe OrderByClause)
    deriving Show

-- | /EBNF:/
--
-- > delete-statement-searched = 'DELETE', 'FROM', table-name
-- >                           , [ 'WHERE', search-condition ];
data Delete = Delete TableName (Maybe SearchCondition)
    deriving Show

-- | Type alias for:
--   /EBNF:/
-- 
-- > column-identifier, { ',', column-identifier }
type ColumnIdList = NonEmpty ColumnIdentifier

-- | Type alias for:
--   /EBNF:/
-- 
-- > insert-value, { ',', insert-value }
type InsertValueList = NonEmpty InsertValue

-- | /EBNF:/
--
-- > insert-statement = 'INSERT', 'INTO', table-name
-- >                  , [ '(', column-identifier, { ',', column-identifier } , ')' ],
-- >                  , 'VALUES', '(', insert-value, { ',', insert-value } ')'
data Insert
    = Insert TableName (Maybe ColumnIdList) InsertValueList
    deriving Show

-- | /EBNF:/
--
-- > update-value = expression | 'NULL';
data UpdateValue
    = UpdateValueExpression Expression
    | UpdateValueNull
    deriving Show

-- | /EBNF:/
--
-- > update-column = column-identifier, '=', update-value;
data UpdateColumn = UpdateColumn ColumnIdentifier UpdateValue
    deriving Show

-- | /EBNF:/
--
-- > update-column-list = update-column, { ',', update-column };
type UpdateColumnList = NonEmpty UpdateColumn

-- | /EBNF:/
--
-- > update-statement-searched = 'UPDATE', table-name
-- >                           , 'SET', update-column-list
-- >                           , [ 'WHERE', search-condition ]
-- >                           ;
data Update = Update TableName UpdateColumnList (Maybe SearchCondition)
    deriving Show

-- | /EBNF:/
--
-- > statement = create-table-statement
-- >           | delete-statement-searched
-- >           | drop-table-statement
-- >           | insert-statement
-- >           | select-statement
-- >           | update-statement-searched
-- >           ;
data Statement
    = CreateStatement CreateTable
    | DeleteStatement Delete
    | DropStatement Drop
    | InsertStatement Insert
    | SelectStatement Select
    | UpdateStatement Update
    deriving Show

-- | /EBNF:/
--
-- > statement-list = statenment, { ';', statement };
newtype StatementList = StatementList (NonEmpty Statement)
    deriving Show