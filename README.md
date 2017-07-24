# SQL Parser
Parser for the minimum SQL syntax that an ODBC driver must support.

## Requirements
1. [Haskell Stack](www.haskellstack.org)
2. [git](https://git-scm.com/) *optional*

## Download the source files

Without git: Donwload and extract the repository

With git: `$ git clone https://github.com/jean-lopes/sql-parser.git`

## Build

```
$ cd sql-parser
$ stack setup
$ stack build --haddock
```

## Testing
### Loading the library with the GHC’s interactive environment (GHCi)
```
$ cd sql-parser
$ stack exec ghci
>>> Prelude> :cd src
>>> Prelude> :load Language.Sql
```

### Parsing a SQL Statement string into the Abstract Syntax Tree (AST)
```
>>> *Language.Sql> fromSql "select * from test"
Right (StatementList (SelectStatement (Select Nothing SelectAll (TableReferenceList (TableReference (TableName (TableIdentifier (UserDefinedName "test"))) :| [])) Nothing Nothing) :| []))
```

### Printing the AST in a more readable way
```
*Language.Sql> ast "select * from test"
statement-list
|
`- statement
   |
   `- select-statement
      |
      +- select-list
      |  |
      |  `- *
      |
      `- table-reference-list
         |
         `- table-reference
            |
            `- table-name
               |
               `- table-identifier
                  |
                  `- user-defined-name
                     |
                     `- test
```

### Converting an AST to SQL
```
>>> *Language.Sql> putStrLn . toSql $ Literal "test"
'test'
```

### Documentation
https://jean-lopes.github.io/sql-parser/
