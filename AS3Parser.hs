module AS3Parser where

import Text.ParserCombinators.Parsec
import Data.Tree
import Data.Maybe
import Scanner
import ASTNode

type AST = Tree ASTNode

printAST :: Either ParseError AST -> IO ()
printAST ( Right ast ) =
    do
        let ast_str = fmap show ast
        putStrLn ( drawTree ast_str )

printAST ( Left err ) = 
    do { let err_str = "There was a parse error so no AST was created!\n\n" ++ ( show err )
       ; putStrLn err_str } 
    


parseAS3 :: String -> Either ParseError AST
parseAS3 input = parse as3 "(unknown)" input

as3 :: Parser AST
as3 = 
    do
        whiteSpace
        package_tree <- packageDecl
        return ( Node ( ASTNode Root "" ) [ package_tree ] )


-- packageDecl : PACKAGE ( identifier )? packageBlock ;
packageDecl =
    do
        reserved "package"
        package_name <- packageName
        package_block <- packageBlock
        return ( Node ( ASTNode Package "package" ) [ package_name, package_block ] )


--packageBlock : LCURLY ( packageBlockEntry )* RCURLY ;
packageBlock =
    do
        entries <- braces ( many packageBlockEntry )
        return ( Node ( ASTNode PackageBlock "" ) entries )

--packageBlockEntry -> importDefinition 
--                   | classDefinition
--                   | interfaceDefinition
--                   | variableDefinition
--                   | methodDefinition
--                   | namespaceDefinition
--                   | useNamespaceDirective
--                   | SEMI ) ;
--
packageBlockEntry = 
    do { import_node <- try importDefinition
       ; return import_node }
    <|>  
    do { class_def_node <- try classDefinition
       ; return class_def_node }

packageNameSeparator = reservedOp "."

reassemblePackageName :: [ String ] -> String
reassemblePackageName =
    foldl1 ( concatAboutSep "." )


concatAboutSep :: String -> String -> String -> String
concatAboutSep sep ls rs = ls ++ sep ++ rs 

packageName =
    do 
        segs <- sepBy identifier packageNameSeparator
        let package_name = reassemblePackageName segs 
        return ( Node ( ASTNode ( PackageName package_name ) "" ) [] )

-- identifierStar -> ident ( DOT ident )* ( DOT STAR )?
-- Not handling the .* bit as yet.
importDefinition =
    do { reserved "import"
       ; ids <- sepBy1 identifier packageNameSeparator
       ; reservedOp ";"
       ; let package_name = reassemblePackageName ids
       ; return ( Node ( ASTNode ( Import package_name ) "" ) [] ) }

-- classDefinition: classDeclMod CLASS ident classExtendsClause implementsClause typeBlock 
classDefinition =
    do { mods <- many classDeclMod 
       ; reserved "class" 
       ; name <- identifier
       ; let node = Node ( ASTNode ( Class mods name ) "" ) []
       ; extended_class <- optClassExtendsClause
       ; let node' = addSuperClassInfo node extended_class
       ; implements_classes <- optImplementsClause
       ; let node'' = addImplementsInfo node' implements_classes
       ; class_blk_node <- classBlock
       ; let node''' = Node ( rootLabel node'' ) ( ( subForest node'' ) ++ [ class_blk_node ]  )
       ; return node''' }



-- classDeclMod : PUBLIC | FINAL | INTERNAL | DYNAMIC
classDeclMod =
    do { reserved "internal" ; return InternalClass }
    <|>
    do { reserved "public" ; return PublicClass }
    <|>
    do { reserved "dynamic" ; return DynamicClass }
    <|>
    do { reserved "final" ; return FinalClass }


-- classExtendsClause : ( EXTENDS identifier )? ;
optClassExtendsClause =
    option Nothing classExtendsClause

classExtendsClause =
    do { reserved "extends" ; id <- identifier ; return ( Just id ) }

addSuperClassInfo :: AST -> Maybe String -> AST
addSuperClassInfo t Nothing = t
addSuperClassInfo t ( Just s ) =
    let child = Node ( ASTNode ( Extends s ) "" ) [] in
    Node ( rootLabel t ) ( ( subForest t ) ++ [ child ]  )

-- implementsClause : ( IMPLEMENTS identifier ( COMMA identifier )* )? ;
optImplementsClause =
    option Nothing implementsClause

implementsClause =
    do { reserved "implements" ; ids <- sepBy1 identifier ( reservedOp "," ) ; return ( Just ids )  };

addImplementsInfo :: AST -> Maybe [ String ] -> AST
addImplementsInfo t Nothing = t
addImplementsInfo t ( Just ifaces ) =
    let child = Node ( ASTNode ( Implements ifaces ) "" ) [] in
    Node ( rootLabel t ) ( ( subForest t ) ++ [ child ]  )

-- classBlock : LCURLY ( classBlockEntry )* RCURLY
classBlock =
    do { entries <- braces ( many classBlockEntry )
       ; return ( Node ( ASTNode ClassBlock "" ) entries ) }


{-
 - classBlockEntry : variableDefinition
 -                 | methodDefinition
 -                 | importDefinition
 -}
classBlockEntry = 
    do { text <- dummyRule
       ; return ( Node ( ASTNode ( Dummy text ) "" ) [] ) } 


-- variableDefinition : varOrConst variableDeclarator ( COMMA variableDeclarator )* semi

-- varOrConst : ( VAR | CONST );
{-
varOrConst = 
    do { reserved "var"
       ; return () }
    <|>
    do { reserved "const"
       ; return () }
 -}


-- variableDeclarator : ident ( typeExpression )? ( variableInitializer )? ;
-- typeExpression : COLON ( identifier | VOID | STAR )
-- declaration : varOrConst variableDeclarator declarationTail ;
-- declarationTail : ( COMMA variableDeclarator )* ;



dummyRule =
    do { text <- identifier
       ; return text }


