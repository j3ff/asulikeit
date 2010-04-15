module AS3Parser where

import Text.ParserCombinators.Parsec
import Data.Tree
import Scanner
import ASTNode



parseAS3 :: String -> Either ParseError [String]
parseAS3 input = parse as3 "(unknown)" input

as3 :: Parser Tree ASTNode
as3 = 
    do
        whiteSpace
        package_tree <- packageDecl
        return ( Node ( ASTNode Root "" ) [ Node package_tree [] ] )


-- packageDecl : PACKAGE ( identifier )? packageBlock ;
packageDecl =
    do
        reserved "package"
        package_name <- packageName
        package_block <- packageBlock
        return ( Node ( ASTNode Package "package" ) [  ] )
        return ( "package" : ( package_name ++ package_block ) )


--packageBlock : LCURLY ( packageBlockEntry )* RCURLY ;
packageBlock =
    do
        entries <- braces ( many packageBlockEntry )
        return ( [ "{" ] ++ ( concat entries ) ++ [ "}"] )

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
    do { toks <- try importDefinition
       ; return toks }
    <|>  
    do { toks <- try classDefinition
       ; return toks }

packageNameSeparator = reservedOp "."

packageName =
    do 
        segs <- sepBy identifier packageNameSeparator
        return segs

-- identifierStar -> ident ( DOT ident )* ( DOT STAR )?
-- Not handling the .* bit as yet.
importDefinition =
    do { reserved "import"
       ; ids <- sepBy1 identifier packageNameSeparator
       ; reservedOp ";"
       ; return ( [ "import" ] ++ ids ) }

classDefinition = do { reserved "class" ; return ( [ "class" ] ) }


