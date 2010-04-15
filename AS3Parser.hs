module AS3Parser where

import Text.ParserCombinators.Parsec
import Scanner



parseAS3 :: String -> Either ParseError [String]
parseAS3 input = parse as3 "(unknown)" input

as3 :: Parser [ String ]
as3 = 
    do
        whiteSpace
        toks <- packageDecl
        return toks


-- packageDecl : PACKAGE ( identifier )? packageBlock ;
packageDecl =
    do
        reserved "package"
        package_name <- packageName
        package_block <- packageBlock
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
       ; return toks
       }
    <|>  
    do { toks <- try classDefinition
       ; return toks
       }

packageNameSeparator = reservedOp "."

packageName =
    do 
        segs <- sepBy identifier packageNameSeparator
        return segs

identifiers :: Parser [ String ]
identifiers =
    do 
        ids <- many identifier
        return ids

-- identifierStar -> ident ( DOT ident )* ( DOT STAR )?
idStar = 
    do
        id <- identifier 
        more_ids <- moreIds
        return ( id : more_ids )

moreIds =
    do { reservedOp "." ; star <- reservedOp "*" ; return ( [ "*" ] ) }
    <|>
    do { reservedOp "." ; ids <- idStar ; return ids }
    <|>
    do { reservedOp ";" ; return ( [ ";" ] ) }


importDefinition = do { reserved "import" ; ids <- idStar ; return ( [ "import" ] ++ ids ) }

classDefinition = do { reserved "class" ; return ( [ "class" ] ) }


