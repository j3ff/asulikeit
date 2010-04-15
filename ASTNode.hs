module ASTNode where

{-
 - This data type is essentially an enumeration of the different node
 - types in the abstract syntax tree.  
 -}
data NodeType = Root
                | Package
                | PackageName String
                | PackageBlock
                | Import
                | Class
                deriving ( Show )

data ASTNode = 
    ASTNode { nodeType        :: NodeType
            , lexeme          :: String }
    deriving ( Show )
         

