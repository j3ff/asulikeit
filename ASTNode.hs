module ASTNode where

data NodeType = Root
                | Package
                | Import
                | Class
                deriving ( Show )

data ASTNode = 
    ASTNode { nodeType        :: NodeType
            , lexeme          :: String }
    deriving ( Show )
         

