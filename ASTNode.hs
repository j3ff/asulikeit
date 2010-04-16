module ASTNode where

data ClassDeclMod = PublicClass
                  | DynamicClass
                  | FinalClass
                  | InternalClass
                  deriving ( Show )

type ClassDeclModList = [ ClassDeclMod ]


data PropDeclMod = StaticMod
                 | PublicMod
                 | ProtectedMod
                 | PrivateMod
                 | InternalMod
                 deriving ( Show )

type PropDeclModList = [ PropDeclMod ]

{-
 - This data type is essentially an enumeration of the different node
 - types in the abstract syntax tree.  
 -}
data NodeType = Root
              | Package
              | PackageName String
              | PackageBlock
              | Import String
              | Class ClassDeclModList String
              | Extends String
              | Implements [ String ]
              | ClassBlock
              | VarDecl PropDeclModList String
              | ConstDecl PropDeclModList String
              | Dummy String
              deriving ( Show )

data ASTNode = 
    ASTNode { nodeType        :: NodeType
            , lexeme          :: String }
    deriving ( Show )
         

