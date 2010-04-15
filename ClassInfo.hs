module ClassInfo where

type Name = String

data Implements = Implements [ String ] 
                    | NoInterfaces
                    deriving ( Show )

data Extends    = Extends String
                    | NoSuper
                    deriving ( Show )

data MemberList = MemberList [ String ]
                    | NoMembers
                    deriving ( Show )

data ClassInfo = ClassInfo {
        name                :: Name,
        implements          :: Implements,
        extends             :: Extends,
        publicProperties    :: MemberList,
        protectedProperties :: MemberList,
        privateProperties   :: MemberList,
        publicMethods       :: MemberList,
        protectedMethods    :: MemberList,
        privateMethods      :: MemberList

    } deriving ( Show )

