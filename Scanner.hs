module Scanner where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language ( emptyDef )

as3Def :: P.LanguageDef st
as3Def =
    emptyDef
    { P.commentStart = "/*"
    , P.commentEnd = "*/"
    , P.commentLine = "//"
    , P.nestedComments = False
    , P.identStart = letter <|> char '_'
    , P.identLetter = alphaNum <|> char '_'
    , P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.reservedNames =
        [
            "as", "break", "case", "catch", "class", "const", "continue", "default",
            "delete", "do", "else", "extends", "false", "finally", "for", "function",
            "if", "implements", "import", "in", "instanceof", "interface", "internal",
            "is", "native", "new", "null", "package", "private", "protected", "public",
            "return", "super", "switch", "this", "throw", "to", "true", "try", "typeof",
            "use", "var", "void", "while", "with", "each", "get", "set", "namespace",
            "include", "dynamic", "final", "native", "override", "static"
        ]
    , P.reservedOpNames = 
        [ 
            "{", "}", "[", "]", ":", "(", ")", ".", "::", "..", "@", "<", ">", "</",
            "/>", "++", "--", "+", "-", "~", "!", "*", "/", "%", "<<", ">>", ">>>",
            "<=", ">=", "==", "!=", "===", "!==", "&", "^", "|", "&&", "||", "?",
            ",", "=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", ">>>=", "&=", "^=",
            "|="
        ]
    , P.caseSensitive = True
    }


lexer :: P.TokenParser ()
lexer = P.makeTokenParser as3Def

parens          = P.parens lexer
braces          = P.braces lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer
whiteSpace      = P.whiteSpace lexer
reservedOp      = P.reservedOp lexer


