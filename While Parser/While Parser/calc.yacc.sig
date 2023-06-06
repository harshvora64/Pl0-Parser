signature pl0rat_TOKENS =
sig
type ('a,'b) token
type svalue
val CALL:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val PROCEDURE:  'a * 'a -> (svalue,'a) token
val OD:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val FI:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val BOOLEAN:  'a * 'a -> (svalue,'a) token
val INTEGER:  'a * 'a -> (svalue,'a) token
val RATIONAL:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val GREATERTHANEQUALS:  'a * 'a -> (svalue,'a) token
val GREATERTHAN:  'a * 'a -> (svalue,'a) token
val LESSTHANEQUALS:  'a * 'a -> (svalue,'a) token
val LESSTHAN:  'a * 'a -> (svalue,'a) token
val NOTEQUALS:  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val ORELSE:  'a * 'a -> (svalue,'a) token
val ANDALSO:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val RINT: (string) *  'a * 'a -> (svalue,'a) token
val TODECIMAL:  'a * 'a -> (svalue,'a) token
val FROMDECIMAL:  'a * 'a -> (svalue,'a) token
val SHOWDECIMAL:  'a * 'a -> (svalue,'a) token
val SHOWRAT:  'a * 'a -> (svalue,'a) token
val RAT:  'a * 'a -> (svalue,'a) token
val MAKE_RAT:  'a * 'a -> (svalue,'a) token
val INVERSE:  'a * 'a -> (svalue,'a) token
val RDEC: (string) *  'a * 'a -> (svalue,'a) token
val RFRAC: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val INT_SUB:  'a * 'a -> (svalue,'a) token
val INT_DIV:  'a * 'a -> (svalue,'a) token
val INT_MOD:  'a * 'a -> (svalue,'a) token
val INT_TIMES:  'a * 'a -> (svalue,'a) token
val INT_PLUS:  'a * 'a -> (svalue,'a) token
val SUB:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val UNMINUS:  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
end
signature pl0rat_LRVALS=
sig
structure Tokens : pl0rat_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
