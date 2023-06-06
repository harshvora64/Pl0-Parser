structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val nesting = ref 0;
val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
val error = fn (e,l : int,_) =>
              print("line " ^ Int.toString(l) ^
                               ": " ^ e ^ "\n")
%%

%s COMMENT;

%header (functor pl0ratLexFun(structure Tokens: pl0rat_TOKENS));
alpha=[A-Za-z];
alphanum=[A-Za-z0-9];
id = [A-Za-z][A-Za-z0-9]*;
digit=[0-9];
ws = [\ \t];
RInt = [~]?[0-9]+;
RFrac = [~]?[0-9]+"/"[0]*[1-9][0-9]*;
RDec = [~]?[0-9]*"."[0-9]*"("[0-9]+")";


%%
<INITIAL>\n       => (pos := (!pos) + 1; lex());
<INITIAL>{ws}+    => (lex());
<INITIAL>";"      => (Tokens.SEMI(!pos,!pos));
<INITIAL>{RFrac} => (Tokens.RFRAC(yytext,!pos,!pos));
<INITIAL>{RDec} => (Tokens.RDEC(yytext,!pos,!pos));

<INITIAL>".+."      => (Tokens.PLUS(!pos,!pos));
<INITIAL>".*."      => (Tokens.TIMES(!pos,!pos));
<INITIAL>".-."      => (Tokens.SUB(!pos,!pos));
<INITIAL>"./."      => (Tokens.DIV(!pos,!pos));

<INITIAL>"~"      => (Tokens.UNMINUS(!pos,!pos));

<INITIAL>"+"      => (Tokens.INT_PLUS(!pos,!pos));
<INITIAL>"*"      => (Tokens.INT_TIMES(!pos,!pos));
<INITIAL>"-"      => (Tokens.INT_SUB(!pos,!pos));
<INITIAL>"/"      => (Tokens.INT_DIV(!pos,!pos));
<INITIAL>"%"      => (Tokens.INT_MOD(!pos,!pos));

<INITIAL>"inverse" => (Tokens.INVERSE(!pos,!pos));
<INITIAL>"make_rat" => (Tokens.MAKE_RAT(!pos,!pos));
<INITIAL>"rat" => (Tokens.RAT(!pos,!pos));
<INITIAL>"showRat" => (Tokens.SHOWRAT(!pos,!pos));
<INITIAL>"showDecimal" => (Tokens.SHOWDECIMAL(!pos,!pos));
<INITIAL>"fromDecimal" => (Tokens.FROMDECIMAL(!pos,!pos));
<INITIAL>"toDecimal" => (Tokens.TODECIMAL(!pos,!pos));
<INITIAL>{RInt} => (Tokens.RINT(yytext,!pos,!pos));
<INITIAL>"!" => (Tokens.NOT(!pos,!pos));
<INITIAL>"&&" => (Tokens.ANDALSO(!pos,!pos));
<INITIAL>"||" => (Tokens.ORELSE(!pos,!pos));
<INITIAL>"=" => (Tokens.EQUALS(!pos,!pos));
<INITIAL>"<>" => (Tokens.NOTEQUALS(!pos,!pos));
<INITIAL>"<" => (Tokens.LESSTHAN(!pos,!pos));
<INITIAL>"<=" => (Tokens.LESSTHANEQUALS(!pos,!pos));
<INITIAL>">" => (Tokens.GREATERTHAN(!pos,!pos));
<INITIAL>">=" => (Tokens.GREATERTHANEQUALS(!pos,!pos));
<INITIAL>":=" => (Tokens.ASSIGN(!pos,!pos));
<INITIAL>"(" => (Tokens.LPAREN(!pos,!pos));
<INITIAL>")" => (Tokens.RPAREN(!pos,!pos));
<INITIAL>"{" => (Tokens.LBRACE(!pos,!pos));
<INITIAL>"}" => (Tokens.RBRACE(!pos,!pos));
<INITIAL>"," => (Tokens.COMMA(!pos,!pos));
<INITIAL>"(*" => (YYBEGIN COMMENT; nesting := 0; continue());
<COMMENT>"(*" => (nesting := !nesting + 1; continue());
<COMMENT>. => (continue());
<COMMENT>\n => (continue());
<COMMENT>"*)" => (if !nesting = 0 then (YYBEGIN INITIAL;continue()) else (nesting := !nesting -1;continue()));
<INITIAL>"rational" => (Tokens.RATIONAL(!pos,!pos));
<INITIAL>"integer" => (Tokens.INTEGER(!pos,!pos));
<INITIAL>"boolean" => (Tokens.BOOLEAN(!pos,!pos));
<INITIAL>"var" => (Tokens.VAR(!pos,!pos));
<INITIAL>"if" => (Tokens.IF(!pos,!pos));
<INITIAL>"then" => (Tokens.THEN(!pos,!pos));
<INITIAL>"else" => (Tokens.ELSE(!pos,!pos));
<INITIAL>"fi" => (Tokens.FI(!pos,!pos));
<INITIAL>"while" => (Tokens.WHILE(!pos,!pos));
<INITIAL>"do" => (Tokens.DO(!pos,!pos));
<INITIAL>"od" => (Tokens.OD(!pos,!pos));
<INITIAL>"tt" => (Tokens.TRUE(!pos,!pos));
<INITIAL>"ff" => (Tokens.FALSE(!pos,!pos));
<INITIAL>"procedure" => (Tokens.PROCEDURE(!pos,!pos));
<INITIAL>"print" => (Tokens.PRINT(!pos,!pos));
<INITIAL>"read" => (Tokens.READ(!pos,!pos));
<INITIAL>"call" => (Tokens.CALL(!pos,!pos));
<INITIAL>{id} => (Tokens.ID(yytext,!pos,!pos));

