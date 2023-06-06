fun lookup "bogus" = 10000
  | lookup s = 0

%%

%eop EOF

(* %pos declares the type of positions for terminals.
   Each symbol has an associated left and right position. *)

%pos int

%right ASSIGN
%left ANDALSO ORELSE
%right EQUALS NOTEQUALS 
%right LESSTHAN LESSTHANEQUALS GREATERTHAN GREATERTHANEQUALS
%left SUB PLUS
%left TIMES DIV
%left INT_PLUS INT_SUB
%left INT_TIMES INT_DIV INT_MOD
%right UNMINUS NOT INVERSE

(* | NUM of BigInt.bigint *)

%term ID of string 
| UNMINUS 
| PLUS 
| TIMES 
| DIV 
| SUB 
| INT_PLUS 
| INT_TIMES 
| INT_MOD 
| INT_DIV 
| INT_SUB 
| PRINT 
| SEMI 
| EOF 
| RFRAC of string 
| RDEC of string
| INVERSE
| MAKE_RAT
| RAT 
| SHOWRAT
| SHOWDECIMAL
| FROMDECIMAL 
| TODECIMAL
| RINT of string
| TRUE
| FALSE
| NOT 
| ANDALSO 
| ORELSE 
| EQUALS 
| NOTEQUALS 
| LESSTHAN 
| LESSTHANEQUALS 
| GREATERTHAN 
| GREATERTHANEQUALS 
| ASSIGN 
| LPAREN 
| RPAREN 
| LBRACE 
| RBRACE 
| COMMA 
| RATIONAL 
| INTEGER 
| BOOLEAN 
| VAR 
| IF 
| THEN 
| ELSE 
| FI 
| WHILE 
| DO 
| OD 
| PROCEDURE 
| READ 
| CALL


%nonterm PROGRAM of program
| BLOCK of block
| DECLARATIONSEQ of declarationseq
| COMMANDSEQ of cmd list
| VARDECLS of vardecls
| PROCDECLS of procdef list
| RATVARDECLS of decl
| INTVARDECLS of decl
| BOOLVARDECLS of decl
| PROCDEF of procdef
| COMMAND of cmd
| EXP of exp
| IDLIST of string list
| COMMANDLIST of cmd list

%name pl0rat

(*%subst PRINT for ID*)
(* %prefer UNMINUS INVERSE INT_TIMES INT_DIV INT_MOD INT_PLUS INT_SUB TIMES DIV PLUS SUB NOT ANDALSO ORELSE LESSTHAN LESSTHANEQUALS GREATERTHAN GREATERTHANEQUALS EQUALS NOTEQUALS *)
%keyword PRINT SEMI RATIONAL INTEGER BOOLEAN TRUE FALSE IF THEN ELSE FI WHILE DO OD PROCEDURE READ CALL MAKE_RAT RAT SHOWRAT SHOWDECIMAL FROMDECIMAL TODECIMAL

%noshift EOF
(*%value ID ("bogus")*)
%nodefault
%start PROGRAM
%verbose
%%
  PROGRAM         : BLOCK (programAST(BLOCK))
  BLOCK           : DECLARATIONSEQ COMMANDSEQ (block(DECLARATIONSEQ,COMMANDSEQ))
  DECLARATIONSEQ  : VARDECLS PROCDECLS (declarationseq(VARDECLS,PROCDECLS))
  VARDECLS        : RATVARDECLS INTVARDECLS BOOLVARDECLS (vardecls(RATVARDECLS,INTVARDECLS,BOOLVARDECLS))
  RATVARDECLS     : RATIONAL IDLIST SEMI (ratvardecls(IDLIST))
                    | (ratvardecls([]))
  INTVARDECLS     : INTEGER IDLIST SEMI (intvardecls(IDLIST))
                    | (intvardecls([]))
  BOOLVARDECLS    : BOOLEAN IDLIST SEMI (boolvardecls(IDLIST))
                    | (boolvardecls([]))
  IDLIST          : ID COMMA IDLIST (ID::IDLIST)
                    | ID ([ID])
  PROCDECLS       : PROCDEF SEMI PROCDECLS (PROCDEF::PROCDECLS)
                    | ([])
  PROCDEF         : PROCEDURE ID BLOCK (procdef(ID,BLOCK))
  COMMANDSEQ      : LBRACE COMMANDLIST RBRACE (COMMANDLIST)
  COMMANDLIST     : COMMAND SEMI COMMANDLIST (COMMAND::COMMANDLIST)
                    | ([])
  COMMAND         : ID ASSIGN EXP (assignmentcmd(ID,EXP))
                    | CALL ID (callcmd(ID))
                    | READ LPAREN ID RPAREN (readcmd(ID))
                    | PRINT LPAREN EXP RPAREN (printcmd(EXP))
                    | IF EXP THEN COMMANDSEQ ELSE COMMANDSEQ FI (conditionalcmd(EXP,COMMANDSEQ1,COMMANDSEQ2))
                    | WHILE EXP DO COMMANDSEQ OD (whilecmd(EXP,COMMANDSEQ))
                    | SHOWRAT LPAREN EXP RPAREN (showratcmd(EXP))
                    | SHOWDECIMAL LPAREN EXP RPAREN (showdecimalcmd(EXP))
                    

  EXP          : RINT (intval(BigInt.int_make_bigint(RINT)))
                    (* | PLUS EXP           (EXP) *)
                    | UNMINUS EXP            (unexp(unminus,EXP))
                    | EXP INT_PLUS EXP    (binexp(int_add,EXP1,EXP2))
                    | EXP INT_TIMES EXP   (binexp(int_mul,EXP1,EXP2))
                    | EXP INT_DIV EXP     (binexp(int_intdiv,EXP1,EXP2))
                    | EXP INT_SUB EXP     (binexp(int_sub,EXP1,EXP2))
                    | EXP INT_MOD EXP     (binexp(int_mod,EXP1,EXP2))
                    

  EXP         : RFRAC           (ratval(Rational.from_frac(RFRAC)))
                    | RDEC            (ratval(Rational.fromDecimal(RDEC)))
                    (* | PLUS EXP           (EXP) *)
                    | EXP PLUS EXP    (binexp(add,EXP1,EXP2))
                    | EXP TIMES EXP   (binexp(multiply,EXP1,EXP2))
                    | EXP DIV EXP     (binexp(divide,EXP1,EXP2))    (*remember to do valOf in the AST*)
                    | EXP SUB EXP     (binexp(subtract,EXP1,EXP2)) 
                    | INVERSE EXP        (unexp(inverse,EXP))
                    | MAKE_RAT LPAREN EXP COMMA EXP RPAREN        (binexp(make_rat,EXP,EXP))
                    | FROMDECIMAL LPAREN EXP RPAREN               (EXP)

  EXP         : TRUE (boolval(true))
                    | FALSE (boolval(false))
                    | EXP EQUALS EXP (binexp(equal,EXP1,EXP2))
                    | EXP NOTEQUALS EXP (binexp(neq,EXP1,EXP2))
                    | EXP LESSTHAN EXP (binexp(less,EXP1,EXP2))
                    | EXP GREATERTHANEQUALS EXP (binexp(geq,EXP1,EXP2))
                    | EXP LESSTHANEQUALS EXP (binexp(leq,EXP1,EXP2))
                    | EXP GREATERTHAN EXP (binexp(greater,EXP1,EXP2))
                    
                    
                    | EXP ANDALSO EXP (binexp(AND,EXP1,EXP2))
                    | EXP ORELSE EXP (binexp(OR,EXP1,EXP2))
                    | NOT EXP (unexp(NOT,EXP))

                    | LPAREN EXP RPAREN (EXP)

                    | ID (id(ID))

                    (* | INT_PLUS EXP (EXP) *)
