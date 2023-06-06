structure AST = struct
    datatype uop = unminus | inverse | (* UNPLUS ?? *) NOT
    and bop = int_add | int_sub | int_mul | int_intdiv | int_mod | add | multiply | divide | subtract | equal | less | greater | leq | geq | neq | AND | OR | BOOLEQ | make_rat
    and typ = RationalType | IntegerType | BooleanType
    and exp = binexp of bop * exp * exp | unexp of uop * exp | intval of BigInt.bigint | ratval of Rational.rational | boolval of bool | id of string
    and cmd = assignmentcmd of string * exp | callcmd of string | readcmd of string | printcmd of exp | conditionalcmd of exp * cmd list * cmd list | whilecmd of exp * cmd list | showratcmd of exp | showdecimalcmd of exp
    and procdef = procdef of string * block
    and decl = ratvardecls of string list | intvardecls of string list | boolvardecls of string list
    and vardecls = vardecls of decl * decl * decl
    and declarationseq = declarationseq of vardecls * procdef list
    and block = block of declarationseq * cmd list
    and program = programAST of block

end;