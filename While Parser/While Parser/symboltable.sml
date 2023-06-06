use "ast.sml";
open AST;
signature SymbolTable =
sig
type symboltable
exception SymbolTableError
(* val initialise: symboltable *)
val exists: symboltable * string * int list -> int option
val Type: symboltable * string * int -> typ
val value: symboltable * string * int -> exp
val typematch: symboltable * string * int * exp -> bool
val update: symboltable * string * int * exp -> symboltable
val insert: symboltable * typ * string list * int -> symboltable
end;

structure SYMBOLTABLE : SymbolTable = struct
    type symboltable = (string * int * typ * exp option) list 
    exception SymbolTableError
    
    (* fun initialise() = [] *)

    fun look (s:string,scope:int) symbol = s = #1(symbol:(string * int * typ * exp option)) andalso scope = #2(symbol)

    fun get(vars:(string * int * typ * exp option) list ,s,scope) = List.find (look (s,scope)) vars

    fun exists(vars,s,scopelist) = 
        if null(scopelist) then NONE else
        case get(vars,s,hd(scopelist)) of
            SOME symbol => SOME (#2 symbol)
            | NONE => exists(vars,s,tl(scopelist))

    fun Type(vars,s, scope) = 
        case get(vars,s,scope) of
            SOME symbol => #3 (symbol)
            | NONE => raise (Fail("Variable not declared"))

    fun value(vars,s, scope) = 
        case get(vars,s,scope) of
            SOME symbol => ( case (#4 (symbol)) of SOME v => v | NONE => raise (Fail("Variable not initialised")) )
            | NONE => raise (Fail("Variable not declared"))

    fun typematch(vars,s,scope,ex) =
        case (Type(vars,s,scope)) of
            RationalType => (case ex of ratval(v1) => true | _ => false)
            | IntegerType => (case ex of intval(v1) => true | _ => false)
            | BooleanType => (case ex of boolval(v1) => true | _ => false)


    fun updatehelp(vars,s,scope,ex,rest) = 
        if null(vars) then [] else
        let val symbol = hd(vars)
        in
            if (look (s,scope) symbol) then 
            let 
                val new_symbol = ( (#1 symbol), (#2 symbol), (#3 symbol), SOME ex)
            in 
                (rest) @ (new_symbol::tl(vars))
            end
            else updatehelp(tl(vars),s,scope,ex,rest@[hd(vars)])
        end

    fun update(vars,s,scope,ex) = updatehelp(vars,s,scope,ex,[])

    fun insert(vars, tp, lst, scope) = 
    if null(lst) then vars
    else 
        insert((hd(lst),scope,tp,NONE)::vars,tp,tl(lst),scope)




    (* if already exists, and reinitialised, then must also change the value to default*)
    
end
    
signature ScopeTable =
sig
type scopetable
exception ScopeTableError
(* val initialise: scopetable *)
val newscopenum: int * ((string * block * int * int list) list) -> int * (int * ((string * block * int * int list) list))
val exists: scopetable * string * int list -> (block * (int list)) option
val insert: (int * ((string * block * int * int list) list)) * procdef list * int * (int list) -> (int * ((string * block * int * int list) list))
end;

structure SCOPETABLE : ScopeTable = struct
    type scopetable = (int * ((string * block * int * int list) list))
    
    exception ScopeTableError

    (* fun initialise() =  (0,[]) *)

    fun newscopenum(procs:int * ((string * block * int * int list) list)) = ((#1 procs) , (((#1 procs)+1), (#2 procs)))

    fun look (s:string,scope:int) symbol = s = #1(symbol:(string * block * int * int list)) andalso scope = #3(symbol)

    fun get(proclist,s,scope) = List.find (look (s,scope)) proclist

    fun existshelp(proclist,s,scopelist) = 
        if null(scopelist) then NONE else
        case get(proclist,s,hd(scopelist)) of
            SOME symbol => SOME (#2 symbol, #4 symbol)
            | NONE => existshelp(proclist,s,tl(scopelist))

    fun exists(procs:int * ((string * block * int * int list) list),s:string,scopelist:int list) = existshelp((#2 procs),s,scopelist)

    fun inserthelp(proclist, pl:procdef list, scope, scopelist) = 
    if null(pl) then proclist
    else 
        case hd(pl) of procdef(s:string, blk:block) => 

            inserthelp((s,blk,scope,scopelist)::proclist,tl(pl),scope,scopelist)

    fun insert(procs:(int * ((string * block * int * int list) list)),pl: procdef list,scope:int,scopelist:int list) = ( (#1 procs) , inserthelp((#2 procs), pl, scope, scopelist) )





end