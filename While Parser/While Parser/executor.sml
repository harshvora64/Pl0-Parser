structure Executor = struct

    fun execExp(vars, procs, ex: exp,scopelist: int list): exp = 
    case ex of 
        binexp(b,ex1,ex2) => exec_binary(vars,procs,b,ex1,ex2,scopelist) 
        | unexp(u,ex1) => exec_unary(vars,procs,u,ex1,scopelist) 
        | id(s) => ( case SYMBOLTABLE.exists(vars,s,scopelist) of 
                SOME scope => 
                    ( case SYMBOLTABLE.Type(vars,s,scope) of
                        RationalType => SYMBOLTABLE.value(vars,s,scope)
                        | IntegerType => SYMBOLTABLE.value(vars,s,scope)
                        | BooleanType => SYMBOLTABLE.value(vars,s,scope) )
                | NONE => raise (Fail ("Variable not declared")) )
        | ratval(v1) => ratval(v1)
        | intval(v1) => intval(v1)
        | boolval(v1) => boolval(v1)


    and exec_binary(vars, procs, b,ex1,ex2,scopelist:int list): exp =
    case (b,execExp(vars, procs, ex1,scopelist),execExp(vars,procs,ex2,scopelist)) of
        (int_add, intval(v1), intval(v2)) => intval(BigInt.int_add(v1,v2))
        | (int_sub, intval(v1), intval(v2)) => intval(BigInt.int_sub(v1,v2))
        | (int_mul, intval(v1), intval(v2)) => intval(BigInt.int_mul(v1,v2))
        | (int_intdiv, intval(v1), intval(v2)) => intval(BigInt.int_intdiv(v1,v2))
        | (int_mod, intval(v1), intval(v2)) => intval(BigInt.int_mod(v1,v2))
        | (add, ratval(v1), ratval(v2)) => ratval(Rational.add(v1,v2))
        | (multiply, ratval(v1), ratval(v2)) => ratval(Rational.multiply(v1,v2))
        | (divide, ratval(v1), ratval(v2)) => ratval(valOf(Rational.divide(v1,v2)))
        | (subtract, ratval(v1), ratval(v2)) => ratval(Rational.subtract(v1,v2))
        | (equal, intval(v1), intval(v2)) => boolval(BigInt.int_equal(v1,v2))
        | (less, intval(v1), intval(v2)) => boolval(BigInt.int_less(v1,v2))
        | (greater, intval(v1), intval(v2)) => boolval(BigInt.int_greater(v1,v2))
        | (leq, intval(v1), intval(v2)) => boolval(BigInt.int_leq(v1,v2))
        | (geq, intval(v1), intval(v2)) => boolval(BigInt.int_geq(v1,v2))
        | (neq, intval(v1), intval(v2)) => boolval(BigInt.int_neq(v1,v2))
        | (equal, ratval(v1), ratval(v2)) => boolval(Rational.equal(v1,v2))
        | (less, ratval(v1), ratval(v2)) => boolval(Rational.less(v1,v2))
        | (greater, ratval(v1), ratval(v2)) => boolval(Rational.greater(v1,v2))
        | (leq, ratval(v1), ratval(v2)) => boolval(Rational.leq(v1,v2))
        | (geq, ratval(v1), ratval(v2)) => boolval(Rational.geq(v1,v2))
        | (neq, ratval(v1), ratval(v2)) => boolval(Rational.neq(v1,v2))
        | (AND, boolval(v1), boolval(v2)) => boolval(v1 andalso v2)
        | (OR, boolval(v1), boolval(v2)) => boolval(v1 orelse v2) 
        | (equal, boolval(v1), boolval(v2)) => boolval(v1 = v2)
        | (MAKE_RAT,intval(v1),intval(v2)) => ratval(valOf(Rational.make_rat(v1,v2)))
        | _ => raise (Fail("Mismatched types"))


    and exec_unary(vars,procs,u:uop, ex1:exp, scopelist:int list): exp =
    case (u,execExp(vars,procs,ex1,scopelist)) of
        (unminus,ratval(v1)) => ratval(Rational.neg(v1))
        | (unminus,intval(v1)) => intval(BigInt.int_neg(v1))
        | (inverse,ratval(v1)) => ratval(valOf(Rational.inverse(v1)))
        | (NOT,boolval(v1)) => boolval(not v1)
        | _ => (raise (Fail("Mismatched types")))

    
    and execCmdList(vars,procs,cl:cmd list,scopelist:int list):(SYMBOLTABLE.symboltable * SCOPETABLE.scopetable * string) = 
    if null(cl) then (vars,procs,"\n") else
    case (hd(cl)) of
        assignmentcmd(id_name:string,ex:exp) => ( case SYMBOLTABLE.exists(vars,id_name,scopelist) of 
            SOME scope => if SYMBOLTABLE.typematch(vars,id_name,scope,execExp(vars,procs,ex,scopelist))
                        then 
                        let val vars_new = (SYMBOLTABLE.update(vars,id_name,scope,execExp(vars,procs,ex,scopelist)))
                        in execCmdList(vars_new,procs,tl(cl),scopelist)
                        end
                        else raise (Fail("Datatype of LHS are RHS is different"))
            | NONE => raise (Fail("Variable not declared")) )

        | callcmd(id_name:string) => let val (vars_new: SYMBOLTABLE.symboltable, procs_new: SCOPETABLE.scopetable,text:string) = 
                                        ( case SCOPETABLE.exists(procs,id_name,scopelist) of
                                        SOME (blk:block,scplst:int list) => (execBlock(vars,procs,blk,scplst))
                                        | NONE => raise (Fail("")) )
                                    in 
                                        let val (vars_new2: SYMBOLTABLE.symboltable, procs_new2: SCOPETABLE.scopetable,text2:string) = (execCmdList(vars_new,procs_new,tl(cl),scopelist))
                                        in (vars_new2,procs_new2,text^text2)
                                        end
                                    end

        |readcmd(id_name:string) => 
        let val s = valOf(TextIO.inputLine TextIO.stdIn)
        in 
            let val ex = strToVal(s)
            in
            ( case SYMBOLTABLE.exists(vars,id_name,scopelist) of 
                SOME scope => if SYMBOLTABLE.typematch(vars,id_name,scope,execExp(vars,procs,ex,scopelist))
                            then 
                            let val vars_new = (SYMBOLTABLE.update(vars,id_name,scope,execExp(vars,procs,ex,scopelist)))
                            in (execCmdList(vars_new,procs,tl(cl),scopelist))
                            end
                            else raise (Fail("Datatype of LHS are RHS is different"))
                | NONE => raise (Fail("Variable not declared")) )
            end
        end

        | printcmd(ex:exp) =>  let val s3 = ( case (execExp(vars,procs,ex,scopelist)) of intval(v1) => ((BigInt.int_show(v1))^"\n") | ratval(v1) => ((Rational.showDecimal(v1))^"\n") | boolval(v1) => ((if v1 then "tt" else "ff")^"\n") | _ => raise (Fail("Can not evaluate expression") ))
                                    in (let val (vars_new: SYMBOLTABLE.symboltable, procs_new: SCOPETABLE.scopetable,text:string) = (execCmdList(vars,procs,tl(cl),scopelist))
                                        in ((vars_new,procs_new,s3 ^ text))
                                    end)
                                end

        | conditionalcmd(ex:exp,cl1:cmd list, cl2:cmd list) => ( case execExp(vars,procs,ex,scopelist) of
                                                                boolval(v1) => (if v1 then (execCmdList(vars,procs,cl1@(tl(cl)),scopelist)) else (execCmdList(vars,procs,cl2@(tl(cl)),scopelist)))
                                                                | _ => raise (Fail("Expression in if statement not of boolean type")) )
        
        | whilecmd(ex:exp,cl1:cmd list) => ( case execExp(vars,procs,ex,scopelist) of
                                            boolval(v1) => if v1 then 
                                            let val (vars_new: SYMBOLTABLE.symboltable, procs_new: SCOPETABLE.scopetable,text:string) = execCmdList(vars,procs,cl1,scopelist)
                                            in let val (vars_new2: SYMBOLTABLE.symboltable, procs_new2: SCOPETABLE.scopetable,text2:string) = (execCmdList(vars_new,procs_new,cl,scopelist))
                                                in (vars_new2,procs_new2,text^text2)
                                                end
                                            end
                                            else (execCmdList(vars,procs,tl(cl),scopelist))
                                            | _ => raise (Fail("Expression in while statement not of boolean type")) )

        | showratcmd(ex:exp) => let val (vars_new: SYMBOLTABLE.symboltable, procs_new: SCOPETABLE.scopetable,text:string) = (execCmdList(vars,procs,tl(cl),scopelist))
                                in (vars_new,procs_new, ( case (execExp(vars,procs,ex,scopelist)) of ratval(v1) => ((Rational.showRat(v1))^"\n") | _ => raise (Fail("")) ) ^ text)
                                end
                            

        | showdecimalcmd(ex:exp) => let val (vars_new: SYMBOLTABLE.symboltable, procs_new: SCOPETABLE.scopetable,text:string) = (execCmdList(vars,procs,tl(cl),scopelist))
                                in (vars_new,procs_new, ( case (execExp(vars,procs,ex,scopelist)) of ratval(v1) => ((Rational.showDecimal(v1))^"\n") | _ => raise (Fail("")) ) ^ text)
                                end



    and varDecl(vars,procs,vl: decl, scope:int): SYMBOLTABLE.symboltable =
    case vl of
        ratvardecls(lst) => SYMBOLTABLE.insert(vars,RationalType,lst,scope)
        | intvardecls(lst) => SYMBOLTABLE.insert(vars,IntegerType,lst,scope)
        | boolvardecls(lst) => SYMBOLTABLE.insert(vars,BooleanType,lst,scope)

    and varDecls(vars,procs,vardecls(r1,i1,b1),scope): SYMBOLTABLE.symboltable = varDecl(varDecl(varDecl(vars,procs,r1,scope),procs,i1,scope),procs,b1,scope)

    and procDecls(vars,procs,pl: procdef list,scopelist): SCOPETABLE.scopetable = SCOPETABLE.insert(procs,pl,hd(scopelist),scopelist)    (* id_name and block *)


    and execBlock(vars,procs,blk,scopelist: int list): (SYMBOLTABLE.symboltable * SCOPETABLE.scopetable * string) = 
        case blk of block(declarationseq(vd:vardecls,pl: procdef list), cl: cmd list) => 
            let 
            val (newsc,procs_temp) = SCOPETABLE.newscopenum(procs)
            val vars_new = varDecls(vars,procs_temp,vd,newsc)
            val procs_new = procDecls(vars,procs_temp,pl,newsc::scopelist)
            in
                execCmdList(vars_new,procs_new,cl,newsc::scopelist)
            end

    and execProgram(prg):string =
        let
            val vars = []
            val procs = (0,[])
        in (case prg of
        programAST(blk) => #3 (execBlock(vars,procs,blk,[])))
        end

    and removeTabs(charlist) = if charlist = [] then ""
    else if hd(charlist) = #" " orelse hd(charlist) = #"\t" orelse hd(charlist) = #"\n" then removeTabs(tl(charlist))
    else str(hd(charlist))^removeTabs(tl(charlist))

    and strToVal(st:string):exp = 
    let val s = removeTabs(explode(st))
    in
        if s = "tt" then boolval(true)
        else if s = "ff" then boolval(false)
        else if String.isSubstring "." s then ratval(Rational.fromDecimal(s))
        else if String.isSubstring "/" s then ratval(Rational.from_frac(s))
        else intval(BigInt.int_make_bigint(s))
    end;

    fun writeFile (filename,content) =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end;

    fun main(prog) = 
        writeFile("output.txt",execProgram(prog))

    (* fun interpret(a,b) =
        writeFile(b,execProgram(PL0RAT.parseFile(a))) *)

end;
