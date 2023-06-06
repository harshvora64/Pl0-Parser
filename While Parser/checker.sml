use "rational.sml";
use "ast.sml";
open AST;
use "symboltable.sml";
use "executor.sml";


fun writefile (destinationFileName : string) =
    let
        val inFile = TextIO.openIn "output.txt"
        val sourceText = TextIO.inputAll inFile
        val _ = TextIO.closeIn inFile

        val outFile = TextIO.openOut destinationFileName
        val _ = TextIO.output (outFile, sourceText)
        val _ = TextIO.closeOut outFile
    in
        ()
    end;


fun filesAreIdentical(file1: string, file2: string): bool =
    let
        val f1 = TextIO.openIn file1
        val f2 = TextIO.openIn file2

        fun readContents(stream: TextIO.instream, accum: string) =
            case TextIO.inputLine stream of
                NONE => accum
              | SOME line => readContents(stream, accum ^ line)

        val contents1 = readContents(f1, "")
        val contents2 = readContents(f2, "")

        val identical = (contents1 = contents2)

        val _ = TextIO.closeIn f1
        val _ = TextIO.closeIn f2

    in
        identical
    end;

(* list of (input_file,output_file,correct_output) *)
val tests=[
    ("tests/check.rat","outputs/check.output","tests/check_correct.output"),
    ("tests/example.rat","outputs/example.output","tests/example_correct.output"),
    ("tests/static_scope.rat","outputs/static_scope.output","tests/static_scope_correct.output"),
    ("tests/scope.rat","outputs/scope.output","tests/scope_correct.output"),
    ("tests/nested_if.rat","outputs/nested_if.output","tests/nested_if_correct.output"),
    ("tests/fibonacci.rat","outputs/fibonacci.output","tests/fibonacci_correct.output"),
    ("tests/recursive.rat","outputs/recursive.output","tests/recursive_correct.output"),
    ("tests/exp2.rat","outputs/exp2.output","tests/exp2_correct.output"),
    ("tests/exp1.rat","outputs/exp1.output","tests/exp1_correct.output"),
    ("tests/scope2.rat","outputs/scope2.output","tests/scope2_correct.output")
];

fun run_tests(tests,commands) =
    case tests of
        [] => ()
        | (input,output,correct)::rest =>
            let
                val _ = Executor.main(hd(commands))
                val _ = writefile(output)
            in
                if filesAreIdentical(output,correct) then
                    (print("Test " ^ input ^ " passed\n"))
                else
                    (print("Test " ^ input ^ " failed\n"));
                run_tests(rest,tl(commands))
            end;


val commands = [
  programAST
    (block
       (declarationseq
          (vardecls
             (ratvardecls [],intvardecls ["factorialArg","factorialResult"],
              boolvardecls []),
           [procdef
              ("factorialRec",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],intvardecls ["i","acc"],
                        boolvardecls []),
                     [procdef
                        ("factorialIter",
                         block
                           (declarationseq
                              (vardecls
                                 (ratvardecls [],intvardecls [],
                                  boolvardecls []),[]),
                            [conditionalcmd
                               (binexp (leq,id "i",id "factorialArg"),
                                [assignmentcmd
                                   ("acc",binexp (int_mul,id "acc",id "i")),
                                 assignmentcmd
                                   ("i",
                                    binexp (int_add,id "i",intval ("+",[1]))),
                                 callcmd "factorialIter"],[])]))]),
                  [assignmentcmd ("i",intval ("+",[1])),
                   assignmentcmd ("acc",intval ("+",[1])),
                   callcmd "factorialIter",
                   assignmentcmd ("factorialResult",id "acc")]))]),
        [assignmentcmd ("factorialArg",intval ("+",[5])),
         callcmd "factorialRec",printcmd (id "factorialResult")]))
  
,
  programAST
    (block
       (declarationseq
          (vardecls
             (ratvardecls ["powerArg1","powerResult"],
              intvardecls ["powerArg2","factorialArg","factorialResult"],
              boolvardecls []),
           [procdef
              ("factorial",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],intvardecls ["i","acc"],
                        boolvardecls []),[]),
                  [assignmentcmd ("i",id "factorialArg"),
                   assignmentcmd ("acc",intval ("+",[1])),
                   whilecmd
                     (binexp (greater,id "i",intval ("+",[])),
                      [assignmentcmd ("acc",binexp (int_mul,id "acc",id "i")),
                       assignmentcmd
                         ("i",binexp (int_sub,id "i",intval ("+",[1])))]),
                   assignmentcmd ("factorialResult",id "acc")])),
            procdef
              ("power",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],intvardecls ["i"],
                        boolvardecls ["cond"]),[]),
                  [assignmentcmd ("i",intval ("+",[])),
                   assignmentcmd
                     ("powerResult",
                      binexp (make_rat,intval ("+",[1]),intval ("+",[1]))),
                   assignmentcmd ("powerResult",ratval (("+",[1]),("+",[1]))),
                   assignmentcmd ("cond",binexp (less,id "i",id "powerArg2")),
                   whilecmd
                     (binexp (equal,id "cond",boolval true),
                      [assignmentcmd
                         ("powerResult",
                          binexp (multiply,id "powerResult",id "powerArg1")),
                       assignmentcmd
                         ("i",binexp (int_add,id "i",intval ("+",[1]))),
                       assignmentcmd
                         ("cond",binexp (less,id "i",id "powerArg2"))])])),
            procdef
              ("boolProc",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],intvardecls ["n"],
                        boolvardecls ["expectedResult"]),[]),
                  [assignmentcmd ("n",intval ("+",[2,1])),
                   assignmentcmd ("expectedResult",boolval true),
                   conditionalcmd
                     (binexp
                        (equal,
                         binexp
                           (equal,binexp (int_mod,id "n",intval ("+",[2])),
                            intval ("+",[])),id "expectedResult"),
                      [printcmd (intval ("+",[1]))],
                      [printcmd (intval ("+",[]))])]))]),
        [assignmentcmd ("factorialArg",intval ("+",[6])),callcmd "factorial",
         printcmd (id "factorialResult"),
         assignmentcmd ("powerArg1",ratval (("+",[3]),("+",[2]))),
         assignmentcmd ("powerArg2",intval ("+",[3])),callcmd "power",
         printcmd (id "powerResult"),callcmd "boolProc"]))
  
,
  programAST
    (block
       (declarationseq
          (vardecls
             (ratvardecls ["a"],intvardecls ["x","y","z"],boolvardecls ["b"]),
           [procdef
              ("f",
               block
                 (declarationseq
                    (vardecls (ratvardecls [],intvardecls [],boolvardecls []),
                     [procdef
                        ("g",
                         block
                           (declarationseq
                              (vardecls
                                 (ratvardecls [],intvardecls [],
                                  boolvardecls []),[]),
                            [printcmd (id "x"),
                             assignmentcmd ("x",intval ("+",[4])),
                             printcmd (id "x")]))]),
                  [printcmd (id "x"),assignmentcmd ("x",intval ("+",[3])),
                   printcmd (id "x"),callcmd "g",printcmd (id "x")])),
            procdef
              ("h",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],intvardecls ["x"],boolvardecls []),[]),
                  [assignmentcmd ("x",intval ("+",[2])),printcmd (id "x"),
                   callcmd "f",printcmd (id "x")]))]),
        [assignmentcmd ("x",intval ("+",[1])),printcmd (id "x"),callcmd "h",
         printcmd (id "x")])) 
,
  programAST
    (block
       (declarationseq
          (vardecls
             (ratvardecls ["r1","r2","r3"],intvardecls ["i1","i2"],
              boolvardecls ["b1","b2"]),
           [procdef
              ("change",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls ["i1"],intvardecls [],boolvardecls []),[]),
                  [assignmentcmd ("i1",ratval (("+",[1,9]),("+",[0,9]))),
                   printcmd (id "i1")]))]),
        [assignmentcmd ("i1",intval ("+",[4])),printcmd (id "i1"),
         callcmd "change",printcmd (id "i1")])) 
,
  programAST
    (block
       (declarationseq
          (vardecls
             (ratvardecls ["x","y","z"],intvardecls ["a","b","c"],
              boolvardecls ["d","e","f"]),[]),
        [assignmentcmd ("a",intval ("+",[2])),
         assignmentcmd
           ("b",
            binexp
              (int_add,
               binexp
                 (int_add,binexp (int_add,id "a",id "a"),
                  binexp (int_mul,id "a",id "a")),
               binexp (int_intdiv,id "a",id "a"))),
         assignmentcmd
           ("c",
            binexp
              (int_mul,
               binexp
                 (int_mul,binexp (int_add,id "a",id "b"),intval ("+",[1])),
               binexp (int_add,id "a",id "b"))),
         assignmentcmd ("x",ratval (("+",[4]),("+",[3]))),
         assignmentcmd
           ("y",binexp (multiply,id "x",binexp (make_rat,id "c",id "b"))),
         assignmentcmd ("z",binexp (divide,id "x",id "y")),
         assignmentcmd ("d",boolval true),assignmentcmd ("e",boolval false),
         assignmentcmd
           ("f",unexp (NOT,binexp (OR,binexp (AND,id "d",id "e"),id "e"))),
         printcmd (id "a"),printcmd (id "b"),printcmd (id "c"),
         printcmd (id "x"),printcmd (id "y"),printcmd (id "z"),
         printcmd (id "d"),printcmd (id "e"),printcmd (id "f"),
         conditionalcmd
           (id "d",
            [printcmd (binexp (int_sub,id "a",id "b")),
             conditionalcmd
               (binexp
                  (geq,id "x",
                   binexp (make_rat,intval ("+",[2]),intval ("+",[1]))),[],
                [printcmd (binexp (multiply,id "y",id "z")),
                 conditionalcmd
                   (binexp (equal,binexp (multiply,id "y",id "z"),id "x"),
                    [printcmd (id "x"),
                     whilecmd
                       (binexp (less,id "a",intval ("+",[0,1])),
                        [printcmd (id "a"),
                         assignmentcmd
                           ("a",binexp (int_add,id "a",intval ("+",[1])))])],
                    [])])],[]),printcmd (id "a")])) 
,
  programAST
    (block
       (declarationseq
          (vardecls
             (ratvardecls ["input","rfibresult"],
              intvardecls ["i","fibresult"],boolvardecls []),
           [procdef
              ("fib",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],
                        intvardecls ["tempi","tempf1","tempf2"],
                        boolvardecls []),[]),
                  [conditionalcmd
                     (binexp (equal,id "i",intval ("+",[])),
                      [assignmentcmd ("fibresult",intval ("+",[]))],
                      [conditionalcmd
                         (binexp (equal,id "i",intval ("+",[1])),
                          [assignmentcmd ("fibresult",intval ("+",[1]))],
                          [assignmentcmd ("tempi",id "i"),
                           assignmentcmd
                             ("i",
                              binexp (int_sub,id "tempi",intval ("+",[1]))),
                           callcmd "fib",
                           assignmentcmd ("tempf1",id "fibresult"),
                           assignmentcmd
                             ("i",
                              binexp (int_sub,id "tempi",intval ("+",[2]))),
                           callcmd "fib",
                           assignmentcmd ("tempf2",id "fibresult"),
                           assignmentcmd
                             ("fibresult",
                              binexp (int_add,id "tempf1",id "tempf2")),
                           assignmentcmd ("i",id "tempi")])])])),
            procdef
              ("fibwhile",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],
                        intvardecls ["tempi","tempf1","tempf2","tempf3"],
                        boolvardecls []),[]),
                  [assignmentcmd ("tempf1",intval ("+",[])),
                   assignmentcmd ("tempf2",intval ("+",[1])),
                   assignmentcmd ("tempi",intval ("+",[2])),
                   whilecmd
                     (binexp (leq,id "tempi",id "i"),
                      [assignmentcmd
                         ("tempf3",binexp (int_add,id "tempf1",id "tempf2")),
                       assignmentcmd ("tempf1",id "tempf2"),
                       assignmentcmd ("tempf2",id "tempf3"),
                       assignmentcmd
                         ("tempi",
                          binexp (int_add,id "tempi",intval ("+",[1])))]),
                   conditionalcmd
                     (binexp (equal,id "i",intval ("+",[])),
                      [assignmentcmd ("fibresult",intval ("+",[]))],
                      [conditionalcmd
                         (binexp (equal,id "i",intval ("+",[1])),
                          [assignmentcmd ("fibresult",intval ("+",[1]))],
                          [assignmentcmd ("fibresult",id "tempf3")])])]))]),
        [assignmentcmd ("i",intval ("+",[8])),callcmd "fib",
         printcmd (id "fibresult"),printcmd (id "i"),
         assignmentcmd ("i",intval ("+",[8])),callcmd "fibwhile",
         printcmd (id "fibresult"),printcmd (id "i")])) 
,
  programAST
    (block
       (declarationseq
          (vardecls
             (ratvardecls [],intvardecls ["x","y","gcd"],boolvardecls []),
           [procdef
              ("gcdd",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],intvardecls ["tempx","tempy"],
                        boolvardecls []),[]),
                  [conditionalcmd
                     (binexp (equal,id "y",intval ("+",[])),
                      [assignmentcmd ("gcd",id "x")],
                      [assignmentcmd ("tempx",id "x"),
                       assignmentcmd ("tempy",id "y"),
                       assignmentcmd ("x",id "tempy"),
                       assignmentcmd
                         ("y",binexp (int_mod,id "tempx",id "tempy")),
                       callcmd "gcdd",assignmentcmd ("x",id "tempx"),
                       assignmentcmd ("y",id "tempy")])]))]),
        [assignmentcmd ("x",intval ("+",[2,1])),
         assignmentcmd ("y",intval ("+",[8,1])),callcmd "gcdd",
         printcmd (id "gcd")])) 
,
  programAST
    (block
       (declarationseq
          (vardecls
             (ratvardecls ["x","y","z"],intvardecls ["a","b","c"],
              boolvardecls ["d","e","f"]),[]),
        [assignmentcmd ("a",intval ("+",[2])),
         assignmentcmd
           ("b",
            binexp
              (int_add,
               binexp
                 (int_add,binexp (int_add,id "a",id "a"),
                  binexp (int_mul,id "a",id "a")),
               binexp (int_intdiv,id "a",id "a"))),
         assignmentcmd
           ("c",
            binexp
              (int_mul,
               binexp
                 (int_mul,binexp (int_add,id "a",id "b"),intval ("+",[1])),
               binexp (int_add,id "a",id "b"))),
         assignmentcmd ("x",ratval (("+",[4]),("+",[3]))),
         assignmentcmd
           ("y",binexp (multiply,id "x",binexp (make_rat,id "c",id "b"))),
         assignmentcmd ("z",binexp (divide,id "x",id "y")),
         assignmentcmd ("d",boolval true),assignmentcmd ("e",boolval false),
         assignmentcmd
           ("f",unexp (NOT,binexp (OR,binexp (AND,id "d",id "e"),id "e"))),
         printcmd (id "a"),printcmd (id "b"),printcmd (id "c"),
         printcmd (id "x"),printcmd (id "y"),printcmd (id "z"),
         printcmd (id "d"),printcmd (id "e"),printcmd (id "f"),
         printcmd (binexp (less,id "a",id "b")),
         printcmd (binexp (leq,id "a",id "b")),
         printcmd (binexp (neq,id "a",id "b")),
         printcmd (binexp (equal,id "a",id "b")),
         printcmd (binexp (greater,id "a",id "b")),
         printcmd (binexp (geq,id "a",id "b")),
         printcmd (binexp (less,id "x",id "z")),
         printcmd (binexp (leq,id "x",id "z")),
         printcmd (binexp (neq,id "x",id "z")),
         printcmd (binexp (equal,id "x",id "z")),
         printcmd (binexp (greater,id "x",id "z")),
         printcmd (binexp (geq,id "x",id "z")),
         printcmd
           (binexp
              (multiply,
               binexp
                 (multiply,id "z",
                  binexp (make_rat,intval ("+",[1,2,1]),intval ("+",[1]))),
               binexp (make_rat,intval ("+",[1,1]),intval ("+",[1])))),
         printcmd
           (binexp
              (leq,
               binexp
                 (divide,
                  binexp
                    (make_rat,
                     binexp
                       (int_mul,
                        binexp
                          (int_intdiv,id "c",binexp (int_add,id "a",id "b")),
                        intval ("+",[3])),intval ("+",[1])),
                  ratval (("+",[1]),("+",[3]))),
               binexp
                 (multiply,
                  binexp
                    (multiply,id "z",
                     binexp (make_rat,intval ("+",[1,2,1]),intval ("+",[1]))),
                  binexp (make_rat,intval ("+",[1,1]),intval ("+",[1]))))),
         printcmd
           (binexp
              (neq,
               binexp
                 (divide,
                  binexp
                    (make_rat,
                     binexp
                       (int_mul,
                        binexp
                          (int_intdiv,id "c",binexp (int_add,id "a",id "b")),
                        intval ("+",[3])),intval ("+",[1])),
                  ratval (("+",[1]),("+",[3]))),
               binexp
                 (multiply,
                  binexp
                    (multiply,id "z",
                     binexp (make_rat,intval ("+",[1,2,1]),intval ("+",[1]))),
                  binexp (make_rat,intval ("+",[1,1]),intval ("+",[1]))))),
         printcmd
           (binexp
              (equal,
               binexp
                 (divide,
                  binexp
                    (make_rat,
                     binexp
                       (int_mul,
                        binexp
                          (int_intdiv,id "c",binexp (int_add,id "a",id "b")),
                        intval ("+",[3])),intval ("+",[1])),
                  ratval (("+",[1]),("+",[3]))),
               binexp
                 (multiply,
                  binexp
                    (multiply,id "z",
                     binexp (make_rat,intval ("+",[1,2,1]),intval ("+",[1]))),
                  binexp (make_rat,intval ("+",[1,1]),intval ("+",[1]))))),
         printcmd
           (binexp
              (geq,
               binexp
                 (divide,
                  binexp
                    (make_rat,
                     binexp
                       (int_mul,
                        binexp
                          (int_intdiv,id "c",binexp (int_add,id "a",id "b")),
                        intval ("+",[3])),intval ("+",[1])),
                  ratval (("+",[1]),("+",[3]))),
               binexp
                 (multiply,
                  binexp
                    (multiply,id "z",
                     binexp (make_rat,intval ("+",[1,2,1]),intval ("+",[1]))),
                  binexp (make_rat,intval ("+",[1,1]),intval ("+",[1]))))),
         printcmd
           (binexp
              (greater,
               binexp
                 (divide,
                  binexp
                    (make_rat,
                     binexp
                       (int_mul,
                        binexp
                          (int_intdiv,id "c",binexp (int_add,id "a",id "b")),
                        intval ("+",[3])),intval ("+",[1])),
                  ratval (("+",[1]),("+",[3]))),
               binexp
                 (multiply,
                  binexp
                    (multiply,id "z",
                     binexp (make_rat,intval ("+",[1,2,1]),intval ("+",[1]))),
                  binexp (make_rat,intval ("+",[1,1]),intval ("+",[1]))))),
         assignmentcmd
           ("f",
            binexp
              (leq,
               binexp
                 (divide,
                  binexp
                    (make_rat,
                     binexp
                       (int_mul,
                        binexp
                          (int_intdiv,id "c",binexp (int_add,id "a",id "b")),
                        intval ("+",[3])),intval ("+",[1])),
                  ratval (("+",[1]),("+",[3]))),
               binexp
                 (multiply,
                  binexp
                    (multiply,id "z",
                     binexp (make_rat,intval ("+",[1,2,1]),intval ("+",[1]))),
                  binexp (make_rat,intval ("+",[1,1]),intval ("+",[1]))))),
         printcmd (id "f"),printcmd (unexp (NOT,id "f")),
         printcmd (binexp (AND,id "f",id "e")),
         printcmd (binexp (OR,id "f",id "e")),
         printcmd
           (binexp
              (OR,
               binexp
                 (greater,binexp (make_rat,id "a",intval ("+",[1])),
                  binexp (make_rat,id "b",intval ("+",[1]))),
               binexp
                 (leq,
                  binexp
                    (divide,
                     binexp
                       (make_rat,
                        binexp
                          (int_mul,
                           binexp
                             (int_intdiv,id "c",
                              binexp (int_add,id "a",id "b")),
                           intval ("+",[3])),intval ("+",[1])),
                     ratval (("+",[1]),("+",[3]))),
                  binexp
                    (multiply,
                     binexp
                       (multiply,id "z",
                        binexp
                          (make_rat,intval ("+",[1,2,1]),intval ("+",[1]))),
                     binexp (make_rat,intval ("+",[1,1]),intval ("+",[1]))))))]))
  
,
  programAST
    (block
       (declarationseq
          (vardecls (ratvardecls [],intvardecls [],boolvardecls []),[]),
        [printcmd
           (unexp
              (unminus,
               binexp
                 (int_sub,intval ("+",[2]),unexp (unminus,intval ("+",[1]))))),
         printcmd
           (binexp
              (int_intdiv,
               unexp
                 (unminus,
                  binexp
                    (int_mul,intval ("+",[2]),
                     unexp (unminus,intval ("+",[1])))),intval ("+",[3]))),
         printcmd
           (binexp
              (multiply,
               binexp
                 (make_rat,
                  unexp
                    (unminus,
                     binexp
                       (int_intdiv,intval ("+",[2]),
                        unexp (unminus,intval ("+",[1])))),intval ("+",[1])),
               ratval (("+",[1,7,2]),("+",[0,9])))),
         printcmd
           (binexp
              (divide,
               binexp
                 (make_rat,
                  unexp
                    (unminus,
                     binexp
                       (int_intdiv,intval ("+",[2]),
                        unexp (unminus,intval ("+",[1])))),intval ("+",[1])),
               ratval (("+",[1,7,2]),("+",[0,9])))),
         printcmd
           (binexp
              (add,
               binexp
                 (make_rat,
                  unexp
                    (unminus,
                     binexp
                       (int_intdiv,intval ("+",[2]),
                        unexp (unminus,intval ("+",[1])))),intval ("+",[1])),
               ratval (("+",[1,7,2]),("+",[0,9])))),
         printcmd
           (binexp
              (divide,
               binexp
                 (make_rat,
                  unexp
                    (unminus,
                     binexp
                       (int_intdiv,intval ("+",[2]),
                        unexp (unminus,intval ("+",[1])))),intval ("+",[1])),
               ratval (("+",[1,7,2]),("+",[0,9])))),
         printcmd
           (binexp
              (multiply,
               binexp
                 (make_rat,
                  unexp
                    (unminus,
                     binexp
                       (int_intdiv,intval ("+",[2]),
                        unexp (unminus,intval ("+",[1])))),intval ("+",[1])),
               ratval (("+",[4,3]),("+",[9])))),
         printcmd
           (binexp
              (multiply,
               binexp
                 (make_rat,
                  unexp
                    (unminus,
                     binexp
                       (int_intdiv,intval ("+",[2]),
                        unexp (unminus,intval ("+",[1])))),intval ("+",[1])),
               unexp (inverse,ratval (("+",[4,3]),("+",[9]))))),
         printcmd
           (binexp
              (make_rat,intval ("+",[0,0,8,8,9,7,8,7,6,5,5,4,5,4,3,3,2,2,1]),
               intval ("+",[0,0,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9]))),
         printcmd
           (ratval
              (("+",[6,6,9,9,9,2,3,1,2,8,1,8,1,8,9,3,3]),
               ("+",[5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,2]))),
         printcmd (binexp (equal,intval ("+",[1]),intval ("+",[1]))),
         printcmd (binexp (equal,intval ("+",[1]),intval ("+",[2]))),
         printcmd (binexp (less,intval ("+",[1]),intval ("+",[2]))),
         printcmd (binexp (leq,intval ("+",[1]),intval ("+",[2]))),
         printcmd (binexp (greater,intval ("+",[1]),intval ("+",[2]))),
         printcmd (binexp (geq,intval ("+",[1]),intval ("+",[2]))),
         printcmd (binexp (neq,intval ("+",[1]),intval ("+",[2]))),
         printcmd
           (binexp
              (int_add,binexp (int_add,intval ("+",[1]),intval ("+",[2])),
               intval ("+",[3]))),
         printcmd
           (binexp
              (equal,binexp (make_rat,intval ("+",[1]),intval ("+",[3])),
               ratval (("+",[1]),("+",[3])))),
         printcmd
           (binexp
              (less,binexp (make_rat,intval ("+",[1]),intval ("+",[3])),
               ratval (("+",[4]),("+",[9])))),
         printcmd
           (binexp
              (geq,binexp (make_rat,intval ("+",[1]),intval ("+",[3])),
               ratval (("+",[4]),("+",[9])))),
         printcmd
           (binexp
              (greater,binexp (make_rat,intval ("+",[1]),intval ("+",[3])),
               ratval (("+",[1]),("+",[3])))),
         printcmd
           (binexp
              (geq,binexp (make_rat,intval ("+",[1]),intval ("+",[3])),
               ratval (("+",[1]),("+",[3])))),
         printcmd (binexp (int_sub,intval ("+",[1]),intval ("+",[2]))),
         printcmd (binexp (int_mul,intval ("+",[1]),intval ("+",[2]))),
         printcmd (binexp (int_intdiv,intval ("+",[1]),intval ("+",[2]))),
         printcmd (binexp (int_mod,intval ("+",[1]),intval ("+",[2]))),
         printcmd
           (binexp
              (add,binexp (make_rat,intval ("+",[1]),intval ("+",[1])),
               binexp (make_rat,intval ("+",[2]),intval ("+",[1])))),
         printcmd
           (binexp
              (subtract,binexp (make_rat,intval ("+",[1]),intval ("+",[1])),
               binexp (make_rat,intval ("+",[2]),intval ("+",[1])))),
         printcmd
           (binexp
              (multiply,binexp (make_rat,intval ("+",[1]),intval ("+",[1])),
               binexp (make_rat,intval ("+",[2]),intval ("+",[1])))),
         printcmd
           (binexp
              (divide,binexp (make_rat,intval ("+",[1]),intval ("+",[1])),
               binexp (make_rat,intval ("+",[2]),intval ("+",[1])))),
         printcmd (intval ("+",[1])),
         printcmd (unexp (unminus,intval ("+",[1]))),
         printcmd (binexp (make_rat,intval ("+",[1]),intval ("+",[1]))),
         printcmd
           (unexp
              (unminus,binexp (make_rat,intval ("+",[1]),intval ("+",[1]))))]))
  
,
  programAST
    (block
       (declarationseq
          (vardecls
             (ratvardecls ["input","rfibresult"],
              intvardecls ["i","fibresult","till"],boolvardecls []),
           [procdef
              ("fib",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],
                        intvardecls ["tempi","tempf1","tempf2"],
                        boolvardecls []),[]),
                  [conditionalcmd
                     (binexp (equal,id "i",intval ("+",[])),
                      [assignmentcmd ("fibresult",intval ("+",[]))],
                      [conditionalcmd
                         (binexp (equal,id "i",intval ("+",[1])),
                          [assignmentcmd ("fibresult",intval ("+",[1]))],
                          [assignmentcmd ("tempi",id "i"),
                           assignmentcmd
                             ("i",
                              binexp (int_sub,id "tempi",intval ("+",[1]))),
                           callcmd "fib",
                           assignmentcmd ("tempf1",id "fibresult"),
                           assignmentcmd
                             ("i",
                              binexp (int_sub,id "tempi",intval ("+",[2]))),
                           callcmd "fib",
                           assignmentcmd ("tempf2",id "fibresult"),
                           assignmentcmd
                             ("fibresult",
                              binexp (int_add,id "tempf1",id "tempf2")),
                           assignmentcmd ("i",id "tempi")])])])),
            procdef
              ("fibinc",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],intvardecls ["i"],boolvardecls []),[]),
                  [assignmentcmd ("i",intval ("+",[])),
                   whilecmd
                     (binexp (leq,id "i",id "till"),
                      [callcmd "fib",printcmd (id "fibresult"),
                       assignmentcmd
                         ("i",binexp (int_add,id "i",intval ("+",[1])))])])),
            procdef
              ("fibc",
               block
                 (declarationseq
                    (vardecls
                       (ratvardecls [],intvardecls ["j"],boolvardecls []),[]),
                  [assignmentcmd ("j",intval ("+",[])),
                   whilecmd
                     (binexp (leq,id "j",id "till"),
                      [assignmentcmd ("i",id "j"),callcmd "fib",
                       printcmd (id "fibresult"),
                       assignmentcmd
                         ("j",binexp (int_add,id "j",intval ("+",[1])))])]))]),
        [assignmentcmd ("i",intval ("+",[7])),
         assignmentcmd ("till",intval ("+",[7])),callcmd "fibinc",
         assignmentcmd ("i",intval ("+",[7])),
         assignmentcmd ("till",intval ("+",[7])),callcmd "fibc"]))
  
   ];


val _ = run_tests(tests,commands)
(* val _=interpret("tests/exp1.rat","tests/exp1.output") *)
