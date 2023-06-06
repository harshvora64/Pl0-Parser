CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "rational.sml";
open Rational;
open BigInt;
use "ast.sml";
open AST;
use "calc.yacc.sig";
use "calc.yacc.sml";
use "calc.lex.sml";
use "load.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details of parse trees *)
Control.Print.stringDepth := 1000; (* and strings *)
open PL0RAT;
use "symboltable.sml";
open SYMBOLTABLE;
open SCOPETABLE;
use "executor.sml";
open Executor;
parseFile("test3.rat");
(* type mytype = AST.program;
fun convertToMyType r = r; *)
(* main(PL0RAT.parseFile("test4.rat")); *)
(* Executor.main(astree) *)
