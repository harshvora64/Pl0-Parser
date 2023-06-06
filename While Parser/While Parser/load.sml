(* BORROWED FROM SML-NJ LEX YACC DOCS *)
use "ast.sml";
open AST;



structure PL0RAT = struct

  structure pl0ratLrVals =
    pl0ratLrValsFun(structure Token = LrParser.Token)

  structure pl0ratLex =
    pl0ratLexFun(structure Tokens = pl0ratLrVals.Tokens);

  structure pl0ratParser =
    Join(structure LrParser = LrParser
        structure ParserData = pl0ratLrVals.ParserData
        structure Lex = pl0ratLex)

  val invoke = fn lexstream => (* The invoke function invokes the parser given a lexer *)
    let val print_error = fn (str,pos,_) =>
        TextIO.output(TextIO.stdOut,
          "***pl0rat Parser Error at character position " ^ (Int.toString pos)
          ^ "***\n" ^ str^ "\n")
    in pl0ratParser.parse(0,lexstream,print_error,())
  end

  fun newLexer fcn = (* newLexer creates a lexer from a given input-reading function *)
    let val lexer = pl0ratParser.makeLexer fcn
      (* val _ = pl0ratLex.UserDeclarations.init()  *)
    in lexer
  end

  fun stringToLexer str = (* creates a lexer from a string *)
    let val done = ref false
    in newLexer (fn n => if (!done) then " " else (done := true; str))
  end

  fun fileToLexer filename = (* creates a lexer from a file *)
    let val inStream = TextIO.openIn(filename)
    in newLexer (fn n => TextIO.inputAll(inStream))
  end

  fun lexerToParser (lexer) = (* creates a parser from a lexer *)
    let val dummyEOF = pl0ratLrVals.Tokens.EOF(0,0)
      val (result,lexer) = invoke lexer
      val (nextToken,lexer) = pl0ratParser.Stream.get lexer
    in if pl0ratParser.sameToken(nextToken,dummyEOF) then
        result
      else (TextIO.output(TextIO.stdOut,
                    "*** INTEXP PARSER WARNING -- unconsumed input ***\n");
                    result)
  end

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
              TextIO.output(TextIO.stdOut,
                            "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
      in pl0ratParser.parse(0,lexstream,print_error,())
      end

  val parseString = lexerToParser o stringToLexer (* parses a string *)

  val parseFile = 
  lexerToParser o fileToLexer (* parses a file *)

  (* fun compileFile (file) = 
    let
        val p = parseFile(file)
        fun getVal(p) = 
            let 
                val value = Executor.execProgram(p);
            in    
                (print("\n\nThe Evaluator output is:\n"),
                value)
            end;
        val (_, answer) = getVal(p)
    in
        answer
    end; *)

end;

(* fun parse () = 
    let val lexer = pl0ratParser.makeLexer
                      (fn _ => Option.getOpt(TextIO.inputLine TextIO.stdIn, ""))
        val dummyEOF = pl0ratLrVals.Tokens.EOF(0,0)
        val dummySEMI = pl0ratLrVals.Tokens.SEMI(0,0)
        fun loop lexer =
            let val (result,lexer) = invoke lexer
                val (nextToken,lexer) = pl0ratParser.Stream.get lexer
             in case result
                  of r =>
                      TextIO.output(TextIO.stdOut,
                            r ^ "\n");
                if pl0ratParser.sameToken(nextToken,dummyEOF) then ()
                else loop lexer
            end
     in loop lexer
    end *)