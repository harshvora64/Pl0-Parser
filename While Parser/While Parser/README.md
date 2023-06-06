# Pl0-Parser
Parser and Compiler for the statically scoped imperative PL0 programming language - While, which supports arithmetic in integers, booleans and arbitrary-precision rationals.

Grammar for pl0_rat language:

    PROGRAM         -> BLOCK 
    BLOCK           -> DECLARATIONSEQ COMMANDSEQ 
    DECLARATIONSEQ  -> VARDECLS PROCDECLS 
    VARDECLS        -> RATVARDECLS INTVARDECLS BOOLVARDECLS 
    RATVARDECLS     -> rational IDLIST ; 
                    | eps
    INTVARDECLS     -> integer IDLIST ; 
                    | eps
    BOOLVARDECLS    -> boolean IDLIST ; 
                    | eps
    IDLIST          -> ID , IDLIST 
                    | ID 
    PROCDECLS       -> PROCDEF ; PROCDECLS 
                    | eps
    PROCDEF         -> procedure ID BLOCK 
    COMMANDSEQ      -> { COMMANDLIST } 
    COMMANDLIST     -> COMMAND ; COMMANDLIST 
                    | eps
    COMMAND         -> ID := EXP 
                    | call ID 
                    | read ( ID ) 
                    | print ( EXP ) 
                    | if EXP then COMMANDSEQ else COMMANDSEQ fi 
                    | while EXP do COMMANDSEQ od 
                    | showRat ( EXP ) 
                    | showDecimal ( EXP ) 


- Here ID is a terminal, which refers to 'identifier'
- words in uppercase represent NON-TERMINALS
- words in lowercase represent terminals

Grammar for expressions:

    // For these rules, exp1 and exp2 should be int_expressions, and output will also be an int_expression 
    // RINT is a terminal, integer literal
    EXP          -> RINT 
                    | ~ EXP           
                    | EXP + EXP    
                    | EXP * EXP   
                    | EXP / EXP     
                    | EXP - EXP     
                    | EXP % EXP     
    
    // For these rules, exp1 and exp2 should be rational_expressions, and output will also be a rational_expression 
    // RFrac and RDec are terminals - rational literals
                    | RFRAC           
                    | RDEC           
                    | EXP .+. EXP    
                    | EXP .*. EXP   
                    | EXP ./. EXP     
                    | EXP .-. EXP     
                    | inverse EXP        
                    | make_rat ( EXP , EXP )       
                    | fromDecimal ( EXP )              

    // For these rules, exp1 and exp2 should be of the same type, and output will be a bool_expression 
                    | tt 
                    | ff 
                    | EXP = EXP 
                    | EXP <> EXP 
                    | EXP < EXP 
                    | EXP > EXP 
                    | EXP <= EXP 
                    | EXP >= EXP 
                    
    // For these rules, exp1 and exp2 should be bool_expressions, and output will also be a bool_expression 
                    | EXP && EXP 
                    | EXP || EXP 
                    | ! EXP

    // For these rules, exp can be of any type
                    | ( EXP )
                    | ID 

- NOTE: Precedence is not used here as ml-yacc allows us to state precedence order of operators explicitly.



Design decisions:
- Variables when declared are initialised with NONE, so they can't be used in expressions before assignment
- Variables declared later overshadow variables declared eariler in the same scope and with the same name, as in SML.

Errors are raised for:
- Type mismatch in expressions:
    - Expressions on either side of an operator are of different types
    - Expressions are not of the type that the operator takes
    - Value assigned to an identifier is not the same as the type
- Symboltable errors:
    - Variable used in assignment/expressions not declared
    - Procedure in call not declared
    - Variable declared but used before initialisation
- For the read command, errors are raised according to the individual datatypes of the input, as follows:
    - An integer or the integer/decimal/recurring part of a rational contains non-numeric characters (int_error)
    - Integer division by zero (int_error)
    - Fraction representation has divide by zero or missing "/"
    - The decimal representation misses a '.', '(' or a ')' (rat_error)
    - Denominator is empty when dividing 2 rationals, and the output format is not rational option (rat_error)

Acknowledgements : SML official documentation for ml-lex and ml-yacc used in making lex and yacc and load.sml files

Instructions to run (SML):
Note: Due to a bug, the generated AST has to be explicitly pasted into the Evaluator function.
1. Run the makefile: make
2. Generate the AST in the terminal using the command: parseFile("filename.rat");
3. Paste the generated AST into the command sequence:
    Executor.main(paste_AST_here);
    writefile(output_filename);