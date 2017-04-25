module reff::Syntax

extend lang::std::Layout;

lexical Id 
  = ([a-z A-Z 0-9 _] !<< [a-z A-Z][\-a-z A-Z 0-9 _]* !>> [a-z A-Z 0-9 _]) \ Keywords
  ;
  
keyword Keywords = "true" | "false" | "ref"  | "return" | "generic";

lexical String = [\"] StrChar* [\"];

lexical StrChar
  = ![\"\\]
  | [\\][\\\"nfbtr]
  ;
  
syntax Value 
	= tru: "true"
	| fals: "false"
	| var: Id
	| string: String s
	| pair: "(" Value v1 "," Value v2 ")"
	| unit: "(" ")"
	| function: "fun" Id "-\>" Computation c
	| handler: Handler h
	| "(" Value v ")"
	| generic: "generic" Id op
	;

syntax OperationClause
	= Id op "(" Id x ";" Id k ")" "-\>" Computation c
	;
	
syntax Handler
	= "handler" "{" ("return" Id x "-\>" Computation returnC)?
				   OperationClause* clauses "}" 
	;
	
start syntax Computation
	= returnComp: "return" Value v
	| opCall: Id op "(" Value v ";" Id y "." Computation c")"
	| seq: "do" Id x "\<-" Computation c1 "in" Computation c2
	| cond: "if" Value v "then" Computation c1 "else" Computation c2
	| app: Value v1 Value v2
	| handling: "with" Value v "handle" Computation c
	| "(" Computation c ")"
	;
	