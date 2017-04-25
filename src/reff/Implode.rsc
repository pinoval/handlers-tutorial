module reff::Implode 

import reff::Syntax;
import reff::AS;

import IO;

alias Cmap = map[str, Computation];
alias Hmap = map[str, Handler];

Value implode((Value) `<Id x>`) = var("<x>");
Value implode((Value) `true`) = \true();
Value implode((Value) `false`) = \false();
Value implode((Value) `<String s>`) = string("<s>");
Value implode((Value) `(<Value v1>, <Value v2>)`) = pair(implode(v1), implode(v2));
Value implode((Value) `fun <Id x> -\> <Computation c>`) = fun("<x>", implode(c));
Value implode((Value) `<Handler h>`) = handler(implode(h));
Value implode((Value) `( <Value v> )`) = implode(v);
Value implode((Value) `()`) = unit();
Value implode((Value) `generic <Id op>`) = fun("x", opCall("<op>",var("x"), "y", \return(var("y"))));  // TODO: hygiene?


OpClause implode((OperationClause) `<Id opId> (<Id x>;<Id k>) -\> <Computation c>`) =  op("<opId>", "<x>", "<k>", implode(c));

Handler implode((Handler) `handler { <OperationClause* clauses> }`) = handler([implode(clause) | clause <- clauses], empty());
Handler implode((Handler) `handler { return  <Id x> -\> <Computation returnC> <OperationClause* clauses> }`) = handler([implode(clause) | clause <- clauses], \return("<x>", implode(returnC)));
 
Computation implode((Computation) `return <Value v>`) = \return(implode(v));
Computation implode((Computation) `do <Id x> \<- <Computation c1> in <Computation c2>`) = do("<x>", implode(c1), implode(c2));
Computation implode((Computation) `<Id op>(<Value v>;<Id y>.<Computation c>)`) = opCall("<op>", implode(v), "<y>", implode(c));
Computation implode((Computation) `if <Value v> then <Computation c1> else <Computation c2>`) = \if(implode(v), implode(c1), implode(c2));
Computation implode((Computation) `<Value v1> <Value v2>`) = app(implode(v1), implode(v2));
Computation implode((Computation) `with <Value v> handle <Computation c>`) = with(implode(v), implode(c));
//Computation implode((Computation) `call <Id op> <Value v>`) = app(fun("x", opCall("<op>", var("x"), "y", \return(var("y")))), implode(v));  // TODO: hygiene?
Computation implode((Computation) `(<Computation c>)`) = implode(c);




