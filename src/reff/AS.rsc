module reff::AS

data Program = program(list[Declaration] decls, Computation root);

data Value 
	= var(str x)
	| \true()
	| \false()
	| string(str s)
	| pair(Value v1, Value v2)
	| unit()
	| handler(Handler h)
	| fun(str x, Computation c)
	;
	
bool isNormal(var(_)) = false;
bool isNormal(pair(v1, v2)) = isNormal(v1) && isNormal(v2);
bool isNormal(Value v) = true;	
	
data Declaration 
	= handlerDec(Handler h)
	| compDec(Computation c)
	;
	
data OpClause
	= op(str id, str x, str k, Computation c)
	;	
	
data RetClause
	= empty()
	| \return(str x, Computation rc)
	;
	
data Handler
	= handler(list[OpClause] clauses, RetClause rc)
	;

data Computation
	= \return(Value v)
	| nativeOp(str name)
	| opCall(str op, Value v, str y, Computation c)
	| do(str x, Computation c1, Computation c2)
	| \if(Value v, Computation c1, Computation c2)
	| app(Value v1, Value v2)
	| with(Value v, Computation c)
	;