module reff::Eval

import reff::AS;

import util::Math;

import IO;

RetClause substReturn(r:\return(x, c), str y, Value v) = r
	when x == y;
	
RetClause substReturn(\return(x, c), str y, Value v) =  \return(x, subst(c, y, v));

RetClause substReturn(e:empty(), str y, Value v) = e;

OpClause substClause(oo:op(id, x, k, c), str y, Value v) = oo
	when x == y || k == y;
	
OpClause substClause(oj:op(id, x, k, c), str y, Value v) = op(id, x, k, subst(c, y, v));

Value substVal(var(x), str y, Value v) = v
	when x == y;

Value substVal(va:var(x), str y, Value v) = va;

Value substVal(h:handler(handler(cs, rc)), str y, Value v) = handler(handler([substClause(c, y, v) | c <- cs], substReturn(rc, y, v)));	
	
Value substVal(f:fun(x, c), str y, Value v) = f
	when x == y;

Value substVal(fun(x, c), str y, Value v) = fun(x, subst(c, y, v));

	
// TODO refine cases
Value substVal(Value v1, str y, Value v) = v1; 

Computation subst(n:nativeOp(x), str y, Value v) = n; 

Computation subst(d:do(str x, Computation c1, Computation c2), str y, Value v) = d
	when x == y;
	
Computation subst(d:do(str x, Computation c1, Computation c2), str y, Value v) = do(x, subst(c1, y, v), subst(c2, y, v));

Computation subst(i:\if(Value v1, Computation c1, Computation c2), str y, Value v) = \if(substVal(v1, y, v), subst(c1, y, v), subst(c2, y, v));

Computation subst(app(v1, v2), y, v) = app(substVal(v1, y, v), substVal(v2, y, v));

Computation subst(\return(v1), y, v) = \return(substVal(v1, y, v));

Computation subst(opCall(id, v1, x, c), y, v) = opCall(id, substVal(v1, y, v), y, c) 
	when x == y;

Computation subst(opCall(id, v1, x, c), y, v) = opCall(id, substVal(v1, y, v), x, subst(c, y, v));

Computation subst(with(v1, c), y, v) = with(substVal(v1, y, v), subst(c, y, v));

Computation step(do(str x, \return(v), Computation c)) = subst(c, x, v);

Computation step(do(str x, opCall(id, v, y, c1), Computation c2)) = opCall(id, v, y, do(x, c1, c2));

Computation step(\if(\true(), Computation c1, Computation c2)) = c1; 

Computation step(\if(\false(), Computation c1, Computation c2)) = c2; 

Computation step(a:app(fun(x,c), v)) = subst(c, x, v);

Computation step(do(str x, Computation c1, Computation c2)) = do(x, c1_, c2)
	when c1_ := step(c1);

Computation step(with(handler(handler(clauses, \return(x, rc))), \return(v))) = subst(rc, x, v);

Computation nativePrint(Value x, str k){
	print("REFF\>\> ");
	println(x);
	return app(var(k), unit());
}

map[str, Computation(Value, str)] nativeMap = 
	("print":nativePrint);

Computation step(with(h:handler(hh:handler([*_, op(id_i, x, k, nativeOp(id_j)),*_], rc)),opCall(id, v, y, c))) =
 {	cr = nativeMap[id](v, y);
 	return subst(cr, y, fun(y, with(h, c)));
 }
	when id_i == id,
		 id_j == id;

Computation step(with(h:handler(handler([*_, op(id_i, x, k, c_i),*_], rc)),opCall(id, v, y, c))) = subst(subst(c_i, k, fun(y, with(h, c))), x, v)
	when id_i == id;


Computation step(with(h:handler(handler(clauses, rc)),opCall(id, v, y, c))) = opCall(id, v, y, with(h, c));

Computation step(with(handler(handler(clauses, rc)), Computation c))  = with(handler(handler(clauses, rc)), c_)
  when c_ := step(c);
  
default Computation step(c) = c;
  

Computation wrap(Computation c) = with(
	handler(
		handler([op("print", "x", "k", nativeOp("print"))], \return("x", \return(var("x"))))), 
	c);


Computation eval(Computation c){
	c = wrap(c);
	solve(c){
		c= step(c);
	}
	return c;
}