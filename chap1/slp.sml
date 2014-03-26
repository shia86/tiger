type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

val max:int*int->int = fn (f:int,s:int) => if f>s then f else s;

(* maxargs: stm->int *)
val rec maxargs:stm->int = fn CompoundStm(stm0,stm1) => max( maxargs stm0, maxargs stm1)
						| AssignStm(id0,exp0) => maxargs_expr exp0
						| PrintStm(exp0::t) => max( maxargs_expr exp0, 1 + maxargs( PrintStm( t)))
						| PrintStm(nil) => 0
and maxargs_expr:exp->int = fn IdExp(id0) => 0
								|  NumExp(int0) => 0
								|  OpExp(exp0,binop0,exp1) => max(maxargs_expr exp0,maxargs_expr exp1)
								|  EseqExp(stm0,exp0) => max(maxargs stm0, maxargs_expr exp0);

(* interp: stm->unit *)
type cell = id * int;
type table = cell list;
val get: (string * int) option -> int = fn SOME(s:string,i:int) => i | NONE => 0;
val getexp: (int * table ) -> int = fn (i:int,t0:table) =>i;
val getexptable: (int * table ) -> table = fn (i:int,t0:table) =>t0;
val lookup = fn (t0:table,id0:id) => get (List.find (fn (s:string,i:int) => String.compare(s,id0)  = EQUAL) t0);
val update = fn (t0:table,id0:id,i:int) => [(id0,i)] @ t0; 
val rec interpStm:stm*table->table = fn (CompoundStm(stm0,stm1),t0:table) => interpStm( stm1,interpStm( stm0,t0))
									| (AssignStm(id0,exp0),t0:table) => 
										let 
											val tmp:(int*table) = interpExp( exp0,t0) 
										in 
											update(getexptable tmp,id0,getexp tmp) 
										end
									| (PrintStm(exp0::t),t0:table) => interpStm( PrintStm( t),getexptable( interpExp (exp0,t0)))
									| (PrintStm(nil),t0:table) => t0 
and interpExp:exp*table->int*table = fn (IdExp(id0),t0:table) => ( lookup( t0,id0),t0)
									| (NumExp(int0),t0:table) => (int0,t0)
									| (OpExp(exp0,binop0,exp1),t0:table) => 
										let 
											val tmp0:(int*table) = interpExp( exp0,t0) 
											val tmp1:(int*table) = interpExp( exp1,getexptable tmp0) 
										in 
											case binop0 of
												Plus => (getexp tmp0 + getexp tmp1,getexptable tmp1)
												|Times => (getexp tmp0 * getexp tmp1,getexptable tmp1)
												|Minus => (getexp tmp0 - getexp tmp1,getexptable tmp1)
												|Div => (getexp tmp0 div getexp tmp1,getexptable tmp1)
										end
									| (EseqExp(stm0,exp0),t0:table) => interpExp( exp0,interpStm (stm0,t0));

(* Result*)
print ("maxargs prog |-> "^Int.toString( maxargs prog) ^ "\n");
val t0:table = [];
val tn:table = interpStm(prog,t0);
