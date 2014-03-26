(* 1.implement a member function *)
type key = string

datatype  tree = LEAF
				|TREE of tree * key * tree

val empty0 = LEAF

fun insert(key,LEAF) = TREE(LEAF,key,LEAF)
	| insert(key,TREE(l,k,r)) =	if key < k
									then TREE(insert(key,l),k,r)
								else if key > k
									then TREE(l,k,insert(key,r))
								else
									TREE(l,key,r)
;;


fun member(key,LEAF) = false
	| member(key,TREE(l,k,r)) =	if key < k
									then member(key,l)
								else if key > k
									then member(key,r)
								else
									true
;;

val t0 = insert("i",insert("p",insert("s",insert("t",LEAF))));
member ("a",t0);
member ("p",t0);

(* 2.Extend the program to include not just membership, but the mapping of keys
to bindings *)
datatype 'a tree = LEAF
				| TREE of 'a tree * key * 'a * 'a tree

fun insert (LEAF,key,value) = TREE(LEAF,key,value,LEAF)
	|insert (TREE(l,k,v,r),key,value) = if key < k
											then TREE(insert(l,key,value),k,v,r)
										else if key > k
											then TREE(l,k,v,insert(r,key,value))
										else
											TREE(l,k,v,r)
;;

fun lookup (LEAF,key) = NONE
	|lookup (TREE(l,k,v,r),key) = if key < k
											then lookup(l,key)
										else if key > k
											then lookup(r,key)
										else
											SOME v
;;

val empty1 = LEAF;
val t1 = insert(insert(insert(empty1,"shia",26),"yuan",21),"xb",31);
lookup(t1,"xb");
