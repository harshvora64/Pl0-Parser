(* Note: Integer lists are of the format that the leftmost digit - head - is the least significant digit *)
signature BIGINT =
sig
type bigint
exception int_error
val int_make_bigint: string -> bigint
val int_zip: string * int list -> bigint
val int_sign: bigint -> string
val int_magn: bigint -> int list
val int_neg: bigint -> bigint
val int_add: bigint * bigint -> bigint
val int_sub: bigint * bigint -> bigint
val int_mul: bigint * bigint -> bigint
val int_gcd: bigint * bigint -> bigint
val int_norm: bigint * bigint -> bigint * bigint
val int_div: bigint * bigint -> bigint * bigint
val int_intdiv: bigint * bigint -> bigint
val int_mod: bigint * bigint -> bigint
val int_equal: bigint * bigint -> bool
val int_less: bigint * bigint -> bool
val int_leq: bigint * bigint -> bool
val int_greater: bigint * bigint -> bool
val int_geq: bigint * bigint -> bool
val int_neq: bigint * bigint -> bool
val int_show: bigint -> string
end;


structure BigInt: BIGINT = 
struct
type bigint = (string * int list)
exception int_error
fun digitize (lst,car) =
    if lst=[] 
        then if not(car = 0) 
            then [car] 
        else [] 
    else 
        [((hd(lst)+car) mod 10)] @ digitize(tl(lst),(hd(lst)+car) div 10);
    (* ensures that all digits are in 0-9 by taking out the appropriate factor of 10 and adding it as carry/borrow to the next place
       eg. [14,3] -> [4,4], [~2,8] -> [8,7]
       This is a comon helper function to add, mul and sub *)

fun lzrhelp(lst) = if lst = [] then lst else if hd(lst) = 0 then lzrhelp(tl(lst)) else lst;
    (* removes leading zeroes(rightmost) - helper function needed to reverse the list so that hd and tl can be used*)
fun leadingZeroRemover(lst) = rev(lzrhelp(rev(lst)));
    (* removes leading zeroes(rightmost) *)


(* add, mul and sub:
   helper functions first operate on each list element ignoring crry or borrow
   eg. [1,4] * 5 -> [5,20]
   Then in the main function, the output is modified to get each digit in the correct range using digitise, and leading zeroes are removed and  *)
fun mulhelp(lst,digit) = if lst = [] then [] else (hd(lst)*digit) :: mulhelp(tl(lst),digit);
fun addhelp(lst,digit) = if lst = [] then [digit] else (hd(lst) + digit) :: tl(lst);
fun addlhelp(lst1,lst2) = if lst2 = [] then lst1 else if lst1 = [] then lst2 else (hd(lst1)+hd(lst2)) :: addlhelp(tl(lst1),tl(lst2));
fun subhelp(lst1,lst2) = if lst2= [] then lst1 else (hd(lst1)-hd(lst2)) :: subhelp(tl(lst1),tl(lst2));

fun mul(lst,digit)=leadingZeroRemover(digitize(mulhelp(lst,digit),0));  (* multiplies list with a digit *)
fun addl(lst1,lst2)=leadingZeroRemover(digitize(addlhelp(lst1,lst2),0));  (* adds digit to a list *)
fun add(lst,digit)=leadingZeroRemover(digitize(addhelp(lst,digit),0));  (* adds digit to a list *)
fun sub(lst1,lst2)= leadingZeroRemover(digitize(subhelp(lst1,lst2),0));  (* suubttracts two lists *)

fun multiplyhelp(lst1,lst2) = if lst2=[] then [] else addl(multiplyhelp((0::lst1),tl(lst2)),mul(lst1,hd(lst2)));
fun multiply(lst1,lst2) = leadingZeroRemover(digitize(multiplyhelp(lst1,lst2),0));

fun leqhelp(lst1,lst2) = if hd(lst1) < hd(lst2) then true else if hd(lst1) > hd(lst2) then false else leqhelp(tl(lst1),tl(lst2));
    (* Helper function to check if first list is less than equal to the second list if they are of same length *)
fun leq(lst1,lst2) = if lst1 = lst2 
                            then true 
                        else 
                            if length(lst1) > length(lst2) 
                                then false 
                            else 
                                if length(lst1) < length(lst2)
                                    then true
                                else
                                    leqhelp(rev(lst1),rev(lst2));
    (* Function to check if first list is less than equal to the second list 
       Checks if of different lengths, if not then calls helper function *)


fun digitFinder(lhs,rm,digit) = if digit <= 0 then digit else if leq(mul(lhs,digit),rm) then digit else digitFinder(lhs,rm,digit-1);
    (* finds the next digit of the answer square root *)

fun strTOchLst(str) = explode(str);
    (* converts string to list of chars *)
fun removeZeroes(ch) = if ch = [] then [] else if hd(ch) = #"0" then removeZeroes(tl(ch)) else ch;
    (* removes leading zeroes from character list *)
fun chTOinhelp(ch) = if ch=[] then [] else
    let val v = ord(hd(ch))-48
    in
        if v<0 orelse v>9 then raise int_error
        else [v] @ chTOinhelp(tl(ch))
    end;
        (* helper function to convert char list to reversed int list (char list is of even length - always ensured) *)

fun chLstTOintLst(ch) = chTOinhelp(rev(ch));
    (* main function to convert char list to reversed int list - if odd length then the 1st element is extracted and added separately, and the rest are added in pairs - list of 2 elements -  
       the integer list is the reverse of the string*)

fun strTOintLst(str) = chLstTOintLst(removeZeroes(strTOchLst(str)));
    (* converts string to list of pairs(list) of digits stored in reverse order *)
fun intLstTOstrhelp(lst) = if lst = [] then [] else [Int.toString(hd(lst))]@intLstTOstrhelp(tl(lst)) ;
    (* helper function to convert int list to string list *)
fun intLstTOstr(lst) = if lst = [] then "0" else concat(intLstTOstrhelp(lst));
    (* concatenates string list into string *)

fun divhelp2(lst1,lst2,lst3,lst4) = if lst2 = [] then (sub(lst1,mul(lst3,digitFinder(lst3,lst1,9))),digitFinder(lst3,lst1,9)::lst4) else divhelp2(hd(lst2)::sub(lst1,mul(lst3,digitFinder(lst3,lst1,9))),tl(lst2),lst3,digitFinder(lst3,lst1,9)::lst4);
fun divhelp(lst1,lst2) = 
    let val v = divhelp2([],rev(lst1),lst2,[])
    in
        (leadingZeroRemover(digitize((#1 v),0)),leadingZeroRemover(digitize((#2 v),0)))
    end;

fun gcdhelp(lst1,lst2) = 
    let val v = #1 (divhelp(lst1,lst2))
    in
        if v=[] then lst2 else gcdhelp(lst2,v)
    end;

fun gcd(lst1,lst2) = if leq(lst1,lst2) then gcdhelp(lst2,lst1) else gcdhelp(lst1,lst2);


fun int_make_bigint(s : string) = if s = "" then ("+",[]) else if String.sub(s,0) = #"~" then ("~",strTOintLst(String.extract(s,1,NONE))) else if String.sub(s,0) = #"+" then ("+",strTOintLst(String.extract(s,1,NONE))) else ("+",strTOintLst(s));
fun int_zip(a : string, b : int list) = (a,b);
fun int_sign(a : (string * int list)) = #1 a;
fun int_magn(a : (string * int list)) = #2 a;
fun int_neg(a : (string * int list)) = 
    if (#1 a) = "~" then
        ("+",(#2 a))
    else
        ("~",(#2 a))


fun int_add(a : (string * int list),b : (string * int list)) = 
    if (#1 a) = "~" then 
        if (#1 b) = "~" then 
            ("~",addl((#2 a),(#2 b)))
        else 
            if leq((#2 a),(#2 b)) then
                ((#1 b),sub((#2 b),(#2 a)))
            else
                ((#1 a),sub((#2 a),(#2 b)))
    else
        if (#1 b) = "+" then 
            ("+",addl((#2 a),(#2 b)))
        else
            if leq((#2 a),(#2 b)) then
                ((#1 b),sub((#2 b),(#2 a)))
            else
                ((#1 a),sub((#2 a),(#2 b)));

fun int_sub(a : (string * int list),b : (string * int list)) = int_add(a,int_neg(b));

fun int_mul(a : (string * int list),b : (string * int list)) =
    if (#1 a) = (#1 b) then
        ("+",multiply((#2 a),(#2 b)))
    else
        ("~",multiply((#2 a),(#2 b)));

fun int_gcd(a : (string * int list),b : (string * int list)) = 
    ("+",gcd((#2 a),(#2 b)));

fun int_norm(a : (string * int list),b : (string * int list)) = 
    let 
        val g = (#2 (int_gcd(a,b)))
        val a2 = (#2 a)
        val b2 = (#2 b)
    in
        if (#1 a) = (#1 b) orelse (#2 a) = [] then
            (("+",(#2 (divhelp(a2,g)))),("+",(#2 (divhelp(b2,g)))))
        else
            (("~",(#2 (divhelp(a2,g)))),("+",(#2 (divhelp(b2,g)))))
    end;

fun int_div(a : (string * int list),b : (string * int list)) =  (*(remainder,quotient)*)
    if (#2 b) = [] then raise int_error else
    if (#1 a) = (#1 b) then
        (("+",(#1 (divhelp((#2 a),(#2 b))))),("+",(#2 (divhelp((#2 a),(#2 b))))))
    else
        (("+",(#1 (divhelp((#2 a),(#2 b))))),("~",(#2 (divhelp((#2 a),(#2 b))))));

fun int_intdiv(a : (string * int list),b : (string * int list)) = #2 (int_div(a,b))

fun int_mod(a : (string * int list),b : (string * int list)) = #1 (int_div(a,b))

fun int_equal(a : (string * int list),b : (string * int list)) = 
    let
        val a2 = (leadingZeroRemover(digitize((#2 a),0)))
        val b2 = (leadingZeroRemover(digitize((#2 b),0)))
    in
        (a2=[] andalso b2 = []) orelse ((#1 a) = (#1 b) andalso (a2 = b2))
    end;


fun int_less(a : (string * int list),b : (string * int list)) = 
    if  int_equal(a,b) then
        false
    else 
        if (#1 a) = "~" andalso (#1 b) = "+" then true else
        if (#1 a) = "+" andalso (#1 b) = "~" then false else
        if (#1 a) = "+" andalso (#1 b) = "+" then leq((#2 a),(#2 b))
        else leq((#2 b),(#2 a));

fun int_leq(a : (string * int list),b : (string * int list)) = int_less(a,b) orelse int_equal(a,b);

fun int_greater(a : (string * int list),b : (string * int list)) = not (int_leq(a,b));

fun int_geq(a : (string * int list),b : (string * int list)) = not (int_less(a,b));

fun int_neq(a : (string * int list),b : (string * int list)) = not (int_equal(a,b));

fun int_show(a : (string * int list)) = if (#1 a) = "+" then intLstTOstr(rev(#2 a)) else "~" ^ intLstTOstr(rev(#2 a));
end;



functor Rat(BInt : BIGINT) :
(* signature RATIONAL =  *)
sig
(* type bigint = BInt.bigint *)
type rational
exception rat_error
val from_frac: string -> rational
val make_rat: BInt.bigint * BInt.bigint -> rational option
val rat: BInt.bigint -> rational option
val reci: BInt.bigint -> rational option
val neg: rational -> rational
val inverse : rational -> rational option
val equal : rational * rational -> bool (* equality *)
val less : rational * rational -> bool (* less than *)
val leq : rational * rational -> bool 
val greater : rational * rational -> bool
val geq : rational * rational -> bool
val neq : rational * rational -> bool
val add : rational * rational -> rational (* addition *)
val subtract : rational * rational -> rational (* subtraction *)
val multiply : rational * rational -> rational (* multiplication *)
val divide : rational * rational -> rational option (* division *)
val showRat : rational -> string
val showDecimal : rational -> string
val fromDecimal : string -> rational
(* val ind : char list * int * int * int -> int * int * int
val makenum : string * string * int -> string *)
val toDecimal : rational -> string
(* (BInt.bigint * BInt.bigint) list *)
end
=
(* structure Rational : RATIONAL = *)
struct
(* type bigint = BInt.bigint *)
type rational = (BInt.bigint * BInt.bigint)
exception rat_error
fun make_rat(a,b) = if BInt.int_magn(b) = [] then NONE else SOME (BInt.int_norm(a,b));
fun frachelp(lst,i) = if lst = [] then raise rat_error else if hd(lst) = #"/" then i else frachelp(tl(lst),i+1);
fun from_frac(s) = 
    let val v = frachelp(explode(s),0)
    in
        let val w = BInt.int_make_bigint(String.extract(s,v+1,NONE))
        in if BInt.int_magn(w) = [] then raise rat_error else valOf(make_rat(BInt.int_make_bigint(String.substring(s,0,v)),w))
        end
    end;
fun rat(a) = SOME (a,BInt.int_zip("+",[1]));
fun reci(a) = if BInt.int_magn(a) = [] then NONE else SOME (BInt.int_zip("+",[1]),a);
fun neg(r : rational) = (BInt.int_neg((#1 r)),(#2 r));
fun inverse(a,b) = if BInt.int_magn(a) = [] then NONE else SOME (BInt.int_norm(b,a));
fun equal(r1 : rational,r2 : rational) = (BInt.int_magn(#1 r1) = [] andalso (BInt.int_magn(#1 r2)) = []) orelse (BInt.int_equal(#1(BInt.int_norm(r1)),#1(BInt.int_norm(r2))) andalso BInt.int_equal(#2(BInt.int_norm(r1)),#2(BInt.int_norm(r2))));
fun less(r1 : rational,r2 : rational) = BInt.int_less(BInt.int_mul((#1 r1,#2 r2)),BInt.int_mul((#2 r1,#1 r2)));
fun leq(r1 : rational,r2 : rational) = less(r1,r2) orelse equal(r1,r2);
fun greater(r1 : rational,r2 : rational) = not (leq(r1,r2));
fun geq(r1 : rational,r2 : rational) = not (less(r1,r2));
fun neq(r1 : rational,r2 : rational) = not (equal(r1,r2));
fun add(r1 : rational,r2 : rational) = (BInt.int_norm(BInt.int_add(BInt.int_mul((#1 r1,#2 r2)),BInt.int_mul((#2 r1,#1 r2))),BInt.int_mul((#2 r1,#2 r2))));
fun subtract(r1 : rational,r2 : rational) = add(r1,neg(r2));
fun multiply(r1 : rational,r2 : rational) = (BInt.int_norm(BInt.int_mul(#1 r1,#1 r2),BInt.int_mul(#2 r1,#2 r2)));
fun divide(r1 : rational,r2 : rational) = if BInt.int_magn(#2 r2) = [] then NONE else SOME (multiply(r1,valOf(inverse(r2))));
fun showRat(r : rational) = BInt.int_show(#1 r)^"/"^BInt.int_show(#2 r)
fun ind(lst,count,dot,brac) = if lst = [] then (dot,brac,count-1) else if hd(lst) = #"." then ind(tl(lst),count+1,dot+count+1,brac) else if hd(lst) = #"(" then ind(tl(lst),count+1,dot,brac+count+1) else ind(tl(lst),count+1,dot,brac);
fun makenum(ans,s,n) = if n>0 then makenum(ans^s,s,n-1) else ans;
fun dTOrhelp(s) = 
    let val v = ind(explode(s),0,~1,~1)
    in  
        if (#1 v) = ~1 orelse (#2 v) = ~1 orelse String.extract(s,(#3 v),SOME 1) <> ")"  then raise rat_error else
        if (#2 v) + 1 = (#3 v) then valOf(make_rat((BInt.int_make_bigint(String.substring(s,0,(#1 v))^String.substring(s,(#1 v)+1,((#2 v)-(#1 v)-1)))),(BInt.int_make_bigint(makenum("1","0", ((#2 v)-(#1 v)-1))))))
        else
        add(valOf(make_rat((BInt.int_make_bigint(String.substring(s,0,(#1 v))^String.substring(s,(#1 v)+1,((#2 v)-(#1 v)-1)))),(BInt.int_make_bigint(makenum("1","0", ((#2 v)-(#1 v)-1)))))),valOf(make_rat((BInt.int_make_bigint(String.substring(s,(#2 v)+1,((#3 v)-(#2 v)-1)))),(BInt.int_make_bigint(makenum("","9", ((#3 v)-(#2 v)-1))^makenum("","0", ((#2 v)-(#1 v)-1)))))))
    end;  
fun fromDecimal(s) = if hd(explode(s)) = #"~" then 
    neg(dTOrhelp(String.extract(s,1,NONE)))
    else dTOrhelp(s);
fun first(a,b) = a;
fun finder x y = if BInt.int_equal(x,first(y)) then true else false;
fun addZero(a : BInt.bigint) = BInt.int_zip(BInt.int_sign(a),0::BInt.int_magn(a))
fun decPrinter(rm : (BInt.bigint * BInt.bigint) list,v : (BInt.bigint * BInt.bigint)) = 
    if null(tl(rm)) then if BInt.int_equal((#1 (hd(rm))), (#1 v)) then ".(" else "."
    else if BInt.int_equal((#1 (hd(rm))), (#1 v)) then 
        decPrinter(tl(rm),v) ^ BInt.int_show((#2 (hd(rm)))) ^ "("
    else
        decPrinter(tl(rm),v) ^ BInt.int_show((#2 (hd(rm))));
fun decPart(rm,a,b) = 
    let val v = BInt.int_div(a,b)
    in
        if List.exists (finder (#1 v)) rm = false then decPart(v::rm,addZero((#1 v)),b)
        else decPrinter(v::rm,v)
    end;
fun toDecimal(r) = 
    let val v  = (BInt.int_div(r))
    val divisor = BInt.int_zip("+",BInt.int_magn(#2 r))
    in 
        let val st = decPart([v],addZero(#1 v),divisor)
        in
            BInt.int_show(#2 v) ^ String.substring(st,0,size(st)-1) ^ ")"
        end
    end;
fun showDecimal(r) = toDecimal(r);
end;

structure Rational = Rat(BigInt);

(* val v1 = BInt.int_make_bigint("43")
val v2 = BInt.int_make_bigint("35")
val v3 = Rational.toDecimal(v1,v2)
val v1 = BInt.int_make_bigint("1")
val v2 = BInt.int_make_bigint("3")
val v4 = Rational.toDecimal(v1,v2)
val v1 = BInt.int_make_bigint("1")
val v2 = BInt.int_make_bigint("24")
val v5 = Rational.toDecimal(v1,v2)
val v1 = BInt.int_make_bigint("43")
val v2 = BInt.int_make_bigint("99")
val v6 = Rational.toDecimal(v1,v2)
val v1 = BigInt.int_make_bigint("~1534724615")
val v2 = BigInt.int_make_bigint("353717")
val v7 = Rational.toDecimal(v1,v2) *)


(* Rational.toDecimal(("~",[5]),("~",[2]));
Rational.toDecimal(("~",[0,1]),("~",[4])); 
val r = (("~",[0,1]),("~",[4])); 
BInt.int_norm(r);
(BInt.int_div(BInt.int_norm(r))); *)

(* valOf(Rational.make_rat((BInt.int_make_bigint(String.substring(s,0,(#1 v))^String.substring(s,(#1 v)+1,((#2 v)-(#1 v)-1)))),(BInt.int_make_bigint(makenum("1","0", ((#2 v)-(#1 v)-1))))))
valOf(Rational.make_rat((BInt.int_make_bigint(String.substring(s,(#2 v)+1,((#3 v)-(#2 v)-1)))),(BInt.int_make_bigint(makenum("","9", ((#3 v)-(#2 v)-1))^makenum("","0", ((#3 v)-(#1 v)-3)))))) *)



(* Rational.toDecimal(valOf(Rational.make_rat(((BInt.int_make_bigint("43")),(BInt.int_make_bigint("99"))))));
1534724615/353717 *)