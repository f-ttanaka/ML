let rec fact n = if n == 0 then 1 else n * (fact (n - 1)) ; 
fact 6;

let rec even n =
  if n == 0 then true else odd (n - 1)
and odd n =
  if n == 0 then false else even (n - 1);

even 6 ;
even 0 ;
odd 11 ;
odd 14 ;

let id x = x 
and const x y = x ;

id 6 ;
id true ;
const 1 true ;