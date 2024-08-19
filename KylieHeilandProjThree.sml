(*This program uses 5 different functions; two of which that work together: mymap1/mymap2 
  and ordlist. Two functions are used as a replacement of the built-in map function. 
  The remaining three functions convert characters to ASCII (ordlist), determine the
  length of a list (mylength), and find the maximum integer in a list.
  Kylie Heiland
  CSCI 421_VA
  Professor Ghunaim
  3/31/23*)
  


(*Has the same behavior as a map function, but is actually foldr*)
fun mymap1 f list=
  foldr(fn(a, b) => (f a)::b) [] list;

(*Has the same behavior as a map function via. recursive calls*)
fun mymap2 f [] = []
  | mymap2 f list = 
    (f(hd list))::(mymap2 f(tl list));

(*Returns the ASCII code of each character from a list of characters, using mymap1*)
fun ordlist (charList) =
  mymap1 (fn x=>ord(x))charList;

ordlist [#"A", #"b", #"C"];

(*Returns the length of the list without the use of the built-in length function*)
fun mylength [] = 0
  | mylength (intList) = 1 + mylength(tl(intList));

mylength [~3, 2, 1, 4, 6, 0, 7, ~2];

(*Finds the maximum integer in a list using recursion*)
fun max (x::nil) = x
  | max(x::y::xs) = 
    if(x > y) 
      then max(x::xs)
    else
      max(y::xs);

max [~3, 2, 10, 4, 6, 0, 7];