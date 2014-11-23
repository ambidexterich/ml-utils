(* Author: Rich Hamburg *)
(* CSC 345 *)
(* Due Date: 11/18/2014 *)

(*
3.3.2 - flip
============
Flips alternating elements of a list
Ex.
- flip([1,2,3,4,5])
[2,1,4,3,5]
*)
fun flip (nil) = nil
|   flip (x::y::xs) = y::x::flip(xs)
|   flip (x) = x;

(*
3.3.3 - deleteIth
=================
Removes the ith element from a list. If ith is out of range, then return the
list as is.

Uses a 1 based index so the first element is 1 and not 0
Ex.
- deleteIth([1,2,3,4,5], 1)
[2,3,4,5]

Ex.
- deleteIth([1,2,3,4,5], 0)
[1,2,3,4,5]
*)
fun deleteIth ([], _) = []
|   deleteIth (x::xs, 1) = xs (* If the head is chosen just return tail *)
|   deleteIth (x::xs, i) = if length(x::xs) < i then x::xs
                       else x::deleteIth(xs, i-1);

(*
3.3.9 - beginsWithAVowel
========================
Tests whether a given word begins with a vowel or not.
Returns true if it does, otherwise false.
Ex.
- beginsWithAVowel("hello")
false
- beginsWithAVowel("ello")
true
*)

(* Test whether a given string starts with a vowel *)
fun beginsWithAVowel (s) = let
                                val l = hd(explode(s))
                             in
                                l = #"A" orelse l = #"a" orelse
                                l = #"E" orelse l = #"e" orelse
                                l = #"I" orelse l = #"i" orelse
                                l = #"O" orelse l = #"o" orelse
                                l = #"U" orelse l = #"u" orelse
                                false
                             end;

(*
3.3.10 - piglatinize
====================
Convert a string into piglatin by appending 'yay' to words that start with
a vowel. For words starting with consonants shift all consonants back
until the first vowel and append 'ay'. It ignores words that have no vowels
or only contains 'y' as the vowel
Ex.
- piglatinize('spry')
spry
- piglatinize('apple')
appleyay
- piglatinize('science')
iencescay
*)

(* HELPERS *)
(* Expose these helpers for testing purposes *)

(* Test if a word contains a vowel *)
fun hasVowel (nil) = false
|   hasVowel (x::xs) = beginsWithAVowel(implode(x::xs)) orelse hasVowel(xs);

(* Shift the items of the list from the front to the back until a vowel is
at the beginning *)
fun shiftToVowel (nil, acc) = acc
|   shiftToVowel (x::xs, acc) = if beginsWithAVowel(implode(x::xs)) then
                                    x::xs @ acc
                                else shiftToVowel(xs, acc @ [x]);
(* END HELPERS *)

val v_suffix = "yay";
val c_suffix = "ay";
(* Convert a string into its piglatin *)
fun piglatinize (s) = if hasVowel(explode(s)) then
                          if beginsWithAVowel(s) then s^v_suffix
                          else
                             implode(shiftToVowel(explode(s), []))^c_suffix
                        else s;
