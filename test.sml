(* Author: Rich Hamburg *)

use "utils.sml";
(**** TESTS ****)
(***
    Passing tests should return "PASS",
   failing tests should return "FAIL"
***)

fun assert (actual, expect) =
    if actual = expect then
        print "PASS\n" else
        print "FAIL\n";

(* 3.3.2 - flip
   ============ *)

(* Single element list *)
print "\n\n- flip([1])\n";
flip([1]);
assert(flip([1]), [1]);

(* Double element list *)
print "\n\n- flip([1,2])\n";
flip([1,2]);
assert(flip([1,2]), [2,1]);

(* Odd number of integers *)
print "\n\n- flip([1,2,3,4,5])\n";
flip([1,2,3,4,5]);
assert(flip([1,2,3,4,5]), [2,1,4,3,5]);

(* Even number of integers *)
print "\n\n- flip([1,2,3,4])\n";
flip([1,2,3,4]);
assert(flip([1,2,3,4]), [2,1,4,3]);

(* Empty list *)
print "\n\n- flip([])\n";
flip([]);
assert(flip([]), []);

(* Nil element *)
print "\n\n- flip(nil)\n";
flip(nil);
assert(flip(nil), nil);

(* Even characters *)
print "\n\n- flip([#\"a\", #\"b\",#\"c\",#\"d\"])\n";
flip([#"a",#"b",#"c",#"d"]);
assert(flip([#"a",#"b",#"c",#"d"]), [#"b",#"a",#"d",#"c"]);

(* 3.3.3 - deleteIth
   ================= *)

(* An empty list *)
print "\n\n- deleteIth([], 3)\n";
deleteIth([], 3);
assert(deleteIth([], 3), []);

(* An out of range index*)
print "\n\n- deleteIth([1,2,3,4,5], 0)\n";
deleteIth([1,2,3,4,5], 0);
assert(deleteIth([1,2,3,4,5], 0), [1,2,3,4,5]);

print "\n\n- deleteIth([1,2,3,4,5], ~1)\n";
deleteIth([1,2,3,4,5], ~1);
assert(deleteIth([1,2,3,4,5], ~1), [1,2,3,4,5]);

print "\n\n- deleteIth([1,2,3,4,5], 6)\n";
deleteIth([1,2,3,4,5], 6);
assert(deleteIth([1,2,3,4,5], 6), [1,2,3,4,5]);

(* In range index *)
print "\n\n- deleteIth([1,2,3,4,5], 4)\n";
deleteIth([1,2,3,4,5], 4);
assert(deleteIth([1,2,3,4,5], 4), [1,2,3,5]);

(* A list of characters *)
print "\n\n- deleteIth([#\"a\",#\"b\",#\"c\",#\"d\"], 3)\n";
deleteIth([#"a", #"b",#"c",#"d"], 3);
assert(deleteIth([#"a", #"b",#"c",#"d"], 3), [#"a",#"b",#"d"]);

(* 3.3.9 - beginsWithAVowel
   ======================== *)

(* Basic lowercase fail case *)
print "\n\n- beginsWithAVowel(\"hello\")\n";
beginsWithAVowel("hello");
assert(beginsWithAVowel("hello"), false);

(* Basic lowercase pass case *)
print "\n\n- beginsWithAVowel(\"allo\")\n";
beginsWithAVowel("allo");
assert(beginsWithAVowel("allo"), true);

print "\n\n- beginsWithAVowel(\"ello\")\n";
beginsWithAVowel("ello");
assert(beginsWithAVowel("ello"), true);

print "\n\n- beginsWithAVowel(\"illo\")\n";
beginsWithAVowel("illo");
assert(beginsWithAVowel("illo"), true);

print "\n\n- beginsWithAVowel(\"ollo\")\n";
beginsWithAVowel("ollo");
assert(beginsWithAVowel("ollo"), true);

print "\n\n- beginsWithAVowel(\"ullo\")\n";
beginsWithAVowel("ullo");
assert(beginsWithAVowel("ullo"), true);

(* Basic uppercase fail case *)
print "\n\n- beginsWithAVowel(\"Hello\")\n";
beginsWithAVowel("Hello");
assert(beginsWithAVowel("Hello"), false);

(* Basic uppercase pass case *)
print "\n\n- beginsWithAVowel(\"Ello\")\n";
beginsWithAVowel("Ello");
assert(beginsWithAVowel("Ello"), true);

(* Single upper character vowel case *)
print "\n\n- beginsWithAVowel(\"I\")\n";
beginsWithAVowel("I");
assert(beginsWithAVowel("I"), true);

(* 3.3.10 - piglatinize
   ==================== *)

(* piglatinize Helpers *)
(* ----------------------------------------- *)

(* No vowels *)
print "\n\n- hasVowel(\"spry\")\n";
hasVowel(explode("spry"));
assert(hasVowel(explode("spry")), false);

(* Starts with a vowel *)
print "\n\n- hasVowel(\"apple\")\n";
hasVowel(explode("apple"));
assert(hasVowel(explode("apple")), true);

(* Starts with a consonant *)
print "\n\n- hasVowel(\"science\")\n";
hasVowel(explode("science"));
assert(hasVowel(explode("science")), true);

(* shiftToVowel *)
(* No vowels *)
print "\n\n- shiftToVowel(\"spry\", [])\n";
shiftToVowel(explode("spry"), []);
assert(shiftToVowel(explode("spry"), []), [#"s",#"p",#"r",#"y"]);

(* Starts with a vowel *)
print "\n\n- shiftToVowel(\"apple\", [])\n";
shiftToVowel(explode("apple"), []);
assert(shiftToVowel(explode("apple"), []), [#"a",#"p",#"p",#"l",#"e"]);

(* Starts with a consonant*)
print "\n\n- shiftToVowel(\"science\", [])\n";
shiftToVowel(explode("science"), []);
assert(shiftToVowel(explode("science"), []), [#"i",#"e",#"n",#"c",#"e",#"s",#"c"]);

(* End piglatinize Helpers *)
(* ----------------------------------------- *)

(* Word without vowels *)
print "\n\n- piglatinize(\"spry\")\n";
piglatinize("spry");
assert(piglatinize("spry"), "spry");

(* Word starting with a vowel *)
print "\n\n- piglatinize(\"apple\")\n";
piglatinize("apple");
assert(piglatinize("apple"), "appleyay");

(* Word starting with multiple consonants *)
print "\n\n- piglatinize(\"science\")\n";
piglatinize("science");
assert(piglatinize("science"), "iencescay");
