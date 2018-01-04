# Advent of code 2017

This repository contains solutions for puzzles from [Advent of code 2017](http://adventofcode.com/2017).

## Run

This project uses Haskell Stack.

Start ghci:
```bash
$ stack ghci
```

Run tests for the day problem:
```
ghci> day9Test
True
ghci> day9extraTest
True
```

Run solution on input file:
```
ghci> day9 <$> readFile "in/day9.in"
```

## [Day 1](http://adventofcode.com/2017/day/1)

### Problem

Given a string of digits, find a sum of all digits which are followed by the same digit. The first digit is considered to follow the last digit in the string.

For example:
* `1122` produces a sum of `3` (`1 + 2`) because the first digit (`1`) matches the second digit and the third digit (`2`) matches the fourth digit.
* `1111` produces `4` because each digit (all `1`) matches the next.
* `1234` produces `0` because no digit matches the next.
* `91212129` produces `9` because the only digit that matches the next one is the last digit, `9`.

### Extra problem

Given a string of digits of even length, find a sum of all digits which are _opposed_ by the same digit. Digits in the string are _opposing_ each other when the difference between their indices in the string is equal to a half of the string's length.

For example:
* `1212` produces `6`: the list contains `4` items, and all four digits match the digit `2` items ahead.
* `1221` produces `0`, because every comparison is between a `1` and a `2`.
* `123425` produces `4`, because both `2`s match each other, but no other digit has a match.
* `123123` produces `12`.
* `12131415` produces `4`.

## [Day 2](http://adventofcode.com/2017/day/2)

### Problem 

Given a several rows of numbers, for each row find a difference between maximum and minimum element of the row and then find the sum of these differences.

For example, given the following input:
```
5 1 9 5
7 5 3
2 4 6 8
```
* The first row's largest and smallest values are `9` and `1`, and their difference is `8`.
* The second row's largest and smallest values are `7` and `3`, and their difference is `4`.
* The third row's difference is `6`.
* In this example, the sum of the differences would be `8 + 4 + 6 = 18`.

### Extra problem

Given a several rows of numbers, for each row find the only divisible pair and then find the sum of division results. A pair is called _divisible_ when a result of division of one element by another is a whole number. 

For example, given the following input:
```
5 9 2 8
9 4 7 3
3 8 6 5
```
* In the first row, the only two numbers that evenly divide are `8` and `2`; the result of this division is `4`.
* In the second row, the two numbers are `9` and `3`; the result is `3`.
* In the third row, the result is `2`.
* In this example, the sum of the results would be `4 + 3 + 2 = 9`.

## [Day 3](http://adventofcode.com/2017/day/3)

### Problem 

Given an index in the infinite spiral below, find [Manhattan Distance](https://en.wikipedia.org/wiki/Taxicab_geometry)
between position of the index and the center.
```
17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...
```

For example:
* Distance from index `1` is `0` because it is already the center.
* Distance from `12` is `3` steps, such as: down, left, left.
* Distance from `23` is only `2` steps: up twice.
* Distance from `1024` is `31` steps.

### Extra problem

Find the first value in an infinite spiral of adjoined sums below, 
which is greater than the given value.
```
147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...
```

The spiral is build incrementally, setting a value of each cell 
to a sum of all adjoined cells which has already been computed.

## [Day 4](http://adventofcode.com/2017/day/4)

### Problem 

Given a list of phrases, count how many of them does not contain duplicate words.
Each word in a phrase consists only of lowercase latin letters.

The answer for the following example is `2` because only the second phrase has duplicate word `aa`.
```
aa bb cc dd
aa bb aa dd
aa bb cc aaa
```

### Extra problem

Given a list of phrases, count how name of them does not contain anagram words.
Two words are anagrams of each other if one word can be obtained by permutation of letters in the other.
Each word in a phrase consists only of lowercase latin letters.

The answer for the following example is `2` because only the second phrase contains anagrams `aba` and `baa`.
```
aa bb cc db
aba db baa dd
aa bb cc aaa
```

## [Day 5](http://adventofcode.com/2017/day/5)

### Problem 

Given an array of offsets, determine how many steps will it take to leave the array.
You begin at the first position in the array.
On each turn, add current position value to index and jump there.
Also increment the value in the cell which you have just left.

Example sequence of states which finishes in `5` steps:
* `(0) 3  0  1  -3` - before we have taken any steps.
* `(1) 3  0  1  -3` - jump with offset `0` (that is, don't jump at all). Fortunately, the instruction is then incremented to `1`.
* `2 (3) 0  1  -3 ` - step forward because of the instruction we just modified. The first instruction is incremented again, now to `2`.
* `2  4  0  1 (-3)` - jump all the way to the end; leave a `4` behind.
* `2 (4) 0  1  -2 ` - go back to where we just were; increment `-3` to `-2`.
* `2  5  0  1  -2 ` - jump `4` steps forward, escaping the array.

### Extra problem

Problem is the same as previous except the increment rule.
Now the value in a cell is incremented only if it is less than 3 and decremented otherwise.

Starting with array:
```
(0) 3  0  1  -3 
```
It is now takes 10 steps to escape, leaving the array in the following state:
```
2 3 2 3 -1
```

## [Day 6](http://adventofcode.com/2017/day/6)

### Problem 

Given an array of numbers, find how many iteration it will take to repeat the configuration.
Iteration consists of several steps:
* find the biggest element in array (first if thereare several);
* empty the value of the biggest element;
* redistribute the value across all elements adding it one by one in circles starting from the next element.

The following sequence shows repeating configuration after `5` steps.
```
0 2 7 0
2 4 1 2 *
3 1 2 3
0 2 3 4
1 3 4 1
2 4 1 2 *
```

### Extra problem

For the previous problem, find the size of the loop in configurations.

For the previous example the size is `4`.

## [Day 7](http://adventofcode.com/2017/day/7)

### Problem 

Given a tree description in the following form, find the root of the tree.
The numbers are node weights.
```
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
```

This tree may look like this when constructed:
```
                gyxo
              /     
         ugml - ebii
       /      \     
      |         jptl
      |        
      |         pbga
     /        /
tknk --- padx - havc
     \        \
      |         qoyq
      |             
      |         ktlj
       \      /     
         fwft - cntj
              \     
                xhth
```

Therefore the root is `tknk`.

### Extra problem

There is an error in weights in the input for the previous problem.
For each node in the tree total weight of each child (including weights of all sub-children) must be equal.

From the given example:
* `ugml` + (`gyxo` + `ebii` + `jptl`) = 68 + (61 + 61 + 61) = 251
* `padx` + (`pbga` + `havc` + `qoyq`) = 45 + (66 + 66 + 66) = 243
* `fwft` + (`ktlj` + `cntj` + `xhth`) = 72 + (57 + 57 + 57) = 243

It is known that there is only one node which weight is incorrect.
Find the correct weight which such a node must have.

In the example it is `ugml` node which has weight `68` 
but should have weight `60` so that the total weight of this node would be equal to `243` 
as it is for other siblings.

## [Day 8](http://adventofcode.com/2017/day/8)

### Problem 

Given a list of instructions for registers in the following form, find max register value after execution of all instructions.
All registers start with value `0`. Register names are not known in advance.
Each instruction must be executed only if condition hold and skipped otherwise.
```
<register name> <inc | dec> <signed number> if <register name> <op> <signed number>
```
Possible `<op>` values: `<` , `>`, `<=`, `>=`, `==`, `!=`.

For example the following instructions are annotated with register values (after execution):
```
b inc 5 if a > 1         [a=0, b=0]
a inc 1 if b < 5         [a=1, b=0]
c dec -10 if a >= 1      [a=1, b=0, c=10]
c inc -20 if c == 10     [a=1, b=0, c=-10]
```

The maximum value among all registers at the end is value `1` for register `a`.

### Extra problem 

For the previous problem, find maximum value among all registers at any point of execution.

The answer for the given example is value `10` for register `c` after third instruction.

## [Day 9](http://adventofcode.com/2017/day/9)

### Problem 

Given a specially formatted string, find sum of scores for all groups in it.

The rules are the following:
* group starts with `'{'` and ends with `'}'`;
* groups can be nested;
* group's score is it's nestedness level;
* the top group has a level `1`;
* groups are separated by `','`;
* there is garbage in the input;
* garbage starts with `'<'` and ends with `'>'`;
* any characters inside garbage except `'!'` must be ignored;
* `'!'` character makes the next character ignored;
* `'!'` can be inside or outside garbage;
* `'!'` also ignores `'!'` if it follows;

Examples:
* `{}`, score of `1`.
* `{{{}}}`, score of `1 + 2 + 3 = 6`.
* `{{},{}}`, score of `1 + 2 + 2 = 5`.
* `{{{},{},{{}}}}`, score of `1 + 2 + 3 + 3 + 3 + 4 = 16`.
* `{<a>,<a>,<a>,<a>}`, score of `1`.
* `{{<ab>},{<ab>},{<ab>},{<ab>}}`, score of `1 + 2 + 2 + 2 + 2 = 9`.
* `{{<!!>},{<!!>},{<!!>},{<!!>}}`, score of `1 + 2 + 2 + 2 + 2 = 9`.
* `{{<a!>},{<a!>},{<a!>},{<ab>}}`, score of `1 + 2 = 3`.

### Extra problem 

For the previous problem, count number of non-ignored characters *inside* garbage.

Examples self-contained garbage:
* `<>`, `0` characters.
* `<random characters>`, `17` characters.
* `<<<<>`, `3` characters.
* `<{!>}>`, `2` characters.
* `<!!>`, `0` characters.
* `<!!!>>`, `0` characters.
* `<{o"i!a,<{i<a>`, `10` characters.

## [Day 10](http://adventofcode.com/2017/day/10)

## [Day 11](http://adventofcode.com/2017/day/11)

### Problem 

Given directions on a hex grid, find the distance between the last position and the center.
```
  \ n  /
nw +--+ ne
  /    \
-+      +-
  \    /
sw +--+ se
  / s  \
```

For example:
* `ne,ne,ne` is `3` steps away.
* `ne,ne,sw,sw` is `0` steps away (back where you started).
* `ne,ne,s,s` is `2` steps away (`se,se`).
* `se,sw,se,sw,sw` is `3` steps away (`s,s,sw`).

### Extra problem 

For the previous problem, find the longest distance to the center throughout the whole journey.