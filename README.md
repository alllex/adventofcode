# Advent of code 2017

This repository contains solutions for puzzles from [Advent of code 2017](http://adventofcode.com/2017).

## [Day 1](http://adventofcode.com/2017/day/1)

### Problem 1

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

#### Extra problem

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