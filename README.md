# Advent Of Code 2022
This is my repository for annual **Advent of Code**.

It is a set of 25 programming challenges that can be solved in any language.  
For this year's edition I have decided to attempt to complete it in Haskell.  
</br>

# Notes on each day's challenge
## [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1)

It was an easy exercise based on analizing lists.  
Bigger challange than the exercise itself, was importing text files to haskell as this was something I have not done before.

## [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2)

Very simple exercise that required me to use Map.  
It was basically implementing scoring system for rock-paper-scissors game

## [Day 3: Rucksack Reorganization](https://adventofcode.com/2022/day/3)

An easy exercise of analizing strings. The solution is based on Set implementation and some list modifications.

First part of the challenge required me to find a character that was common in both halfs of the string:  
```
"wgqJtbJMqZVTwWPZZT" --> wgqJtbJMq | ZVTwWPZZT
                         ^              ^
```


## [Day 4: Camp Cleanup](https://adventofcode.com/2022/day/4)

Interesting problem that required input modification. Strings needed to be split into lists of numbers, 
however thanks to their symmetric form it was not hard.

## [Day 5: Supply Stacks](https://adventofcode.com/2022/day/5)

The hardest part of this exercise was parsing the input. It was made out of two parts, where one graphically represented columns of boxes. 
However, with a couple of functions I have managed to convert it into a Map.  
The rest of the exercise was quite straightforward as it required only string modifications.

This shows how I have converted first part of the input:
```
[A]                     Map:
[B]     [G]     -->     1: "ABCD"
[C] [E] [H]             2: "EF"
[D] [F] [I]             3: "GHI"
```

## [Day 6: Tuning Trouble](https://adventofcode.com/2022/day/6)

This day was suprisingly easy. Just some string analizing with use of Sets.

In the first part I had to find a first occurance of a substring of length 4, in which all characters are unique:
```
bvwbjplbgvbhsrlpgdmjqwftvncz
 ^^^^
```
Second part was similar, but the length of a substring was 14.
## [Day 7: No Space Left On Device](https://adventofcode.com/2022/day/7)

It was really interesting exercise.  
The input for challange day can be described as a console output after traversal of directories.  
I have managed to solve it with quite messy parsing and the use of reqursion and trees.

This shows how the input should have been interpreted:
```
$ cd /              /               
$ ls                ├─ a            
dir a               |  ├─ e         
14848514 b.txt      |  |  └─ i      (size: 584)   
8504156 c.dat       |  ├─ f         (size: 29116) 
dir d               |  ├─ g         (size: 2557)
$ cd a              |  └─ h.lst     (size: 62596)  
$ ls                ├─ b.txt        (size: 14848514)
dir e               ├─ c.dat        (size: 8504156)
29116 f             └─ d            
2557 g                 ├─ j         (size: 4060174) 
62596 h.lst            ├─ d.log     (size: 8033020)
$ cd e                 ├─ d.ext     (size: 5626152)
$ ls                   └─ k         (size: 7214296)
584 i                   
$ cd ..                 
$ cd ..                 
$ cd d                  
$ ls                    
4060174 j               
8033020 d.log           
5626152 d.ext           
7214296 k        
```

## [Day 8: Treetop Tree House](https://adventofcode.com/2022/day/8)

Another very interesting challenge!  
The exercise required me to treat input as a matrix and implement some operations for it.  
The first part of the challenge required to determin whether a tree would be visible from outside of the forest. The tree is visible if there are no trees higher than it that would obstruct a view.  

Below is visualization showing which trees would be visible:

```
422411333           █████████
301324204           █   ██  █
410011302           █     █ █
103430412           █ ███ █ █
431144033   -->     ██  ██ ██
213325510           █ █  ██ █
031003015           ██      █
030330000           ██ ██   █
113235411           █████████
```

## [Day 9: Rope Bridge](https://adventofcode.com/2022/day/9)

This challenge was quite hard - especially the second part. But it was also very engaging.  
I would have had much bigger problem with this exercise, 
however thankfully I had covered quite simmilar task duiring lectures on Functional Programming. 
Thanks to that, I was able to reuse some of my code and the logic that comes with it.  
The second part of the problem involved reqursion and made the whole problem more difficult. 
But to get solution for it, I just needed to extend my code from part 1.

Visualization below shows how the rope would behave:  
*(H signifies the head of the rope, and T its tail)*

```
Pull H up:
.....    .....    .....
.....    ..H..    ..H..
..H.. -> ..... -> ..T..
.T...    .T...    .....
.....    .....    .....

Pull H to the right:
.....    .....    .....
.....    .....    .....
..H.. -> ...H. -> ..TH.
.T...    .T...    .....
.....    .....    .....

Pull H down:
.....    .....    .....
..T..    ..T..    .....
..H.. -> ..... -> ..T..
.....    ..H.. -> ..H..
.....    .....    .....
```
Second part of the challenge added more ropes connected to each other, such that tail of the rope was fixed to the head of the next one.

## [Day 10: Cathode-Ray Tube](https://adventofcode.com/2022/day/10)

Although the challenge was not very complicated, the solution for it was very original.  
As oppose to previous exercises, where the answer was just a series of numbers or letters, this time the solution was much more graphical. 

The input consited of a bunch of instructions, which decoded in a specified way, led to the console output like this:
```
.###.#..#.###..####.###.
#....#..#.#..#.#....#..#
.##..#..#.#..#.###..#..#
...#.#..#.###..#....###.
#..#.#..#.#....#....#.#.
.##..####.#....####.#..#
```
And this is how would the output look like with the use of other characters for bigger contrast and better visibility:
```
 ███ █  █ ███  ████ ███  
█    █  █ █  █ █    █  █ 
█    █  █ █  █ ███  █  █ 
 ██  █  █ ███  █    ███  
   █ █  █ █    █    █ █  
███   ██  █    ████ █  █ 
 ```
 I think that this is a really interesting and unique way of getting a solution.

 ## [Day 11: Monkey in the Middle](https://adventofcode.com/2022/day/11)
 This is the first day where I actually had a problem with getting solutons!  
 First of all, the input required much more parsing than before:
 ```
 Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
 ```
 Thankfully, a couple of days ago, during my Funcitonal Programming module at university, we covered parser. This was very helpfull today, so the input conversion was not that hard, although it was still gave me some hard times.  

 With nicely parsed input the soluton for first part was moderatly difficult, however not significantly comparing to previous days.

 But this is was not the case with second part!  
 Small modification to the exercise caused some numbers to be so large that integers started to overflow. *(In fact ridiculously large - from what I have found it could be around [10^72 digits long](https://www.reddit.com/r/adventofcode/comments/zioepr/2022_day_11_part_2_ridiculous_worry_levels/)!)*. However, after discovering some correlations in the input *--inspired by small online search--* and the use of modular arithmetics I have managed to get those values to be significantly smaller.

 It was definitely an intersting challenge! 

