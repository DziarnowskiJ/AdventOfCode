# Advent Of Code 2022
This is my repository for the annual **Advent of Code**.
 
It is a set of 25 programming challenges that can be solved in any language.  
For this year's edition, I have decided to attempt to complete it in Haskell.  
</br>
 
# Notes on each day's challenge
## [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1)
 
It was an easy exercise based on analyzing lists.  
A bigger challenge than the exercise itself was importing text files to haskell as this was something I have not done before.
 
## [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2)
 
A very simple exercise that required me to use Map.  
It was just implementation of the scoring system for the rock-paper-scissors game
 
## [Day 3: Rucksack Reorganization](https://adventofcode.com/2022/day/3)
 
This was an easy exercise for analyzing strings. The solution is based on Set implementation and some list modifications.
 
The first part of the challenge required me to find a character that was common in both halves of the string:  
```
"wgqJtbJMqZVTwWPZZT" --> wgqJtbJMq | ZVTwWPZZT
                         ^              ^
```
 
 
## [Day 4: Camp Cleanup](https://adventofcode.com/2022/day/4)
 
An interesting problem that required input modification. Strings needed to be split into lists of numbers,
however thanks to their symmetric form it was not hard.
 
## [Day 5: Supply Stacks](https://adventofcode.com/2022/day/5)
 
The hardest part of this exercise was parsing the input. It was made out of two parts, where one graphically represented columns of boxes.
However, with a couple of functions, I have managed to convert it into a Map.  
The rest of the exercise was quite straightforward as it required only string modifications.
 
This shows how I have converted first part of the input:
```
[A]                     Map:
[B]     [G]     -->     1: "ABCD"
[C] [E] [H]             2: "EF"
[D] [F] [I]             3: "GHI"
```
 
## [Day 6: Tuning Trouble](https://adventofcode.com/2022/day/6)
 
This day was surprisingly easy. Just some string analysis with the use of Sets.
 
In the first part I had to find the first occurrence of a substring of length 4, in which all characters are unique:
```
bvwbjplbgvbhsrlpgdmjqwftvncz
 ^^^^
```
The second part was similar, but the length of a substring was 14.
## [Day 7: No Space Left On Device](https://adventofcode.com/2022/day/7)
 
It was a really interesting exercise.  
The input for challenge day can be described as a console output after the traversal of directories.  
I have solved it with messy parsing and the use of recursion and trees.
 
This shows how the input should have been interpreted:
```
$ cd /                    /              
$ ls                      ├─ a            
dir a                     |  ├─ e        
14848514 b.txt            |  |  └─ i      (size: 584)  
8504156 c.dat             |  ├─ f         (size: 29116)
dir d                     |  ├─ g         (size: 2557)
$ cd a                    |  └─ h.lst     (size: 62596)  
$ ls                      ├─ b.txt        (size: 14848514)
dir e                     ├─ c.dat        (size: 8504156)
29116 f                   └─ d            
2557 g                       ├─ j         (size: 4060174)
62596 h.lst                  ├─ d.log     (size: 8033020)
$ cd e                       ├─ d.ext     (size: 5626152)
$ ls                         └─ k         (size: 7214296)
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
The first part of the challenge required determining whether a tree would be visible from the outside of the forest. The tree is visible if there are no trees higher than it that would obstruct a view.  
 
Below is the visualization showing which trees would be visible:
 
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
I would have had a much bigger problem with this exercise,
however, thankfully I had covered a quite similar task during lectures on Functional Programming.
Thanks to that, I was able to reuse some of my code and the logic that comes with it.  
The second part of the challenge added more ropes connected to each other, such that the tail of the rope was fixed to the head of the next one. Solution for it required recursion which made the whole problem more difficult.
But to get the answer for it, I just needed to extend my code from part 1.

 
Visualization below shows how the rope would behave:  
*(H signifies the head of the rope, T its tail, and numbers intermediate knots)*
 
```
Rope with only head and tail:    |     Rope with intermediate knots:
                                 |     
Pull H up:                       |     Pull H up:
.....    .....    .....          |     ..........    ......H...    ......H...
.....    ..H..    ..H..          |     ......H...    ..........    ......1...
..H.. -> ..... -> ..T..          |     .654..1... -> .654..1... -> .654......
.T...    .T...    .....          |     .T..32....    .T..32....    .T..32....
.....    .....    .....          |     ..........    ..........    ..........
                                 |         ┌────────────────────────────┘ 
Pull H to the right:             |         V
.....    .....    .....          |     ......H...    ......H...    ......H...
.....    .....    .....          |     ......1...    ......1...    ......1...
..H.. -> ...H. -> ..TH.          |     .654..2...    .654.32...    .65.432...
.T...    .T...    .....          |     .T..3.....    .T........    .T........
.....    .....    .....          |     ..........    ..........    ..........
                                 |         ┌────────────────────────────┘ 
Pull H down:                     |         V
.....    .....    .....          |     ......H...    ......H...    
..T..    ..T..    .....          |     ......1...    ......1...    
..H.. -> ..... -> ..T..          |     .6.5432... -> ..65432...    
.....    ..H..    ..H..          |     .T........    .T........    
.....    .....    .....          |     ..........    ..........    
```
 
## [Day 10: Cathode-Ray Tube](https://adventofcode.com/2022/day/10)
 
Although the challenge was not very complicated, the solution for it was very original.  
As opposed to previous exercises, where the answer was just a series of numbers or letters, this time the solution was much more graphical.
 
The input consisted of a bunch of instructions, which were decoded in a specified way, and led to the console output like this:
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
 This is the first day where I had a problem with getting solutions!  
 First of all, the input required much more parsing than before:
 ```
 Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
 ```
 Thankfully, a couple of days ago, during my Functional Programming module at university, we covered parsers. This was very helpful today, so the input conversion was not that hard, although it still gave me some hard times.  
 
 With nicely parsed input the solution for the first part was moderately difficult, but not significantly harder compared to previous days.
 
 But this was not the case with the second part!  
 A small modification to the exercise caused some numbers to be so large that integers started to overflow. *(In fact ridiculously large - from what I have found it could be around [10^72 digits long](https://www.reddit.com/r/adventofcode/comments/zioepr/2022_day_11_part_2_ridiculous_worry_levels/)!)*. However, after discovering some correlations in the input *--inspired by small online search--* and the use of modular arithmetics I have managed to get those values to be significantly smaller.
 
 It was an interesting challenge!

 ## [Day 12: Hill Climbing Algorithm](https://adventofcode.com/2022/day/12)
 I was saved by the lectures again!
 
Today's challenge required me to find the shortest possible path between two points. This would be hard if I were to implement it by myself. However, during one of the lectures, we covered the traversal of mazes in haskell. The solutions were based on a breadth-first search. Thanks to this, I was able to reuse some of the code, although I had to significantly modify it to work with this problem.
 
The challenge's input was a grid of small letters and two capitals *('S' and 'E', signifying start and finish points)*. Each step of the path could be made only on the same letter or one alphabetically after *(so from '*b*' we can move to another '*b*' or '*c*', but not to '*a*' nor '*d*')*
The example below shows how the program should compute the path:
 ```
Input grid:         Shortest possible path:

Sabqponm            v..v<<<<
abcryxxl     -->    >v.vv<<^
accszExk            .>vv>E^^
acctuvwj            ..v>>>^^
abdefghi            ..>>>>>^
 ```
 
Part two of the challenge modified the exercise in such a way that there were multiple possible start points of the path. In the case of my input, it was 2063. As this number was too large to try computing the path for each of those points I decided to analyze the input file. Closer inspection led to the discovery that all '*b*'s are only in the second column of the grid and the first column is filled with '*a*'s. That means that a possible path starts with one of those '*a*'s from the first column. This reduced number of paths to 41. Still many but I have decided to run my program and wait for the results. In the meantime, I looked for some inspiration on how to optimize my solution even more. I found a small suggestion that helped me significantly improve the performance of the code. Funnily, at the moment I have implemented it, my original program returned proper values.

Later this day, I decided to one more time find an even more efficient solution for the second part of the challenge. This time, the algorithm starts the search at the end *('E')* and looks for the closest point where the elevation is denoted as '*a*'. I was thinking about using this solution before, however, I have decided not to as I thought implementing it would be too difficult - turns out that it was not!
