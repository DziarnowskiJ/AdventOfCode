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
 
Below is the visualization showing which trees would be visible *(marked with ' █ ' symbol )*:
 
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


 ## [Day 13: Distress Signal](https://adventofcode.com/2022/day/13)

Today's challenge was rather simple. After parsing recursive lists, I needed to implement a comparison function for the elements shown below.  
The first part required me to determine whether pairs of signals are in ascending order and if so to keep their index. The second part was based on adding two *signals* that were supposed to be *markers*, and then sorting the whole list.
 
I have decided to do it through the use of *Eq* and *Ord* instances for my custom datatype representing a *signal*. Although it was not very hard, the fact that I was doing it for the first time made it a bit time-consuming. However, this approach made the solution for the second part very easy, as I could simply use the sort function on a list.

```
Input:                                   Sorted signals with marked dividers:

[1,1,3,1,1]                              []
[1,1,5,1,1]                              [[]]
                                         [[[]]]
[[1],[2,3,4]]                            [1,1,3,1,1]
[[1],4]                                  [1,1,5,1,1]
                                         [[1],[2,3,4]]
[9]                                      [1,[2,[3,[4,[5,6,0]]]],8,9]
[[8,7,6]]                                [1,[2,[3,[4,[5,6,7]]]],8,9]
                         ->              [[1],4]
[[4,4],4,4]                           >  [[2]]
[[4,4],4,4,4]                            [3]
                                         [[4,4],4,4]
[7,7,7,7]                                [[4,4],4,4,4]
[7,7,7]                               >  [[6]]
                                         [7,7,7]
[]                                       [7,7,7,7]
[3]                                      [[8,7,6]]
                                         [9]
[[[]]]                        
[[]]        
        
[1,[2,[3,[4,[5,6,7]]]],8,9]        
[1,[2,[3,[4,[5,6,0]]]],8,9]        
```
## [Day 14: Regolith Reservoir](https://adventofcode.com/2022/day/14)
It was a hard day!  
This challenge required me to predict the path of falling sand particles.
Part 1 was not very hard, and a small tweak made it possible to optimize the solution.
However, as it turns out, it was not optimal enough to provide the solution for part 2.
 
The graphics below, show the initial cave and the graph on the right signifies the location of all sand particles:
 
```
+ - the point from which the sand is flowing
. - empty space in the cave
# - rock formation stopping the sand
o - sand that came to a full stop (is not moving anymore)
~ - sand that will be flowing forever (nothing stops it)
 
......+...        .......+...
..........        .......~...
..........        ......~o...
..........        .....~ooo..
....#...##   ->   ....~#ooo##
....#...#.        ...~o#ooo#.
..###...#.        ..~###ooo#.
........#.        ..~..oooo#.
........#.        .~o.ooooo#.
#########.        ~#########.
..........        ~..........
```
 
Unfortunately, I have yet to manage to make a coding solution for part 2 yet.
What I did, was print the resulting cave into a console, and then I manually placed marks for
empty spaces (those not covered by sand). Later I counted all those occurrences and subtracted them from the
size of the triangle and all rock foundations. Although it was time-consuming and prone to make mistakes, I managed to complete it. However, I plan to implement this solution in a coding form in the future.
 
## [Day 15: Beacon Exclusion Zone](https://adventofcode.com/2022/day/15)
 
This day was rather unique. The challenge itself was not very hard, but the thing that made the whole exercise difficult was its scale. The question considered finding covered and uncovered areas on the manhattan grid. The input was a set of pairs of points, each describing the size and location of diamond shapes covering the grid. In the first part, I had to determine how many points are covered on a specific line. This was very easy on the test input which was quite small, but the actual input described a square grid with a side of length of approximately 4.000.000 units. For the second part, the solution required finding a single point that is not covered by a shaded region. For the whole grid, it was 16 * 10^13 points to check!
Fortunately, with some help from the internet, I have managed to create an efficient algorithm for part one which I have also used to solve part 2.
 
Graphical representation of the test input:
```
 
                         #
          #             ###
         ###      #    #####
        #####    ###  #######
       #######  ##############
      #########################
     ###########################
    ############################
   ##############################
    ##############################
     ##############################
      ##############################
       ##############################
       #############################
      #############################
     ################# ###########
      ###########################
       #########################
        #######################
       #######################
      #########################
     ###########################
    #############################
   ###############################
  #################################
 ###################################
###################################
 ################### # ###########
  #################     #########
   ###############       #######
    #############         #####
     ###########           ###
      #########             #
       #######
        #####
         ###
          #
```
 
## [Day 16: Proboscidea Volcanium](https://adventofcode.com/2022/day/16)
Today's challenge can be described as a shortest-path puzzle with an additional parameter that had to be maximized.
 
The input could be read as an unweighted graph, and the goal was to find a way to traverse it to accumulate the highest sum of numbers from the nodes. The second part was somewhat similar, but instead of one agent traversing the graph, there two of them were simultaneously going through this it.
 
My solution first converted the unweighted graph (that contained nodes of value 0) to a fully connected weighted one which had only nodes with values greater than 0. Then it was a simple breadth-first-search for the highest sum of nodes. For the second part, I assumed that each agent would only visit nodes that the other agent did not. This made my solution quite computational-heavy as it had to go through many different permutations of the set of nodes, however eventually *(after a much longer time than I am willing to admit)* it lead to the correct answer.
 
Representation of the input graph and the converted, weighted one:  
*(node in square brackets was the starting one, and number next to a line determine the cost of traversing the path)*
```
 (22)  (2)  ─ (13)                      (21)
  |     |      |                         |2
 (0)   (20) ─ [0]          ->   (22)    [0]  ─1─ (13)
  |     |      |                 |3      |1       |1
 (0) ─ (3)    (0) ─ (21)        (3) ─1─ (20) ─1─ (2)
```
## [Day 17: Pyroclastic Flow](https://adventofcode.com/2022/day/17)
This was a very interesting challenge as it was based on the Tetris game.  
The problem was about dropping 5 types of rocks into the tall, narrow chamber. The horizontal movement was described by the input in form of a string of repeating chars '<' and '>'.
 
Possible shapes of rocks:
```
     |     |     | $ |
     |  o  |   X | $ |
     | ooo |   X | $ | @@
#### |  o  | XXX | $ | @@
```
 
The first exercise asked about the height of the pile of rocks after dropping 2022 of them (assuming the rocks fall in cyclical order). This was rather simple and required only proper implementation of described rules of rocks' movement. However, as usual with Advent of Code, the second part took everything to next level of difficulty by increasing the amount of dropped rocks to 1.000.000.000.000! Trying a naive way and brute force was a terrible idea as it would take too much time. After some investigation, both on the [internet](https://www.reddit.com/r/adventofcode/comments/znykq2/2022_day_17_solutions) and in my solutions, I discovered that after some point, there is a repeating pattern of placement of rocks. This allowed me to algebraically determine the height, leading to a much faster solution.
 
Chamber after dropping 5 rocks into it:
```
|  @@   |
|  @@   |
|  $    |
|  $    |
|  $ X  |  
|  $ X  |  
|  XXX  |  
|  o    |
| ooo   |
|  o####|
+-------+
```
 
## [Day 18: Boiling Boulders](https://adventofcode.com/2022/day/18)
It was a fun challenge, and surprisingly, not that difficult.
 
This exercise is the first one so far that considered 3-dimensional space. It was about finding the surface of a shape created from small cubes. The first part of the question asked for the surface area of all parts that are exposed to air. For the second part, the question was similar, however, 'air pockets' inside the shape had to be discarded.
 
This is a 2-D visualization of the problem
```
  Initial shape      Part 1          Part 2
                                                     
                      1111            1111
       ####         12####2         12####2    
     #### ##       1####4##2       1#### ##2      
     ##  ####  ->  1##32####1  ->  1##  ####1    
     ### ####      1###3####1      1### ####1  
      #####         2#####21        2#####21      
                     11111           11111
```
 
The first part of the challenge was rather straightforward. I am quite pleased with my solution for the second part. As shown below, I have found a way to fill the 'air pockets' of the initial shape by getting the inverse of it. From that point, I just had to use the same algorithm that I used for the first part.
```
Filled square  -     Initial shape     =  Negative of shape    
 
 ##########                                  ##########
 ##########              ####                ###    ###
 ##########            #### ##               #    #  ##
 ##########    -       ##  ####        =     #  ##    #
 ##########            ### ####              #   #    #
 ##########             #####                ##     ###
 ##########                                  ##########
                                                 |
     ┌───────────────────────────────────────────┘
     V
 Negative of   -  Reachable points from =   air pockets  
initial shape      the top-left corner      
                                                                 
 ##########           ##########          
 ###    ###           ###    ###          
 #    #  ##           #       ##                  #
 #  ##    #    -      #        #        =       ##
 #   #    #           #        #                 #
 ##     ###           ##     ###          
 ##########           ##########        
                                                 |
     ┌───────────────────────────────────────────┘
     V
'air pockets'  +      Initial shap      =    Filled shape
                                                     
                         ####                   ####  
      #                #### ##                #######
    ##         +       ##  ####         =     ########
     #                 ### ####               ########
                        #####                  #####                  
```
 
## [Day 19: Not Enough Minerals](https://adventofcode.com/2022/day/19)
For now, this challenge is too difficult for me. I have managed to write a "solution" for part 1, however, based on my calculations, it would take more than a day for the answer to be computed. Obviously, I do not consider this a valid solution and I plan to work on it in the future.

## [Day 20: Grove Positioning System](https://adventofcode.com/2022/day/20)
Another not-so-difficult easy challenge.
 
Today's exercise was about modifying the list of numbers. Each number corresponded to the number of spaces it needs to be moved inside the list. My solution was based on splitting this list into 3 parts, such that on the left and right I had parts of the list and in the middle the element that had to be moved. Based on the value of this element, the algorithm was inserting it in the proper sublist and connected them back together. I am quite satisfied with my solution, even though part two takes about 2-3 minutes to complete.  
This challenge was exceptionally interesting to do in Haskell, as it by default does not support indices in lists, so I had to write some special methods that would work around it.

Consecutive steps during the processing of [1, 2, -3, 3, -2, 0, 4]:
```
Initial arrangement:
1, 2, -3, 3, -2, 0, 4

1 moves between 2 and -3:
2, 1, -3, 3, -2, 0, 4

2 moves between -3 and 3:
1, -3, 2, 3, -2, 0, 4

-3 moves between -2 and 0:
1, 2, 3, -2, -3, 0, 4

3 moves between 0 and 4:
1, 2, -2, -3, 0, 3, 4

-2 moves between 4 and 1:
1, 2, -3, 0, 3, 4, -2

0 does not move:
1, 2, -3, 0, 3, 4, -2

4 moves between -3 and 0:
1, 2, -3, 4, 0, 3, -2
```
