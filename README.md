Description
------------
Implements a queueing system in which a number of threads are contending for space inside a "unisex bathroom".The bathroom can contain men, women or janitors, but no mixes. The bathroom has a finite capacity and if it becomes full, threads must queue up in their type-exclusive lines. If men are in the bathroom, a woman lines up, and then a man shows up, the man is not allowed in as the bathroom needs to change "genders" to allow the woman in. If a janitor ever shows up, nobody else is allowed in the bathroom as it needs cleaning; however, if a janitor shows up and another is already in the bathroom, the new janitor must commit suicide.

Input
-----
People are loaded into the system through an input thread. Input looks like this (M = men, W = women, J = janitor, S = stop):

```
M 4
M 2
W 5
J 8
S 14
M 2
```

The integer value with each code denotes the logical time each person will occupy their stall, or, in the case of S, the duration over which the input thread halts the reading of input.

The logical clock
-----------------

The scenario revolves around a logical clock which must be ticked either by a person in the bathroom or by the input thread if there are no occupants of the bathroom. This scenario will only occur if the input thread is asked to stop loading people from an input file for some logical time. 

The actual problem being solved
--------------------------------
When people enter the bathroom, they are assigned a stall. Each stall has a toilet lid that can either be in the up position, or the down. Men require the lid to be up, women down. As women leave their stalls, they leave the lid down. However, as men leave, they flip a coin: heads = leave lid up, tails = put it down. The problem solved here will display the amount of times each stall was used, the lid state was changed, and left unchanged.

How to run
----------
The program itself reads from standard input; however, it is recommended that users redirect their stdin from a file. Two programs exist in this repository.

*mid* will generate an input file, and runs on the command line as
```
./mid <number of lines> <output filename>
```

*unisex* runs as
```
./unisex <number of stalls> < <input filename>
```
