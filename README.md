# Advent of Code 2022

This is a gathering of all my solutions to Advent of Code 2022 in Haskell. It is mostly to practice my Haskell a bit. I am usually slow so it should not spoil anything before 16-20 hours after reveal.

I do not care about speed or efficiency. I try to make those solutions at least a little bit elegant but I wont loose sleep over it if they aren't. Only one rule: nothing out of `base` for the problems.

This year I will try to write a few comments so that my friends who are not proficient in Haskell can still appreciate my algorithmic genius.

## Building

This program is built with [cabal](https://www.haskell.org/cabal/). You should install it on your system using your usual package manager.

You can then download and build the program as follow:

```bash 
# Use git
$ git clone https://github.com/TechnoTecna/advent-of-code-2022.git

# Move into the directory
$ cd advent-of-code-2022

# Build the day you want with cabal (replace XX with the day)
$ cabal build aocXX
# Forexample if you want to build day 1
$ cabal build aoc01
```

## Usage

You can now run the program with cabal as follow:

```bash
$ cabal run aocXX -- <input file>
# For example if you want to run the day 1 challenge on my input file
$ cabal run aoc01 -- "./data/AoCInput01"
```
