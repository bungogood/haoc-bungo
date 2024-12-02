# Haoc

Haskell Advent of Code Template and CLI tool, inspired by [Rust AOC](https://github.com/fspoettel/advent-of-code-rust).

## Installation

requires:
- [stack](https://docs.haskellstack.org/en/stable/README/)
- [aoc-cli](https://github.com/scarvalhojr/aoc-cli) (optional for downloading input files)

## Usage

```
haoc - a tool for solving Advent of Code challenges

Usage: haoc COMMAND

  Advent of Code CLI tool

Available options:
  -h,--help                Show this help text

Available commands:
  setup                    Setup environment for a specific year and day
  download                 Download input for a specific year and day
  run                      Run a solution for a specific year and day
  cookie                   Set the session cookie
```

### Setup day

Create a new day module and add it to the `app/Solver.hs` file. If no year or day is provided, it will use the current year and day.

```bash
stack run -- setup 2020 1
```

Geneates a blank module with `part1` and `part2` functions of type `Solution -> String -> String`.

### Download input

Download the input file for a specific year and day. If no year or day is provided, it will use the current year and day.

```bash
stack run -- download 2020 1
```

### Run solution

Run the solution for a specific year and day. If no year or day is provided, it will use the current year and day.

```bash
stack run -- run 2020 1
```

## TODO

- [ ] Add tests
- [ ] Allow for submitions thoguh `aoc-cli`
