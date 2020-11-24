# advent-of-code-ocr

"OCR" letter recognition for [Advent of Code][aoc] puzzles, compatible with all
puzzles from 2015 to 2019.  Also comes with a handy command line utility to
parse the ASCII art directly from stdin:

[aoc]: https://adventofcode.com

```
$ cat abc.txt
.##..###...##.
#..#.#..#.#..#
#..#.###..#...
####.#..#.#...
#..#.#..#.#..#
#..#.###...##.
$ cat abc.txt | advent-of-code-ocr
ABC
```

## Installation

To use as a library, add *advent-of-code-ocr* as a hackage dependency like you
would any other.

To use as an executable:

```bash
# using stack
stack install advent-of-code-ocr
# using cabal
cabal install advent-of-code-ocr
# from source
git clone https://github.com/mstksg/advent-of-code-ocr
cd advent-of-code-ocr
cabal install     # or stack install
```

## Compatibility

Should be compatible with all challenges from 2015 to 2019.  However, I have
only tested it with:


*   2016 Day 8
*   2018 Day 10
*   2019 Day 8
*   2019 Day 11

And it is possible I have missed some.  If you notice any I am missing, please
let me know!
