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

## Credit

Much of the content for the letterforms was gathered on freenode ##adventofcode
and solutions threads on reddit; the large font for the most part has been
gathered by [u/usbpc102][] on reddit. `O` and `I` letterforms for the small
font contributed by [u/TheShallowOne][] on reddit for version 1.1.10, and the S
small font letterform contributed by [@gilgamec][].

[u/usbpc102]: https://gist.github.com/usbpc/5fa0be48ad7b4b0594b3b8b029bc47b4
[u/TheShallowOne]: https://www.reddit.com/r/adventofcode/comments/k0lzz6/adventofcodeocr_command_line_utilityhaskell/gdwwu39
[@gilgamec]: https://github.com/gilgamec
