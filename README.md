# purefd

purefd is a proof-of-concept clone of [fd](https://github.com/sharkdp/fd) written in [Haskell](https://www.haskell.org/).

The goal is to have purefd implement all of fd's functionality while maintaining performance as close to (or possibly exceeding question mark exclamation point) that of fd's.

Why? Mainly because I became interested in squeezing as much performance as possible out of a pure functional language, and also I saw this project as a fun way to git gud at Haskell.

What's implemented? Multithreaded directory traversal, regex and glob matching, and a few cli options. One major thing that's currently missing is gitignore handling, which is arguably a core feature of fd and as such is a high priority todo item.

How's performance? Based on my half-assed manual benchmarks, performance is somewhere in between that of [GNU find](https://www.gnu.org/software/findutils/) and fd. So at the very least we're currently beating a single-threaded C program (thank god).
