
```
                      │      Seq      │      MSeq      │    Sequence    │   RRBVector    │  FingerTree
──────────────────────┼───────────────┼────────────────┼────────────────┼────────────────┼────────────────
rnf                   │   28 μs   1x  │   26 μs  0.9x  │   57 μs  2.1x  │   30 μs  1.1x  │  173 μs  6.2x
fromList              │   72 μs   1x  │   98 μs  1.4x  │   27 μs  0.4x  │   31 μs  0.4x  │  362 μs    5x
fromList nf           │  110 μs   1x  │  133 μs  1.2x  │   87 μs  0.8x  │   61 μs  0.6x  │  671 μs  6.1x
fromList fusion       │   75 μs   1x  │  104 μs  1.4x  │   94 μs  1.3x  │   84 μs  1.1x  │  454 μs  6.1x
foldr short-circuit   │   31 μs   1x  │   31 μs    1x  │   63 μs    2x  │   10 μs  0.3x  │  247 μs  7.9x
cps sum foldr         │   63 μs   1x  │   62 μs    1x  │  113 μs  1.8x  │  124 μs    2x  │  431 μs  6.8x
foldl short-circuit   │   22 μs   1x  │   22 μs    1x  │   62 μs  2.8x  │   10 μs  0.5x  │  320 μs   14x
cps sum foldl         │   51 μs   1x  │   50 μs    1x  │  107 μs  2.1x  │  125 μs  2.4x  │  770 μs   15x
sum  toList           │   62 μs   1x  │   61 μs    1x  │  107 μs  1.7x  │   71 μs  1.1x  │  430 μs    7x
foldl'                │   16 μs   1x  │   17 μs  1.1x  │   71 μs  4.3x  │   22 μs  1.3x  │  426 μs   26x
foldr'                │   15 μs   1x  │   16 μs  1.1x  │   70 μs  4.7x  │   22 μs  1.5x  │  761 μs   51x
toList                │   57 μs   1x  │   58 μs    1x  │   44 μs  0.8x  │   17 μs  0.3x  │  486 μs  8.5x
ifoldr short-circuit  │   32 μs   1x  │   32 μs    1x  │   96 μs    3x  │  7.4 μs  0.2x  │
cps sum ifoldr        │   61 μs   1x  │   63 μs    1x  │  144 μs  2.3x  │  135 μs  2.2x  │
ifoldl short-circuit  │   23 μs   1x  │   23 μs    1x  │   44 μs  1.9x  │  7.5 μs  0.3x  │
cps sum ifoldl        │   51 μs   1x  │   51 μs    1x  │  146 μs  2.8x  │  137 μs  2.7x  │
ifoldl'               │   19 μs   1x  │   19 μs    1x  │  177 μs  9.4x  │   14 μs  0.7x  │
ifoldr'               │   21 μs   1x  │   21 μs    1x  │  184 μs    9x  │   16 μs  0.8x  │
==                    │  115 μs   1x  │  117 μs    1x  │  157 μs  1.4x  │  127 μs  1.1x  │  1.1 ms  9.6x
compare               │  116 μs   1x  │  121 μs    1x  │  175 μs  1.5x  │  160 μs  1.4x  │  1.2 ms   10x
replicate             │  102 ns   1x  │  134 ns  1.3x  │  8.3 ns  0.1x  │   60 ns  0.6x  │
generate              │   55 μs   1x  │   61 μs  1.1x  │   17 ns    0x  │                │
big <> big            │   11 ns   1x  │   12 ns  1.1x  │  161 ns   15x  │  3.8 μs  344x  │   25 ns  2.3x
big <> 2              │  104 ns   1x  │  103 ns    1x  │   22 ns  0.2x  │  1.2 μs   12x  │   25 ns  0.2x
cons many             │  1.8 ms   1x  │  1.9 ms    1x  │  131 μs  0.1x  │  1.7 ms  0.9x  │  248 μs  0.1x
snoc many             │  1.9 ms   1x  │  1.9 ms    1x  │  133 μs  0.1x  │  636 μs  0.3x  │  258 μs  0.1x
index many            │  401 μs   1x  │  457 μs  1.1x  │  829 μs  2.1x  │  147 μs  0.4x  │
lookup many           │  406 μs   1x  │  474 μs  1.2x  │  827 μs    2x  │   28 μs  0.1x  │
update many           │  1.3 ms   1x  │  1.4 ms  1.1x  │  1.9 ms  1.5x  │  589 μs  0.5x  │
insertAt many         │  1.6 ms   1x  │  1.5 ms  0.9x  │  1.8 ms  1.1x  │   31 ms   19x  │
deleteAt many         │  1.6 ms   1x  │  1.4 ms  0.9x  │  1.7 ms  1.1x  │   29 ms   19x  │
uncons many           │  948 μs   1x  │  842 μs  0.9x  │  129 μs  0.1x  │  1.6 ms  1.7x  │  359 μs  0.4x
unsnoc many           │  905 μs   1x  │  864 μs    1x  │  131 μs  0.1x  │  380 μs  0.4x  │  366 μs  0.4x
splitAt many          │  2.2 ms   1x  │  2.0 ms  0.9x  │  1.7 ms  0.8x  │  2.1 ms  0.9x  │
tails                 │  2.2 ms   1x  │                │   19 ns    0x  │                │
inits                 │  2.4 ms   1x  │                │   22 ns    0x  │                │
chunks 2              │  129 μs   1x  │                │   38 ns    0x  │                │
chunks 2 nf           │  172 μs   1x  │                │  400 μs  2.3x  │                │
chunks sqrt           │   28 μs   1x  │                │  133 ns    0x  │                │
chunks sqrt nf        │   63 μs   1x  │                │  100 μs  1.6x  │                │
foldMap               │   16 μs   1x  │   16 μs    1x  │   58 μs  3.5x  │  4.5 μs  0.3x  │  170 μs   10x
map                   │   32 μs   1x  │   34 μs  1.1x  │   14 ns    0x  │   33 μs    1x  │   37 ns    0x
map nf                │   60 μs   1x  │   62 μs    1x  │  171 μs  2.9x  │  106 μs  1.8x  │  366 μs  6.1x
imap                  │   41 μs   1x  │   34 μs  0.8x  │   16 ns    0x  │   61 μs  1.5x  │
liftA2                │  206 μs   1x  │  200 μs    1x  │   47 ns    0x  │                │
<*                    │  129 μs   1x  │                │   26 ns    0x  │   11 ms   84x  │
*>                    │   74 ns   1x  │                │   28 ns  0.4x  │   10 ms >999x  │
>>= 1                 │  104 μs   1x  │  143 μs  1.4x  │   12 ms  112x  │  199 ms >999x  │
>>= 2                 │  273 μs   1x  │  276 μs    1x  │  489 μs  1.8x  │   55 ms  200x  │
>>= 3                 │  321 μs   1x  │  366 μs  1.1x  │   11 ms   33x  │  165 ms  514x  │
filter                │   57 μs   1x  │   56 μs    1x  │  114 μs    2x  │                │
mapMaybe              │   56 μs   1x  │   55 μs    1x  │                │                │
mapEither             │  221 μs   1x  │  187 μs  0.8x  │                │                │
span                  │   31 μs   1x  │   32 μs    1x  │   63 μs    2x  │                │
findIndex             │   14 μs   1x  │   25 μs  1.8x  │   66 μs  4.7x  │   27 μs  1.9x  │
reverse               │   70 μs   1x  │   70 μs    1x  │   14 ns    0x  │                │   31 ns    0x
reverse nf            │  105 μs   1x  │  102 μs    1x  │  182 μs  1.7x  │                │  411 μs  3.9x
intersperse           │   72 μs   1x  │   80 μs  1.1x  │   64 ns    0x  │                │
intersperse nf        │  136 μs   1x  │  136 μs    1x  │  513 μs  3.8x  │                │
scanl                 │   32 μs   1x  │   34 μs  1.1x  │   40 ns    0x  │                │
scanl nf              │   63 μs   1x  │   64 μs    1x  │  267 μs  4.2x  │                │
scanr                 │   33 μs   1x  │   35 μs  1.1x  │   39 ns    0x  │                │
scanr nf              │   64 μs   1x  │   64 μs    1x  │  281 μs  4.4x  │                │
sort random           │  1.4 ms   1x  │  1.4 ms    1x  │  2.8 ms    2x  │                │
sort asc              │  186 μs   1x  │  175 μs  0.9x  │  706 μs  3.8x  │                │
zipWith               │  122 μs   1x  │  124 μs    1x  │   33 ns    0x  │  266 μs  2.2x  │
zipWith nf            │  156 μs   1x  │  159 μs    1x  │  560 μs  3.6x  │  331 μs  2.1x  │
zipWith3              │  180 μs   1x  │  182 μs    1x  │   64 ns    0x  │                │
zipWith3 nf           │  214 μs   1x  │  217 μs    1x  │  1.2 ms  5.8x  │                │
unzipWith             │  162 μs   1x  │   75 μs  0.5x  │   35 ns    0x  │   58 μs  0.4x  │
unzipWith nf          │  242 μs   1x  │  143 μs  0.6x  │  484 μs    2x  │  120 μs  0.5x  │
mconcat               │  128 μs   1x  │                │  212 μs  1.6x  │   13 ms  105x  │
binarySearchFind      │  395 μs   1x  │  395 μs    1x  │                │                │
isPrefixOf            │  120 μs   1x  │  131 μs  1.1x  │                │                │
infixIndices full     │  206 μs   1x  │                │                │                │
infixIndices all      │  173 μs   1x  │                │                │                │
binarySearchPrefix    │               │  400 μs    1x  │                │                │
binarySearchSuffix    │               │  412 μs    1x  │                │                │
measured split        │               │  2.9 ms    1x  │                │                │   32 ms   11x
```

```
           │ seqn PQueue   │ pqueue PQueue  │ fingertree PQueue
───────────┼───────────────┼────────────────┼───────────────────
fromList   │  358 μs   1x  │  219 μs  0.6x  │  1.7 ms  4.9x
insert     │  3.0 ms   1x  │  310 μs  0.1x  │  1.2 ms  0.4x
minView    │  3.2 ms   1x  │  3.5 ms  1.1x  │  100 ms   32x
sort list  │  3.7 ms   1x  │  3.8 ms    1x  │  105 ms   28x
```
