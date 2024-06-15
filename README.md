# seqn

[![Hackage](https://img.shields.io/hackage/v/seqn?logo=haskell&color=blue)](https://hackage.haskell.org/package/seqn)
[![Haskell-CI](https://github.com/meooow25/seqn/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/meooow25/seqn/actions/workflows/haskell-ci.yml)

`seqn` offers two sequence types:

* `Seq`, an immutable sequence supporting operations such as index, insert,
  delete, split, append, in logarithmic time. `Seq` is well-suited to use cases
  where there are frequent changes to the structure of the sequence.

* `MSeq`, a sequence like `Seq`, which additionally supports constant time
  access to the accumulated "measure" of all its elements. See the documentation
  for `MSeq` for more about measures.

`seqn` also offers a priority-queue structure, `PQueue`, with logarithmic time
queue operations.

## Documentation

Please find the documentation on Hackage: [seqn](https://hackage.haskell.org/package/seqn)

## Alternatives

The following structures are similar to `Seq` and `MSeq`, but may be better
suited to some use cases.

* Alternatives to `Seq`:
  * [`Data.Sequence.Seq`](https://hackage.haskell.org/package/containers-0.7/docs/Data-Sequence.html#t:Seq)
    from `containers`
  * [`Data.RRBVector.Vector`](https://hackage.haskell.org/package/rrb-vector-0.2.1.0/docs/Data-RRBVector.html#t:Vector)
    from `rrb-vector`
* Alternatives to `MSeq`:
  * [`Data.FingerTree.FingerTree`](https://hackage.haskell.org/package/fingertree-0.1.5.0/docs/Data-FingerTree.html#t:FingerTree)
    from `fingertree`

For a detailed comparison, [see here](https://github.com/meooow25/seqn/tree/master/bench).

## Acknowledgements

The interface and implementation of `seqn` is largely influenced by
the libraries [`containers`](https://hackage.haskell.org/package/containers) and
[`fingertree`](https://hackage.haskell.org/package/fingertree).
