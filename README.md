# ALBA

Prototype implementation of _Approximate Lower Bound Arguments_ from the [paper](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/) published by IOG Research:

> **Approximate Lower Bound Arguments**
>
> _Pyrros Chaidos, Prof Aggelos Kiayias, Leonid Reyzin, Anatoliy Zinovyev_
>
> May 2024, Eurocrypt'24

## Build

This code is written in Haskell with some native libraries dependencies for faster hashing. It requires the following software to be built:

* GHC
* Cabal
* [libsodium](https://doc.libsodium.org/)

To install sodium, you can follow instructions for [cardano-node](https://developers.cardano.org/docs/get-started/installing-cardano-node/#downloading--compiling) but please not ALBA does not depend (yet) on Cardano-specific sodium extensions.

GHC and Cabal can be installed through [GHCUp](https://www.haskell.org/ghcup/).

To build all components:

```
cabal build all
```

To run tests:

```
cabal test
```
