# ALBA

Prototype Haskell implementation of _Approximate Lower Bound Arguments_ from the [paper](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/) published by IOG Research:

> **Approximate Lower Bound Arguments**,  _Pyrros Chaidos, Prof Aggelos Kiayias, Leonid Reyzin, Anatoliy Zinovyev_, May 2024, Eurocrypt'24

> [!IMPORTANT]
> This code is NOT fit for production, it's not been optimised, thoroughly tested, nor audited by competent cryptographers.
> Its one and only purpose is to help people who are more familiar with code than equations to have a better understanding of ALBAs

👉 Checkout documentation on https://alba.cardano-scaling.org

## Build

This code is written in Haskell with some native libraries dependencies for faster hashing. It requires the following software to be built:

* GHC
* Cabal
* [libsodium](https://doc.libsodium.org/)

To install sodium, you can follow instructions for [cardano-node](https://developers.cardano.org/docs/get-started/installing-cardano-node/#downloading--compiling) but please not ALBA does not depend (yet) on Cardano-specific sodium extensions.

GHC and Cabal can be installed through [GHCUp](https://www.haskell.org/ghcup/).

To build all components, assuming pkg-config is propertly configured and `libsodium` is installed:

```
cabal build all
```

To run tests:

```
cabal test
```
## Run

This package provides a small executable, unsurprisingly called `alba`, one can use to generate random data and proofs from those. It can also verify a given proof.

Help is available from the command-line:

```
$ cabal run alba -- --help
alba: Command-line utility for creating and verifying ALBA proofs
...

```

Here are a few examples use:

* Generate a proof for a set of size 900 from a maximum bound of 1000, assuming 80% honest ratio, with each item of length 256 bytes, in a file called `alba.proof`:

  ```
  $ cabal run alba -- prove --bound 1000 --size 900 --len 500 --output alba.proof
  Generating proof Options {size = 900, bound = 1000, len = 500, params = Params {λ_sec = 128, λ_rel = 128, n_p = 800, n_f = 200}, output = "alba.proof"}
  Written proof to 'alba.proof' (34557 bytes)
  ```

* Verifying the above generated proof with same parameters:

  ```
  $ cabal run alba -- verify --bound 1000 --size 900 --len 500 --output alba.proof
  Verifying proof with Options {size = 900, bound = 1000, len = 500, params = Params {λ_sec = 128, λ_rel = 128, n_p = 800, n_f = 200}, output = "alba.proof"}
  Verified proof Proof (6,["df5fd475c65f745cf63dc2705f33f8a9190460ffc97a0eaa9...
  ```

  A textual representation of the proof is dumped on the standard output.

* Generate a proof for a subset of elements smaller than $n_p$ blows up:

  ```
  $ cabal run alba -- prove --bound 1000 --size 700 --len 500 --output alba.proof
  ```

## Benchmarks

### Running benchmarks

This package comes with [criterion](http://www.serpentine.com/criterion/)-based benchmarks defined in the [ALBABench](bench/ALBABench.hs) module.
To run benchmarks and generate a nice HTML page:

```
cabal bench --benchmark-options '-o bench.html'
```

It should be relatively straightforward to tweak the benchmarks for different figures, see the [documentation](https:/alba.cardano-scaling.org/benchmarks) for latest results.
