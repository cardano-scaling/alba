# ALBA

Prototype implementation of _Approximate Lower Bound Arguments_ from the [paper](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/) published by IOG Research:

> **Approximate Lower Bound Arguments**,  _Pyrros Chaidos, Prof Aggelos Kiayias, Leonid Reyzin, Anatoliy Zinovyev_, May 2024, Eurocrypt'24

> [!IMPORTANT]
> This code is NOT fit for production, it's not been optimised, thoroughly tested, nor audited by competent cryptographers.
> Its one and only purpose is to help people who are more familiar with code than equations to have a better understanding of ALBAs

## Introduction

_Approximate Lower Bound Arguments_ are a form of cryptographic certificates that allow a _prover_ to convince a _verifier_ they know some large set of elements by providing only a small subset of those elements. This excellent [X thread](https://x.com/Quantumplation/status/1783188333046255997) provides a good intuition on why ALBAs are useful and how they work, and details of the theory behind this construction are beyond the scope of this introduction and can be found in the paper.

### Proving

Here is an alternative graphical representation of how ALBA certificates are produced, using the basic construction. We start with a set denote $S_p$ of elements such that each is _unique_ and its availability can be asserted by a verifier through some predicate (called $R$) in the paper.

![Initial set](proving-0.jpg)

The first step is to construct the cartesian product of each element of $S_p$, here called $a$ through $k$, with all the integers lower than some parameter $d$, yielding a set of size $\|S_p\| \times d$

![Initial tupling](proving-1.jpg)

From this set, we pseudo-randomly select $n_p$ pairs using a "random oracle" $\mathbb{H}$. $\mathbb{H}$ is effectively just a hash function and to select the adequate number of pairs we pick those with a hash value modulo $n_p$ equals to 0, yielding a set of size roughly $n_p$ of pair of an integer and some item in $S_p$.

![Selecting pairs](proving-2.jpg)

We then expand the set again, creating triples of an integer and 2 elements from $S_p$ through a cartesian product between the selected pairs and the initial set, yielding a set of size $\|S_p\| \times n_p$.

![Expanding set](proving-3.jpg)

And we again pseudo-randomly select $n_p$ elements from this new set using the same function $\mathbb{H}$, yielding a set of triples of approximate size $n_p$

![Selecting triples](proving-4.jpg)

This process is iterated $n$ times according to the protocol parameters (see the paper or [the code](https://github.com/cardano-scaling/alba/blob/8893e4b2de2cb9d74f135ec4535fbfca6acf83d3/src/ALBA.hs#L162) for details on how these parameters are computed), yielding a set of (roughly) $n_p$ tuples of length $n+1$ where each tuple is some "random" integer along with $n$ items from $S_p$.

The last stage consists in applying a function $\mathbb{H}_1$ to select _one_ element from this set which is the final _proof_.

![Selecting proof](proving-5.jpg)


### Verifying

[Verifying](https://github.com/cardano-scaling/alba/blob/8893e4b2de2cb9d74f135ec4535fbfca6acf83d3/src/ALBA.hs#L259) a given proof $P$ is straightforward. Given the known share of honest items $n_p$ and a security parameter $\lambda$ (set at 128 in the paper) controlling the exponentially decaying probability a proof with dishonest items can be constructed, the verifier needs to check:

* The sequence of hashes from the items in the proof are all 0 modulo $n_p$,
* The value of $H_1$ for the whole sequence is 0 modulo some derived parameter $q$,
* And of course that each item in the sequence is valid w.r.t. the predicate $R$.

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

## Benchmarks

### Running benchmarks

This package comes with [criterion](http://www.serpentine.com/criterion/)-based benchmarks defined in the [ALBABench](bench/ALBABench.hs) module.
To run benchmarks and generate a nice HTML page:

```
cabal bench --benchmark-options '-o bench.html'
```

It should be relatively straightforward to tweak the benchmarks for different figures

### Sample results

This repository contains some specific benchmarks outputs formatted by criterion as HTML standalone pages:

* [Simple benchmark](bench.html): Shows proving time for various number of items

  ![](bench.png)
* Large benchmarks (in [HTML](bench-1000-100000.html) and [CSV](bench-1000-100000.csv)): Runtime for all list of items from 1000 to 100000 stepped by 1000. The following picture represents a fraction of that data:

  ![](bench-1000-100000.png)
* Variance benchmark (in [HTML](bench-5000x10.html) and [CSV](bench-5000x10.csv)): Runtime for 10 different list of 5000 items

  ![](bench-5000x10.png)

While the benchmark shows execution time is roughly linear as the number of items increases, there are locally wild variations: A smaller number of items could lead to a significantly larger execution time. This seems easily explainable by the fact the number of hashes executed depends on the "shape" of the tree of data one needs to traverse. Even with identical set of data, different number of items yields different values for parameters $d$, $u$ and $q$ which necessarily modify the computed hashes and therefore the depth-first search for a proof.

To confirm this hypothesis, the "Variance benchmark" runs prover with _different_ set of data for _same_ number of items. Running ALBA prover on 10 different lists of 5000 items yields also varying running time, with a difference of a factor 2 between slowest and fastest. Again, this is explanable by the "random" nature of the data: Different lists can lead to different traversal of the tree when constructing the proof, some requiring more hashes than others.

All these numbers were obtained on a single [C2, 4 CPUs, 16GB Ram](https://cloud.google.com/compute/docs/compute-optimized-machines#c2_series) machine from Google Cloud running Ubuntu 22.04.
