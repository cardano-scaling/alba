# ALBA

Prototype implementation of _Approximate Lower Bound Arguments_ from the [paper](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/) published by IOG Research:

> **Approximate Lower Bound Arguments**,  _Pyrros Chaidos, Prof Aggelos Kiayias, Leonid Reyzin, Anatoliy Zinovyev_, May 2024, Eurocrypt'24

> [!IMPORTANT]
> This code is NOT fit for production, it's not been optimised, thoroughly tested, nor audited by competent cryptographers.
> Its one and only purpose is to help people who are more familiar with code than equations to have a better understanding of ALBAs

## Introduction

_Approximate Lower Bound Arguments_ are a form of cryptographic certificates that allow a _prover_ to convince a _verifier_ they know some large set of elements by providing only a small subset of those elements. This excellent [X thread](https://x.com/Quantumplation/status/1783188333046255997) provides a good intuition on why ALBAs are useful and how they work, and details of the theory behind this construction are beyond the scope of this introduction and can be found in the paper.

Here is an alternative graphical representation of how ALBA certificates are produced:

![ALBA Proving Process](proving.jpg)

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

To run benchmarks (takes a while):

```
cabal bench
```
