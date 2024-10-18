# Introduction

## Definition

_Approximate Lower Bound Arguments_ are a form of cryptographic certificates that allow a _prover_ to convince a _verifier_ they know some set of elements by providing only a small subset of those elements. More formally, given some set of elements, and some bounds \\(n_p\\) and \\(n_f\\), where \\(n_p > n_f\\), about the size of this set, ALBA provides an algorithm that allows to build a _proof_ they know _at least_ \\(n_f\\) elements, where the proof size is a constant value derived from the \\(\frac{n_p}{n_f}\\) ratio.

The algorithm provides the following guarantees, given the prover has \\(S_p\\) elements and a security parameter \\(\lambda\\):

* if \\(|S_p| \geq n_p\\) then the prover has a probability lower than \\(2^{-\lambda}\\) of _failure_ to build a proof, or in other words they are sure to find one,
* if \\(|S_p| \leq n_f\\) then the prover has a probability lower than \\(2^{-\lambda}\\) of _success_ to build a proof,
* in between those 2 bounds, the probability of being able to build a proof drops exponentially (see our [simulation](./simulation.md) page for a graphical exploration of this probability).

Details of the theory behind this construction are beyond the scope of this introduction and can be found in the paper.

## Usefulness

This excellent [X thread](https://x.com/Quantumplation/status/1783188333046255997) provides a good intuition on why ALBAs is useful and how it works.
