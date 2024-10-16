# ALBA
This is the rust library of the *Telescope* protocol proposed in the paper [Approximate Lower Bound Arguments](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/), May 2024, Eurocrypt'24 by _Pyrros Chaidos, Prof Aggelos Kiayias, Leonid Reyzin, Anatoliy Zinovyev_.

Alba is a generic protocol to prove succinctly a lower bound of the size of a *potentially* weighted set.
Say we have a set $S_p$ of size $|S_p| \geq n_p$, and a lower bound $n_f < n_p$ of it we want to prove.
Alba gives us a method to generate a proof of knowledge of this bound by finding the smallest subset of $S_p$ of size $u$ to convince a verifier.
The paper presents several schemes and optimizations.
The basic scheme is enhanced in the "prehashed" version thanks to sorting $S_p$ with a *balls and bins* sorting algorithm reducing the number of hashes done per round.
A lottery scheme is also introduced to support Alba in a decentralized settings as well as a modification to use PRF in the CRS settings instead of using the ROM.

> \[!IMPORTANT\]
> This code is NOT fit for production, it's not been optimised, thoroughly tested, nor audited by competent cryptographers.
> Its one and only purpose is to help people who are more familiar with code than equations to have a better understanding of ALBAs

👉 Checkout documentation on <https://alba.cardano-scaling.org>

## Content
The plan is to cover the versions of the Telescope protocol given below:
- :hammer_and_wrench: **Centralized**: Telescope construction with prehashing and bounded DFS scheme given in _Section 3.2.2_.
- :hammer_and_wrench: **Lottery**: Simple lottery construction given in _Section 4.1_.
- :hammer_and_wrench: **Decentralized**: Telescope construction with prehashing and bounded DFS scheme combined with lottery scheme given in _Section 4.2_.
- :hammer_and_wrench: **Weighted Decentralized** : Telescope construction with prehashing and bounded DFS scheme combined with Sortition given in _Section 5_.
 
We provide tests and benches for each version.
Additionally, we deliver comprehensive [documentation][crate::cargodocs] :book: aimed at connecting theory with practical implementation.

## Compiling the Library
Compile the library and run the tests:
```shell
cargo build --release
cargo test
```

