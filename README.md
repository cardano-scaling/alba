# ALBA
This is a Rust library of _Approximate Lower Bound Arguments_ proposed in the [paper](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/), May 2024, Eurocrypt'24 by _Pyrros Chaidos, Aggelos Kiayias, Leonid Reyzin, Anatoliy Zinovyev_.

### Introduction
ALBA enables a prover who has a large collection of data to convince a verifier that their set includes at least a minimum number of elements that meet a specific condition, called a predicate, even though the prover only reveals a subset of the data.
By approximating a lower bound on the prover's knowledge, ALBA makes use of a controlled gap between the size of prover's actual knowledge and the threshold of the verifier checks they know.
This design results in highly efficient proofs, achieving nearly optimal proof sizes in both non-interactive and distributed environments.
ALBA's primary applications include large-scale decentralized signature schemes.
It is particularly well-suited for decentralized or blockchain scenarios, where it enhances communication efficiency among multiple provers sharing witness.

In a decentralized voting system, participants (voters) submit votes that support different options or candidates.
To validate the results without revealing all individual votes, an ALBA protocol could be used.
Each voter's choice can be considered an _element_ that meets a certain predicate (e.g., a valid vote for a candidate).
Instead of tallying every vote publicly, ALBA allows an aggregator (like an election authority) to generate a compact proof showing that a sufficient number of valid votes has been cast for each candidate to meet the required threshold for a decision (such as reaching a quorum or winning a majority).
Combining ALBA with zero-knowledge technology such as _zkSNARKs_, we can efficiently keep individual votes private while enabling quick, efficient validation of the voting outcome, making it particularly useful for secure, private, and scalable voting systems where privacy and efficiency are critical.

The paper introduces various ALBA protocol constructions tailored to different needs.
The basic construction allows a prover to show possession of a large set by creating a proof sequence of elements that meet staged hash-based conditions, efficiently excluding small sets.
_Pre-hashing_ improves this by precomputing hashes to group elements into _bins_, reducing computation and enhancing performance, especially with large sets.
For smaller sets, the generalized Telescope scheme allows multiple attempts to form a proof, adjusting parameters to maintain efficiency across sizes.
Decentralized versions of ALBA include the _Simple Lottery Construction_, where each party holding an element decides to share it based on a random _lottery_ mechanism, with an aggregator forming the proof from a target number of shared elements.
The _Decentralized Telescope_ adapts the Telescope scheme for multiple parties, who individually apply it and share qualifying elements with an aggregator.
Finally, the weighted extension supports elements with integer weights, allowing the prover to meet a total weight threshold, making ALBA versatile for contexts where elements have varying significance.

The library covers the following constructions of the ALBA protocol:
1. Centralized Telescope
2. Simple Lottery Construction
3. Decentralized Telescope
4. Wighted-Decentralized Telescope

### Disclaimer

> This code is NOT fit for production, it's not been optimised, thoroughly tested, nor audited by competent cryptographers.
> Its one and only purpose is to help people who are more familiar with code than equations to prototype larger protocols using ALBA.

### Documentation
👉 We deliver comprehensive [documentation][crate::docs] aimed at connecting theory with practical implementation.

👉 Checkout website on this [link](https://alba.cardano-scaling.org).

### Compiling the library
Compile the library:
```shell
cargo build --release
```

### Tests and Benchmarks
Run tests with `cargo test`. Run benchmarks with `cargo bench`. 
