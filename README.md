# ALBA
This is a Rust library of _Approximate Lower Bound Arguments_ proposed in the [paper](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/), May 2024, Eurocrypt'24 by _Pyrros Chaidos, Aggelos Kiayias, Leonid Reyzin, Anatoliy Zinovyev_.

## Introduction
ALBA is a cryptographic primitive that enables a prover to convince a verifier that their dataset includes at least a minimum number of elements meeting a specific condition, called a predicate, without revealing the entire dataset.
The core idea of ALBA is to introduce a controlled gap between the prover's actual knowledge and the threshold the verifier wants to confirm. 
This gap not only gives ALBA its name (_Approximate Lower Bound Argument_) but also enables highly efficient algorithms for generating compact proofs.
ALBA proofs are particularly small and efficient in scenarios with significant discrepancy between the dataset size and the threshold to prove, or involving weight oracles, and are generated significantly faster than traditional general purpose SNARKs.
Its main applications include voting and certifying digital signatures in large-scale decentralized networks as well as other blockchain scenarios where it improves communication efficiency among multiple provers sharing a common witness.

For example, in a decentralized voting system, participants (voters) submit votes that support different options or candidates.
To validate the results without revealing all individual votes, an ALBA protocol could be used to prove a majority was reached.
Each vote can be considered an _element_ that meets a certain predicate (e.g., a valid vote for a specific candidate).
Instead of tallying every vote publicly, ALBA allows an aggregator (like an election authority) to generate a compact proof showing that a sufficient number of valid votes has been cast for each candidate (such as reaching a quorum or winning a majority).
Combining ALBA with zero-knowledge technology, such as _zk-SNARKs_, can efficiently keep individual votes private while potentially reducing the proof size even more.
Beyond voting, ALBA's versatility makes it suitable for various use cases requiring efficient, scalable proof systems that balance privacy, speed, and resource efficiency.

### ALBA in a Nutshell
ALBA's core construction, the _centralized telescope_, operates as follows:  
The prover holds a set of $n_p$ elements (e.g. signatures, data points, or weighted items) that satisfy a predicate. 
The verifier needs to be convinced that the prover knows strictly more than $n_f$ elements, where $n_f < n_p$. 
ALBA generates a proof by choosing a subset of, potentially repeated, elements. These elements are selected through a random walk satisfying random oracle checks at each step, outputting a tuple significantly smaller than $n_f$.
The larger the ratio $n_p / n_f$, the smaller the proof size, making ALBA practical for scenarios involving large datasets and low thresholds.

ALBA furthermore supports *weighted elements*, where each item has an integer weight. 
In this case, the prover has in possession elements with a total weight of at least $n_p$ and convinces the verifier that the total weight exceeds $n_f$.
In *decentralized settings*, ALBA adapts to distributed data environments. 
Multiple participants play a lottery and send their elements to an aggregator accordingly to its result. The aggregator then combines them into a single, compact proof for the verifier.

ALBA can seamlessly handle both *un/weighted* and *de/centralized* configurations at the same time. 
This flexibility is particularly valuable in applications like *proof-of-stake blockchains*, where weights represent stake amounts. 
ALBA there can be used to ensure that honest participants' stakes outweigh malicious contributions, providing robust security in distributed systems.
The *decentralized construction* of ALBA also stands out for its flexibility, offering tradeoffs between proof size and communication complexity. 
This feature allows protocol designers to optimize ALBA for various decentralized applications.

### Why Use ALBA?
ALBA is an ideal choice for applications that require:
- *Fast proof generation and verification*, such as in blockchain systems or multisignature schemes.
- *Efficient decentralized collaboration*, enabling multiple participants to jointly prove knowledge.
- *Flexibility in tradeoffs*, balancing proof size and communication overhead.
Whether it is for multisignatures, proof-of-stake systems, or secure voting protocols, ALBA provides a robust, scalable, and efficient solution for proving knowledge across diverse use cases.

## Implementation
The library implements ALBA schemes based on two core constructions: *Telescope* and *Lottery*. 
These constructions form the foundation for the various configurations, including centralized and decentralized setups as well as unweighted and weighted scenarios.

The *Telescope* construction allows the prover to build a sequence of elements that satisfy staged hash-based conditions.
This process efficiently filters relevant elements, narrowing down the data to subsets that meet the required criteria.
By introducing bounded repetitions and constraints such as pre-hashing on the prover's search, the construction ensures scalability and efficiency for large datasets.

The *Lottery* construction offers a decentralized approach where participants probabilistically decide whether to share their elements with an aggregator. 
The aggregator collects enough elements and concatenates them to generate a proof. 
This method is inherently decentralized and can also handle weighted scenarios by incorporating element weights into the lottery process.

Using these constructions, the library supports eight ALBA schemes, covering a wide range of configurations:
- _Telescope bounded_ (Section 3.2.2), whether centralized or not (Section 4.2), unweighted or not (Section 5),
- _Lottery_ (Section 4.1), whether centralized or not, unweighted or not (Section 5).

## ⚠️ Disclaimer
This code is NOT fit for production, it has not been optimized, thoroughly tested, nor audited.
Its one and only purpose is to help people who are more familiar with code than equations to prototype larger protocols using ALBA.

## Documentation
📖 We deliver comprehensive [documentation][crate::docs] aimed at connecting theory with practical implementation.

🌐 Checkout website on this [link](https://alba.cardano-scaling.org).

## Compiling
Compile the library by:
```shell
cargo build --release
```

Run tests with: 
```shell
cargo test
```

Run benchmarks with: 
```shell
cargo bench
```

Compile the documentation by:
```shell
cargo doc --no-deps --open
```
