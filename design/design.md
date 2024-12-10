# ALBA implementation design document

[@tolikzinovyev](https://github.com/tolikzinovyev) December 2024

## Introduction

This is the first design document for our Rust implementation of
Approximate Lower Bound Arguments (ALBA) intended to guide the development
of the library.
We aim to create a high quality reference implementation of ALBA sufficient
for accurately prototyping larger protocols utilizing this primitive such as
protocols for voting and multisignatures schemes.
In the Cardano universe, projects that can benefit from ALBA include
Mithril, Peras, Leios and cross-chain bridges.
While these use cases will guide the development of our library, one can
imagine other applications / protocols that can utilize ALBA, and
our aim is to allow the open-source community to study and experiment
with it.

### Overview of ALBA

ALBA is a cryptographic primitive that allows one to prove knowledge of
some number of data elements of some kind.
Given parameters $n_f$ and $n_p$ such that $n_f < n_p$,
the prover who possesses $n_p$ elements of interest is able to convince
the verifier that he knows more than $n_f$ elements of interest.
Hence the name, *approximate* lower bound argument.
The larger the ratio $n_p / n_f$ is, the smaller the ALBA certificate is.

ALBA also works in the *weighted* regime where data elements can have
an integer weight.
The prover who possesses elements of total weight at least $n_p$ is able to
convince the verifier that he knows elements of total weight more than $n_f$.
Such a gap between $n_p$ and $n_f$ exists naturally in secure distributed
systems including proof-of-stake blockchains where the amount of honest stake
significantly exceeds the amount of malicious stake;
thus, for example, the honest nodes can use ALBA to efficiently certify some
information, like a ledger snapshot, while malicious nodes cannot.

Additionally, ALBA supports a *decentralized* setting where knowledge of
the data elements is spread out across multiple parties;
some will send a message to an aggregator who in turn
will produce a proof for the verifier.
The primitive in this setting is called decentralized ALBA.
Its main construction, called decentralized Telescope,
provides a tradeoff between the proof size and the number of parties that
communicate, leaving it up to protocol designers to make an optimal choice
for their application.

Compared to SNARKs that can solve the same problem, ALBA constructions offer
much faster proving times: in the order of a second.
Additionally, the decentralized Telescope ALBA construction gives rise to
the only known multisignature scheme with a configurable tradeoff between
proof size and (sublinear) communication complexity.
The only other known way to achieve sublinear communication complexity is
through a lottery as used, for example, by Algorand's sortition or
the [Mithril](https://mithril.network) protocol.
The lottery has slightly better communication complexity but a significantly
worse proof size.

See the [website](https://alba.cardano-scaling.org),
[docs](https://github.com/cardano-scaling/alba/tree/main/docs) or
the [paper](https://eprint.iacr.org/2023/1655) to get a more detailed overview
of ALBA.

### Applications of ALBA

This cryptographic primitive has a number of applications, in the Cardano space
and outside.

Mithril is a system that allows SPOs to certify the current state of the Cardano
blockchain, allowing, for example, nodes to get up-to-date significantly faster
than by processing the whole chain of blocks.
The core component of Mithril is a stake-based (weighted) threshold
multisignature scheme, also called Mithril, that lets stakeholders collectively
sign a message that convinces a verifier that nodes with more than some
threshold of the participating stake signed this message.
To faciliate fast node bootstrapping, this message is a commitment to the whole
state of the ledger.
ALBA can be used to construct a new weighted threshold multisignature scheme
with a significantly better proof size which
can allow Cardano nodes to process the chain of Mithril certificates faster.
If a SNARK is used to compress Mithril certificates to a very small size,
ALBA can reduce the overall proving time.

Peras and Leios are new extensions of the Ouroboros consensus protocol used in
Cardano.
Ouroboros Peras aims to reduce the block settlement time by executing a byzantine fault
tolerant agreement protocol to decide the next block when adversarial
conditions are mild.
The consensus participants certify the chosen block to allow bootstrapping
nodes to follow the right chain.
A natural method for this certification is to use a
weighted threshold multisignature scheme constructed using decentralized ALBA.

Ouroboros Leios aims to increase the throughput of the blockchain in terms of
the number of transactions per second.
It does so by decoupling the block payloads from the consensus mechanism which
operates only on references to blocks.
To make sure that the chosen block's payload is correct and available
(which potentially could be not due to malicious behavior),
all blocks in the input to the consensus protocol need to have
a certificate of availability proving that at least one honest node has verified
the block content and is available to distribute it.
A natural solution to certify data availability would be to use a
weighted threshold multisignature scheme constructed using decentralized ALBA.

Cross-chain bridges are protocols allowing one blockchain to verify
information on another blockchain.
In essense this is the same problem that the Mithril solves with a potentially
added constraint that on-chain verification must be computationally cheap.
Whereas Mithril is specific to the Cardano blockchain, one can envision
a similar protocol, powered by ALBA, for other cryptocurrencies including
partner chains like Midnight.

Applications of ALBA are not limited to Cardano and blockchains in general.
For instance, the [paper](https://eprint.iacr.org/2023/1655) describes how
this primitive can be used to optimize the prover running time of
a Universally Composable SNARK.

### Organization

The following section describes goals and non-goals of this library, partly
informed by the potential applications.
[Proposed design choices](#proposed-design-choices) details some of
the technical decisions for the library.
[Roadmap](#roadmap) describes a rough plan for the development.

## Requirements

We intend to make an implementation of ALBA that can be used in
Mithril, Peras, Leios, cross-chain bridges and potentially other protocols
inside and outside the Cardano universe.
Our implementation will mostly be guided by our main "clients":
Mirthril, Peras and Leios.

ALBA is a generic tool that lets one prove knowledge of any kind of data
elements, but
as noted [here](#applications-of-alba), these three applications
don't need the full expressivity of ALBA, they really need
a multisignature scheme, where the data elements would specifically be
signatures.
Thus there is question of whether we want our library to provide a general
ALBA interface or simply an interface for a multisignature scheme.
We will do both.
1. We will implement the most generic version of ALBA to make it suitable for
as many applications as possible.
This means that there is only a generic notion of "data elements" and no notion
of signatures, VRF, etc.
2. We will use the generic ALBA library to implement a multisignature scheme,
either as a small example in this repository or a full-fledged library in a
separate repository.
Either way, the multisignature implementation would serve as a flagship
example of how the ALBA library can be used.

The requirements for our (general) ALBA implementation can be roughly split
in five groups: 
1. [definitions](#definitions):
what do the ALBA schemes we implement guarantee, under what assumptions?
2. [correctness](#correctness):
does our implementation provide the same guarantees of
soundness and completeness as the ALBA schemes on paper?
3. [performance](#performance):
proof size, communication complexity, computational complexity.
4. [interfaces](#interfaces):
the applications should be able to use our implementation relatively easily
5. [repository](#repository): code readability, documentation, etc.

### Definitions

The definitions requirement can be stated as follows.
Taking an ALBA protocol we implement, can you construct (on paper) a larger
protocol, such as Mithril, and mathematically prove its guarantees such as
completeness and soundness?
Currently, ALBA schemes satisfying the definitions in the ALBA paper cannot
be used to construct Mithril, Peras and Leios.

First, all three require a multisignature scheme secure against
grinding attacks.
In short, unlike the setting in the ALBA paper, the theorem statement to be
proven is not fixed in advance, but can depend on the previous actions of
the adversary.
The adversary can simulate (grind) different actions to see if there is one
that will allow him to cheat.
For instance, the lottery in Mithril uses a seed from the Ouroboros protocol
to pseudo-randomly select the signing committee;
this seed can to some extent be controlled by the adversary.
Research is needed to see how grinding can be handled and on what
level: ALBA or the multisignature scheme.

Additionally, Mithril and possibly Peras and Leios cannot provide a weight
oracle as required by the definitions in the ALBA paper.
The weight oracle must report the weight of any data element.
For Mithril, if the data elements are signatures, the weight oracle would
have to report for any signature the amount of stake of the creator of the
signature.
Clearly, this is impossible.
Even if the data element was a pair of public key and signature, the weight
oracle would have no way of verifying that the public key owns any stake.
Again, research is needed to construct the right ALBA scheme on
paper before implementing it.

For now, we should implement the core component of (information-theoretic)
ALBA that uses hashing to define valid element tuples as defined in the paper,
without introducing grinding parameters or a weight oracle.

### Correctness

The definitions of ALBA require, for input parameters $\lambda_{sec}$ and
$\lambda_{rel}$, that the soundness error is at most $2^{-\lambda_{sec}}$
and the completeness error is at most $2^{-\lambda_{rel}}$.
Ideally, our implementation should also make this guarantee.

However, at this point, this is not our goal, since achieving that would
require significant additional effort.
We need to roll out a reference implemenentation so that our primary clients
(Mithril, Peras, Leios) can incorporate it in their prototypes, and also so
that the external community can study and experiment with ALBA as well.

The following is a list of known discrepancies that prevent us from making
the rigorous guarantees about soundness and completeness errors.
1. tiny rounding errors in parameter derivation due to the use of
floating-point numbers
2. tiny errors when using a uniformly random hash output to sample distributions
needed for ALBA such as Bernoulli, uniform, binomial, Poisson
3. lack of a rigorous analysis of the $H_1$ optimization described in
section 3.3 of the paper

### Performance

There are three performance metrics in consideration:
1. proof size;
2. communication complexity;
3. computational complexity.

The space complexity (memory usage) is not considered since it is very small
in any ALBA scheme.
When evaluating performance, one should keep in mind that there exists
a tradeoff between the proof size and communication complexity in
decentralized ALBA.
We do not currently have fixed performance requirements for our library.
For completeness, the following is a discussion of what some potential
applications might require.

#### Applications' requirements
Here we consider Peras, Leios and Mithril.

Peras requires the ALBA proof to fit in a single block.
It is only needed if Peras goes into cooldown which should be expected to
happen rarely.
Depending on how ALBA is used, the lower bound parameter $n_f$ would be set
to either 50% or 75% of the total stake.
Leios would produce ALBA proofs very frequently.
Their requirement on proof size -- below 5 kilobytes.
Mithril currently uses a lottery;
therefore, using ALBA instead can only decrease the proof size.
It is possible that Peras and Leios requirements on the proof size cannot
be satisfied using ALBA alone.
In that case, further techniques can be considered such as SNARKs and
taking the stake distribution into account
(see [Fait Accompli](https://eprint.iacr.org/2023/1273)).

These protocols' requirements about communication and computational complexity
are not currently known.
However, the communication complexity of the implemented ALBA schemes will not greatly
exceed that of the simple lottery, and the prover running time is expected to be
in the order of a second.

### Interfaces

Peras and Leios are implemented in Haskell and thus require bindings to our
Rust implementation.
Mithril is implemented in Rust and thus can use our implementation natively.

Beyond this, there are no other clearly stated requirements.
The section [below](#proposed-design-choices) describes some design choices for
the library's interfaces.

### Repository

Our reference implementation should be easy to read, study, modify and
experiment with.
Hence, the source code should be readable, well structured and sufficiently
documented.
Documentation outside the source code (markdown, etc) should explain how to
use the library with examples.

## Proposed design choices

This section describes various design choices for our implementation.

### ALBA algorithms

ALBA comes in multiple different flavors.
Two different setting dimensions, unweighted / weighted and
centralized / decentralized, define four different ALBA variants.
See [here](#overview-of-alba) for an overview.

There also exist multiple ALBA schemes;
currently four are known for the centralized unweighted setting:
1. (basic) Telescope (section 3.1 in the paper),
2. Telescope with Prehashing (section 3.2 in the paper),
3. Telescope with prehashing, repetitions and bounded DFS
("Telescope Bounded" in short, section 3.2.2 in the paper),
4. simple lottery (section 4.1 in the paper).

The same four schemes can be made decentralized by adding a simple lottery
(or just changing interfaces in case of the simple lottery which itself is
already a decentralized scheme), see section 4.1 and 4.2 in the paper.
Additionally, the support for weights can be introduced in the decentralized
schemes by having the simple lottery take into account weights, as done in
Algorand's sortition (see section 5 in the paper).
(Thus, the simple lottery in the decentralized weighted setting is equivalent
to Algorand's sortition.)
Finally, we can create centralized weighted ALBA schemes by remodeling the
decentralized weighted schemes as centralized;
this is currently the only known efficient way to implement weights.

We will implement Telescope Bounded,
as well as, the simple lottery in all four settings (centralized+decentralized,
unweighted+weighted).
The following is the full list of eight implemented schemes.
* Centralized unweighted Telescope Bounded (section 3.2.2)
* Decentralized unweighted Telescope Bounded (section 3.2.2 + 4.2)
* Centralized+decentralized weighted Telescope Bounded (section 3.2.2 + 4.2 + 5)
* Centralized+decentralized unweighted simple lottery (section 4.1)
* Centralized+decentralized weighted simple lottery (section 4.1 + 5)

#### Discussion

We will not implement ALBA schemes (1) and (2) for the following reasons.
1. The basic Telescope is way slower and offers almost no advantages over
the other Telescope schemes.
2. Telescope with Prehashing is only applicable when the number of honest
prover's elements $n_p$ is large
(about a million for typical parameters) and it offers no advantage
over Telescope Bounded except for a tiny difference in the certificate size
(the integer $v$).
(As an optimization of Telescope Bounded, one can omit this integer when
serializing a certificate if the number of allowed repetitions $r = 1$.)

We will implement the simple lottery scheme; despite its large proof sizes
it offers very fast proving time which might be important for some applications.
Additionally, in the decentralized setting, it has the smallest communication
complexity, so it's likely to be the top choice for applications where
the certificate size is not important.

We will implement schemes that seem redundant (e.g., unweighted schemes
since the user could just use a weighted variant with each weight equal 1,
or centralized schemes since the user could just use a decentralized variant
directly) for two reasons:
1. to provide a simpler interface for the user;
2. because they might be more efficient now or when optimizations are
implemented in the future; for instance, unweighted variants can be made
more efficient due to no lottery and less complexity, and in the decentralized
setting, an unweighted version could be more efficient due to not needing to
sample the binomial distribution.

We should be able to reuse most of the logic between different schemes anyway,
so the overhead shouldn't be large.

### Choice of hash function

The "random" functions $H_0$, $H_1$, $H_2$ and others in the ALBA constructions
need to be implemented using some concrete function, either a cryptographic
hash function modeled as a random oracle or a
[pseudorandom function](https://en.wikipedia.org/wiki/Pseudorandom_function_family).
There are numerous options available for one or both models:
SHA, BLAKE, AES, ChaCha, etc.

In our implementation, we will let users provide their own function.
Hash functions evolve, old ones get broken (SHA-1), and so our ALBA
implementation shouldn't get stuck with an inefficient / broken hash function.
Moreover, the user might want to instantiate ALBA with a PRF (e.g. AES) instead
of a hash function modeled as a random oracle for better theoretical guarantees.
With Rust's advanced trait system, making the hash function generic should be
easy, and the runtime code performance should be fast due to compile time
function resolution.

Moreover, we should *require* users to provide their own hash function.
One reason is that when ALBA is used alongside other sub-protocols, that also
need hashing, in a larger protocol, it is the user's responsibility to implement
[domain separation](https://en.wikipedia.org/wiki/Domain_separation)
to provide the different sub-protocols with independent hash functions.
Otherwise, the underlying assumptions needed for security might break and the
larger protocol can become insecure
(see [here](https://link.springer.com/chapter/10.1007/978-3-030-45724-2_1)).
Additionally, different sub-protocols should not use different hash function
constructions (e.g., SHA and BLAKE) since there might be hidden correlations
between them and that would be equivalent to not using domain separation.

Thus, the user must use a particular hash function $H$ of their choice to
instantiate several domain separated hash functions $H_i(x) = H(i, x)$ that
would be provided to the sub-protocols (one of them being ALBA).
To our ALBA implementation, this domain separation would be transparent.

We will still provide an example implementing a hash function with the right
interface for ALBA using, for example, BLAKE underneath, but with a warning that
it is provided for demonstration / prototyping only and should not be used in
production.

### Generic data elements

To make our library application agnostic and as applicable as possible,
the data elements will have a generic type.
For example, the ALBA implementation should work with any signature type that
exposes the underlying data as a `u8` slice.
The exact interfaces will need to be determined.

## Roadmap

We have already implemented the Telescope Bounded scheme
in the centralized unweighted setting.
Following that, we will have an implementation of the simple lottery, also
in the centralized unweighted setting.
After that we can think about good interfaces for our decentralized / weighted
schemes, and implement those.

Concurrently, we can think about how to test and benchmark our implementations,
as well as, implement a generic hash function, generic data elements, etc.

Periodically, it would be good to demo our library to the internal / external
community to spread awareness and gather early feedback.

Finally, our library would benefit from future research to improve
usability (see [definitions requirements](#definitions)), correctness
(see [correctness requirements](#correctness)) and performance.
