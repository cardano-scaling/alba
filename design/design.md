# ALBA implementation design document

## Introduction

This is the first design doc for our implementation of Approximate Lower Bound
Arguments intended to guide the development of the library.
See the [website](https://alba.cardano-scaling.org) or
the [paper](https://eprint.iacr.org/2023/1655) to get an overview of this
cryptographic primitive.

We intend to create a high quality reference implementation of ALBA sufficient
for accurately prototyping larger protocols utilizing this primitive.
Applications that could benefit, at least in the Cardano universe, include
Mithril, Peras, Leios and cross-chain bridges.
These use cases will generally guide the development of our library, though
our aim is to also allow the open-source community to study and experiment
with ALBA.

Down the road, it will be possible to make this library production ready
by removing various simplifying assumptions and thus making the implementation
more rigorous.

The following describes various technical decisions and a proposed course of
action.

## Implemented cryptographic primitives

ALBA is a very generic tool that lets one prove knowledge of any kind of data
elements.
On the other hand, many potential applications (such as those listed above),
don't need the full expressivity of ALBA, they really need a decentralized
multi-signature protocol.
With great power comes great responsibility, and we, the developers of this
library, need to take into account the tradeoff between freedom and usability.
One solution is to have both.

1. We will implement the most generic version of ALBA to make it suitable for
as many applications as possible.
This means that there is only a generic notion of "data elements" and no notion
of signatures, VRF, etc.

2. We will use the generic ALBA library to implement a multi-signature scheme,
either as a small example in this repository or a full-fledged library in a
separate repository.
Either way, the multi-signature implementation would serve as a flagship
example of how the ALBA library can be used.

## ALBA algorithms

ALBA comes in multiple different flavors.
One variation dimension is the support for weights.
In the unweighted setting, the prover is given $\ge n_p$ elements and he needs
to prove that he has more than $n_f$ elements ($n_p > n_f$).
In the weighted setting, on the other hand, elements have an integer weight and
the prover is given elements of total weight $\ge n_p$ and he wants to prove that
he knows enough elements of total weight $> n_f$.

Another dimension of ALBA flavors is decentralization.
In the centralized setting, there are only two parties, prover and verifier,
and the prover is assumed to know all $n_p$ elements
(or elements of total weight $n_p$).
In the decentralized setting, there are multiple different provers each holding
one element (weighted or unweighted based on the model); the other two parties
are aggregator and verifier.
Some provers will send a message to the aggregator who will in turn produce the
final proof $\pi$ for the verifier.

See section 1 in the paper for an overview of these two flavor dimensions.

There also exist multiple ALBA schemes;
currently four are known for the the centralized unweighted setting:
1. (basic) Telescope (Section 3.1 in the paper),
2. Telescope with Prehashing (Section 3.2 in the paper),
3. Telescope with prehashing, repetitions and bounded DFS
("Telescope Bounded" in short, Section 3.2.2 in the paper),
4. simple lottery (Section 4.1 in the paper).

The same four schemes can be made decentralized by adding a simple lottery
(or doing nothing in case of the simple lottery which itself is already a
decentralized scheme), see Section 4.1 and 4.2 in the paper.
Additionally, the support for weights can be introduced in the decentralized
schemes by having the simple lottery take into account weights, as done in
Algorand's sortition (see Section 5 in the paper).
Thus, the simple lottery in the decentralized weighted setting is equivalent
to Algorand's sortition.
Finally, we can create the centralized weighted ALBA schemes by remodeling the
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

### Discussion
We will not implement the other two ALBA schemes because the basic Telescope is
way slower and offers almost no advantages, while Telescope with Prehashing is
only applicable when
the number of honest prover's elements $n_p$ is large and it offers no advantage
over Telescope Bounded except for a very tiny difference in the certificate size
(the integer $v$).
As an optimization of Telescope Bounded, one can omit this integer when
serializing a certificate if the number of allowed repetitions $r = 1$.
The simple lottery scheme, on the other hand, has large proof sizes but offers
very fast proving time which might be important for some applications.
Additionally, in the decentralized setting, it has the smallest communication
complexity $mu$, so it's likely to be the top choice for applications where
the certificate size is not important.

We will implement schemes that seem redundant (e.g., unweighted schemes
since the user could just use a weighted variant with each weight equal 1,
or centralized schemes since the user could just use a decentralized variant
directly) for two reasons:
1. to provide a simpler interface for the user;
2. because they might be more efficient now or when optimizations are
implemented in the future; for instance, unweighted variants can be concretely
more efficient due to no lottery and less complexity, and in the decentralized
setting, an unweighted version could be more efficient due to not needing to
sample the binomial distribution.
We should be able to reuse most of the logic between different schemes anyway,
so the overhead shouldn't be large.

## Choice of hash function
The "random" functions $H_0$, $H_1$, $H_2$ and others in ALBA constructions need to be
implemented using some concrete function, either a cryptographic hash function
modeled as a random oracle or a pseudo-random function (todo: add references).
There are numerous options available for one or both models:
SHA, BLAKE, AES, ChaCha, etc.

In our implementation, we will let the user provide his own function.
Hash functions evolve, old ones get broken (SHA-1), and so our ALBA
implementation shouldn't get stuck with an inefficient / broken hash function.
Moreover, the user might want to instantiate ALBA with a PRF (e.g. AES) instead
of a hash function modeled as a random oracle for better theoretical guarantees.
With Rust's advanced trait system, making the hash function generic should be
easy, and the runtime code performance should be fast due to compile time
function resolution.

We will provide an example implementing a hash function with the right interface
for ALBA using, for example, BLAKE underneath, but with a warning that
it is provided for demonstration / prototyping only and should not be used in
production.
One reason is that when ALBA is used alongside other sub-protocols that also
need hashing, it is the user's responsibility to implement
[domain separation](https://en.wikipedia.org/wiki/Domain_separation)
to provide the different
sub-protocols with independent hash functions.
Otherwise, the underlying assumptions needed for security might break and the
larger protocol can become insecure
(see [here](https://link.springer.com/chapter/10.1007/978-3-030-45724-2_1)).
the hash function do not collide.
Additionally, different hash functions (e.g., SHA and BLAKE) shouldn't mix and
match.
The following example is contrived, but it demonstrates why using different
hash function constructions within the same protocol can be insecure.
Suppose H and H' are two distinct random looking hash functions.
You might expect that the function G given by G(x) = H(x) xor H'(x) is also
random looking, but it's not true.
Let H be a hash function modeled as a random oracle and define H(x) = ~H'(x),
where ~ is bitwise not; then H' is also random looking
but G(x) = 111...111 for all x.
Even if we use domain separation for H and H', we cannot guarantee that
G(x) = H(0, x) xor H'(1, x) is random looking.
Define H'(b, x) = H(~b, x); then G(x) = 000...000 for all x.

## Mathematical rigor and production-readiness
At this point, it is not our goal to make the implementation 100% correct and
secure.
We need to roll out a prototype so that our primary clients (Peras, etc) can
incorporate it in their prototypes, and also so that the external community can
start playing with it as well.
This means that there might be tiny rounding errors in parameter derivation due
to the use of floats, and there might be tiny errors when using a uniformly
random hash output to sample some distribution needed for ALBA
(e.g., Bernoulli, Uniform, Binomial, Poisson).
We will also probably not rigorously analyze the $H_1$ optimization described in
section 3.3 of the paper.

## Parameter calculation
ALBA has two kinds of parameters: user friendly parameters such as
$\lambda_{sec}$, $\lambda_{rel}$, $n_p$, $n_f$,
and algorithm specific parameters such as $u$, $d$ and $q$.

Our implementation will have parameter derivation code converting user friendly
to algorithm specific parameters.
However, this should be decoupled from the ALBA algorithms:
through documentation, we will encourage the developer to run parameter
derivation manually in advance and then hardcode the output algorithm specific
parameters in their application.
The ALBA functions such as `prove` and `verify` will thus receive as input
the algorithm specific parameters.

The rational is that we want to be able to evolve parameter derivation
(e.g., to optimize parameters for performance, or making the derivation logic
more rigorous) while maintaining backward compatibility with previous versions
of the library.

## Deviations from the ALBA paper
In the future, our implementation will likely deviate from the paper.

One reason is that the definitions given there are quite poor.
They are, unfortunately, inapplicable for most applications.
For example, the assumption that there exists a weight oracle, reporting
the weight of any data element given as input, is often unrealistic.
In Mithril, for instance, light clients don't know the whole blockchain, but
would need to verify that signatures in an ALBA certificate come from
accounts with real stake.
The ALBA paper simply assumes that the light clients can query the weight oracle
to get the stake of the signature's creator, but in practice the ALBA
certificate will need to contain for each signature the correct stake and
a proof of its correctness (e.g., by supplying a merkle proof).
Another problem not addressed in the paper is grinding;
for example, the adversary can move his money to new accounts thus creating
new valid signatures, which can help him create a rogue ALBA proof.
These are research problems, and when solved, we will need to enhance / modify
our implementation as well.

Additionally, we should investigate using the Poisson distribution instead of
the Binomial as it can potentially simplify calculations and help make our code
rigorous in the future.

## Integration with larger protocols
Mithril, which is written in Rust, should be able to use ALBA directly.
For applications written in Haskell, such as Peras and Leios,
Haskell bindings will be developed.

## Roadmap
We've already started with an implementation of the Telescope Bounded scheme
in the centralized unweighted setting.
Following that, we will have an implementation of the simple lottery, also
in the centralized unweighted setting.
After that we can think about good interfaces for our decentralized / weighted
schemes, and implement those.

Concurrently, we can think about how to test and benchmark our implementations,
as well as, implement a generic hash function, generic data elements, etc.
