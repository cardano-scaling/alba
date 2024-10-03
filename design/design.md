Applications?
mithril, peras, leios

What cryptographic primitives will we implement?
I propose that we implement the generic ALBA primitive to make it as applicable
as possible.
This means there is only a generic notion of "elements" and no notions of
signatures, vrf, etc.
This would be somewhat difficult to work with for all our three applications
that really just need a weighted multisignature scheme.
Hence, my second proposal is to use this generic ALBA implementation to implement
a convenient multisignature scheme.
It would also serve as a nice example of how the generic ALBA code can be used.

Which ALBA algorithms to implement?
For the centralized unweighted setting, there are four known algorithms to date:
(basic) Telescope, Telescope with prehashing, Telescope with prehashing,
repetitions and bounded DFS, and finally simple lottery.
I propose that we only implement the third scheme -- Telescope with prehashing,
repetitions and bounded DFS, and possibly the simple lottery.
The basic Telescope is way slower and offers almost no advantage over the other
schemes.
Telescope with prehashing is only applicable when the number of honest prover's
element n_p is large which is probably uncommon.
Moreover, it offers no advantage over the third scheme except for a very tiny
difference in the certificate size (integer v).
As an optimization of the Telescope with prehashing, repetitions and bounded
DFS, we could even eliminate this integer from the certificate if the
number of allowed repetitions r = 1.
Finally, the simple lottery, while having large proof sizes, offers very
fast proving times which might be important for some applications.
For the decentralized setting and weighted settings, we again have four
possible algorithms:
the three Telescope algorithms combined with the simple lottery, and the
simple lottery itself.
I propose we implement Telescope with prehashing, repetitions and bounded DFS,
as well as, the single simple lottery.
Similar to above, the other Telescope schemes offer little advantage over this
Telescope algorithm.
The simple lottery, however, offers the smallest communication complexity mu
and fastest aggregation time.
It can likely be the top choice for applications where certificate size is not
important.
We should decide if we want to provide both unweighted and weighted versions, or
simply the weighted versions.
At least for the centralized setting, unweighted versions can be concretely more
efficient due to no lottery and less complexity.
For the decentralized setting, an unweighted version could be more efficient as
well due to not needing to sample the binomial distribution.

What hash function to use for ALBA?
Ideally, the user should be able to provide his own hash function.
Hash functions evolve, old ones get broken (sha1) and so our ALBA implementation
shouldn't get stuck with an inefficient / broken hash function.
Moreover, the user might want to instantiate ALBA with a PRF (e.g. AES) instead
of a hash function modeled as a random oracle for better theoretical guarantees.
With Rust's advanced trait system, providing the right interfaces should be
easy, and the runtime code performance should be fast due to compile time
function resolution.
We might think about offering a default hash function (e.g. blake2), but this
should be done with care.
As many applications use a hash function for multiple sub-protocols, it is
important to specify distinct hash function seeds for each so that inputs to
the hash function do not collide.

How mathematically rigorous should our implementation be?
At this point, probably not very rigorous.
We need to roll out a prototype so that our primary clients (mithril, etc) can
incorporate it in their prototypes, and also so that the external community can
start playing with it as well.
This means that tiny errors when converting a uniformly random hash output to
an ALBA specific distribution will be ignored, and sampling a binomial / poisson
distribution for the weighted ALBA will follow a simple but not well understood
algorithm.

How will parameters be chosen / computed?
I propose that we separate parameter derivation for the actual ALBA code.
This means that converting user friendly parameters such as n_p, n_f, lambdas to
algorithm specific parameters such as d, q should be done separately and ahead
of time.
The ALBA functionality itself will only accept algorithm specific parameters.
Through documentation, we should encourage developers to run parameter
derivation manually and hardcode the result in the application.
The rational is that we want to be able to evolve parameter derivation
(e.g. to optimize parameters for performance, or ensuring more rigor) while
maintaining backward compatibility with previous versions of the library.

Are there any deviations from the ALBA paper?
Yes!
First, the definitions given in that paper are quite poor.
They are, unfortunately, inapplicable in most applications.
One example is the assumption in the paper that there exists a weight oracle
that for each element reports its weight.
This is quite unrealistic.
For instance, a digital signature tells you nothing about the stake of its signer.
Instead, we will have some kind of "proof" that the public key behind the
signature indeed has stake (e.g. by supplying the merkle path).
The issue of the right definitions / certificate structure is TODO.
Additionally, we should investigate using the poisson distribution instead of
the binomial one as it can give us simpler calculations, easier rigorous
arguments in the future, and more performant code.

How will we integrate this Rust library with Peras and Leios?
Haskell bindings will be developed.

What are our first steps?
The implementation of the unweighted Telescope scheme with prehashing,
repetitions and bounded DFS is a good first step.
No matter if we choose to provide unweighted algorithms or not, it will be
a building block for the decentralized / weighted settings.
We should think about how to test it and also implement proper benchmarks
showing the prove time distribution and other useful metrics.
