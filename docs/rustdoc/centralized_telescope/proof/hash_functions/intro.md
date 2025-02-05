## Overview
The centralized telescope makes use of three core oracle functions: `bin_hash`, `round_hash`, and `proof_hash`, each serving a distinct role in the system.

The `bin_hash` function assigns an element $s$ to a bin within the range $[0, n_p[$ based on the `retry_counter` [$v$](variables#params-v). 
It computes a hash by concatenating $s$ with the byte representation of [$v$](variables#params-v), then interprets it as a random number and reduces it modulo $n_p$ to obtain a uniform value. 
n rare cases, it may return `⊥` if the hash lacks sufficient randomness or is too short.

The `round_hash` function generates a round identifier for a search attempt by computing a hash from two input values. 
It extracts a uniform sample from this hash to produce a round identifier within $[0, n_p[$. 
If sampling fails, it returns `⊥` along with the computed hash.

The `proof_hash` function determines proof validity using a probabilistic check. 
It computes a hash from the round's hash value and performs a Bernoulli trial based on the probability threshold [$q$](variables#params-q). 
The function returns `true` with probability in $[q - \varepsilon, q]$ for a negligible $\varepsilon$; otherwise, it returns `false`, ensuring that valid proofs are accepted with the desired probability.
