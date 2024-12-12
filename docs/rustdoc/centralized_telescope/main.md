# Telescope - Construction with Bounded DFS

In scenarios where $n_p$ is small, the probability of successfully constructing a valid proof in a single attempt decreases significantly compared to cases where $n_p$ is large.
For large $n_p$, the prover can efficiently find valid tuples due to the abundance of elements satisfying the conditions, allowing the construction with prehashing to work seamlessly.
However, with small $n_p$, the limited size of $S_p$ reduces the likelihood of constructing a valid proof within a single search.
To overcome this, the approach leverages **retries with randomized indices** and **bounded DFS**, ensuring the prover can amplify the probability of success even with fewer elements while maintaining efficiency and security.

## Overview
- This construction leverages retries and hash-based randomization to amplify the completeness when $n_p$ is small.
- The prover retries multiple times ($r$ attempts) using different random indices $v$, increasing the chances of constructing a valid proof.
- A bounded DFS search is employed to limit computational costs by setting a maximum number of steps.

### Core components
The generalized construction uses the same key parameters as prehashed construction but introduces:
- Retries: Each retry is indexed by $v \in \[1, r\]$, where $r$ is chosen to ensure completeness.
- Bounded Search: The DFS search depth is restricted by a parameter limit, which prevents excessive computation.

The random functions used are:
- $H_0 ~~:$ Prehashes elements of $S_p$ by generating a uniformly random value in $\[n_p\]$, creating bins for efficient filtering.
- $H_1 ~~:$ Validates sequences by checking consistency with $H_0$. The function generates a uniformly random value in $\[n_p\]$.
- $H_2 ~~:$ A final check determining the validity of the full sequence. Returns $1$ with the probability $q$.

## Protocol
The protocol ensures the prover can successfully construct a valid proof even when $n_p$ is small by employing retries, hash-based binning, and bounded DFS.
- Each element $s \in S_p$ is prehashed using a randomized hash function $H_0(v, s)$ for a specific retry index $v$.
- The result of $H_0(v, s)$ assigns $s$ to a bin, effectively partitioning the prover's set $S_p$ into smaller groups for efficient search.
- The prover attempts $r$ retries, each indexed by $v \in \[1, r\]$, to amplify the completeness of the protocol.
- For each retry, a fresh partitioning of $S_p$ is generated using $H_0$, providing a randomized search space for the proof construction.
- For each retry $v$, the prover initializes a bounded DFS process:
    - Starting from each $t \in \[1, d\]$, the prover attempts to construct a valid sequence $(v, t, s_1, \ldots, s_u)$.
    - The DFS explores elements within the bins precomputed by $H_0$, reducing unnecessary checks and focusing on valid extensions.
    - The DFS search is restricted by a predefined limit to prevent runaway computation.
- When a sequence of length $u$ is constructed, it is validated using $H_2$, which determines whether the sequence qualifies as a valid proof.
- If $H_2(v, t, s_1, \ldots, s_u) = 1$, the sequence is accepted as a valid proof.
