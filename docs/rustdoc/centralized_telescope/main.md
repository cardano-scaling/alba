# Telescope - Construction with Bounded DFS

In scenarios where $n_p$ is small, the parameters given for prehashed construction (relative to the security parameter, $\lambda$) are not optimal, resulting in a reduced probability of constructing a valid proof in a single attempt.
For large $n_p$, the rapid growth in potential proof tuples ensures valid ones can be found efficiently, allowing the prehashed construction to work seamlessly.
In contrast, small $n_p$ limits the search space, making the previous parameters inadequate.
To address this, the **construction with bounded DFS** expands on the prehashed version with *retries*, *randomized indexing*, and *bounding* the DFS.

## Overview
- When $n_p$ is large, the rapid increase in potential proof tuples enhances the chances of finding a valid proof, simplifying the construction process.
- For small $n_p$, the element distribution across bins becomes skewed, some bins may contain multiple elements while others remain empty.
  - It reduces the probability of finding a valid proof in a single attempt.
- Although the average number of elements per bin remains one, this imbalance increases the completeness error. 
  - Prehashing can still be applied, but the error becomes more pronounced.
- To mitigate this, multiple retries indexed by $v$ are introduced, each using fresh randomization via hash functions. 
  - It significantly lowers the completeness error and improves success rates.
- A bounded DFS search with a predefined step limit ensures computational efficiency while maintaining a deterministic worst-case runtime.

### Core components
This generalized construction uses the same key parameters as prehashed construction but introduces:
- Retries with index $v$: 
  - The prover retries the proof generation process up to $r$ times.
  - Each retry uses a different retry counter $v$, where $v \in \[r\]$, to diversify the search process.
- Seeded binning:
  - Elements in $S_p$ are prehashed into bins using $H_0(v, \cdot)$ where $v \in \[r\]$, grouping elements based on their hash values. We prehashed using the retry counter to get different bins for each repetition, reducing the risk of badly distributed bins.
  - This process limits the search space for DFS, making it more efficient.
- Bounded DFS:
  - A depth-first search explores valid proof sequences of size $u$, using the bins for efficient lookup.
  - The total number of steps is bounded by a limit to ensure efficiency.

## Protocol
1. **Initialization**:
   - The prover begins by preparing for up to $r$ retries. 
   - Each retry introduces a unique retry counter $v \in \[r\]$ to diversify the search space.
   - Each retry is an independent attempt to construct a valid proof.
2. **Prehashing**:
   - For each retry, elements in $S_p$ are hashed using the hash function $H_0(v, s)$.
   - This process groups elements based on their hash values, reducing the search space by allowing DFS to focus only on relevant elements.
3. **Exploring starting points**:
   - The prover iterates over all possible starting indices $t \in \[d\]$.
   - For each $t$, the prover begins constructing a proof sequence $(v, t, s_1, \ldots, s_u)$, starting with $t$ and extending the sequence using elements from $S_p$ within their corresponding bins.
4. **Bounded DFS**:
   - A bounded DFS search is used to construct the sequence $(v, t, s_1, \ldots, s_u)$, with a shared step limit $B$ applied across all starting points $t$.
   - At each step of DFS:
     - The algorithm searches a new element for the current sequence $(v, t, s_1, \ldots, s_k)$ in bin numbered $H_1(v, t, s_1, \ldots, s_k)$.
     - If the step limit $B$ is reached or no valid extension exists, the DFS backtracks and explores another retry $r$, restarting the process with a new partitioning of elements. 
     - Otherwise, the DFS updates the step limit and then recursively calls itself with the extended tuple.
5. **Validation**:
   - When the DFS has constructed a sequence of $u$ elements $(v, t, s_1, \ldots, s_u)$, it verifies whether the full sequence satisfies the final condition using $H_2(v, t, s_1, \ldots, s_u)$.
   - If the sequence passes this check, it is accepted as a valid proof, and the prover outputs it.
6. **Retry Mechanism**:
   - If no valid proof is found for a given $t$ or if the step limit $B$ is exhausted for all $t$, the prover moves to the next retry by incrementing $v$.
   - With the new $v$, the hash function $H_0(v, s)$ is applied to organize elements of $S_p$ into bins, and the process resumes from step 3.
7. **Completion**:
   - If a valid proof is found in any retry, the prover outputs the proof immediately.
   - If all $r$ retries are exhausted without finding a valid proof, the process terminates, and the prover outputs $\bot$.
