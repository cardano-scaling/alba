## Prove

The `prove` function is responsible for generating a valid proof by iterating over multiple retries, attempting to construct a proof within a bounded number of steps. It initializes a step counter to keep track of the total number of DFS calls made throughout the process. The function then loops up to `max_retries` (\( r \)), where each iteration represents an independent attempt to find a valid proof.

For each retry \( v \), the function calls `prove_index`, which performs binning and initiates the DFS search for a proof. The number of DFS calls taken in each retry is accumulated into the step counter. If `prove_index` successfully finds a proof, it is immediately returned, as there is no need to continue searching. If the function exhausts all retries without finding a proof, it returns failure, indicating that no valid proof could be constructed within the given constraints.

This approach ensures that the prover has multiple opportunities to generate a valid proof, allowing the process to recover from cases where a single search attempt might fail due to unfavorable element distributions in the prover’s dataset.

- $\mathsf{prove} (n_p,~$ [$params$](variables#parameters) $, ~S_p) \rightarrow ($ [$proof$](variables#proof)$/ \bot)$:
---
- $step \leftarrow 0$
- **for** each $~~ v \in \[$ [$params.r$](variables#params-r) $\]:$
  - $(dfs\\_calls, ~ proof) \leftarrow$ [$\mathsf{prove\\_index}$](internal_functions#prove-index) $(n_p,~$ [$params$](variables#parameters), $~S_p$, $~v+1)$
  - $step = step + dfs\\_calls$
  - **if** $~~ proof ~~!= \bot:$
    - **return** $~~ proof.$
- **return** $~~ \bot.$
---
