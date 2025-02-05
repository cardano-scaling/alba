## DFS

The `dfs` function performs a depth-first search to construct a valid proof by iterating through elements in bins and progressively building the proof sequence. It starts by checking whether the current sequence of elements has reached the required proof size \( u \). If so, it evaluates the proof’s validity using the proof hash function \( \mathsf{H_2} \), which determines whether the sequence meets the required probability threshold \( q \). If valid, the function returns a proof containing the retry counter \( v \), search index \( t \), and sequence of elements \( s_1, \dots, s_u \); otherwise, it returns failure.

If the proof sequence is incomplete, the function proceeds by iterating over elements in the bin corresponding to the current round identifier \( id \). If the number of DFS calls has already reached the maximum bound \( b \), the function terminates early to prevent excessive computation. Otherwise, each element from the bin is added to the current sequence, forming a new candidate sequence. The round hash function \( \mathsf{H_1} \) is then applied to generate a new candidate round identifier, mapping the updated sequence to a new position in the prover’s dataset.

If the new round identifier is valid, the function recursively calls `dfs` on the updated round, continuing the search for a valid proof. If a valid proof is found in any recursive step, it is immediately returned. Otherwise, the function updates the step counter and continues searching. If all elements in the bin are exhausted without finding a valid proof, the function returns failure, signaling that the current search path did not lead to a solution.

- $\mathsf{dfs} ($ [$params$](variables#parameters) $,~ bins,~ $ [$round$](variables#round) $,~ step) \rightarrow (step, ~$ [$proof$](variables#proof)$)$:
---
- **if** $~~ \mathsf{size}($ [$round.slist$](variables#round-slist) $) ==~ $ [$params.u$](variables#params-u) $:$
  - **if** $~~ $ [$\mathsf{H_2}$](hash_functions#proof-hash) $($ [$params.q$](variables#params-q) $,~ $ [$round$](variables#round) $) ==~ true :$
    - $proof \leftarrow ($ [$round.v$](variables#round-v) $,~ $ [$round.t$](variables#round-t) $,~ $ [$round.slist$](variables#round-slist) $)$
  - **else** : 
    - $proof \leftarrow \bot$
  - **return** $(step,~ proof).$
- **for** each $~~ s \in bins\[$ [$round.id$](variables#round-id) $\]:$
  - **if** $~~ step ~== $ [$params.b$](variables#params-b) $:$
    - **return** $(step,~ \bot).$
  - $new\\_slist \leftarrow$ [$round.slist$](variables#round-slist) $~ \cup ~~ \\{s\\}$
  - $(hash,~ id) \leftarrow $ [$\mathsf{H_1}$](hash_functions#round-hash) $($ [$round.hash$](variables#round-digest) $,~ s,~ n_p)$
  - **if** $~~ id ~~!= \bot:$
    - $updated\\_round \leftarrow ($ [$round.v$](variables#round-v) $,~ $ [$round.t$](variables#round-t) $,~ new\\_slist,~ hash,~ id,~ n_p)$
    - $(dfs\\_calls, ~ proof) \leftarrow$ [$\mathsf{dfs}$](#dfs) $($ [$params$](variables#parameters) $,~ bins,~ updated\\_round,~ step+1)$
    - **if** $~~ proof ~~!= \bot:$
      - **return** $(dfs\\_calls,~ proof).$
    - $step \leftarrow dfs\\_calls$
- **return** $(step,~ \bot).$
---
