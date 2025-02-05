## DFS
The `dfs` function performs a depth-first search to construct a valid proof by iterating through elements in bins. 
Its process is as follows:
- If the sequence reaches `proof_size` $u$, it checks validity using `proof_hash` $\mathsf{H_2}$.
  - If valid, returns the proof; otherwise, returns total DFS calls and an empty proof.
- If incomplete, iterates over elements in the bin corresponding to the current round identifier $id$.
  - If DFS calls reach `dfs_bound` $b$, terminates early.
  - Adds elements to the sequence and computes a new round identifier using `round_hash` $\mathsf{H_1}$.
- If the new identifier is valid, recursively calls `dfs` on the updated round.
  - If a proof is found, returns it immediately.
  - Otherwise, updates the step counter and continues searching.
- If all elements are exhausted without finding a proof, returns total DFS calls and an empty proof.

---

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
