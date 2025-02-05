## Prove Index
The `prove_index` function organizes the prover's dataset into bins and performs multiple search attempts using DFS. 
Its functionality is as follows:
- Initializes empty bins and assigns elements using the `bin_hash` function $\mathsf{H_0}$.
- If an element cannot be assigned to a valid bin, the function continues execution.
- Iterates over `search_width` $d$ search attempts to find a proof.
- Generates a candidate round using `round_hash` $\mathsf{H_1}$, based on `retry_counter` $v$ and `search_index` $t$.
- If the round identifier is valid, initializes an empty round and calls `dfs` to explore proof sequences.
- If DFS finds a valid proof, returns the proof and the number of DFS calls.
- If no proof is found, updates the step counter and continues searching.
- If all search attempts fail, returns the total number of DFS calls alongside an empty proof.

---

- $\mathsf{prove\\_index} (n_p,~$ [$params$](variables#parameters) $, ~S_p, ~v) \rightarrow (step, ~$ [$proof$](variables#proof)$/ \bot)$:
---
- $bins \leftarrow \big\\{ \\{ ~ \\}$ **for** $~ i \in \[n_p\] \big\\}:$
- **for** each $~~ s \in S_p:$
    - $ind \leftarrow$ [$\mathsf{H_0}$](hash_functions#bin-hash) $(n_p,~ v,~ s)$
    - **if** $~~ ind ~~!= \bot:$
        - $bins\[ind\] \leftarrow bins\[ind\] \cup \\{s\\}$
    - **else**:
        - **return** $(0,~ \bot).$
- $step \leftarrow 0$
- **for** each $~~ t \in \[$ [$params.d$](variables#params-d) $\]:$
    - **if** $~~ step ~ \geq ~$ [$params.b$](variables#params-b) $:$
        - **return** $(0,~ \bot).$
    - $(hash,~ id) \leftarrow $ [$\mathsf{H_1}$](hash_functions#round-hash) $(\mathsf{bytes}(v),~ \mathsf{bytes}(t),~ n_p)$
    - **if** $~~ id ~~!= \bot:$
        - $round \leftarrow (v,~ t,~ \\{ ~ \\},~ hash,~ id,~ n_p)$
        - $(dfs\\_calls, ~ proof) \leftarrow$ [$\mathsf{dfs}$](internal_functions#dfs) $($ [$params$](variables#parameters) $,~ bins,~ round,~ step+1)$
        - **if** $~~ proof ~~!= \bot :$
            - **return** $(dfs\\_calls, ~ proof).$
        - $step \leftarrow dfs\\_calls$
- **return** $(step,~ \bot).$
---
