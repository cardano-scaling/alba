Prover functionality

### Prove
The `prove` function initiates the search for a valid proof. It tries multiple configurations by iterating over indices. For each index, it calls `prove_index` to explore possible proofs.
- Loop through all possible configurations.
- Call `prove_index` for each configuration.
- If a valid proof is found, return it.
- If no valid proof is found after all configurations, return an empty proof.

`prove`($setup$ = ($n_p$, $u$, $r$, $d$, $q$, $b$), $set$) $\rightarrow proof$ = ($r$, $d$, $items$)
- **For** each $v$ in $r$:
    - $proof \leftarrow$ `prove_index` $(setup, set, v)$
    - **If** $proof \quad != \quad$ NONE:
        - **RETURN** $proof$
- $proof \leftarrow (r:0,\quad d:0, \quad items: vec!\[~\])$
- **RETURN** $proof$

### Prove Index
For a given index, `prove_index` tries to find a valid proof by searching all possible starting points. It calls `dfs` for each starting point to explore deeper paths.

### **Steps:**
- Set a limit for each DFS attempt to prevent infinite loops.
- Loop through all possible starting points.
- For each point, call `dfs` to explore further.
- If `dfs` finds a valid proof, return it.
- If no valid proof is found, return _none_.

`prove_index`($setup$ = ($n_p$, $u$, $r$, $d$, $q$, $b$), $set$, $v$) $\rightarrow (count, proof$ = ($r$, $d$, $items$))
- $bins \leftarrow vec!\[~ \]$ $\[~\]$
- **For** $i$ in $n_p$:
    - $bins\[i\] \leftarrow \[~\]$
- **For** each $s$ in $set$:
    - $index \leftarrow$ `h0`($setup, v, s$)
    - $bins\[index\] \leftarrow s$
- $count \leftarrow 0$
- **For** each $t$ in $d$:
    - **If** $count == b$:
        - **RETURN** ($0$, NONE).
    - $count++$
    - $round \leftarrow$ `new_round`($v, t, n_p$)
    - $result \leftarrow$ `dfs`($setup, bins, round, count$)
    - **If** $result \quad != \quad$ NONE:
        - **RETURN** ($count$, $result$)
- **RETURN** ($count$, NONE)

### DFS
`dfs` performs a depth-first search to find a sequence of elements that satisfies the hash constraints. It checks each element recursively and stops if the limit is reached.
- If the sequence reaches the desired length, perform the final hash check.
    - If valid, return the proof.
    - If not, return _none_.
- For each element in the current bin, recursively explore the next element.
- If no valid proof is found within the current path, return _none_.

`dfs`($setup$ = ($n_p$, $u$, $r$, $d$, $q$, $b$), $bins$, $round$ = ($v$, $t$, $s_{list}$, $h$, $h_{usize}$, $n_p$), $count$) $\rightarrow proof$ = ($r$, $d$, $items$)
- **If** $s_{list}.len == u$
    - **If** `h2`($setup$, $round$)
        - $proof \leftarrow (v, t, s_{list})$
        - **RETURN** $proof$.
    - **Else**
        - **RETURN** NONE.
- $bin \leftarrow bins\[h_{usize}\]$
- **For** $s$ in $bin$:
    - **If** $count == d$:
        - **RETURN** NONE.
    - $count++$
    - $round \leftarrow$ `update_round`($round$, $s$)
    - $result \leftarrow$ `dfs`($setup$, $bins$, $round$, $count$)
- **RETURN** $result$
