Telescope construction with prehashing and bounded DFS scheme given in _Section 3.2.2_.

In some cases, $n_p$ may be small, which can affect the completeness and efficiency of the proof construction. The protocol adapts to such cases by introducing optimizations and tighter bounds.
- In the basic ALBA scheme, optimal performance requires $n_p \geq \lambda^3$.
- When $n_p$ is small (e.g., $n_p < \lambda^3$), the probability of successfully finding valid proof paths diminishes.
- Ensure that the proof remains efficient and sound even when $n_p$ is small by introducing an improved protocol with tighter guarantees on the number of attempts and running time.
- When $n_p$ is too small to guarantee a valid proof in a single attempt, the protocol allows the prover to make multiple attempts.
- Each attempt is indexed by an integer $v \in \{1, \ldots, r\}$, where $r$ is the number of repetitions. This ensures higher completeness by amplifying the success probability.
  - Set $r := \lambda_{\text{rel}}$, ensuring the completeness error becomes $2^{-\lambda_{\text{rel}}}$.
  - This approach maintains completeness even when the prover has a limited number of elements in their set.

### Optimized Proof Construction
- A valid proof now consists of a tuple $(v, t, s_1, \ldots, s_u)$, where:
  - $v$ is the repetition index.
  - $t \in \{1, \ldots, d\}$ is another index used to explore proof paths.
  - $s_1, \ldots, s_u$ are elements from the prover's set.
- The tuple must satisfy:
  $$
  H_1(v, t, s_1, \ldots, s_{i-1}) = H_0(v, s_i), \quad \forall i \in \{1, \ldots, u\}
  $$
  $$
  H_2(v, t, s_1, \ldots, s_u) = 1
  $$

### Performance
- **Proof Size:** $u = \left\lceil \frac{\lambda_{\text{sec}} + \log \lambda_{\text{rel}} + 5 - \log \log e}{\log \left(\frac{n_p}{n_f}\right)} \right\rceil$
- **Number of Repetitions:**  $r := \lambda_{\text{rel}}$ ensures completeness with error $2^{-\lambda_{\text{rel}}}$.
- **Expected Running Time:** The average running time for the prover is: $n_p + \mathcal{O}(u^2)$.
- **Worst-Case Running Time:** The worst-case running time is bounded by: $n_p + \mathcal{O}(u^2 \cdot \lambda_{\text{rel}})$.


## Structures
### Params
- $\lambda_{sec}$: Soundness security parameter.
- $\lambda_{rel}$: Completeness security parameter.
- $n_p$: Approximate size of set $S_p$ to lower bound.
- $n_f$: Target lower bound:

### Setup
- $n_p$: Approximate size of set $S_p$ to lower bound.
- $u$: Proof size (in $S_p$ elements). 
- $r$: Proof max counter. 
- $d$: Proof max 2nd counter. 
- $q$: Probability.
- $b$: Computation bound. 

### Round
- $v$: attempt_index,
- $t$: Proof 2nd counter, tuple index,
- $s_{list}$: item list
- $h$: item Hash,
- $h_{usize}$: Round candidate hash mapped to /[1, n_p/],
- $n_p$: approximate size of Sp,

### Proof
- $r$: num of repetitions
- $d$: search depth
- $items$: proof items

## Functions
### Setup new protocol
**Input:** Params.

**Output:** Setup.

- Compute $u$, $s_1$, $s_2$:
  $$
  u \leftarrow \left\lceil \frac{\lambda_{\text{sec}} + \log \lambda_{\text{rel}} + 5 - \log \log e}{\log \left(\frac{n_p}{n_f}\right)} \right\rceil
  $$
  $$
  ratio \leftarrow \frac{9 n_p \log e}{17u^2}
  $$
  $$
  s_1 \leftarrow ratio - 7, \quad s_2 \leftarrow ratio - 2.
  $$

- If ($s_1 < 1$ or $s_2 < 1$) $\implies$ SMALL CASE, $\quad n_p \leq \lambda^2$
  $$
  r \leftarrow \lambda_{rel}, \quad d \leftarrow \lceil 32\cdot \ln(12)\cdot u \rceil,
  $$
  $$
  q \leftarrow \frac{2 \cdot \ln(12)}{d}, \quad b \leftarrow \Big\lfloor \frac{8 \cdot (u + 1) \cdot d}{\ln(12)} \Big\rfloor.
  $$

- Set $\lambda_{rel}^{(2)}$:
  $$
  \lambda_{rel}^{(2)} \leftarrow min(\lambda_{rel}, s_2 )
  $$

- If ($u < \lambda_{rel}^{(2)}$) $\implies$ HIGH CASE, $\quad n_p \geq \lambda^3$
  - Compute $d$:
    $$
    d \leftarrow \Bigg\lceil \frac{16 \cdot u \cdot (\lambda_{rel}^{(2)} + 2)}{\log{e}} \Bigg\rceil
    $$
  - If $n_p \geq \frac{d^2 \cdot \log{e}}{9 \cdot (\lambda_{rel}^{(2)} + 2)}$ $\implies$ QUIT.
  - Compute $r$, $q$, $b$:
    $$
    r \leftarrow \Big\lceil \frac{\lambda_{rel}}{\lambda_{rel}^{(2)}} \Big\rceil, \quad q \leftarrow \frac{2 \cdot (\lambda_{rel}^{(2)} + 2)}{d \cdot \log{e}}
    $$
    $$
    b \leftarrow \Bigg\lfloor\frac{\lambda_{rel}^{(2)} + 2 + \log{u}} {\lambda_{rel}^{(2)} + 2}\cdot \Big(3 \cdot u \cdot \frac{d}{4}\Big) + d + u\Bigg\rfloor
    $$

- Else $\implies$ MID CASE, $\quad \lambda^3 > n_p > \lambda^2$
  - Set $\lambda_{rel}^{(1)}$, compute $\bar{\lambda}$ and $d$:
    $$
    \lambda_{rel}^{(1)} \leftarrow min(\lambda_{rel}, s_1 )
    $$
    $$
    \bar{\lambda} \leftarrow \frac{\lambda_{rel}^{(1)} + 7}{\log{e}}, \quad d \leftarrow \lceil 16 \cdot u \cdot \bar{\lambda}\rceil
    $$
  - If $n_p \geq \frac{d^2}{9 \cdot \bar{\lambda}}$ $\implies$ QUIT.
  - Compute $w$, $r$, $q$, $b$:
    $$
    w \leftarrow \mathsf{compute_w}(u, \lambda_{rel}^{(1)}), \quad r \leftarrow \Big\lceil\frac{\lambda_{rel}}{\lambda_{rel}^{(1)}}\Big\rceil, \quad q \leftarrow \mathsf{recip}\Big(2 \cdot \frac{\bar{\lambda}}{d}\Big)
    $$
    $$
    b \leftarrow \Bigg\lfloor\Big(\frac{w \bar{\lambda}}{d} + 1\Big) \cdot \mathsf{exp}\Big(\frac{2 u w \bar{\lambda}}{n_p} + \frac{7 u}{w}\Big)d u + d \Bigg\rfloor
    $$

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
