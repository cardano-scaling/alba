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

## Algorithm: Main Construction

### Parameters
- Soundness security parameter: $\lambda_{sec}$
- Completeness security parameter: $lambda_{rel}$
- Approximate size of set $S_p$ to lower bound: $n_p$
- Target lower bound: $n_f$

### Setup
#### Fields
- Approximate size of set $S_p$ to lower bound: $n_p$
- Proof size (in $S_p$ elements): $u$
- Proof max counter: $r$
- Proof max 2nd counter: $d$
- Probability: $q$
- Computation bound: $b$

#### Computation

**Input:** $\lambda_{sec}$, $\lambda_{rel}$, $n_p$, $n_f$.

**Output:** $n_p$, $u$, $r$, $d$, $q$, $b$.
- Compute $u$:
  - $u \leftarrow \left\lceil \frac{\lambda_{\text{sec}} + \log \lambda_{\text{rel}} + 5 - \log \log e}{\log \left(\frac{n_p}{n_f}\right)} \right\rceil$

- Compute $s_1$, $s_2$:
  - $ratio \leftarrow \frac{9 n_p \log e}{17u^2}$
  - $s_1 \leftarrow ratio - 7, \quad s_2 \leftarrow ratio - 2.$

- If $s_1 < 1$ or $s_2 < 1$:   
  - $r \leftarrow \lambda_{rel}$
  - $d \leftarrow \lceil 32\cdot \ln(12)\cdot u \rceil$
  - $q \leftarrow 2 \cdot \ln(12) / d$
  - $b \leftarrow \lfloor 8 \cdot (u + 1) \cdot d / \ln(12) \rfloor$

- $\lambda_{rel}^{(2)} \leftarrow min(\lambda_{rel}, s_2 )$

- If $u < \lambda_{rel}^{(2)}$:  
  - $d \leftarrow \lceil 16 \cdot u \cdot (\lambda_{rel}^{(2)} + 2) / \log{e} \rceil$ 
  - If $n_p \geq d^2 \cdot \log{e} / (9 \cdot (\lambda_{rel}^{(2)} + 2))$:
    - Quit
  - $r \leftarrow \lceil \lambda_{rel} / \lambda_{rel}^{(2)} \rceil$
  - $q \leftarrow (2 \cdot (\lambda_{rel}^{(2)} + 2) / (d \cdot \log{e}))$
  - $b \leftarrow \lfloor((\lambda_{rel}^{(2)} + 2 + \log{u}) / (\lambda_{rel}^{(2)} + 2))\cdot (3 \cdot u \cdot d / 4) + d + u\rfloor$
- Else:
  - $\lambda_{rel}^{(1)} \leftarrow min(\lambda_{rel}, s_1 )$
  - $\bar{\lambda} \leftarrow (\lambda_{rel}^{(1)} + 7) / \log{e} $
  - $d \leftarrow \lceil 16 \cdot u \cdot \bar{\lambda})\rceil$
  - If $n_p \geq d^2 / (9 \cdot \bar{\lambda})$:
    - Quit
  - $w \leftarrow compute_w(u, \lambda_{rel}^{(1)})$ 
  - $r \leftarrow \lceil\lambda_{rel} / \lambda_{rel}^{(1)}\rceil$
  - $q \leftarrow \mathsf{recip}(2 \cdot \bar{\lambda} / d)$
  - $b \leftarrow ....$






