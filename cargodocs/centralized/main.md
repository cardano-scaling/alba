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


### Documentation
See the documentation:
- The data structures are given in: [Structures][crate::cargodocs::centralized::structs]
- Setting up the protocol parameters: [Setup][crate::cargodocs::centralized::setup]
- The prover functionality including the functions `prove`, `prove_index`, `dfs` is given in: [Prover][crate::cargodocs::centralized::prover]
- The verifier functionality is given in: [Verifier][crate::cargodocs::centralized::verifier]