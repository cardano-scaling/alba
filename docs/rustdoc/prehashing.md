# Telescope - Construction with Prehashing

**Construction with Prehashing** optimizes the [basic Telescope ALBA][crate::docs::basic] protocol by reducing the computational cost of checking sequences.
Here, a sequence means an ordered set of elements or tuples that meet certain hash conditions in the protocol.
In the basic approach, we would attempt to extend a given sequence "blindly", that is trying all elements of $S_p$. 
Finding the correct extensions required multiple invocations of the random oracle $H_1$, which could be computationally expensive.
Instead, in the **prehashing technique**, we filter the elements to extend a sequence with.
We do so by sorting the elements in bins, depending on their precomputed hash values, and when extending, instead of checking all elements like in the naive method, we check only the elements in the bin associated to the given sequence.

On average, each bin contains just one element, making this approach significantly faster and more efficient.
This adjustment is particularly advantageous when $S_p$ is large, as it compresses the data while maintaining the security and correctness of the proof system.

## Overview 
- A hash function is applied to the elements of the prover's set $S_p$, creating a prehashed representation of each element in the set, which is also sorted into bins based on the hash values.
- Proof tuples are constructed with the help of prehashed elements rather than the original set.

### Core components
The construction with prehashing retains the same parameters as described in basic construction. 
Additionally, it incorporates a new hash function $H_0$, specifically introduced for prehashing the elements of the set $S_p$.
For this construction, the random functions are as follows:
- $H_0: ~~$ a random function, generating a uniformly random value in $\[n_p\]$.
- $H_1: ~~$ a random function, generating a uniformly random value in $\[n_p\]$.
- $H_2: ~~$ A random function returning $1$ with probability $q$, applied as a final test to determine if a tuple qualifies as a valid proof.

## Protocol
- The prover precomputes hash values for each element $s \in S_p$ using $H_0$, assigning each element to the "bin" numbered $H_0(s)$.
- A sequence is similarly associated to a bin, by hashing the sequence with $H_1$.
- These precomputed "bins" show how a sequence can be extended, the "balls" in the bins being the potential extension of the sequence.
- The prover starts constructing sequences from $t \in 1, \ldots, d$ and aims to build sequences of length $u$.
- When constructing sequences, the prover selects elements directly from the current bin to extend the sequence, continuing this process until the sequence reaches the required length $u$.
- After constructing a sequence of length $u$, the prover validates it with $H_2$. The sequence is a valid proof if $H_2(t, s_1, \dots, s_u) = 1$.

The values precomputed with $H_0$ enable quick checks to see if a new element can extend a sequence, based on whether $H_1(t, s_1, \dots, s_k)$ matches the bin with label $H_0(s_{k+1})$.
This avoids checking all possible extensions, dramatically reducing the prover’s workload.

### Parameter generation
The parameters for the construction with prehashing are carefully selected to balance *security* and *efficiency*.
Based on the security parameters ($\lambda_{sec}$, $\lambda_{rel}$) and $n_f$, $n_p$, the parameters such as $u$, $d$, and $q$ are computed.
As described in the basic construction, $\lambda_{sec}$ sets the probability that an adversary with at most $n_f$ elements can produce a valid proof.
$\lambda_{rel}$ ensures that an honest prover, with a sufficiently large set $S_p$, can reliably generate a valid proof.

- The proof size $u$ is proportional to $\lambda_{sec} + \log\lambda_{rel}$ and inversely proportional to $\log(n_p / n_f)$:
  - As the prover's set size $n_p$ increases relative to $n_f$, the required proof length decreases logarithmically, reducing the adversary's advantage.
  - $u$ influences other parameters like $d$, reinforcing its importance in the protocol's efficiency.
- The maximum number of subtrees to search, $d$, scales linearly with the proof size $u$ and the security parameter $\lambda_{rel}$.
  - A higher value of $d$ increases the likelihood of constructing a valid proof, ensuring robustness in random oracle evaluations.
- The probability that a tuple of full size is selected, $q$, is inversely proportional to $d$ and approximates $\lambda_{rel} / d$:
  - Since $d$ is related to $u$, $q$ is also approximately inversely proportional to $u$.
  - A smaller $q$ improves security by lowering the chance of accepting cheating proofs but decreases the probability of finding valid proofs, requiring a larger $d$ to compensate.

$$
u \coloneqq \Bigg\lceil \frac{\lambda_{sec} + \log (\lambda_{rel} + \log 3) + 1 - \log \log e}{\log (n_p / n_f)}\Bigg\rceil,
$$
$$
d \coloneqq \Bigg\lceil \frac{16 u (\lambda_{rel} + \log 3)}{\log e}\Bigg\rceil , \quad q \coloneqq \frac{2 (\lambda_{rel} + \log 3)}{d \log e}.
$$
Furthermore, to maintain a high probability of successfully constructing a valid proof, $n_p$ must satisfy:
$$
n_p \geq \frac{d^2 \log e}{9(\lambda_{rel} + \log 3)}.
$$
- This condition ensures that the prover's set is large enough to achieve a well-distributed hashing of elements into bins.
- If $n_p$ is too small, bins may not be sufficiently populated, reducing the chances of constructing valid tuples.

### Example walkthrough
- **Prover’s set** $S_p = (A, B, C, D, E)$
- **Proof length** $u = 3$
- **Search width** $d = 2$ (i.e., $t \in \{1, 2\}$)
---
> **Step 1:** Prehash the elements with $H_0$.
> - The prover precomputes $H_0(s)$ for all elements in $S_p$, assigning each element to a bin:
>   - $H_0(A) = 1$
>   - $H_0(B) = 2$
>   - $H_0(C) = 1$
>   - $H_0(D) = 4$
>   - $H_0(E) = 4$
> - Bins are grouped as follows:
>   - $\mathsf{bin_1}$: $(A, C)$
>   - $\mathsf{bin_2}$: $(B)$
>   - $\mathsf{bin_3}$: $\emptyset$
>   - $\mathsf{bin_4}$: $(D, E)$
> ---
> **Step 2:** Proof construction for $t = 1$.
> - The prover starts with $t = 1$ and attempts to construct a valid sequence of length $u = 3$.
> - Depth $k = 1$:
>   - Compute $H_1(1)$ to determine the first bin to explore.
>     - Assume $H_1(1) = 1$, so the prover looks in bin 1.
>   - $\mathsf{bin_1}$: $(A, C)$
>     - Extend the sequence with $s_1 = A$.
>   - Current sequence: $(t, s_1) = (1, A)$.
> - Depth $k = 2$:
>   - Compute $H_1(t, s_1)$ to determine the next bin to explore.
>     - Assume $H_1(1, A) = 3$, so the prover looks in bin 3.
>   - $\mathsf{bin_3}$: $\emptyset$
>     - No valid $s_2$ can be selected.
> - Backtracking for $t = 1$:
>   - After exhausting all possible sequences for $t = 1$, no valid proof is found.
>   - The prover backtracks and tries $t = 2$.
> ---
> **Step 3:** Proof construction for $t = 2$.
> - Depth $k = 1$:
>   - Compute $H_1(2)$ to determine the first bin to explore.
>     - Assume $H_1(2) = 4$, so the prover looks in bin 4.
>   - $\mathsf{bin_4}$: $(D, E)$
>     - Extend the sequence with $s_1 = D$.
>   - Current sequence: $(t, s_1) = (2, D)$.
> - Depth $k = 2$:
>   - Compute $H_1(t, s_1)$ to determine the next bin to explore.
>     - Assume $H_1(2, D) = 2$, so the prover looks in bin 2.
>   - $\mathsf{bin_2}$: $(B)$
>     - Extend the sequence with $s_2 = B$.
>   - Current sequence: $(t, s_1, s_2) = (2, D, B)$.
> - Depth $k = 3$:
>   - Compute $H_1(2, s_1, s_2)$ to determine the next bin to explore.
>     - Assume $H_1(2, D, B) = 1$, so the prover looks in bin 1.
>   - $\mathsf{bin_1}$: $(A, C)$
>     - Extend the sequence with $s_3 = A$.
>   - Current sequence: $(t, s_1, s_3, s_3) = (2, D, B, A)$.
> ---
> **Step 4:** Final Validation.
> - A tuple with size $u = 3$ is obtained: $(t, s_1, s_2, s_3) = (2, D, B, A)$.
> - If $H_2(2, D, B, A) = 1$, the sequence is valid, and the prover outputs it as proof.
---

## Functions
#### Proving
---
> $\mathsf{Prove}(S_p) \rightarrow \pi$
> - Input:
>   - $S_p:~~$ `prover_set`, a set of elements to be proven.
> - Output:
>   - $\pi:~~$ `proof`, a valid proof $(t, s_1, \ldots, s_u)$ or $\bot$.
> ---
> - $bins \leftarrow ( ~ )$ 
> - **for** each $~~ i \in \[n_p\]:$
>   - $bins\[i\] \leftarrow ( ~ )$
> - **for** each $~~ s \in S_p:$
>   - $ind \leftarrow \mathsf{H_0}(s)$
>   - $bins\[ ind \] \leftarrow bins\[ ind \] \cup (s)$
> - **for** each $~~ t \in \[d\]:$
>   - $\pi \leftarrow \mathsf{DFS}(t, ( ~ ), bins)$
>   - **if** $~~ \pi ~!= \bot:$
>     - **return** $~~ \pi.$
> - **return** $~~ \bot.$
---
#### Depth fist search
---
> $\mathsf{DFS}(t, slist, S_p) \rightarrow \pi$
> - Input:
>   - $t:~~$ `search_counter`, index of the current subtree being searched.
>   - $slist:~~$ `element_sequence`, candidate element sequence.
>   - $bins:~~$ `bins`, a hash-based partition of $S_p$ elements.
> - Output:
>   - $\pi:~~$ `proof`, a valid proof $(t, s_1, \ldots, s_u)$ or $\bot$.
> ---
> - **if** $~~ \mathsf{len}(slist) == u:$
>   - **if** $~~ \mathsf{H_2}(t, slist) == 1:$
>     - $\pi \leftarrow (t, slist)$
>     - **return** $~~ \pi.$
>   - **return** $~~ \bot.$
> - $ind \leftarrow \mathsf{H_1}(t, slist)$
> - **for** each $~~ s \in bins\[ind\]:$
>   - $slist_{new} \leftarrow slist \cup (s)$
>   - $\pi \leftarrow \mathsf{DFS}(t, slist_{new}, bins)$
>   - **if** $~~ \pi ~!= \bot:$
>     - **return** $~~ \pi.$
> - **return** $~~ \bot.$
---
#### Verification
---
> $\mathsf{Verify}(\pi) \rightarrow 0/1$
>
> - Input:
>   - $\pi:~~$ `proof`, a proof of the form $(t, s_1, \ldots, s_u)$.
> - Output:
>   - $0/1$.
> ---
> - **if** $~~ t ~∉ \[d\]:$
>   - **return** $~~ 0.$
> - **for** each $~~ i \in \[u\]:$
>   - **if** $~~ \mathsf{H_1} (t, s_1, \ldots, s_{i-1}) ~!= \mathsf{H_0}(s_i)$:
>     - **return** $~~0.$
> - **return** $~~ \mathsf{H_2}(t, s_1, \ldots, s_u).$
---
