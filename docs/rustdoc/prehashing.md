# Telescope - Construction with Prehashing

**Construction with Prehashing** optimizes the basic Telescope ALBA protocol by reducing the computational cost of checking sequences.
Here, a sequence means an ordered set of elements or tuples that meet certain hash conditions in the protocol.
In the basic approach, we would attempt to extend a given sequence "blindly" with all elements of $S_p$. 
Finding the correct extension required multiple invocations of the random oracle $H_1$, which could be computationally expensive.
Instead, in the **prehashing technique**, we filter the elements to extend a sequence with. 
We do so by precomputing hash values using a new oracle $H_0$ before running the DFS algorithm, and associating each element and bin with these.

Precomputed hashes enable quick verification of whether a new element can extend a sequence. 
The elements are first sorted into bins based on their hash values. 
During verification, only the content of the relevant bin is checked, rather than examining all possible combinations. 
On average, each bin contains just one element, making this approach significantly faster and more efficient.
This adjustment is particularly advantageous when $S_p$ is large, as it compresses the data while maintaining the security and correctness of the proof system.

## Overview 
- A hash function is applied to the elements of the prover's set $S_p$, creating a prehashed representation of each element in the set, which is also sorted into bins based on the hash values.
- Proof tuples are constructed using the prehashed elements rather than the original set.

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
- When constructing sequences, the prover selects elements directly from the current bin to extend the sequence, continuing this process until the sequence reaches the required length $u$.
- The prover starts constructing sequences from $t \in 1, \ldots, d$ and aims to build sequences of length $u$.
- After constructing a sequence of length $u$, the prover validates it with $H_2$. The sequence is a valid proof if $H_2(t, s_1, \dots, s_u) = 1$.

The precomputed $H_0$ values enable quick checks to see if a new element can extend a sequence, based on whether $H_1(t, s_1, \dots, s_k)$ matches $H_0(s_{k+1})$.
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
- **Prover’s set** $S_p = \{A, B, C, D\}$
- **Proof length** $u = 3$
- **Search width** $d = 2$ (i.e., $t \in \{1, 2\}$)
- **Hash functions**:
  - $H_0(s)$: Prehash for binning elements of $S_p$.
  - $H_1(v, t, s_1, \ldots, s_k)$: Determines the next bin to explore based on the sequence so far.
  - $H_2(v, t, s_1, \ldots, s_u)$: Final validity check for the sequence.
> ---
> - The prover precomputes $H_0(s)$ for all elements in $S_p$, assigning each element to a bin:
>   - $H_0(A) = 2$
>   - $H_0(B) = 1$
>   - $H_0(C) = 2$
>   - $H_0(D) = 3$
> - Bins are grouped as follows:
>   - Bin 1: $\{B\}$
>   - Bin 2: $\{A, C\}$
>   - Bin 3: $\{D\}$
> ---
> - The prover starts with $t = 1$ and attempts to construct a valid sequence of length $u = 3$ using bounded DFS.
> - Depth $k = 1$:
>   - Compute $H_1(1)$ to determine the first bin to explore.
>     - Assume $H_1(1) = 2$, so the prover looks in bin 2.
>   - Select $s_1 = A$ from bin 2.
>     - Compute $H_1(1, A)$ to check where to explore next.
>     - $H_1(1, A)$ takes the current sequence $(1, A)$ and outputs the bin index for the next step.
>     - Assume $H_1(1, A) = 3$, meaning the prover should explore bin 3.
> - Depth $k = 2$:
>   - Look in bin 3, which contains $\{D\}$.
>   - Select $s_2 = D$.
>   - Compute $H_1(1, A, D)$ to check where to explore next.
>     - Assume $H_1(1, A, D) = 1$, meaning the prover should explore bin 1.
> - Depth $k = 3$ (Final Element):
>   - Look in bin 1, which contains $\{B\}$.
>   - Select $s_3 = B$.
>   - Compute $H_1(1, A, D, B)$ to check if this sequence can proceed further.
>     - Assume $H_1(1, A, D, B) = 3$, which indicates a mismatch with the required condition. The sequence is invalid.
> - Backtracking for $t = 1$:
>   - After exhausting all possible sequences for $t = 1$, no valid proof is found.
>   - The prover backtracks and tries other combinations of $s_1$, $s_2$, and $s_3$.
> - Step 3: Retry with $t = 2$
>   - The prover now starts the process for $t = 2$.
> - Depth $k = 1$:
>   - Compute $H_1(2)$ to determine the first bin.
>     - Assume $H_1(2) = 1$, so the prover looks in bin 1.
>   - Select $s_1 = B$.
>   - Compute $H_1(2, B)$ to check where to explore next.
>     - Assume $H_1(2, B) = 2$, meaning the prover should explore bin 2.
> - Depth $k = 2$:
>   - Look in bin 2, which contains $\{A, C\}$.
>   - Select $s_2 = A$.
>   - Compute $H_1(2, B, A)$ to check where to explore next.
>     - Assume $H_1(2, B, A) = 3$, meaning the prover should explore bin 3.
> - Depth $k = 3$ (Final Element):
>   - Look in bin 3, which contains $\{D\}$.
>   - Select $s_3 = D$.
>   - Compute $H_1(2, B, A, D)$ to check if this sequence is valid so far.
>     - Assume $H_1(2, B, A, D) = 3$, which satisfies the condition.
> - Step 4: Final Validation for $t = 2$
>   - Validate the full sequence $(2, B, A, D)$ using $H_2(2, B, A, D)$.
>     - If $H_2(2, B, A, D) = 1$, the sequence is valid, and the prover outputs it as proof.
> ---

## Functions
#### Proving
---
> $\mathsf{Prove}(S_p) \rightarrow \pi$
> - Input:
>   - $S_p:~~$ `prover_set`, a set of elements to be proven.
> - Output:
>   - $\pi:~~$ `proof`, a valid proof $(t, s_1, \ldots, s_u)$ or $\bot$.
> ---
> - $bins \leftarrow$ { }
> - **for** each $~~ i \in \[1,~ n_p\]:$
>   - $bins\[i\] \leftarrow \[~\]$
> - **for** each $~~ s \in S_p:$
>   - $ind \leftarrow \mathsf{H_0}(s)$
>   - $bins\[ ind \] \leftarrow bins\[ ind \] \cup \[s\]$
> - **for** each $~~ t \in \[1,~ d\]:$
>   - $\pi \leftarrow \mathsf{DFS}(t, \[~\], bins)$
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
> - $bin_{current} \leftarrow \mathsf{H_1}(t, slist)$
> - **for** each $~~ s \in bins\[bin_{current}\]:$
>   - $slist_{new} \leftarrow slist \cup \[s\]$
>   - **if** $~~ \mathsf{H_1}(t, slist_{new}) == 1:$
>     - $\pi \leftarrow \mathsf{DFS}(t, slist_{new}, bins)$
>     - **if** $~~ \pi ~!= \bot:$
>       - **return** $~~ \pi.$
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
> - **for** each $~~ i \in \[1, ~u\]:$
>   - **if** $~~ \mathsf{H_1} (t, s_1, \ldots, s_{i-1}) ~!= \mathsf{H_0}(s_i)$:
>     - **return** $~~0.$
> - **return** $~~ \mathsf{H_2}(t, s_1, \ldots, s_u).$
---
