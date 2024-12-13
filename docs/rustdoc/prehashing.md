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
Let's assume the prover's set $S_p$ contains the elements $\{A, B, C, D\}$, and the prover needs to build sequences of length $u = 3$. 
The prover is working with $d = 2$ distinct sequences (i.e., the prover will try to construct a sequence for each $t = 1$ and $t = 2$).

> ---
> - The prover first precomputes the hash values for each element in their set using the random oracle $H_0$. This assigns each element to a "bin" based on the result of $H_0$.
>   - $H_0(A) = 2$
>   - $H_0(B) = 1$
>   - $H_0(C) = 2$
>   - $H_0(D) = 3$
> - The prover begins the construction process by selecting $t = 1$ as the starting point and aims to construct a sequence of length $u = 3$.
> - At Depth $k = 1$:
>   - Select $s_1 = A$ from the set $S_p$.
>   - Compute $H_1(1, A)$ to check if this initial sequence is valid.
>   - If $H_1(1, A) = H_0(A)$, the sequence $(1, A)$ is valid so far. Since $H_0(A) = 2$, let's assume $H_1(1, A) = 2$, and the sequence passes this check.
> - At Depth $k = 2$:
>   - Extend the sequence by selecting the next element $s_2 = C$.
>   - Check if the sequence $(1, A, C)$ is valid by computing $H_1(1, A, C)$.
>   - Also check if the prehash value of $C$ matches the current sequence's bin: $bin_2$, from $H_0(A)$. Since $H_0(C) = 2$, the sequence can be extended.
>   - If $H_1(1, A, C) = 2$, the sequence $(1, A, C)$ is valid so far.
> - At Depth $k = 3$ (Final Element):
>   - Add the third element $s_3 = B$ to extend the sequence to $(1, A, C, B)$.
>   - Compute $H_1(1, A, C, B)$ and check if the prehash value of $B$ matches the current bin ($bin_2$). Since $H_0(B) = 1$, which does not match $bin_2$, this sequence extension fails.
> - Backtracking:
>   - Backtrack and try a different element for $s_3$. Now, try $s_3 = D$.
>   - Compute $H_1(1, A, C, D)$. Since $H_0(D) = 3$ and does not match $bin_2$, this also fails.
>   - Backtrack further but continue trying until a valid sequence is found.
> - Since the prover couldn't find a valid sequence for $t = 1$, they now try $t = 2$ and start the DFS process again.
> - At Depth $k = 1$:
>   - Select $s_1 = B$, compute $H_1(2, B)$, and check if it matches $H_0(B)$. If $H_1(2, B) = 1$, this sequence is valid so far.
> - At Depth $k = 2$:
>   - Select $s_2 = A$. Since $H_0(A) = 2$, check if $H_1(2, B, A) = 2$. If this holds, the sequence $(2, B, A)$ is valid at this step.
> - At Depth $k = 3$ (Final Element):
>   - Add $s_3 = C$ to extend the sequence to $(2, B, A, C)$. Since $H_0(C) = 2$, compute $H_1(2, B, A, C)$ and check if it matches $bin_2$. If $H_1(2, B, A, C) = 2$, this sequence is valid so far.
> - Once the prover has constructed the full sequence $(2, B, A, C)$, they perform the final validation using the random oracle $H_2$.
>   - The prover computes $H_2(2, B, A, C)$. If this returns $1$, the entire sequence is valid, and the prover has successfully found a valid proof.
>     - The sequence $(2, B, A, C)$ serves as proof that the prover knows a sufficiently large set of elements that satisfy the predicate.
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
