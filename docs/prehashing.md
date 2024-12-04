# Telescope - Construction with Prehashing

**Construction with Prehashing** optimizes the basic Telescope ALBA protocol by reducing the computational cost of checking sequences.
In the basic approach, each sequence extension required multiple evaluations of the random oracle $H_1$, which could be computationally expensive.
The **prehashing technique** precomputes hash values for individual elements using $H_0$, allowing the prover to perform fewer operations when constructing sequences.
Precomputed hashes enable quick verification of whether a new element can extend a sequence, avoiding the need to check every combination in full detail.
This adjustment is particularly advantageous when $S_p$ is large, as it compresses the data while maintaining the security and correctness of the proof system.

## Overview 
- A hash function is applied to the prover's set $S_p$, creating a prehashed representation of each element in the set.
- Proof tuples are constructed using the prehashed elements rather than the original set.

### Core components
The construction with prehashing retains the same parameters and random oracles as described in [basic construction][crate::docs::basic#core-components]. 
Additionally, it incorporates a new hash function $H_0$, specifically introduced for prehashing the elements of the set $S_p$.

## Protocol
- The prover precomputes hash values for each element $s \in S_p$ using $H_0$, assigning each element to a "bin" based on $H_0(s)$.
- These precomputed "bins" help decide if a sequence can be extended, treating elements as "balls" placed in corresponding bins.
- When constructing sequences, the prover checks if the current sequence and the next element fall into the same bin. If they do, the sequence is extended; otherwise, a different element is tried.
- The prover starts constructing sequences from $t \in 1, \ldots, d$ and aims to build sequences of length $u$.
- The precomputed $H_0$ values enable quick checks to see if a new element can extend a sequence, based on whether $H_1(t, s_1, \dots, s_k)$ matches $H_0(s_{k+1})$.
- This avoids checking all possible extensions, dramatically reducing the prover’s workload.
- After constructing a sequence of length $u$, the prover validates it with $H_2$. The sequence is valid if $H_2(t, s_1, \dots, s_u) = 1$.

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
>   - Also check if the prehash value of $C$ matches the current sequence's bin ($bin_2$, from $H_0(A)$). Since $H_0(C) = 2$, the sequence can be extended.
>   - If $H_1(1, A, C) = 2$, the sequence $(1, A, C)$ is valid so far.
> - At Depth $k = 3$ (Final Element):
>   - Add the third element $s_3 = B$ to extend the sequence to $(1, A, C, B)$.
>   - Compute $H_1(1, A, C, B)$ and check if the prehash value of $B$ matches the current bin ($bin_2$). Since $H_0(B) = 1$, which does not match $bin_2$, this sequence extension fails.
> - Backtracking:
>   - Backtrack and TRY a different element for $s_3$. Now, try $s_3 = D$.
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
>   - $S_p:~~$ A set of elements.
> - Output:
>   - $\pi:~~$ A valid proof $(t, s_1, ..., s_u)$ or $\emptyset$.
> ---
> - $bins \leftarrow $ { }
> - **for** each $~~ i \in 1, \ldots, n_p:$
>   - $bins\[i\] \leftarrow \[~\]$
> - **for** each $~~ s \in S_p:$
>   - $ind \leftarrow \mathsf{H}_0(s)$
>   - $bins\[ ind \] \leftarrow bins\[ ind \] \cup \[s\]$
> - **for** each $~~ t \in 1, \ldots, d:$
>   - $\pi \leftarrow \mathsf{DFS}(t, \[~\], bins)$
>   - **if** $~~ \pi ~=\not \emptyset:$
>     - **return** $~~ \pi.$
> - **return** $~~ \emptyset.$
---

#### Depth fist search
---
> $\mathsf{DFS}(t, slist, v, S_p) \rightarrow \pi$
> - Input:
>   - $t:~~$ Search counter
>   - $slist:~~$ Element sequence.
>   - $bins:~~$ A set of elements.
> - Output:
>   - $\pi:~~$ A valid proof $(t, s_1, ..., s_u)$ or $\emptyset$.
> ---
> - **if** $~~ \mathsf{len}(slist) = u$:
>   - **if** $~~ \mathsf{H}_2(t, slist) = 1$:
>     - $\pi \leftarrow (t, slist)$
>     - **return** $~~ \pi$.
>   - **return** $~~ \emptyset$.
> - $current$_$bin \leftarrow \mathsf{H}_1(t, slist)$
> - **for** $~~ s \in bins\[current$_$bin\]$:
>   - $newlist \leftarrow slist \cup \[s\]$
>   - **if** $~~ \mathsf{H}_1(t, newlist) = 1$:
>     - $\pi \leftarrow \mathsf{DFS}(t, newlist, bins)$
>     - **if** $~~ \pi ~=\not \emptyset$:
>       - **return** $~~ \pi$.
> - **return** $~~ \emptyset$.
---
#### Verification
---
> $\mathsf{Verify}(\pi) \leftarrow 0/1$
>
> - Input:
>   - $\pi = (t, s_1, ..., s_u)$.
> - Output:
>   - $0/1$.
> ---
> - **if** $~~ t \in\not  \[d\]$:
>   - **return** $~~ 0$.
> - **for** $~~ i = 1\ldots u$:
>   - **if** $~~ \mathsf{H_1} (t, s_1, \ldots, s_{i-1}) ~=\not \mathsf{H_0}(s_i)$:
>     - **return** $~~0$.
> - **return** $~~ \mathsf{H}_2(t, s_1, \ldots, s_u)$.
---
