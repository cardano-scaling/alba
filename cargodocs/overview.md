## Key Concepts and Overview of the Protocol
- **Predicate** - $(R)$:
    - A condition that each element in the set must satisfy.
    - The prover's goal is to convince the verifier that they know more than a certain number ($n_f$) of elements that satisfy this predicate.
- **Security parameter** - $(\lambda_{sec})$:
    - A parameter that controls the soundness of the proof (the probability that a dishonest prover can falsely convince the verifier).
- **Security parameter** - $(\lambda_{rel})$:
    - A parameter that controls the completeness of the proof (the probability that an honest prover can successfully convince the verifier).
- **Random oracles** - $(H_0, H_1, H_2)$:
    - These are hash functions used in the protocol to create conditions that the prover's elements must satisfy.
    - They simulate the behavior of random functions and are crucial for ensuring the security and efficiency of the protocol.
- The prover starts with a small base set and aims to build a valid sequence of elements that satisfies conditions imposed by random oracles.
- The **"telescope"** approach incrementally extends the sequence by adding one element at a time, checking each step against the condition of the random oracle $H_1$ to ensure validity.
- Using a **depth-first search (DFS)** method, the prover explores possible sequences efficiently, aiming to construct a full sequence that meets all conditions.
- The random oracle $H_2$ introduces a rare condition that the final sequence must satisfy, ensuring the prover genuinely knows a large set of valid elements.
- The protocol reduces the number of random oracle evaluations, making it more efficient than brute-force searches, with an expected running time of $\mathcal{O}(n_p \cdot \lambda^2)$ and worst-case time of $\mathcal{O}(n_p \cdot \lambda^3)$.
- In the **"Construction with Prehashing"** variant, the prover precomputes hash values using $H_0$, improving efficiency by reducing the expected running time to $n_p + \mathcal{O}(\lambda^2)$ and worst-case time to $n_p + \mathcal{O}(\lambda^3)$.
- **Soundness**: A dishonest prover cannot easily convince the verifier without knowing a sufficiently large set of elements. The parameter $\lambda_{sec}$ controls this probability.
- **Completeness**: An honest prover with enough valid elements can successfully convince the verifier. The parameter $\lambda_{rel}$ controls the chance of failure for an honest prover.


## Telescope ALBA - Basic Construction

### Key components and parameters
- $d$: The maximum number of iterations.
- $u$: The length of each sequence the prover needs to build.
- $n_p$: The size of the prover's set of elements.
- $n_f$: A threshold value, much smaller than $n_p$, which the verifier uses to approximate the prover's knowledge.
- $q$: The probability with which $H_2$ outputs $1$, controlling the likelihood that a sequence is accepted.
- $H_1$: A random function that returns 1 with a probability of $\frac{1}{n_p}$. This function is used to decide whether a sequence of elements should be considered valid.
- $H_2$: A random function that returns 1 with probability $q$. It introduces an additional condition that a sequence must satisfy to be valid.

### Overview of the construction
- The prover starts with a set of elements $S_p$.
    - Each element satisfies a predicate $R$.
    - The prover's goal is to construct sequences of these elements that meet the conditions set by the random oracles $H_1$ and $H_2$.
- The prover attempts to construct $d$ sequences, each of length $u$. Each sequence is constructed incrementally, one element at a time.
    - The prover begins with an integer $t \in 1,\ldots, d$.
    - Selects the first element $s_1$ from $S_p$.
    - For this pair $(t, s_1)$ to be valid, the random oracle $H_1(t, s_1)$ must return $1$, which happens with probability $\frac{1}{n_p}$.
    - If the pair is valid, the prover proceeds to add a second element $s_2$ to the sequence.
    - Creates the tuple $(t, s_1, s_2)$.
    - The tuple is valid if $H_1(t, s_1, s_2)$ returns $1$.
    - This process continues until a sequence of length $u$ is constructed.
- The prover checks whether the entire sequence satisfies the condition $H_2$.
    - Constructs the sequence $(t, s_1, \ldots, s_u)$.
    - The sequence is considered valid if $H_2(t, s_1, \ldots, s_u)$ returns $1$, which happens with probability $q$.
    - If the sequence is valid, it serves as proof that the prover knows a sufficiently large set of elements that meet the predicate $R$.
- **Depth-First Search (DFS) Approach:** As in above, the prover selects a starting integer $t \in 1\ldots d$ and for each $t$, will attempt to construct a valid sequence of length $u$ as follows:
    - The prover begins with $k = 1$.
      > $k$ is a variable representing the current depth or step in the sequence being constructed. It indicates how many elements have been added to the sequence so far.
    - At this step, the prover forms the initial sequence $(t, s_1)$ and checks whether the sequence is valid.
    - If the sequence is valid, the prover then increments $k$ to $2$, and extends the sequence to $(t, s_1, s_2)$.
    - The prover increments $k$ again if the extended sequence is valid and continues this process until $k$ reaches $u$.
    - If at any step $k$, the sequence $(t, s_1, \dots, s_k)$ is found to be invalid (i.e., $H_1(t, s_1, \dots, s_k) = 0$), the prover backtracks. This means the prover returns to the previous step $k-1$ and tries a different element for $s_k$.
    - Backtracking allows the prover to explore alternative sequences by trying different elements at each step.
    - If the prover successfully constructs a sequence of length $u$ (i.e., reaches $k = u$ with all checks passing), they perform a final check using $H_2(t, s_1, \dots, s_u)$. If this check is successful, the sequence is considered fully valid, and the prover can present it as proof.
    - If no valid sequence is found after exploring all possibilities for a given $t$, the prover moves to the next $t$ and repeats the process.

### Security
- The soundness of the protocol ensures that a dishonest prover, who knows fewer than $n_f$ elements that satisfy the predicate $R$, will not be able to construct a valid sequence with high probability. This is because the probability of passing all the $H_1$ and $H_2$ checks by chance is very low if the prover doesn't know enough valid elements.
- Completeness ensures that an honest prover, who knows more than $n_p$ elements, will be able to construct a valid sequence with high probability. The parameters $d$, $u$, and $q$ are chosen to balance the success rate, making it highly likely that a valid sequence can be constructed if the prover indeed knows enough valid elements.

### Efficiency considerations:
- The prover's expected running time is $\mathcal{O}(n_p \times \lambda^2)$ because the prover needs to try different sequences and check their validity using the random oracles.
- The worst-case time is $\mathcal{O}(n_p \times \lambda^3)$, which occurs if the prover has to explore many possible sequences before finding a valid one.
- By using a DFS approach and carefully choosing parameters, the protocol minimizes the number of oracle queries, making it more efficient than brute-force approaches.


## Telescope ALBA - Construction with Prehashing
- **Construction with Prehashing** optimizes the basic Telescope ALBA protocol by reducing the computational cost of checking sequences.
- In the basic approach, each sequence extension required multiple evaluations of the random oracle $H_1$, which could be computationally expensive.
- The **prehashing technique** precomputes hash values for individual elements using $H_0$, allowing the prover to perform fewer operations when constructing sequences.
- Precomputed hashes enable quick verification of whether a new element can extend a sequence, avoiding the need to check every combination in full detail.
- This significantly reduces the prover's workload during sequence construction, improving efficiency.

### Overview of the construction with prehashing
- The prover precomputes hash values for each element $s \in S_p$ using $H_0$, assigning each element to a "bin" based on $H_0(s)$.
- These precomputed "bins" help decide if a sequence can be extended, treating elements as "balls" placed in corresponding bins.
- When constructing sequences, the prover checks if the current sequence and the next element fall into the same bin. If they do, the sequence is extended; otherwise, a different element is tried.
- The prover starts constructing sequences from $t \in 1, \ldots, d$ and aims to build sequences of length $u$.
- The precomputed $H_0$ values enable quick checks to see if a new element can extend a sequence, based on whether $H_1(t, s_1, \dots, s_k)$ matches $H_0(s_{k+1})$.
- This avoids checking all possible extensions, dramatically reducing the prover’s workload.
- After constructing a sequence of length $u$, the prover validates it with $H_2$. The sequence is valid if $H_2(t, s_1, \dots, s_u) = 1$.

---
> #### Example walkthrough:
> Let's assume the prover's set $S_p$ contains the elements $\{A, B, C, D\}$, and the prover needs to build sequences of length $u = 3$. The prover is working with $d = 2$ distinct sequences (i.e., the prover will try to construct a sequence for each $t = 1$ and $t = 2$).
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
---

### Security:
- Prehashing does not weaken the protocol's security. The prover cannot fake prehash values since they are determined by $H_0$, ensuring a dishonest prover cannot easily cheat.
- An honest prover with more than $n_p$ valid elements can still build a valid sequence with high probability. Prehashing only improves efficiency, not security.

### Efficiency improvements:
- Prehashing elements with $H_0$ eliminates the need to check every sequence extension, narrowing the search space and making the prover's task more efficient.
- Basic construction had a running time of $\mathcal{O}(n_p \times \lambda^2)$. Prehashing reduces this to $n_p + \mathcal{O}(\lambda^2)$, with a worst-case time of $n_p + \mathcal{O}(\lambda^3)$.
- While the prover incurs a one-time cost to precompute $H_0$ for each element, this is offset by the reduced checks during sequence construction.
