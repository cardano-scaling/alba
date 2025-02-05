## Hash Functions

### Bin hash
- Used for assigning an element $s$ to a bin within the range $[0, n_p[$, based on the `retry_counter` $v$.
- Computes a hash by concatenating $s$ to the byte representation of $v$.
- Interprets the hash as a random number and reduces it modulo $n_p$ to obtain a uniform value.
- With negligible probability, may return `⊥` if the hash is insufficiently random or too short.

---
- $\mathsf{H_0}(n_p,~ v,~ s) \rightarrow (value ~ /~ \bot):$
---
- $hash \leftarrow \mathsf{digest}(\mathsf{bytes}(v) ~ || ~ s)$
- $value \leftarrow \mathsf{sample\\_uniform}(hash,~ n_p)$
- **return** $value.$
---

### Round hash
- It is used for generating a round identifier for a given search attempt.
- Computes a hash from given two inputs.
- Extracts a uniform sample from the hash to produce a round identifier within $[0, n_p[$.
- Returns the computed hash and identifier or `⊥` if sampling fails.

---
- $\mathsf{H_1}(input_1,~ input_2,~ n_p) \rightarrow (hash,~ value ~ /~ \bot):$
---
- $hash \leftarrow \mathsf{digest}(input_1 ~ || ~ input_2)$
- $value \leftarrow \mathsf{sample\\_uniform}(hash,~ n_p)$
- **return** $~~ (hash,~ value).$
---

### Proof hash
- It is used to determine whether a proof is valid based on a probabilistic check.
- Computes a hash from the round's hash value.
- Uses the hash and probability threshold $q$ to perform a Bernoulli trial.
- Returns `true` with probability in $\[q - \varepsilon, q\]$ for a negligible $\varepsilon$, otherwise returns `false`.

---
- $\mathsf{H_2}(q,~ $ [$round$](variables#round) $) \rightarrow (true ~ /~ false):$
---
- $hash \leftarrow \mathsf{digest}($ [$round.hash$](variables#round-digest) $)$
- **return** $~~ \mathsf{sample\\_bernoulli}(hash,~ q).$
---
