## Bin hash
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
