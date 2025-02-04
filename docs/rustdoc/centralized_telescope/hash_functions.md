## Hash Functions

### Bin hash
- $\mathsf{H_0}(n_p,~ v,~ s) \rightarrow (value ~ /~ \bot):$
---
- $hash \leftarrow \mathsf{digest}(\mathsf{bytes}(v) ~ || ~ s)$
- $value \leftarrow \mathsf{sample\\_uniform}(hash,~ n_p)$
- **return** $value.$
---

### Round hash
- $\mathsf{H_1}(input_1,~ input_2,~ n_p) \rightarrow (hash,~ value ~ /~ \bot):$
---
- $hash \leftarrow \mathsf{digest}(input_1 ~ || ~ input_2)$
- $value \leftarrow \mathsf{sample\\_uniform}(hash,~ n_p)$
- **return** $~~ (hash,~ value).$
---

### Proof hash
- $\mathsf{H_2}(q,~ $ [$round$](variables#round) $) \rightarrow (true ~ /~ false):$
---
- $hash \leftarrow \mathsf{digest}($ [$round.hash$](variables#round-digest) $)$
- **return** $~~ \mathsf{sample\\_bernoulli}(hash,~ q).$
---
