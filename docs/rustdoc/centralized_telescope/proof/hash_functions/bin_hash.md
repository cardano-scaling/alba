<a id="bin-hash">$\mathsf{H_0}$</a> $(n_p,~ v,~ s) \rightarrow (value ~ /~ \bot):$
---
- $hash \leftarrow \mathsf{digest}(\mathsf{bytes}(v) ~ || ~ s)$
- $value \leftarrow \mathsf{sample\\_uniform}(hash,~ n_p)$
- **return** $value.$
---
