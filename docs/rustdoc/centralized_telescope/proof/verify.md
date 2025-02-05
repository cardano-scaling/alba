## Verify
The `verify` function checks the validity of a given proof by ensuring that it satisfies the necessary constraints. It begins by verifying that the proof’s search index \( t \) does not exceed the maximum search width \( d \), the retry count \( v \) does not exceed the maximum retries \( r \), and the number of elements in the proof sequence matches the required proof size \( u \). If any of these conditions are violated, the function immediately returns `false`.

Next, the function reconstructs the initial round using the round hash function \( \mathsf{H_1} \), which maps the retry counter \( v \) and search index \( t \) to a unique identifier \( id \). If this identifier is invalid, the function returns `false`. Otherwise, it initializes an empty round structure with the computed hash and identifier.

The function then iterates through each element in the proof sequence. For each element, it computes its corresponding bin index using the bin hash function \( \mathsf{H_0} \). If the computed bin index is invalid, the function returns `false`. If the bin index matches the current round identifier, the function updates the round by computing a new round hash using \( \mathsf{H_1} \). If the update is successful, the function continues; otherwise, it returns `false`. If an element’s bin index does not match the expected identifier, the function immediately rejects the proof.

Finally, after processing all elements in the sequence, the function applies the proof hash function \( \mathsf{H_2} \) to check whether the proof satisfies the probability constraint \( q \). If the final check is successful, the function returns `true`, confirming the validity of the proof. Otherwise, it returns `false`, indicating that the proof does not meet the required criteria.

- $\mathsf{verify}($ [$proof$](variables#proof)$, n_p,~ $ [$params$](variables#parameters) $)\rightarrow (true ~ / ~ false):$
---
- **if** $ ~~($[$proof.t$](variables#proof-t) $\geq$ [$params.d$](variables#params-d) $) ~ \Vert ~ ($ [$proof.v$](variables#proof-v) $\geq$ [$params.r$](variables#params-r) $) ~ \Vert ~ (\mathsf{size}($ [$proof.slist$](variables#proof-slist) $)~ != $ [$params.u$](variables#params-u)$):$
  - **return** $false.$
- $(hash,~ id) \leftarrow $ [$\mathsf{H_1}$](hash_functions#round-hash) $(\mathsf{bytes}($ [$proof.v$](variables#proof-v) $),~ \mathsf{bytes}($ [$proof.t$](variables#proof-t) $),~ n_p)$
- **if** $~~ id ~~== \bot:$
  - **return** $false.$
- $round \leftarrow ($ [$proof.v$](variables#proof-v) $,~ $ [$proof.t$](variables#proof-t) $,~ \\{ ~ \\},~ hash,~ id,~ n_p)$
- **for** each $~~ s \in$ [$proof.slist$](variables#proof-slist) $:$
  - $bin\\_id \leftarrow$ [$\mathsf{H_0}$](hash_functions#bin-hash) $(n_p,~ $ [$proof.v$](variables#proof-v) $,~ s )$
  - **if** $~~ bin\\_id ~== \bot :$
    - **return** $false.$
  - **if** $~~ bin\\_id ~== ~ $ [$round.id$](variables#round-id) $:$
    - $(hash,~ id) \leftarrow $ [$\mathsf{H_1}$](hash_functions#round-hash) $($ [$round.hash$](variables#round-digest) $,~ s,~ n_p)$
    - **if** $~~ id ~~!= \bot:$
      - $round.hash ~ \leftarrow ~ hash$
      - $round.id ~ \leftarrow ~ id$
    - **else** :
      - **return** $false.$
  - **else** :
    - **return** $false.$
- **return** [$\mathsf{H_2}$](hash_functions#proof-hash) $($ [$params.q$](variables#params-q) $,~ round).$ 
---
