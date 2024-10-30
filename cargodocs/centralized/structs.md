Data structures

## Structures
### Params
- $\lambda_{sec}$: Soundness security parameter.
- $\lambda_{rel}$: Completeness security parameter.
- $n_p$: Approximate size of set $S_p$ to lower bound.
- $n_f$: Target lower bound:

### Setup
- $n_p$: Approximate size of set $S_p$ to lower bound.
- $u$: Proof size (in $S_p$ elements).
- $r$: Proof max counter.
- $d$: Proof max 2nd counter.
- $q$: Probability.
- $b$: Computation bound.

### Round
- $v$: attempt_index,
- $t$: Proof 2nd counter, tuple index,
- $s_{list}$: item list
- $h$: item Hash,
- $h_{usize}$: Round candidate hash mapped to /[1, n_p/],
- $n_p$: approximate size of Sp,

### Proof
- $r$: num of repetitions
- $d$: search depth
- $items$: proof items