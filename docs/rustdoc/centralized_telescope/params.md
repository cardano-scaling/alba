# Parameter Setup

## Overview

## Parameter generation protocol
Recall the telescope parameters:
- $n_p: ~~$ Total number of elements available to the prover.
- $n_f: ~~$ The lower bound to prove on prover set.
- $\lambda_{sec}: ~~$ The probability that an adversary, who knows at most $n_f$ elements, can construct a valid proof. 
- $\lambda_{rel}: ~~$ The probability that an honest prover, with knowledge of a sufficiently large set $S_p$, can successfully generate a valid proof. 

### Parameters
- $u: ~~$
- $d: ~~$
- $q: ~~$
- $r: ~~$
- $b: ~~$

### Initialization
1. Set the proof size $u$:
$$
u \coloneqq \left\lceil \frac{\lambda_{\text{sec}} + \log \lambda_{\text{rel}} + 5 - \log \log e}{\log \left(\frac{n_p}{n_f}\right)} \right\rceil
$$
$$

2. Set the check values $s_1$ and $s_2$:
$$
ratio \coloneqq \frac{9 n_p \log e}{17u^2}
$$

$$
s_1 \coloneqq ratio - 7, \quad s_2 \coloneqq ratio - 2.
$$
3. Set the values of $\lambda_{rel}^{(1)}$ and $\lambda_{rel}^{(2)}$:

If $s_1 < 1 \implies \lambda_{rel}^{(1)} \coloneqq \bot$, else $\implies \lambda_{rel}^{(1)} \coloneqq \mathsf{min}(\lambda_{rel}, s_1)$

If $s_2 < 1 \implies \lambda_{rel}^{(2)} \coloneqq \bot$, else $\implies \lambda_{rel}^{(2)} \coloneqq \mathsf{min}(\lambda_{rel}, s_2)$

### Cases
#### Small case
If ($s_1 < 1$ or $s_2 < 1$) $\implies$ SMALL CASE, $\quad n_p \leq \lambda^2$.

$$
r \coloneqq \lambda_{rel}, \quad d \coloneqq \lceil 32\cdot \ln(12)\cdot u \rceil,
$$
$$
q \coloneqq \frac{2 \cdot \ln(12)}{d}, \quad b \coloneqq \Big\lfloor \frac{8 \cdot (u + 1) \cdot d}{\ln(12)} \Big\rfloor.
$$

---
#### Mid case
If $s_2 > 1$, we know that $\lambda_{rel}^{(2)} \coloneqq min(\lambda_{rel}, s_2)$. 
In this case, if ($u > \lambda_{rel}^{(2)}$) $\implies$ MID CASE, $\quad \lambda^3 > n_p > \lambda^2$.

First, we set $\lambda_{rel}^{(1)}$, and compute $\bar{\lambda}$ and $d$:
$$
\lambda_{rel}^{(1)} \coloneqq min(\lambda_{rel}, s_1 )
$$
$$
\bar{\lambda} \coloneqq \frac{\lambda_{rel}^{(1)} + 7}{\log{e}}, \quad d \coloneqq \lceil 16 \cdot u \cdot \bar{\lambda}\rceil
$$
Then, we check the prover's set size. 
If $n_p \geq \frac{d^2}{9 \cdot \bar{\lambda}}$, we abort the process.
Otherwise, we compute $w$, $r$, $q$, $b$ as follows:
$$
w \coloneqq \mathsf{compute_w}(u, \lambda_{rel}^{(1)}), \quad r \coloneqq \Big\lceil\frac{\lambda_{rel}}{\lambda_{rel}^{(1)}}\Big\rceil, \quad q \coloneqq \mathsf{recip}\Big(2 \cdot \frac{\bar{\lambda}}{d}\Big)
$$
$$
b \coloneqq \Bigg\lfloor\Big(\frac{w \bar{\lambda}}{d} + 1\Big) \cdot \mathsf{exp}\Big(\frac{2 u w \bar{\lambda}}{n_p} + \frac{7 u}{w}\Big)d u + d \Bigg\rfloor
$$

---
#### High case
Since $s_2 > 1$ and $\lambda_{rel}^{(2)} \coloneqq min(\lambda_{rel}, s_2)$.
If ($u > \lambda_{rel}^{(2)}$) $\implies$ HIGH CASE, $\quad n_p \geq \lambda^3$.

First, we compute $d$:
$$
  d \coloneqq \Bigg\lceil \frac{16 \cdot u \cdot (\lambda_{rel}^{(2)} + 2)}{\log{e}} \Bigg\rceil.
$$
Then, we check the prover's set size.
If $n_p \geq \frac{d^2 \cdot \log{e}}{9 \cdot (\lambda_{rel}^{(2)} + 2)}$, we abort the process.
Otherwise, we compute $r$, $q$, $b$ as follows:
$$
r \coloneqq \Big\lceil \frac{\lambda_{rel}}{\lambda_{rel}^{(2)}} \Big\rceil, \quad q \coloneqq \frac{2 \cdot (\lambda_{rel}^{(2)} + 2)}{d \cdot \log{e}}
$$
$$
b \coloneqq \Bigg\lfloor\frac{\lambda_{rel}^{(2)} + 2 + \log{u}} {\lambda_{rel}^{(2)} + 2}\cdot \Big(3 \cdot u \cdot \frac{d}{4}\Big) + d + u\Bigg\rfloor
$$
