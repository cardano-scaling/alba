## Parameter generation protocol
### Initialization
1. Set the proof size $u$:

$$
u \coloneqq \left\lceil \frac{\lambda_{\text{sec}} + \log \lambda_{\text{rel}} + 5 - \log \log e}{\log \left(\frac{n_p}{n_f}\right)} \right\rceil
$$

2. Set the check values $s_1$ and $s_2$:

   $$
   ratio \coloneqq \frac{9 n_p \log e}{17u^2}
   $$
   $$
   s_1 \coloneqq ratio - 7, \quad s_2 \coloneqq ratio - 2.
   $$

3. Set the values of $\lambda_{rel}^{(1)}$ and $\lambda_{rel}^{(2)}$:
    - If $s_1 < 1 \implies \lambda_{rel}^{(1)} \coloneqq \bot$, else $\implies \lambda_{rel}^{(1)} \coloneqq \mathsf{min}(\lambda_{rel}, s_1)$
    - If $s_2 < 1 \implies \lambda_{rel}^{(2)} \coloneqq \bot$, else $\implies \lambda_{rel}^{(2)} \coloneqq \mathsf{min}(\lambda_{rel}, s_2)$

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
If $s_2 > 1$, we know that $\lambda_{rel}^{(2)} \coloneqq \mathsf{min}(\lambda_{rel}, s_2)$.
In this case, if ($u > \lambda_{rel}^{(2)}$) $\implies$ MID CASE, $\quad \lambda^3 > n_p > \lambda^2$.

First, we set $\lambda_{rel}^{(1)}$, and compute $\overline{\lambda_{rel}}$ and $d$:

$$
\lambda_{rel}^{(1)} \coloneqq \mathsf{min}(\lambda_{rel}, s_1 )
$$
$$
\overline{\lambda_{rel}} \coloneqq \frac{\lambda_{rel}^{(1)} + 7}{\log{e}}, \quad d \coloneqq \lceil 16 \cdot u \cdot \overline{\lambda_{rel}}\rceil
$$
Then, we check the prover's set size.
If $n_p \geq \frac{d^2}{9 \cdot \bar{\lambda}}$, we abort the process.
Otherwise, we compute $w$, $r$, $q$, $b$ as follows:

$$
w \coloneqq \mathsf{min}\Big\\{w: w \in \mathbb{N} \wedge w \geq u \wedge \frac{14 \cdot w^2 \cdot (w + 2) \cdot e^\frac{w+1}{w}} {e \cdot (w + 2 - e^{1/w}) \cdot (w + 1)!} \le 2^{-\lambda_{rel}^{(1)}}\Big\\}
$$
For realistic values of $\lambda_{rel}^{(1)}$, $w = u$ can be used.

$$
r \coloneqq \Big\lceil\frac{\lambda_{rel}}{\lambda_{rel}^{(1)}}\Big\rceil, \quad q \coloneqq 2 \cdot \frac{\overline{\lambda_{rel}}}{d}
$$
$$
b \coloneqq \Bigg\lfloor\Big(\frac{w \overline{\lambda_{rel}}}{d} + 1\Big) \cdot \mathsf{exp}\Big(\frac{2 u w \overline{\lambda_{rel}}}{n_p} + \frac{7 u}{w}\Big)d u + d \Bigg\rfloor
$$

---
#### High case
Since $s_2 > 1$ and $\lambda_{rel}^{(2)} \coloneqq \mathsf{min}(\lambda_{rel}, s_2)$.
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
