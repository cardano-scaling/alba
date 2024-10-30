Setting up the protocol parameters

### Setup new protocol
**Input:** Params.

**Output:** Setup.

- Compute $u$, $s_1$, $s_2$:
  $$
  u \leftarrow \left\lceil \frac{\lambda_{\text{sec}} + \log \lambda_{\text{rel}} + 5 - \log \log e}{\log \left(\frac{n_p}{n_f}\right)} \right\rceil
  $$
  $$
  ratio \leftarrow \frac{9 n_p \log e}{17u^2}
  $$
  $$
  s_1 \leftarrow ratio - 7, \quad s_2 \leftarrow ratio - 2.
  $$

- If ($s_1 < 1$ or $s_2 < 1$) $\implies$ SMALL CASE, $\quad n_p \leq \lambda^2$
  $$
  r \leftarrow \lambda_{rel}, \quad d \leftarrow \lceil 32\cdot \ln(12)\cdot u \rceil,
  $$
  $$
  q \leftarrow \frac{2 \cdot \ln(12)}{d}, \quad b \leftarrow \Big\lfloor \frac{8 \cdot (u + 1) \cdot d}{\ln(12)} \Big\rfloor.
  $$

- Set $\lambda_{rel}^{(2)}$:
  $$
  \lambda_{rel}^{(2)} \leftarrow min(\lambda_{rel}, s_2 )
  $$

- If ($u < \lambda_{rel}^{(2)}$) $\implies$ HIGH CASE, $\quad n_p \geq \lambda^3$
    - Compute $d$:
      $$
      d \leftarrow \Bigg\lceil \frac{16 \cdot u \cdot (\lambda_{rel}^{(2)} + 2)}{\log{e}} \Bigg\rceil
      $$
    - If $n_p \geq \frac{d^2 \cdot \log{e}}{9 \cdot (\lambda_{rel}^{(2)} + 2)}$ $\implies$ QUIT.
    - Compute $r$, $q$, $b$:
      $$
      r \leftarrow \Big\lceil \frac{\lambda_{rel}}{\lambda_{rel}^{(2)}} \Big\rceil, \quad q \leftarrow \frac{2 \cdot (\lambda_{rel}^{(2)} + 2)}{d \cdot \log{e}}
      $$
      $$
      b \leftarrow \Bigg\lfloor\frac{\lambda_{rel}^{(2)} + 2 + \log{u}} {\lambda_{rel}^{(2)} + 2}\cdot \Big(3 \cdot u \cdot \frac{d}{4}\Big) + d + u\Bigg\rfloor
      $$

- Else $\implies$ MID CASE, $\quad \lambda^3 > n_p > \lambda^2$
    - Set $\lambda_{rel}^{(1)}$, compute $\bar{\lambda}$ and $d$:
      $$
      \lambda_{rel}^{(1)} \leftarrow min(\lambda_{rel}, s_1 )
      $$
      $$
      \bar{\lambda} \leftarrow \frac{\lambda_{rel}^{(1)} + 7}{\log{e}}, \quad d \leftarrow \lceil 16 \cdot u \cdot \bar{\lambda}\rceil
      $$
    - If $n_p \geq \frac{d^2}{9 \cdot \bar{\lambda}}$ $\implies$ QUIT.
    - Compute $w$, $r$, $q$, $b$:
      $$
      w \leftarrow \mathsf{compute_w}(u, \lambda_{rel}^{(1)}), \quad r \leftarrow \Big\lceil\frac{\lambda_{rel}}{\lambda_{rel}^{(1)}}\Big\rceil, \quad q \leftarrow \mathsf{recip}\Big(2 \cdot \frac{\bar{\lambda}}{d}\Big)
      $$
      $$
      b \leftarrow \Bigg\lfloor\Big(\frac{w \bar{\lambda}}{d} + 1\Big) \cdot \mathsf{exp}\Big(\frac{2 u w \bar{\lambda}}{n_p} + \frac{7 u}{w}\Big)d u + d \Bigg\rfloor
      $$
