# ALBA Parameters Simulation

ALBA provides strong security guarantees about the capability to generate a proof given some number of items \\(S_p\\):

* a honest prover with more than \\(n_p\\) items has a probability of failing to generate a proof lower than \\(2^{-128}\\),
* symetrically, a (dishonest) prover with less than \\(n_f\\) items has probability of succeeding to generate a proof lower than  \\(2^{-128}\\).

This page provides a simple and user-friendly of estimating how
various ALBA parameters impact the size of the proof and the computing
power needed for an adversary to compute a proof, given a number of items \\(S_p\\) varying between the two parameters' bounds.
It is expected to be a quick way to estimate what would be "good" values for those parameters in a specific context, depending on security requirements and expected adversarial power.

One can define:

* \\(n_p\\): The size of the _honest set_ of items.
* \\(n_f\\): The size of the _faulty set_ of items. \\(n_f\\) must be lower than \\(n_p\\).
* Item size: The size of a single item, this is used to compute the full size of a proof.
* \\(S_p\\): The number of items actually available to an adversary, or put differently, how much adversarial stake should we expect.
* CPU power: The computing power available to an adversary, given as a number of "stock" CPU.

From these parameters, we compute:

* the size of the proof for the given \\(n_p/n_f\\) ratio and individual
  item size, which is given by the formula from section 3.2.2,
  Corollary 3, from the [ALBA
  paper](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/),
* the (worst case) time needed to find a proof through exhaustive
  search, for some specific number of \\(S_p\\) and CPU power. This time
  is given in seconds, assuming a single CPU can compute a single
  _hash_ in \\(10^{-8}\\) seconds, and single proof computation time as
  given in the same corrollary.

We also display a chart showing the probability of an adversary being
able to compute a single valid proof, given some number of items \\(S_p\\)
available between \\(n_p\\) and \\(n_f\\). The formula used, initially proposed in this [GitHub discussion](https://github.com/cardano-scaling/alba/discussions/17) is:

\\[
 r \cdot {\left( \frac{|S_p|}{n_p} \right)}^u\cdot qd
\\]

where _u_, _r_, _q_, and _d_ are defined in the aforementioned
section of the paper. Note the vertical axis' scale is logarithmic.


<div class="col col-md-8">
  <div class="row">
    <div id="chart" class="row" style="width: 100%; margin: auto;" >
      <canvas id="main_chart"></canvas>
    </div>
  </div>
  <div class="row g-3" style="margin: auto;">
    <div class="col col-md-3">
        <label for="n_p" class="form-label">\(n_p\)<span class="help" data-bs-toggle="tooltip" title="Expected number of honest set of items.">?</span></label>
        <input type="number" step="10" class="form-control" id="n_p" value="800">
    </div>
    <div class="col col-md-3">
        <label for="n_f" class="form-label">\(n_f\)<span class="help" data-bs-toggle="tooltip" title="Expected number of faulty set of items.">?</span></label>
        <input type="number" step="10" class="form-control" id="n_f" value="200">
    </div>
    <div class="col col-md-3">
        <label for="item" class="form-label">Item size<span class="help" data-bs-toggle="tooltip" title="Size of a single item.">?</span></label>
        <input type="number" step="10" class="form-control" id="item" value="600">
    </div>
    <div class="col col-md-3">
        <label for="proof_size" class="form-label">Proof size<span class="help" data-bs-toggle="tooltip" title="Total size of proof.">?</span></label>
        <input type="number" class="form-control" id="proof_size" disabled>
    </div>
  </div>
  <div class="row g-3" style="width: 95%; margin: auto;">
    <div class="col col-md-3">
        <label for="s_p" class="form-label">\(S_p\)<span class="help" data-bs-toggle="tooltip" title="Number of items actually available.">?</span></label>
        <input type="number" step="10" class="form-control" id="s_p" value="300">
    </div>
    <div class="col col-md-3">
        <label for="cpu" class="form-label">CPU power<span class="help" data-bs-toggle="tooltip" title="CPU power available.">?</span></label>
        <input type="number" step="10" class="form-control" id="cpu" value="100">
    </div>
    <div class="col col-md-3">
        <label for="proof_time" class="form-label">Proof time (s)<span class="help" data-bs-toggle="tooltip" title="Number of items actually available.">?</span></label>
        <input type="number" class="form-control" id="proof_time" disabled>
    </div>
    <div class="col-md-3"/>
  </div>
</div>
