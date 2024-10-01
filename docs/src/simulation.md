# ALBA Parameters Simulation

ALBA provides strong security guarantees about the capability to generate a proof given some number of items \\(S_p\\):

* a honest prover with at least \\(n_p\\) items has a probability of
  failing to generate a proof lower than \\(2^{-128}\\),
* symetrically, a (dishonest) prover with at most \\(n_f\\) items
  has probability of succeeding to generate a proof lower than
  \\(2^{-128}\\).

This page provides a simple and user-friendly way of estimating how
various ALBA parameters impact the size of the proof and probability
of having a proof, given a number of items \\(S_p\\) varying between
the two parameters' bounds. It is expected to be a quick way to
estimate what would be "good" values for those parameters in a
specific context, depending on security requirements and expected
adversarial share.

One can define:

* \\(n_p\\): The size of the _honest set_ of items.
* \\(n_f\\): The size of the _faulty set_ of items. \\(n_f\\) must be lower than \\(n_p\\).
* Item size: The size of a single item, this is used to compute the full size of a proof.
* \\(S_p\\): The number of items actually available to an adversary, or put differently, how much adversarial stake should we expect.

From these parameters, we display a chart showing the probability of
an adversary being able to compute a valid proof, given some
number of items \\(S_p\\) available between \\(n_p\\) and
\\(n_f\\). The formula used, initially proposed in this [GitHub
discussion](https://github.com/cardano-scaling/alba/discussions/17)
is:

\\[
 r \cdot {\left( \frac{S_p}{n_p} \right)}^u\cdot qd
\\]

where _u_, _r_, _q_, and _d_ are defined in the aforementioned
section of the paper. Note the vertical axis' scale is logarithmic.

We also display two specific values:

* the size of the proof for the given \\(n_p/n_f\\) ratio and individual
  item size, which is given by the formula from section 3.2.2,
  Corollary 3, from the [ALBA
  paper](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/),
* the probability of having a proof for some specific number of \\(S_p\\) (computed.


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
        <label for="proof_proba" class="form-label">Proof probability<span class="help" data-bs-toggle="tooltip" title="Probability of having a proof for some number of items.">?</span></label>
        <input type="number" class="form-control" id="proof_proba" disabled>
    </div>
    <div class="col-md-3"/>
  </div>
</div>
