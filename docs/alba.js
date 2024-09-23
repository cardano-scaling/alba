document.addEventListener('DOMContentLoaded', () => {

  const lam = 128;
  const HASH_FACTOR = 1e8;

  const node = document.getElementById('main_chart');
  const n_p = document.getElementById('n_p');
  const n_f = document.getElementById('n_f');
  const item = document.getElementById('item');
  const proof_size = document.getElementById('proof_size');
  const s_p = document.getElementById('s_p');
  const proof_proba = document.getElementById('proof_proba');

  function U(n_p, n_f) {
    return Math.ceil((lam + Math.log2(lam) + 5 - Math.log2(Math.log2(Math.E))) / Math.log2(n_p / n_f));
  };

  function probabilityOfProof(u, n_p, n_f, x) {
    // from https://github.com/cardano-scaling/alba/discussions/17

    S_1_bot = n_p / (17 ** 2 / (9 * Math.log2(Math.E)) * u ** 2) - 7 < 1;
    S_2_bot = n_p / (17 ** 2 / (9 * Math.log2(Math.E)) * u ** 2) - 2 < 1;

    if (!S_1_bot && !S_2_bot) {
      throw Error("S_1_bot or S_2_bot");
    }

    const d = Math.ceil(32 * Math.log(12) * u);
    const q = 2 * Math.log(12) / d;
    const r = Math.ceil(lam);

    return r * (x / n_p) ** u * d * q;
  };

  // Returns updated labels and data
  function updatedData() {
    const data = [];
    const labels = [];
    const n_p_v = Number(n_p.value);
    const n_f_v = Number(n_f.value);
    const u = U(n_p_v, n_f_v);
    // number of points
    const length = (n_p_v - n_f_v) / 10;

    for (var i = 0; i < length; i++) {
      const y = (i * 10) + n_f_v;
      labels.push(y);
      data.push(probabilityOfProof(u, n_p_v, n_f_v, y));
    }

    return [labels, data];
  }

  const [labels, data] = updatedData();

  // throughput chart
  const chart = new Chart(node, {
    title: {
      text: "Probabiliy of making a proof"
    },
    data: {
      // N.B.: Make sure colors are picked from an inclusive color palette.
      // See for instance: https://medium.com/@allieofisher/inclusive-color-palettes-for-the-web-bbfe8cf2410e
      labels,
      datasets: [
        {
          type: 'line',
          label: "proof probability",
          data,
          function: probabilityOfProof,
          backgroundColor: '#6FDE6E',
          borderColor: '#6FDE6E',
          fill: false
        }
      ]
    },
    options: {
      scales: {
        y: {
          type: 'logarithmic',
          min: 0,
          ticks: {
            stepSize: 0.0001,
            autoSkip: true,
            callback: (val) => (val.toExponential(1))
          }
        },
        x: {
          type: 'linear',
          title: {
            text: 'S_p',
            display: true
          },
        }
      }
    }
  });

  function updateProofSize() {
    const n_p_v = Number(n_p.value);
    const n_f_v = Number(n_f.value);
    const single = Number(item.value);
    const u = U(n_p_v, n_f_v);
    proof_size.value = u * single;
  }

  function updateProofProba() {
    const n_p_v = Number(n_p.value);
    const n_f_v = Number(n_f.value);
    const s_p_v = Number(s_p.value);
    const u = U(n_p_v, n_f_v);
    const proba = probabilityOfProof(u, n_p_v, n_f_v, s_p_v);
    // expected prover time is n_p + O (u^2), we neglect the n_p part here
    proof_proba.value = proba.toExponential(4);
  }

  function updateChart() {
    const [labels, data] = updatedData();
    chart.data.labels = labels;
    chart.data.datasets[0].data = data;
    chart.update();
    updateProofSize();
    updateProofProba();
  };

  n_p.addEventListener('change', updateChart);
  n_f.addEventListener('change', updateChart);
  item.addEventListener('change', updateChart);
  s_p.addEventListener('change', updateProofProba);

  updateProofSize();
  updateProofProba();
});
