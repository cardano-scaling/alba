document.addEventListener('DOMContentLoaded', () => {


  function sliceOf(slot) {
    return Math.floor(slot / parameters._L);
  }

  const node = document.getElementById('main_chart');

  function proabilityOfProof(S_p) {
    const lam = 128;
    const n_p = Number(document.getElementById('n_p').value);
    const n_f = Number(document.getElementById('n_f').value);

    // from https://github.com/cardano-scaling/alba/discussions/17

    const u = Math.ceil((lam + Math.log2(lam) + 5 - Math.log2(Math.log2(Math.E))) / Math.log2(n_p / n_f));

    S_1_bot = n_p / (17 ** 2 / (9 * Math.log2(Math.E)) * u ** 2) - 7 < 1;
    S_2_bot = n_p / (17 ** 2 / (9 * Math.log2(Math.E)) * u ** 2) - 2 < 1;

    if (!S_1_bot && !S_2_bot) {
      throw Error("S_1_bot or S_2_bot");
    }

    const d = Math.ceil(32 * Math.log(12) * u);
    const q = 2 * Math.log(12) / d;
    const r = Math.ceil(lam);

    console.log(`u = ${u}, d = ${d}, q = ${q}, r = ${r}`);

    return r * (S_p / n_p) ** u * d * q;
  };

  Chart.register({
    id: 'function-sampler',
    beforeInit: function(chart, args, options) {
      // We get the chart data
      var data = chart.config.data;

      // For every dataset ...
      for (var i = 0; i < data.datasets.length; i++) {

        // For every label ...
        for (var j = 0; j < data.labels.length; j++) {

          // We get the dataset's function and calculate the value
          var fct = data.datasets[i].function,
            x = data.labels[j],
            y = fct(x);
          // Then we add the value to the dataset data
          data.datasets[i].data.push(y);
        }
      }
    }
  });

  // throughput chart
  const chart = new Chart(node, {
    title: {
      text: "Probabiliy of making a proof"
    },
    data: {
      // N.B.: Make sure colors are picked from an inclusive color palette.
      // See for instance: https://medium.com/@allieofisher/inclusive-color-palettes-for-the-web-bbfe8cf2410e
      labels: Array.from({ length: 60 }, (_, i) => (i * 10) + 200),
      datasets: [
        {
          type: 'line',
          label: "proof probability",
          data: [],
          function: proabilityOfProof,
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
          ticks: {
            callback: (val) => (val.toExponential())
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
});
