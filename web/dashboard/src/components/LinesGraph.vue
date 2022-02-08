<script lang="ts">
// import * as Plot from "@observablehq/plot";
import { LineChart } from 'vue-chart-3';
import { toPairs, mapObjIndexed, groupBy, prop } from 'ramda';

import { Chart, registerables } from "chart.js";
Chart.register(...registerables);

export default {
  components: { LineChart },
  data() {

    const data = // {{{ 
      {
        "took": 6,
        "timed_out": false,
        "_shards": {
          "total": 5,
          "successful": 5,
          "skipped": 0,
          "failed": 0
        },
        "hits": {
          "total": 65,
          "max_score": null,
          "hits": []
        },
        "aggregations": {
          "4": {
            "buckets": [
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 245
                      },
                      "key": "Tony",
                      "doc_count": 9
                    }
                  ]
                },
                "key_as_string": "1639699200000",
                "key": 1639699200000,
                "doc_count": 9
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 160
                      },
                      "key": "Tony",
                      "doc_count": 2
                    },
                    {
                      "1": {
                        "value": 40
                      },
                      "key": "romes",
                      "doc_count": 1
                    }
                  ]
                },
                "key_as_string": "1639958400000",
                "key": 1639958400000,
                "doc_count": 3
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 50
                      },
                      "key": "Miguel Mesquita",
                      "doc_count": 1
                    }
                  ]
                },
                "key_as_string": "1640131200000",
                "key": 1640131200000,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 108
                      },
                      "key": "Tony",
                      "doc_count": 6
                    }
                  ]
                },
                "key_as_string": "1640822400000",
                "key": 1640822400000,
                "doc_count": 6
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 110
                      },
                      "key": "Bah",
                      "doc_count": 3
                    },
                    {
                      "1": {
                        "value": 100
                      },
                      "key": "romes",
                      "doc_count": 1
                    }
                  ]
                },
                "key_as_string": "1640995200000",
                "key": 1640995200000,
                "doc_count": 4
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 100
                      },
                      "key": "Bah",
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 100
                      },
                      "key": "romes",
                      "doc_count": 1
                    }
                  ]
                },
                "key_as_string": "1641081600000",
                "key": 1641081600000,
                "doc_count": 2
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 100
                      },
                      "key": "romes",
                      "doc_count": 1
                    }
                  ]
                },
                "key_as_string": "1641168000000",
                "key": 1641168000000,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 100
                      },
                      "key": "romes",
                      "doc_count": 1
                    }
                  ]
                },
                "key_as_string": "1641254400000",
                "key": 1641254400000,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 136
                      },
                      "key": "Tony",
                      "doc_count": 6
                    }
                  ]
                },
                "key_as_string": "1641340800000",
                "key": 1641340800000,
                "doc_count": 6
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 30
                      },
                      "key": "romes",
                      "doc_count": 2
                    }
                  ]
                },
                "key_as_string": "1641513600000",
                "key": 1641513600000,
                "doc_count": 2
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 120
                      },
                      "key": "romes",
                      "doc_count": 2
                    }
                  ]
                },
                "key_as_string": "1641600000000",
                "key": 1641600000000,
                "doc_count": 2
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 111
                      },
                      "key": "Tony",
                      "doc_count": 6
                    }
                  ]
                },
                "key_as_string": "1641686400000",
                "key": 1641686400000,
                "doc_count": 6
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 1
                      },
                      "key": "TESTE",
                      "doc_count": 1
                    }
                  ]
                },
                "key_as_string": "1641859200000",
                "key": 1641859200000,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 100
                      },
                      "key": "romes",
                      "doc_count": 1
                    }
                  ]
                },
                "key_as_string": "1641945600000",
                "key": 1641945600000,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 110
                      },
                      "key": "romes",
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 3
                      },
                      "key": "TESTE",
                      "doc_count": 3
                    }
                  ]
                },
                "key_as_string": "1642032000000",
                "key": 1642032000000,
                "doc_count": 4
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 50
                      },
                      "key": "romes",
                      "doc_count": 1
                    }
                  ]
                },
                "key_as_string": "1642291200000",
                "key": 1642291200000,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 14
                      },
                      "key": "TESTE",
                      "doc_count": 6
                    },
                    {
                      "1": {
                        "value": 6
                      },
                      "key": "254702881725349891",
                      "doc_count": 4
                    }
                  ]
                },
                "key_as_string": "1642377600000",
                "key": 1642377600000,
                "doc_count": 10
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 126
                      },
                      "key": "romesrf",
                      "doc_count": 5
                    }
                  ]
                },
                "key_as_string": "1642464000000",
                "key": 1642464000000,
                "doc_count": 5
              }
            ]
          }
        }
      }
    // }}}

    const datap =
      toPairs(
        groupBy(prop("name"), data.aggregations["4"]["buckets"]
        .flatMap(v =>
          v["3"]["buckets"]
          .map(u => { return {
            x: new Date(v["key"]).toISOString().split("T")[0],
            name: u["key"],
            y: u["1"]["value"],
            color: "blue"
          }}))))
      .map(([key, val]) => {
        const randColor = Math.floor(Math.random()*16777215).toString(16);
        return { label: key, data: val, backgroundColor: randColor, borderColor: randColor,  }})

    return {
      datap: datap,
      plotData: { datasets: datap },
      options: {
        parsing: {
          xAxisKey: 'x',
          yAxisKey: 'y'
        },
        responsive: true,
      }
    }
  }
}

</script>

<template>
  <LineChart :chartData=plotData :options=options />
</template>

<style scoped>
</style>
<!--
  vim: foldenable sw=2 ts=2 et
-->

