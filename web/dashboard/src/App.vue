<script lang="ts">
import { defineComponent } from 'vue'
import { definitionCount, fieldSum } from '@cob/dashboard-info'

import GroupAllActivity from './components/GroupAllActivity.vue'
import GroupRecentActivity from './components/GroupRecentActivity.vue'
import Heatmap from './components/Heatmap.vue'
import LinesGraph from './components/LinesGraph.vue'

import { useCookies } from "vue3-cookies";

export default defineComponent({
  setup() {
    const { cookies } = useCookies();
    return { cookies };
  },
  components: { GroupAllActivity, GroupRecentActivity, Heatmap, LinesGraph },
  created() {
    let urlParams = new URLSearchParams(window.location.search);
    let token = urlParams.get('token');
    this.cookies.set('cobtoken', token);
  },
  data() {
    return {
      allActivity: {
        pushups: fieldSum(35, "amount", "exercise_type:pushups"),
        abs: fieldSum(35, "amount", "exercise_type:abs"),
        squats: fieldSum(35, "amount", "exercise_type:squats"),
        kilometers: fieldSum(35, "amount", "exercise_type:kilometers")
      },
      recentActivity: [
        {
          name: "romes",
          img: "https://avatars.githubusercontent.com/u/21295306?v=4",
          pushups: 4000,
          abs: 244,
          squats: 345,
          kilometers: 13,
          lastSeen: new Date()
        }
      ],
      heatmapData: { // {{{
        "took": 18,
        "timed_out": false,
        "_shards": {
          "total": 5,
          "successful": 5,
          "skipped": 0,
          "failed": 0
        },
        "hits": {
          "total": 139,
          "max_score": null,
          "hits": []
        },
        "aggregations": {
          "2": {
            "doc_count_error_upper_bound": 0,
            "sum_other_doc_count": 0,
            "buckets": [
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 120
                      },
                      "key": 6,
                      "doc_count": 2
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 5,
                      "doc_count": 2
                    },
                    {
                      "1": {
                        "value": 100
                      },
                      "key": 2,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 100
                      },
                      "key": 1,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 1,
                "doc_count": 6
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
                      "key": 7,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 110
                      },
                      "key": 4,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 100
                      },
                      "key": 3,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 2,
                "doc_count": 3
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 33
                      },
                      "key": 6,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 5,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 1,
                  "buckets": [
                    {
                      "1": {
                        "value": 44
                      },
                      "key": 7,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 8
                      },
                      "key": 6,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 42
                      },
                      "key": 5,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 40
                      },
                      "key": 4,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 36
                      },
                      "key": 3,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 6,
                "doc_count": 6
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 40
                      },
                      "key": 5,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 46
                      },
                      "key": 4,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 75
                      },
                      "key": 1,
                      "doc_count": 2
                    }
                  ]
                },
                "key": 7,
                "doc_count": 4
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
                      "key": 5,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 42
                      },
                      "key": 4,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 40
                      },
                      "key": 3,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 90
                      },
                      "key": 1,
                      "doc_count": 2
                    }
                  ]
                },
                "key": 8,
                "doc_count": 5
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 6
                      },
                      "key": 6,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 40
                      },
                      "key": 5,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 80
                      },
                      "key": 1,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 9,
                "doc_count": 3
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 40
                      },
                      "key": 4,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 6.01
                      },
                      "key": 3,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 10,
                "doc_count": 2
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 39.84
                      },
                      "key": 4,
                      "doc_count": 2
                    },
                    {
                      "1": {
                        "value": 7
                      },
                      "key": 3,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 11,
                "doc_count": 3
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 35
                      },
                      "key": 3,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 42.46
                      },
                      "key": 2,
                      "doc_count": 2
                    }
                  ]
                },
                "key": 12,
                "doc_count": 3
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 186.01
                      },
                      "key": 1,
                      "doc_count": 5
                    }
                  ]
                },
                "key": 13,
                "doc_count": 5
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 36
                      },
                      "key": 5,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 70
                      },
                      "key": 4,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 65
                      },
                      "key": 1,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 14,
                "doc_count": 3
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 150
                      },
                      "key": 1,
                      "doc_count": 3
                    }
                  ]
                },
                "key": 15,
                "doc_count": 3
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
                      "key": 2,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 182
                      },
                      "key": 1,
                      "doc_count": 3
                    }
                  ]
                },
                "key": 16,
                "doc_count": 4
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 7.26
                      },
                      "key": 2,
                      "doc_count": 2
                    }
                  ]
                },
                "key": 17,
                "doc_count": 2
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 0.9
                      },
                      "key": 3,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 60
                      },
                      "key": 2,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 18,
                "doc_count": 2
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
                      "key": 7,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 4,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 2,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 19,
                "doc_count": 3
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 3,
                  "buckets": [
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 7,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 6,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 124.51
                      },
                      "key": 5,
                      "doc_count": 5
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 4,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 150
                      },
                      "key": 2,
                      "doc_count": 2
                    }
                  ]
                },
                "key": 20,
                "doc_count": 13
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
                      "key": 7,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 60
                      },
                      "key": 6,
                      "doc_count": 2
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 5,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 60
                      },
                      "key": 1,
                      "doc_count": 2
                    }
                  ]
                },
                "key": 21,
                "doc_count": 6
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 35
                      },
                      "key": 7,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 32
                      },
                      "key": 6,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 5,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 32
                      },
                      "key": 3,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 1,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 22,
                "doc_count": 5
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 35
                      },
                      "key": 6,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 130
                      },
                      "key": 5,
                      "doc_count": 3
                    },
                    {
                      "1": {
                        "value": 35
                      },
                      "key": 2,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 23,
                "doc_count": 5
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 25
                      },
                      "key": 7,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 5.5
                      },
                      "key": 6,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 20
                      },
                      "key": 3,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 180
                      },
                      "key": 1,
                      "doc_count": 4
                    }
                  ]
                },
                "key": 24,
                "doc_count": 7
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 150
                      },
                      "key": 6,
                      "doc_count": 3
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 5,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 3,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 25,
                "doc_count": 5
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 260
                      },
                      "key": 7,
                      "doc_count": 3
                    },
                    {
                      "1": {
                        "value": 60
                      },
                      "key": 5,
                      "doc_count": 2
                    },
                    {
                      "1": {
                        "value": 50
                      },
                      "key": 3,
                      "doc_count": 2
                    }
                  ]
                },
                "key": 26,
                "doc_count": 7
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 31
                      },
                      "key": 6,
                      "doc_count": 2
                    },
                    {
                      "1": {
                        "value": 240
                      },
                      "key": 3,
                      "doc_count": 3
                    },
                    {
                      "1": {
                        "value": 50
                      },
                      "key": 2,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 80
                      },
                      "key": 1,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 27,
                "doc_count": 7
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
                      "key": 4,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 300
                      },
                      "key": 3,
                      "doc_count": 5
                    }
                  ]
                },
                "key": 28,
                "doc_count": 6
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 35
                      },
                      "key": 4,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 32
                      },
                      "key": 1,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 29,
                "doc_count": 2
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 90
                      },
                      "key": 5,
                      "doc_count": 2
                    }
                  ]
                },
                "key": 31,
                "doc_count": 2
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 35
                      },
                      "key": 7,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 30
                      },
                      "key": 5,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 37,
                "doc_count": 2
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 240
                      },
                      "key": 3,
                      "doc_count": 3
                    }
                  ]
                },
                "key": 38,
                "doc_count": 3
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 60
                      },
                      "key": 6,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 40,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 55
                      },
                      "key": 2,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 41,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 60
                      },
                      "key": 4,
                      "doc_count": 2
                    }
                  ]
                },
                "key": 44,
                "doc_count": 2
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 20
                      },
                      "key": 3,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 46,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 25
                      },
                      "key": 7,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 10
                      },
                      "key": 1,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 47,
                "doc_count": 2
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 25
                      },
                      "key": 4,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 48,
                "doc_count": 1
              },
              {
                "3": {
                  "doc_count_error_upper_bound": 0,
                  "sum_other_doc_count": 0,
                  "buckets": [
                    {
                      "1": {
                        "value": 40
                      },
                      "key": 1,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 51,
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
                      "key": 7,
                      "doc_count": 1
                    },
                    {
                      "1": {
                        "value": 100
                      },
                      "key": 6,
                      "doc_count": 1
                    }
                  ]
                },
                "key": 52,
                "doc_count": 2
              }
            ]
          }
        }
      }, // }}}
      linesData: { // {{{ 
        took: 6,
          timed_out: false,
          _shards: {
            total: 5,
              successful: 5,
              skipped: 0,
              failed: 0,
          },
          hits: {
            total: 65,
              max_score: null,
              hits: [],
          },
          aggregations: {
            "4": {
              buckets: [
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 245,
                        },
                        key: "Tony",
                        doc_count: 9,
                      },
                    ],
                  },
                  key_as_string: "1639699200000",
                  key: 1639699200000,
                  doc_count: 9,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 160,
                        },
                        key: "Tony",
                        doc_count: 2,
                      },
                      {
                        "1": {
                          value: 40,
                        },
                        key: "romes",
                        doc_count: 1,
                      },
                    ],
                  },
                  key_as_string: "1639958400000",
                  key: 1639958400000,
                  doc_count: 3,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 50,
                        },
                        key: "Miguel Mesquita",
                        doc_count: 1,
                      },
                    ],
                  },
                  key_as_string: "1640131200000",
                  key: 1640131200000,
                  doc_count: 1,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 108,
                        },
                        key: "Tony",
                        doc_count: 6,
                      },
                    ],
                  },
                  key_as_string: "1640822400000",
                  key: 1640822400000,
                  doc_count: 6,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 110,
                        },
                        key: "Bah",
                        doc_count: 3,
                      },
                      {
                        "1": {
                          value: 100,
                        },
                        key: "romes",
                        doc_count: 1,
                      },
                    ],
                  },
                  key_as_string: "1640995200000",
                  key: 1640995200000,
                  doc_count: 4,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 100,
                        },
                        key: "Bah",
                        doc_count: 1,
                      },
                      {
                        "1": {
                          value: 100,
                        },
                        key: "romes",
                        doc_count: 1,
                      },
                    ],
                  },
                  key_as_string: "1641081600000",
                  key: 1641081600000,
                  doc_count: 2,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 100,
                        },
                        key: "romes",
                        doc_count: 1,
                      },
                    ],
                  },
                  key_as_string: "1641168000000",
                  key: 1641168000000,
                  doc_count: 1,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 100,
                        },
                        key: "romes",
                        doc_count: 1,
                      },
                    ],
                  },
                  key_as_string: "1641254400000",
                  key: 1641254400000,
                  doc_count: 1,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 136,
                        },
                        key: "Tony",
                        doc_count: 6,
                      },
                    ],
                  },
                  key_as_string: "1641340800000",
                  key: 1641340800000,
                  doc_count: 6,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 30,
                        },
                        key: "romes",
                        doc_count: 2,
                      },
                    ],
                  },
                  key_as_string: "1641513600000",
                  key: 1641513600000,
                  doc_count: 2,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 120,
                        },
                        key: "romes",
                        doc_count: 2,
                      },
                    ],
                  },
                  key_as_string: "1641600000000",
                  key: 1641600000000,
                  doc_count: 2,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 111,
                        },
                        key: "Tony",
                        doc_count: 6,
                      },
                    ],
                  },
                  key_as_string: "1641686400000",
                  key: 1641686400000,
                  doc_count: 6,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 1,
                        },
                        key: "TESTE",
                        doc_count: 1,
                      },
                    ],
                  },
                  key_as_string: "1641859200000",
                  key: 1641859200000,
                  doc_count: 1,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 100,
                        },
                        key: "romes",
                        doc_count: 1,
                      },
                    ],
                  },
                  key_as_string: "1641945600000",
                  key: 1641945600000,
                  doc_count: 1,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 110,
                        },
                        key: "romes",
                        doc_count: 1,
                      },
                      {
                        "1": {
                          value: 3,
                        },
                        key: "TESTE",
                        doc_count: 3,
                      },
                    ],
                  },
                  key_as_string: "1642032000000",
                  key: 1642032000000,
                  doc_count: 4,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 50,
                        },
                        key: "romes",
                        doc_count: 1,
                      },
                    ],
                  },
                  key_as_string: "1642291200000",
                  key: 1642291200000,
                  doc_count: 1,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 14,
                        },
                        key: "TESTE",
                        doc_count: 6,
                      },
                      {
                        "1": {
                          value: 6,
                        },
                        key: "254702881725349891",
                        doc_count: 4,
                      },
                    ],
                  },
                  key_as_string: "1642377600000",
                  key: 1642377600000,
                  doc_count: 10,
                },
                {
                  "3": {
                    doc_count_error_upper_bound: 0,
                    sum_other_doc_count: 0,
                    buckets: [
                      {
                        "1": {
                          value: 126,
                        },
                        key: "romesrf",
                        doc_count: 5,
                      },
                    ],
                  },
                  key_as_string: "1642464000000",
                  key: 1642464000000,
                  doc_count: 5,
                },
              ],
            },
          },
      },
      // }}}
    }
  }
})

</script>

<template>

  <section class="section m-0 p-0">
    <!-- Navbar {{{ -->
    <div class="container">
      <div class="columns">
        <div class="column is-10 is-offset-1">
          <nav class="navbar my-5" role="navigation" aria-label="main navigation">
            <div class="navbar-brand">
              <a class="navbar-item" href="index.html">
                <p class="block is-size-5 has-text-weight-bold">
                  Workout Bot
                </p>
                <!-- <img src="https://bulma.io/images/bulma-logo.png" width="112" height="28"> -->
              </a>

              <!-- <a role="button" class="navbar-burger" aria-label="menu" aria-expanded="false" data-target="??"> -->
              <!--     <span aria-hidden="true"></span> -->
              <!--     <span aria-hidden="true"></span> -->
              <!--     <span aria-hidden="true"></span> -->
              <!-- </a> -->
            </div>

            <div class="navbar-menu">
              <div class="navbar-start">

              </div>

              <div class="navbar-end">
                <div class="navbar-item">
                  <div class="buttons">
                    <a class="button is-dark has-text-weight-bold" href="dashboard.html">
                      Dashboard
                    </a>
                    <a class="button is-light has-text-weight-bold" href="documentation.html">
                      Documentation
                    </a>
                  </div>
                </div>
              </div>
            </div>
          </nav>
        </div>
      </div>
    </div>
    <!-- }}} -->
  </section>

  <section class="section m-0 p-0">
    <div class="container">
      <div class="columns">
        <div class="column is-10 is-offset-1">
          <div class="hero is-medium">
            <div class="hero-body px-5 py-6">
              <div class="columns is-vcentered">
                <div class="column is-12">
                  <p class="is-size-6 heading">Group: All Activity</p>
                  <p>
                    A display of your group's strength! The total amount of exercise done in each category by all members summed together, all time.
                  </p>
                  <br>
                  <GroupAllActivity :data="allActivity"/>
                  <br>

                  <p class="is-size-6 heading">Group: Recent Activity</p>
                  <p>
                    An overview of your group's activity in the last month, sorted by recent activity.
                  </p>
                  <br>
                  <GroupRecentActivity :data="recentActivity" />
                  <br>

                  <p class="is-size-6 heading">
                    Personal: Daily Exercise Heatmap
                  </p>
                  <p>
                    You've exercised <strong>34</strong> days out of
                    <strong>70</strong> days ever since you started. That is
                    <strong>48%</strong> of all days.
                  </p>
                  <p>
                    Your current streak is <strong>24 days</strong> and your all time longest streak is <strong>24 days</strong>.
                  </p>
                  <br>
                  <br>
                  <Heatmap :data="heatmapData" />
                  <br>

                  <!-- Upgrade cal-heatmap to recent version -->
                  <p class="is-size-6 heading">Group: Leaderboard</p>
                  <p>
                    First is <strong>romes</strong> with <strong>330 pushups</strong> done, and second is <strong>tony</strong> with <strong>200 pushups</strong>.
                  </p>
                  <p>
                    You're currently in <strong>1st</strong> place with <strong>330 pushups</strong> done.
                  </p>
                  <p>
                    You're <strong>0 pushups</strong> away from <strong>1st</strong> place.
                  </p>
                  <br>
                  <br>
                  <LinesGraph :data="linesData" />

                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </section>

</template>

<style>
@import './assets/base.css';
</style>
<!--
  vim: foldenable sw=2 ts=2 et
-->
