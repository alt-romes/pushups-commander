import * as Plot from "https://cdn.skypack.dev/@observablehq/plot";
// import * as L from "https://cdn.skypack.dev/@observablehq/stdlib";
let aapl = [{Date: "2013-05-13", Open: 436.605316, High: 438.383728, Low: 433.868103, Close: 435.929688, "Adj Close": 435.929688, Volume: 2915700},
    {Date: "2013-05-14", Open: 435.914795, High: 441.473633, Low: 435.735962, Close: 440.683777, "Adj Close": 440.683777, Volume: 3179000}]
let amzn = [{Date: "2013-05-13", Open: 262.769989, High: 265.880005, Low: 262, Close: 264.51001, "Adj Close": 264.51001, Volume: 2149500},
    {Date: "2013-05-14", Open: 264.5, High: 269.399994, Low: 264.029999, Close: 268.329987, "Adj Close": 268.329987, Volume: 2700000}]
let stocks = [["AAPL", aapl], ["AMZN", amzn]]
    .flatMap(([Symbol, data]) => data.map(d => ({Symbol, ...d})))

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


const datap = data.aggregations["4"]["buckets"]
    .flatMap(v =>
        v["3"]["buckets"]
        .map(u => { return {
            Date: new Date(v["key"]),
            Name: u["key"],
            Amount: u["1"]["value"]
        }}))
console.log(datap)

let plot = Plot.plot({
    style: "overflow: visible;",
    color: {
        legend: true
    },
    y: {
        grid: true
    },
    marks: [
        Plot.ruleY([0]),
        Plot.line(datap,{
            x: "Date", 
            y: "Amount",
            stroke: "Name"
        })
    ]
})
document.getElementById("lines").appendChild(plot);

