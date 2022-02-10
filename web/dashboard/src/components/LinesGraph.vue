<script lang="ts">
import { LineChart } from "vue-chart-3";
import { toPairs, groupBy, prop } from "ramda";

import { Chart, registerables } from "chart.js";
Chart.register(...registerables);

export default {
  props: ["data"],
  components: { LineChart },
  computed: {
    plotData() {
      const datap = toPairs(
        groupBy(prop("name"), this.data.aggregations["4"]["buckets"]
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

      return { datasets: datap }
    }
  },
}
</script>

<template>
  <LineChart :chartData="plotData" />
</template>

<style scoped>
</style>
<!--
  vim: foldenable sw=2 ts=2 et
-->

