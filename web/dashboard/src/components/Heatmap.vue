<script lang="ts">
import { defineComponent } from 'vue'

export default defineComponent({
  props: ["data"],
  data() {
    return {
      smallHeatMap:null,
      mediumHeatMap:null,
      largeHeatMap:null
    }
  },
  computed: {
    datap() {
        const startTimestamp = (new Date(new Date().getFullYear()-1, 1, 1)).getTime()/1000
        const mkTime = (weekOfYear : number, dayOfWeek : number) => ((weekOfYear - 1) * 7 + dayOfWeek) * 3600*24 + startTimestamp
        const datap =
        Object.fromEntries(this.data.aggregations["2"]["buckets"].flatMap((v : {key: number, "3": {buckets: {key: number, "1": {value: number}}[]}}) =>
          v["3"]["buckets"].map((u : {key: number, "1": {value: number}}) => [mkTime(v["key"], u["key"]), u["1"]["value"]])))

      return datap
    }
  },
  mounted() {
        const getDateMonthsAgo = (m : number) => { let d = new Date(); d.setMonth(d.getMonth() - m); return d };

        const calConfig = {
          domain: "month",
          subDomain: "day",
          weekStartOnMonday: false,
          cellSize: 12,
          cellRadius: 4,
          domainMargin: [1, 1, 1, 1],
          domainGutter: 10,
          domainLabelFormat: "%b '%y",
          legendCellSize: 5,
          legendCellPadding: 3,
          legendHorizontalPosition: "right",
          highlight: "now",
          tooltip: true,
          data: this.datap,
          /* Properties to overwrite */
          start: new Date(),
          range: 12
        }

        this.smallHeatMap = new CalHeatMap()
        this.smallHeatMap.init({
          ...calConfig,
          range: 12,
          start: getDateMonthsAgo(11)
        });

        this.mediumHeatMap = new CalHeatMap()
        this.mediumHeatMap.init({
          ...calConfig,
          itemSelector: "#cal-heatmap-medium",
          range: 8,
          start: getDateMonthsAgo(7)
        });

        this.largeHeatMap = new CalHeatMap()
        this.largeHeatMap.init({
          ...calConfig,
          itemSelector: "#cal-heatmap-small",
          range: 6,
          start: getDateMonthsAgo(5)
        });
        // TODO: Range depends on window size (small = 6, big = 12) instead of hiding and showing divs
  },
  watch: {
    data: {
      deep: true,
      handler() {
        this.smallHeatMap.update(this.datap)
        this.mediumHeatMap.update(this.datap)
        this.largeHeatMap.update(this.datap)
      }
    }
  }
})
</script>

<template>
  <div>
    <div id="scripts"></div>
    <div class="cal-heatmap is-hidden-touch is-hidden-desktop-only is-hidden-widescreen-only" id="cal-heatmap"></div>
    <div class="cal-heatmap is-hidden-touch is-hidden-fullhd" id="cal-heatmap-medium"></div>
    <div class="cal-heatmap is-hidden-desktop" id="cal-heatmap-small"></div>
  </div>
</template>

<style scoped>
</style>
<!--
  vim: foldenable sw=2 ts=2 et
-->

