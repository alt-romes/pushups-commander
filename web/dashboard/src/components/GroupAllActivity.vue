<script lang="ts">
import { defineComponent } from 'vue'

export default defineComponent({
  props: {
    data: {
      type: Object as () => {
        pushups: number,
        abs: number,
        squats: number,
        kilometers: number
      },
      required: true
    }
  }  
})
</script>

<template>
  <nav class="level">
    <div class="level-item has-text-centered">
      <div>
        <p class="heading">Pushups</p>
        <p class="title">{{ data.pushups }}</p>
      </div>
    </div>
    <div class="level-item has-text-centered">
      <div>
        <p class="heading">Abs</p>
        <p class="title">{{ data.abs }}</p>
      </div>
    </div>
    <div class="level-item has-text-centered">
      <div>
        <p class="heading">Squats</p>
        <p class="title">{{ data.squats }}</p>
      </div>
    </div>
    <div class="level-item has-text-centered">
      <div>
        <p class="heading">Kilometers</p>
        <p class="title">{{ data.kilometers }}</p>
      </div>
    </div>
  </nav>
</template>

<style scoped>
</style>
<!--
  vim: foldenable sw=2 ts=2 et
-->
