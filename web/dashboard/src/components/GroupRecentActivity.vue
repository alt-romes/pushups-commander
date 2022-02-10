<script lang="ts">
import { defineComponent } from 'vue'

export default defineComponent({
  props: {
    data: {
      type: Object as () => {
        name: String,
        img: String,
        pushups: number,
        abs: number,
        squats: number,
        kilometers: number,
        lastSeen: Date,
      }[]
    }
  },
  methods: {
    date2String: (date : Date) => date.toISOString().split("T")[0]
  }
})
</script>

<template>
  <div class="columns is-multiline is-vcentered">

    <template v-for="user in data" :key="user.name">
      <div class="column is-3">
        <div class="box">
          <div class="media">
            <div class="media-left">
              <figure class="image is-48x48">
                <img :src="user.img" alt="Profile picture">
              </figure>
            </div>
            <div class="media-content">
              <p class="title is-4">{{user.name}}</p>
              <p class="subtitle is-6">@{{user.name}}</p>
            </div>
          </div>
          <div class="content">
            {{user.name}} has done <strong>{{user.pushups}} pushups</strong>,
            <strong>{{user.abs}} abs</strong>, <strong>{{user.squats}}
              squats</strong>, and <strong>{{user.kilometers}} km</strong>.
            <br>
            <small>Last seen: <time :datetime="date2String(user.lastSeen)">{{date2String(user.lastSeen)}}</time></small>
          </div>
        </div>
      </div>
    </template>

  </div>
</template>

<style scoped>
</style>
<!--
  vim: foldenable sw=2 ts=2 et
-->
