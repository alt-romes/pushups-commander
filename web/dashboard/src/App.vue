<script setup lang="ts">
import GroupAllActivity from './components/GroupAllActivity.vue'
import GroupRecentActivity from './components/GroupRecentActivity.vue'
import Heatmap from './components/Heatmap.vue'
import LinesGraph from './components/LinesGraph.vue'
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
                    <GroupAllActivity />
                    <br>

                    <p class="is-size-6 heading">Group: Recent Activity</p>
                    <p>
                      An overview of your group's activity in the last month, sorted by recent activity.
                    </p>
                    <br>
                    <GroupRecentActivity />
                    <br>

                    <!-- <p class="is-size-6 heading"> -->
                    <!--   Personal: Daily Exercise Heatmap -->
                    <!-- </p> -->
                    <!-- <p> -->
                    <!--   You've exercised <strong>34</strong> days out of -->
                    <!--   <strong>70</strong> days ever since you started. That is -->
                    <!--   <strong>48%</strong> of all days. -->
                    <!-- </p> -->
                    <!-- <p> -->
                    <!--   Your current streak is <strong>24 days</strong> and your all time longest streak is <strong>24 days</strong>. -->
                    <!-- </p> -->
                    <!-- <br> -->
                    <!-- <br> -->
                    <!-- <Heatmap /> -->
                    <!-- <br> -->

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
                    <LinesGraph />

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
