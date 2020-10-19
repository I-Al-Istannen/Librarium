<template>
  <v-app>
    <v-main>
      <nav-bar></nav-bar>
      <snackbar ref="global-snackbar"></snackbar>
      <router-view />
      <theme-selector @use-dark-theme="setDarkTheme"></theme-selector>
    </v-main>
  </v-app>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import NavigationBar from './components/NavigationBar.vue'
import Snackbar from './components/Snackbar.vue'
import { vxm } from './store'
import { Watch } from 'vue-property-decorator'
import ThemeSelector from './components/ThemeSelector.vue'

@Component({
  components: {
    'nav-bar': NavigationBar,
    snackbar: Snackbar,
    'theme-selector': ThemeSelector
  }
})
export default class App extends Vue {
  private setDarkTheme(darkTheme: boolean) {
    vxm.user.darkThemeSelected = darkTheme
  }

  @Watch('isDarkTheme')
  private onDarkThemeChanged() {
    this.$vuetify.theme.dark = this.isDarkTheme
  }

  private get isDarkTheme() {
    return vxm.user.darkThemeSelected
  }

  created(): void {
    this.$vuetify.theme.dark = this.isDarkTheme
  }

  mounted(): void {
    if (vxm.user.usesBrowsersThemePreferences && this.isDarkTheme) {
      this.$globalSnackbar.setSuccess(
        'theme',
        'Selected dark mode based on your browser preferences.'
      )
    }
  }
}
</script>

<style>
/*noinspection CssUnusedSymbol*/
.v-toolbar > .v-toolbar__content {
  height: auto !important;
  min-height: 64px;
}
/*noinspection CssUnusedSymbol*/
.v-toolbar {
  height: auto !important;
  min-height: 64px;
}
</style>
