import App from './App.vue'
import Vue from 'vue'
import axios from 'axios'
import router from './router'
import vuetify from './plugins/vuetify'
import { extractErrorMessage } from '@/util/ErrorUtils'
import { store, vxm } from './store'

Vue.config.productionTip = false

axios.defaults.baseURL = store.state.baseUrl

axios.interceptors.request.use(config => {
  if (vxm.user.loggedIn) {
    config.auth = {
      username: vxm.user.user!,
      password: vxm.user.password!
    }
  }
  return config
})

const loadingElements: Set<number> = new Set()

// Intercept responses to show errors
axios.interceptors.response.use(
  response => {
    loadingElements.delete(response.config.randomTag!)

    const prefix = response.config.snackbarTag || ''
    vue.$globalSnackbar.finishedLoading(prefix)

    if (response.config.showSuccessSnackbar) {
      vue.$globalSnackbar.setSuccess(prefix, 'Success!')
    }

    return response
  },
  error => {
    loadingElements.delete(error.config.randomTag)

    if (!error.config.hideFromSnackbar && !error.config.hideErrorSnackbar) {
      const prefix = error.config.snackbarTag || ''
      vue.$globalSnackbar.setError(prefix, extractErrorMessage(error))
    }
    return Promise.reject(error)
  }
)

const vue = new Vue({
  router,
  store,
  vuetify,
  render: h => h(App)
})

vue.$mount('#app')
