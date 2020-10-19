import Vuex from 'vuex'
import { extractVuexModule, createProxy } from 'vuex-class-component'
import Vue from 'vue'
import { UserStore } from './modules/userStore'
import VuexPersistence from 'vuex-persist'

export interface RootState {
  baseUrl: string
  userModule: UserStore
}

Vue.use(Vuex)

const persistence = new VuexPersistence<Partial<RootState>>({
  storage: window.localStorage,
  modules: ['user']
})

export const store = new Vuex.Store({
  state: {
    baseUrl: process.env.VUE_APP_BASE_URL
  } as RootState,
  modules: {
    ...extractVuexModule(UserStore)
  },
  plugins: [persistence.plugin]
})

export const vxm = {
  user: createProxy(store, UserStore)
}
