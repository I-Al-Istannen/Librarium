import Vue from 'vue'
import VueRouter, { RouteConfig, RouterOptions } from 'vue-router'
import Home from '../views/Home.vue'
import { mdiHome } from '@mdi/js'

Vue.use(VueRouter)

export type RouteName = 'home'

type RouteInfo = Partial<{
  name: RouteName
  meta: Partial<{
    navigable: boolean
    label: string
    icon: string
  }>
}> &
  RouteConfig

const routes: RouteInfo[] = [
  {
    // Redirect / to /home
    path: '/',
    redirect: '/home',
    meta: {
      navigable: false,
      label: 'Home'
    }
  },
  {
    path: '/home',
    name: 'home',
    component: Home,
    meta: {
      label: 'Home',
      navigable: true,
      icon: mdiHome
    }
  }
]

class VueRouterEx extends VueRouter {
  matcher: any

  public routes: RouteConfig[] = []

  constructor(options: RouterOptions) {
    super(options)
    const { addRoutes } = this.matcher
    const { routes } = options

    this.routes = routes!

    this.matcher.addRoutes = (newRoutes: RouteConfig[]) => {
      this.routes.push(...newRoutes)
      addRoutes(newRoutes)
    }
  }
}

Vue.use(VueRouterEx)

const router = new VueRouterEx({
  mode: 'history',
  base: process.env.BASE_URL,
  routes: routes
})

router.afterEach(to => {
  Vue.nextTick(() => {
    document.title = to.meta.label
      ? 'Bibliothek - ' + to.meta.label
      : 'Bibliothek'
  })
})

export default router
