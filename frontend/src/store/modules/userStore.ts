import { createModule, mutation, action } from 'vuex-class-component'
import axios from 'axios'

const VxModule = createModule({
  namespaced: 'user',
  strict: false
})

export class UserStore extends VxModule {
  public user: string | null = null
  public password: string | null = null

  /**
   * Logs the user in.
   *
   * @param payload the username and password
   * @return true if the credentials were valid
   */
  @action
  async login(payload: { user: string; password: string }): Promise<boolean> {
    const response = await axios.get('/books', {
      auth: {
        username: payload.user,
        password: payload.password
      }
    })

    if (response.status === 200) {
      this.user = payload.user
      this.password = payload.password
      return true
    }
    return false
  }

  @mutation
  logout() {
    this.user = null
    this.password = null
  }

  /**
   * Checks whether the user is logged in.
   */
  get loggedIn(): boolean {
    return this.user !== null && this.password !== null
  }
}
