import { action, createModule } from 'vuex-class-component'
import axios from 'axios'
import { Book } from '@/store/types'
import { bookFromJson } from '@/util/JsonHelpers'
import Vue from 'vue'

const VxModule = createModule({
  namespaced: 'books',
  strict: false
})

export class BookStore extends VxModule {
  private _books: Book[] = []
  private _locations: string[] = []

  @action
  async fetchBooks(filter: {
    title?: string
    location?: string
    isbn?: string
    summary?: string
  }): Promise<void> {
    const response = await axios.get('/search', {
      params: {
        title: filter.title || undefined,
        location: filter.location || undefined,
        isbn: filter.isbn || undefined,
        summary: filter.summary || undefined
      }
    })

    this._books = response.data.map(bookFromJson)
  }

  @action
  async addBook(isbn: string) {
    const response = await axios.put(`/book/${isbn}`)

    const book = bookFromJson(response.data)

    // update or add
    const index = this._books.findIndex(it => it.isbn === book.isbn)
    if (index >= 0) {
      Vue.set(this._books, index, book)
    } else {
      this._books.splice(0, 0, book)
    }
  }

  @action
  async fetchAllLocations() {
    const response = await axios.get('/location')

    this._locations = response.data
  }

  @action
  async fetchCover(isbn: string) {
    const response = await axios.get(`/book/${isbn}/cover`, {
      responseType: 'blob'
    })
    return response.data
  }

  @action
  async setLocation(payload: { isbn: string; location: string | null }) {
    let response
    if (payload.location === null) {
      response = await axios.delete(`/book/${payload.isbn}/location`)
    } else {
      response = await axios.put(
        `/book/${payload.isbn}/location`,
        payload.location,
        {
          headers: {
            'Content-Type': 'text/plain;charset=utf-8'
          }
        }
      )
    }
    const book = bookFromJson(response.data)

    const index = this._books.findIndex(it => it.isbn === book.isbn)
    if (index >= 0) {
      Vue.set(this._books, index, book)
    }
  }

  get books() {
    return this._books
  }

  get locations() {
    return this._locations
  }
}
