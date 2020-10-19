import { action, createModule } from 'vuex-class-component'
import axios from 'axios'
import { Book } from '@/store/types'
import { bookFromJson } from '@/util/JsonHelpers'

const VxModule = createModule({
  namespaced: 'books',
  strict: false
})

export class BookStore extends VxModule {
  private _books: Book[] = []

  @action
  async fetchBooks(filter: {
    title?: string
    location?: string
    isbn?: string
    summary?: string
  }): Promise<void> {
    const response = await axios.get('/books')

    this._books = response.data.map(bookFromJson)
  }

  @action
  async fetchCover(isbn: string) {
    const response = await axios.get(`/book/${isbn}/cover`, {
      responseType: 'blob'
    })
    return response.data
  }

  get books() {
    return this._books
  }
}
