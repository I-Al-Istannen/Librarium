import { Flavor } from '@/util/FlavourTypes'

type Location = Flavor<'location', string>

export class Book {
  readonly isbn: string
  readonly title: string
  readonly author: string[] | null
  readonly summary: string | null
  readonly pages: number | null
  readonly location: Location | null
  readonly language: string | null
  readonly crawler: string | null

  constructor(
    isbn: string,
    title: string,
    author: string[] | null,
    summary: string | null,
    pages: number | null,
    location: Location | null,
    language: string | null,
    crawler: string | null
  ) {
    this.isbn = isbn
    this.title = title
    this.author = author
    this.summary = summary
    this.pages = pages
    this.location = location
    this.language = language
    this.crawler = crawler
  }

  get chunkedIsbn() {
    let result = ''
    for (let i = 0; i < this.isbn.length; i++) {
      if (i !== 0 && i % 3 === 0) {
        result += ' '
      }
      result += this.isbn[i]
    }

    return result
  }
}
