import { Book } from '@/store/types'

export function bookFromJson(json: any): Book {
  return new Book(
    json._isbn,
    json._title,
    json._author || null,
    json._summary || null,
    json._pages || null,
    json._location || null,
    json._language || null,
    json._crawledBy || null
  )
}
