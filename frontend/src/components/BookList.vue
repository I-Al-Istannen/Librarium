<template>
  <v-data-iterator :items="books" item-key="isbn">
    <template #default="{ items }">
      <v-row>
        <v-col v-for="item in items" :key="item.isbn">
          <v-card class="ma-5" outlined>
            <v-card-title>{{ item.title }}</v-card-title>
            <v-card-subtitle>
              {{ item.chunkedIsbn }} - {{ item.language }}
            </v-card-subtitle>
            <v-card-text>
              <v-row no-gutters align="center">
                <v-col cols="auto">
                  <v-avatar size="125">
                    <v-tooltip right>
                      <template #activator="{ on }">
                        <v-img
                          v-on="on"
                          :src="bookUrls[item.isbn]"
                          contain
                        ></v-img>
                      </template>
                      <img alt="Cover image" :src="bookUrls[item.isbn]" />
                    </v-tooltip>
                  </v-avatar>
                </v-col>
                <v-col>
                  {{ item.summary }}
                </v-col>
              </v-row>

              <v-row no-gutters justify="space-around">
                <v-col
                  cols="auto"
                  v-for="prop in propertiesForBook(item)"
                  :key="prop.label"
                >
                  <v-card flat>
                    <v-card-title>
                      <v-icon left>{{ prop.icon }}</v-icon>
                      {{ prop.label }}
                    </v-card-title>
                    <v-card-subtitle class="pb-0 mb-0">
                      {{ prop.extraction(item) }}
                    </v-card-subtitle>
                  </v-card>
                </v-col>
              </v-row>
            </v-card-text>
          </v-card>
        </v-col>
      </v-row>
    </template>
  </v-data-iterator>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'
import { Book } from '@/store/types'
import { vxm } from '@/store'
import {
  mdiAccountMultiple,
  mdiBookOpenPageVariantOutline,
  mdiBookshelf,
  mdiRobot
} from '@mdi/js'

@Component
export default class BookList extends Vue {
  @Prop()
  private books!: Book[]

  private bookUrls: { [isbn: string]: string | null } = {}

  private additionalProperties: {
    label: string
    predicate: (Book) => boolean
    extraction: (Book) => boolean
  } = [
    {
      label: 'Ort',
      icon: mdiBookshelf,
      predicate: book => book.location,
      extraction: book => book.location
    },
    {
      label: 'Seiten',
      icon: mdiBookOpenPageVariantOutline,
      predicate: book => book.pages,
      extraction: book => book.pages
    },
    {
      label: 'Autoren',
      icon: mdiAccountMultiple,
      predicate: book => book.author,
      extraction: book => book.author.join(', ')
    },
    {
      label: 'Crawler',
      icon: mdiRobot,
      predicate: book => book.crawler,
      extraction: book => book.crawler
    }
  ]

  private propertiesForBook(item: Book) {
    return this.additionalProperties.filter(it => it.predicate(item))
  }

  private async coverForBook(book: Book): Promise<void> {
    const blob = await vxm.books.fetchCover(book.isbn)
    Vue.set(this.bookUrls, book.isbn, URL.createObjectURL(blob))
  }

  @Watch('books')
  private onBooksChanged() {
    this.bookUrls = {}
    this.books.forEach(it => this.coverForBook(it))
  }
}
</script>
