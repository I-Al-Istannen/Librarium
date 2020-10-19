<template>
  <span>
    <change-location-dialog
      :book="currentEditBook"
      @close="currentEditBook = null"
    ></change-location-dialog>
    <v-data-iterator :items="books" item-key="isbn">
      <template #no-data>
        <v-row align="center" justify="center" class="pt-5">
          <v-col cols="auto" class="font-weight-bold">
            Keine Bücher gefunden :/
          </v-col>
        </v-row>
      </template>
      <template #default="{ items }">
        <v-row>
          <v-col
            v-for="item in items"
            :key="item.isbn"
            cols="6"
            md="6"
            class="my-0 py-0"
          >
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
                  <v-col cols="auto">
                    <v-card flat>
                      <v-card-title>
                        <v-icon>{{ locationIcon }}</v-icon>
                        Ort
                      </v-card-title>
                      <v-card-subtitle class="pb-0 mb-0">
                        <v-chip
                          outlined
                          class="py-0 my-0"
                          @click="currentEditBook = item"
                        >
                          {{ item.location || 'Set...' }}
                          <v-icon right small>{{ changeLocationIcon }}</v-icon>
                        </v-chip>
                      </v-card-subtitle>
                    </v-card>
                  </v-col>

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
  </span>
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
  mdiLeadPencil,
  mdiRobot
} from '@mdi/js'
import ChangeLocationDialog from '@/components/dialogs/ChangeLocationDialog.vue'
@Component({
  components: { ChangeLocationDialog }
})
export default class BookList extends Vue {
  @Prop()
  private books!: Book[]

  private currentEditBook: Book | null = null
  private bookUrls: { [isbn: string]: string | null } = {}

  private additionalProperties: {
    label: string
    icon: string
    predicate: (book: Book) => boolean
    extraction: (book: Book) => string
  }[] = [
    {
      label: 'Seiten',
      icon: mdiBookOpenPageVariantOutline,
      predicate: book => book.pages !== null,
      extraction: book => '' + book.pages
    },
    {
      label: 'Autoren',
      icon: mdiAccountMultiple,
      predicate: book => book.author !== [],
      extraction: book => book.author!.join(', ')
    },
    {
      label: 'Crawler',
      icon: mdiRobot,
      predicate: book => book.crawler !== null,
      extraction: book => book.crawler!
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
  private onBooksChanged(newBooks: Book[], oldBooks: Book[]) {
    const onlyInOld: Set<string> = new Set(oldBooks.map(it => it.isbn))
    newBooks.forEach(it => onlyInOld.delete(it.isbn))

    onlyInOld.forEach(it => delete this.bookUrls[it])

    this.books.forEach(it => {
      if (this.bookUrls[it.isbn] === undefined) {
        this.coverForBook(it)
      }
    })
  }

  // ICONS
  private locationIcon = mdiBookshelf
  private changeLocationIcon = mdiLeadPencil
}
</script>
