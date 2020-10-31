<template>
  <span>
    <change-location-dialog
      :book="currentEditBook"
      @close="currentEditBook = null"
    ></change-location-dialog>
    <v-data-iterator
      :items="books"
      item-key="isbn"
      @current-items="scheduleObservation"
    >
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
            cols="12"
            md="6"
            class="my-0 py-0"
          >
            <v-card
              class="ma-5 book-observe-card"
              outlined
              :data-isbn="item.isbn"
            >
              <v-card-title>
                {{ item.title }}
                <v-spacer></v-spacer>
                <v-btn icon @click="deleteBook(item)">
                  <v-icon color="error">{{ deleteIcon }}</v-icon>
                </v-btn>
              </v-card-title>
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
                  <v-col cols="12" md="9">
                    <div class="summary">
                      {{ item.summary }}
                    </div>
                    <v-btn
                      class="pa-0 ma-0"
                      small
                      text
                      @click="toggleExpandSummary"
                    >
                      Mehr...
                    </v-btn>
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
  mdiDelete,
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
  private intersectionObserver?: IntersectionObserver

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
    try {
      const blob = await vxm.books.fetchCover(book.isbn)
      Vue.set(this.bookUrls, book.isbn, URL.createObjectURL(blob))
    } catch (ignored) {
      // ignore this
    }
  }

  @Watch('books')
  private onBooksChanged(newBooks: Book[], oldBooks: Book[]) {
    const onlyInOld: Set<string> = new Set(oldBooks.map(it => it.isbn))
    newBooks.forEach(it => onlyInOld.delete(it.isbn))

    onlyInOld.forEach(it => delete this.bookUrls[it])

    this.scheduleObservation()
  }

  private deleteBook(book: Book) {
    if (!window.confirm(`Do you really want to delete ${book.title}?`)) {
      return
    }
    vxm.books.deleteBook(book.isbn)
  }

  private toggleExpandSummary(e: Event) {
    let target: HTMLElement | null = e.target as HTMLElement
    while (target && !target.classList.contains('v-card')) {
      target = target.parentElement
    }

    if (!target) {
      return
    }

    const summaryItem = target.getElementsByClassName('summary')[0]

    if (summaryItem.classList.contains('expanded')) {
      summaryItem.classList.remove('expanded')
    } else {
      summaryItem.classList.add('expanded')
    }
  }

  private scheduleObservation() {
    Vue.nextTick(() => {
      Array.from(document.getElementsByClassName('book-observe-card')).forEach(
        it => {
          this.intersectionObserver!.observe(it)
        }
      )
    })
  }

  private mounted() {
    this.intersectionObserver = new IntersectionObserver(
      entries => {
        entries.forEach(entry => {
          if (!entry.isIntersecting) {
            return
          }

          if (this.intersectionObserver) {
            this.intersectionObserver.unobserve(entry.target)
          }

          const isbn = entry.target.getAttribute('data-isbn')

          const book = this.books.find(it => it.isbn === isbn)

          if (book) {
            this.coverForBook(book)
          }
        })
      },
      {
        threshold: 0.0,
        root: null
      }
    )
  }

  private beforeDestroy() {
    if (this.intersectionObserver) {
      this.intersectionObserver.disconnect()
    }
  }

  // ICONS
  private locationIcon = mdiBookshelf
  private changeLocationIcon = mdiLeadPencil
  private deleteIcon = mdiDelete
}
</script>

<style scoped>
.summary {
  max-height: 20ch;
  white-space: pre-line;
  overflow: hidden;
  transition: max-height 0.5s ease-in;
}

/*noinspection CssUnusedSymbol*/
.summary.expanded {
  max-height: 100vh;
}
</style>
