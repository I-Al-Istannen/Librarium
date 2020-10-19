<template>
  <v-container>
    <v-row>
      <v-col>
        <v-card>
          <v-card-title>
            <v-toolbar dark color="primary">Suche Bücher</v-toolbar>
          </v-card-title>
          <v-card-text>
            <v-row @keydown.enter="fetchBooks">
              <v-col>
                <v-text-field label="ISBN" v-model="searchIsbn"></v-text-field>
              </v-col>
              <v-col>
                <v-text-field
                  label="Titel"
                  v-model="searchTitle"
                ></v-text-field>
              </v-col>
              <v-col>
                <v-text-field
                  label="Inhalt"
                  v-model="searchSummary"
                ></v-text-field>
              </v-col>
              <v-col>
                <v-text-field
                  label="Ort"
                  v-model="searchLocation"
                ></v-text-field>
              </v-col>
            </v-row>
          </v-card-text>
          <v-card-actions>
            <v-spacer></v-spacer>
            <v-btn text color="primary" @click="fetchBooks">Search</v-btn>
            <v-spacer></v-spacer>
          </v-card-actions>
        </v-card>
      </v-col>
    </v-row>
    <v-row>
      <v-col>
        <v-card>
          <book-list :books="books"></book-list>
        </v-card>
      </v-col>
    </v-row>
    <v-row>
      <v-col>
        <add-book></add-book>
      </v-col>
    </v-row>
  </v-container>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator'
import { vxm } from '@/store'
import BookList from '@/components/BookList.vue'
import { Book } from '@/store/types'
import AddBook from '@/components/AddBook.vue'

@Component({
  components: {
    'add-book': AddBook,
    'book-list': BookList
  }
})
export default class Home extends Vue {
  private searchIsbn: string = ''
  private searchTitle: string = ''
  private searchLocation: string = ''
  private searchSummary: string = ''

  private fetchBooks() {
    vxm.books.fetchBooks({
      summary: this.searchSummary,
      isbn: this.searchIsbn,
      location: this.searchLocation,
      title: this.searchTitle
    })
  }

  private get books(): Book[] {
    return vxm.books.books
  }
}
</script>
