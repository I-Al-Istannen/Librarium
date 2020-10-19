<template>
  <v-card>
    <v-card-title>
      <v-toolbar dark color="primary">Buch hinzufügen</v-toolbar>
    </v-card-title>
    <v-card-text class="d-flex align-center justify-center">
      <div style="width: 600px">
        <scan-barcode @code="codeScanned"></scan-barcode>
        <v-text-field
          @keydown.enter="addBook"
          label="Enter Isbn..."
          v-model="isbn"
        ></v-text-field>
        <v-combobox
          v-model="location"
          :items="allLocations"
          chips
          label="Ort"
          prepend-icon="mdi-bookshelf"
          placeholder="Enter your own"
        >
          <template v-slot:selection="{ attrs, item, select, selected }">
            <v-chip v-bind="attrs" :input-value="selected" @click="select">
              {{ item }}
            </v-chip>
          </template>
        </v-combobox>
      </div>
    </v-card-text>
    <v-card-actions>
      <v-spacer></v-spacer>
      <v-btn
        :loading="addBookLoading"
        color="primary"
        text
        @click="addBook"
        :disabled="!isbn"
      >
        Hinzufügen
      </v-btn>
      <v-spacer></v-spacer>
    </v-card-actions>
  </v-card>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { vxm } from '@/store'
import ScanBarcode from '@/components/dialogs/ScanBarcode.vue'

@Component({
  components: { ScanBarcode }
})
export default class AddBook extends Vue {
  private isbn: string = ''
  private addBookLoading: boolean = false
  private location: string = ''

  private get allLocations() {
    return vxm.books.locations
  }

  private codeScanned(code: string) {
    this.isbn = code
  }

  private async addBook() {
    if (!this.isbn) {
      return
    }
    this.addBookLoading = true
    try {
      const isbn = this.isbn
      const location = this.location
      await vxm.books.addBook(isbn)
      if (this.location) {
        await vxm.books.setLocation({
          isbn: isbn,
          location: location
        })
      }

      await vxm.books.fetchAllLocations()
    } finally {
      this.addBookLoading = false
    }
  }

  private mounted() {
    vxm.books.fetchAllLocations()
  }
}
</script>
