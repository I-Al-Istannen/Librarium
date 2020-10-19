<template>
  <v-card>
    <v-card-title>
      <v-toolbar dark color="primary">Buch hinzufügen</v-toolbar>
    </v-card-title>
    <v-card-text>
      <v-text-field label="Enter Isbn..." v-model="isbn"></v-text-field>
    </v-card-text>
    <v-card-actions>
      <v-spacer></v-spacer>
      <v-btn :loading="addBookLoading" color="primary" text @click="addBook">
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

@Component
export default class AddBook extends Vue {
  private isbn: string = ''
  private addBookLoading: boolean = false

  private async addBook() {
    this.addBookLoading = true
    try {
      await vxm.books.addBook(this.isbn)
    } finally {
      this.addBookLoading = false
    }
  }
}
</script>
