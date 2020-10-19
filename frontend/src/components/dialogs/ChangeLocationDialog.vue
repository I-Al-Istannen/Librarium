<template>
  <v-dialog v-model="locationEditDialogOpen" width="700">
    <v-card>
      <v-card-title>
        <v-toolbar dark color="primary">Ändere den Ort</v-toolbar>
      </v-card-title>
      <v-card-text>
        <v-combobox
          v-model="currentEditingLocation"
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
      </v-card-text>
      <v-card-actions>
        <v-spacer></v-spacer>
        <v-btn text color="primary" @click="changeLocation">Accept</v-btn>
        <v-btn text color="error" @click="closeDialog">Close</v-btn>
        <v-spacer></v-spacer>
      </v-card-actions>
    </v-card>
  </v-dialog>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'
import { Book } from '@/store/types'
import { vxm } from '@/store'

@Component
export default class ChangeLocationDialog extends Vue {
  private locationEditDialogOpen: boolean = false
  private currentEditingLocation: string = ''

  @Prop()
  private book: Book | null = null

  @Watch('book')
  private onBookChange() {
    this.locationEditDialogOpen = !!this.book
    if (this.book) {
      vxm.books.fetchAllLocations()
      this.currentEditingLocation = this.book.location || ''
    }
  }

  private closeDialog() {
    this.$emit('close')
  }

  private get allLocations() {
    return vxm.books.locations
  }

  private async changeLocation() {
    await vxm.books.setLocation({
      isbn: this.book!.isbn,
      location: this.currentEditingLocation || null
    })
    this.$emit('close')
  }
}
</script>
