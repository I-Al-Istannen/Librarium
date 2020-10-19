<template>
  <v-dialog v-model="locationEditDialogOpen" width="700">
    <v-card>
      <v-card-title>
        <v-toolbar dark color="primary">Change location</v-toolbar>
      </v-card-title>
      <v-card-text>
        <v-text-field
          v-model="currentEditingLocation"
          label="Location..."
        ></v-text-field>
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
      this.currentEditingLocation = this.book.location
    }
  }

  private closeDialog() {
    this.$emit('close')
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
