<template>
  <v-dialog width="800" v-model="dialogOpen" fullscreen>
    <template #activator="{ on }">
      <v-row align="center" justify="center" no-gutters>
        <v-col cols="auto">
          <v-btn v-on="on" color="primary" outlined>
            <v-icon left>{{ scanIcon }}</v-icon>
            Scan
          </v-btn>
        </v-col>
      </v-row>
    </template>
    <v-card>
      <v-card-text>
        <v-quagga
          v-if="dialogOpen"
          :onDetected="barcodeFound"
          :readerSize="readerSize"
          :readerTypes="['ean_reader']"
        ></v-quagga>
      </v-card-text>
      <v-card-actions>
        <v-spacer></v-spacer>
        <v-btn color="error" text @click="dialogOpen = false">Abbrechen</v-btn>
        <v-spacer></v-spacer>
      </v-card-actions>
    </v-card>
  </v-dialog>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import VueQuagga from 'vue-quaggajs'
import { mdiBarcodeScan } from '@mdi/js'

Vue.use(VueQuagga)

@Component
export default class ScanBarcode extends Vue {
  private dialogOpen: boolean = false

  private readerSize = {
    width: 640,
    height: 480
  }

  private barcodeFound(result: { codeResult: { code: string } }) {
    this.dialogOpen = false
    this.$emit('code', result.codeResult.code)
  }

  // ICONS
  private scanIcon = mdiBarcodeScan
}
</script>
