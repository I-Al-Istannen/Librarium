<template>
  <div>
    <v-dialog width="600" v-model="dialogOpen">
      <template #activator="{ on }">
        <slot :on="on" name="activator"></slot>
      </template>

      <v-card>
        <v-toolbar dark color="primary">
          <v-toolbar-title>Login</v-toolbar-title>
        </v-toolbar>
        <v-card-text>
          <v-form v-model="formValid" ref="form">
            <v-text-field
              type="text"
              :rules="[nonEmpty]"
              label="Username"
              v-model="username"
              @keyup.enter="login"
            ></v-text-field>
            <v-text-field
              type="password"
              :rules="[nonEmpty]"
              label="Password"
              v-model="password"
              @keyup.enter="login"
            ></v-text-field>
          </v-form>
        </v-card-text>
        <v-card-actions>
          <v-spacer></v-spacer>
          <v-btn color="primary" :disabled="!formValid" @click="login"
            >Login</v-btn
          >
          <v-spacer></v-spacer>
          <v-btn color="error" @click="dialogOpen = false">Close</v-btn>
        </v-card-actions>
      </v-card>
    </v-dialog>
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import { Component, Watch } from 'vue-property-decorator'
import { vxm } from '@/store'

@Component
export default class LoginDialog extends Vue {
  private username: string = ''
  private password: string = ''

  private formValid: boolean = false
  private dialogOpen: boolean = false

  @Watch('role')
  private onRoleChange() {
    ;(this.$refs.form as any).validate()
  }

  @Watch('dialogOpen')
  private onOpened(opened: boolean) {
    if (!opened) {
      this.password = ''
      this.username = ''
    }
  }

  private nonEmpty(input: string): boolean | string {
    return input.trim().length > 0 ? true : 'This field must not be empty!'
  }

  private login() {
    if (!this.formValid) {
      return
    }

    vxm.user
      .login({ user: this.username, password: this.password })
      .then(() => (this.dialogOpen = false))
      .then(() => this.$globalSnackbar.setSuccess('', 'Login successful'))
  }
}
</script>

<style scoped></style>
