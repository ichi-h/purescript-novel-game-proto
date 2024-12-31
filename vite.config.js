import { defineConfig } from 'vite'

export default defineConfig({
  resolve: {
    alias: {
      '@': '/src',
      '#': '/output',
    },
  },
  server: {
    port: 8080,
  },
})
