process.env.NODE_ENV = 'test'

globalThis.cofounddev = {
  exit_code: 0,
  api: {
    tested() {
      process.exit(globalThis.cofounddev.tested || 0)
    }
  }
}

await import('./ui.test.js')
