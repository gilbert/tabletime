export default async function(app) {
  app.get('/hello', r => r.end('Welcome to cofound'))
}
