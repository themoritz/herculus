npm i
jekyll build --watch --incremental &
`npm bin`/browser-sync start --server --serveStatic _site --files _site/** --no-notify
