function hexl$getWebSocketUrl () {
  try {
    return process.env.WEBSOCKET_URL
  } catch (e) {
    return "ws://localhost:3000/websocket"
  }
}

function hexl$getApiUrl () {
  try {
    return process.env.API_URL
  } catch (e) {
    return "http://localhost:3000"
  }
}
