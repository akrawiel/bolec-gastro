import { Elm } from './elm/Main.elm'

Elm.Main.init({
  node: document.querySelector('main'),
  flags: {
    apiUrl: process.env.API_URL
  }
})
