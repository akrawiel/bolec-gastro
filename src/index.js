import dotenv from 'dotenv'

import { Elm } from './elm/Main.elm'

dotenv.config()

Elm.Main.init({
  node: document.querySelector('main'),
  flags: {
    apiUrl: process.env.API_URL
  }
})
