import { Elm } from '../elm/Main.elm'

document.addEventListener('DOMContentLoaded', function (event) {
    const app = Elm.Main.init({
        node: document.getElementById('cpu-emulation-demo')
    })
})
