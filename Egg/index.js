const elmContainer = document.getElementById("elm-container")
const app = Elm.Egg.Main.embed(elmContainer)
const urls = [
  "https://cdnjs.cloudflare.com/ajax/libs/react/16.2.0/umd/react.development.js",
  "https://cdnjs.cloudflare.com/ajax/libs/react-dom/16.2.0/umd/react-dom.development.js",
  "https://unpkg.com/@babel/standalone/babel.min.js"
]
const load = (url) => {
  return new Promise(function(resolve, reject) {
    var scriptElement = document.createElement("script")
    scriptElement.src = url
    scriptElement.onload = resolve
    document.body.appendChild(scriptElement)
  })
}
const start = () => {
  app.ports.code.subscribe(msg => {
    let code
    try {
      code = Babel.transform(msg.sourcecode, {
        presets: ["es2015", "react"]
      })
    } catch(err) {
      code = {
        code: `function Child() {
          return React.createElement('p', {}, ${JSON.stringify(err)})
        }
        `
      }
    }
    const container = document.getElementById("playground-container")
    const prevScript = document.querySelector("#compiled")
    if (prevScript) {
      ReactDOM.unmountComponentAtNode(container)
      prevScript.parentNode && prevScript.parentNode.removeChild(prevScript)
    }
    const script = document.createElement("script")
    script.id = "compiled"
    script.textContent = code.code
    document.body.appendChild(script)
    container && ReactDOM.render(React.createElement(Child), container) 
  })
}
load(urls[0])
  .then(a => load(urls[1]))
  .then(a => load(urls[2]))
  .then(start)

