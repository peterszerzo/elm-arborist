import Elm from "./Landing.elm";
import elmJson from "../../elm.json";

const { version } = elmJson;

Elm.Elm.Landing.init({
  flags: version,
  node: document.getElementById("app")
});
