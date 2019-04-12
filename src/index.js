import '../node_modules/bulma/css/bulma.min.css'
import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {}
});

app.ports.formatJsonRequest.subscribe(function({json, spacing}) {
  let result;
  try {
    result = {
      data: JSON.stringify(JSON.parse(json), undefined, spacing)
    };
  } catch(err) {
    result = {
      error: err.message
    };
  }
  app.ports.formatJsonResponse.send(result);
});

app.ports.copyJsonToClipboardRequest.subscribe(function(textareaId) {
  let result;
  try {
    document.querySelector(`#${textareaId}`).select();
    document.execCommand('copy');
    result = "Text copied to clipboard";
  } catch(err) {
    result = "Couldn't copy text to clipboard";
  }
  app.ports.copyJsonToClipboardResponse.send(result);
});

registerServiceWorker();
