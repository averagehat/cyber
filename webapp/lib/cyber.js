'use strict'
import * as PURE from 'module'; // or './module'
//import PURE from './output/Main/index';
//import PURE from './output/Main/index.js';
// const PURE = require('./app');

var x = document.createElement('script');
x.src = 'app.js';
document.getElementsByTagName("head")[0].appendChild(x);

// <script src="./output/Main/index.js"> </script>

makeSpan = function (text) { 
  var para = document.createElement("SPAN");      
  para.innerText = text;
  return para;
}

sendCommand = function (evt) {
  evt.preventDefault();
  var op = PURE.str2Op(evt.value)
  if (state.loggedOut == true) {
      alert('Jacked out of the system.');
  }
  var newState = PURE.step(op)(state);
  var stateStr = PURE.showState(newState);
  var newStateSpan = makeSpan(stateStr);
  var cmdInput = document.getElementById('readline');
  cmdInput.insertAdjacentHTML('beforebegin', '<br/>');
  term.type(newStateSpan);
  var counter = document.getElementById('counter');
  counter.innerText = newState.alerts;
}

init = function () {
  var cmdInput = document.getElementById('readline');
  cmdInput.addEventListener("submit", sendCommand); 
}

init();
var term = new Termynal('#termynal');
