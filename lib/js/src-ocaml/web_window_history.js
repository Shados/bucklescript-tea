// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function length($$window) {
  var history = $$window.history;
  if (history !== undefined) {
    return history.length;
  } else {
    return -1;
  }
}

function back($$window) {
  var history = $$window.history;
  if (history !== undefined) {
    return history.back;
  }
  
}

function forward($$window) {
  var history = $$window.history;
  if (history !== undefined) {
    return history.forward;
  }
  
}

function go($$window, to$p) {
  var history = $$window.history;
  if (history !== undefined) {
    return history.go(to$p);
  }
  
}

function pushState($$window, state, title, url) {
  var history = $$window.history;
  if (history !== undefined) {
    return history.pushState(state, title, url);
  }
  
}

function replaceState($$window, state, title, url) {
  var history = $$window.history;
  if (history !== undefined) {
    return history.replaceState(state, title, url);
  }
  
}

function state($$window) {
  var history = $$window.history;
  if (history !== undefined) {
    return history.state;
  }
  
}

exports.length = length;
exports.back = back;
exports.forward = forward;
exports.go = go;
exports.pushState = pushState;
exports.replaceState = replaceState;
exports.state = state;
/* No side effect */
