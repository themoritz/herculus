"use strict";

var callbacks = new Map();

exports.attachClickOutside = function (el) {
  return function (cb) {
    return function () {
      var newCb = function (e) {
        if (!el.contains(e.target)) {
          cb();
        }
      };
      callbacks.set(el, newCb);
      return setTimeout(function () {
        document.body.addEventListener('click', newCb);
      }, 10);
    };
  };
};

exports.detachClickOutside = function (el) {
  return function () {
    if (callbacks.has(el)) {
      var cb = callbacks.get(el);
      document.body.removeEventListener('click', cb);
    }
    return;
  };
};
