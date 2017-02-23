"use strict";

exports.basilSet = function (key) {
  return function (val) {
    return function () {
      return basil.set(key, val);
    };
  };
};

exports.basilGet = function (key) {
  return function () {
    return basil.get(key);
  };
};
