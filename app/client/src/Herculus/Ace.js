"use strict";

exports.setBlockScrollingInfinity = function (editor) {
  return function () {
    return editor.$blockScrolling = Infinity;
  };
};
