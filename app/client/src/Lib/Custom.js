"use strict";

exports.fromJSDateImpl = function (jsdate) {
  return function () {
    var utc = new Date(jsdate.getTime() - 60 * 1000 * jsdate.getTimezoneOffset());
    return utc.toISOString();
  };
};
