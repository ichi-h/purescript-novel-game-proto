"use strict";

export function debug(object) {
  return function () {
    console.log(object);
  }
}

export function warn(object) {
  return function () {
    console.warn(object);
  }
}

export function error(object) {
  return function () {
    console.error(object);
  }
}
