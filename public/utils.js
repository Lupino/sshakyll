// XMLHttpRequest's API is annoying. $.ajax() is easier except that its error handling makes no
// sense.
//
// I based this somewhat on:
//   https://plus.google.com/u/0/+JeffreyYasskin/posts/XjWc5oGs3dP
//
// (He just happened to post this on the same day I needed it.)
function fetch(url, options) {
  options = options || {};
  return new Promise(function(resolve, reject) {
    var xhr = new XMLHttpRequest();
    xhr.onload = function () {
      if (xhr.status >= 400) {
        reject(new Error("XHR returned status " + xhr.status + ":\n" + xhr.responseText));
      } else {
        resolve(xhr);
      }
    };
    xhr.onerror = function(e) { reject(e); };
    if (options.hasOwnProperty('responseType'))
      xhr.responseType = options.responseType;
    var method = 'get';
    if (options.hasOwnProperty('method'))
      method = options.method;
    xhr.open(method, url)
    var data = undefined;
    if (options.hasOwnProperty('data'))
      data = options.data;
    xhr.send(data);
  });
}

function doGet(url) {
  return fetch(url + '?t=' + new Date() / 1000);
}
function doPost(url, data) {
  return fetch(url, { method: "post", data: data });
}
function doPut(url, data) {
  return fetch(url, { method: "put", data: data });
}
function doDelete(url) {
  return fetch(url, { method: "delete" });
}
function catchErrors(promise) {
  promise.catch(function (err) {
    console.error(err);
  });
}

function isTextFile(fn) {
  if (/\.(json|js|html|markdown|md|rst|css|htm|xml|txt)$/i.exec(fn)) {
    return true;
  }
  return false;
}

function isImageFile(fn) {
  if (/\.(jpg|jpeg|gif|png|svg|bmp|tiff)/i.exec(fn)) {
    return true;
  }
  return false;
}
