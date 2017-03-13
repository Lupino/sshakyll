// Because highlight.js is a bit awkward at times
var languageOverrides = {
  js: 'javascript',
  html: 'xml'
};

var md = markdownit({
  html: true,
  highlight: function(code, lang){
    if(languageOverrides[lang]) lang = languageOverrides[lang];
    if(lang && hljs.getLanguage(lang)){
      try {
        return hljs.highlight(lang, code).value;
      }catch(e){}
    }
    return '';
  }
})
  .use(markdownitFootnote);

function filesToTree(parent, files) {
  var result = [];
  for (var file in files) {
    var content = files[file];
    var entry = {text: file};
    var path = parent + "/" + file;
    entry.serverPath = path;
    if (typeof content === "object") {
      entry.children = filesToTree(path, content);
      entry.state = { "opened": false };
      entry.isDir = true;
    } else {
      entry.icon = false;
      entry.isDir = false;
    }
    result.push(entry);
  }
  return result;
}

function initTree(text, eventCallback) {
  var data = JSON.parse(text);
  $("#tree").on("activate_node.jstree", function (e, data) {
    data = data.node.original;
    data.instance = 'TreeNode'
    eventCallback(data);
  }).jstree({core: {data: filesToTree("", data)}});
}

function clearTree() {
  $("#tree").jstree(true).destroy();
  $("#tree").empty();
}

function setOutput(val){
  var out = document.getElementById('out');
  var old = out.cloneNode(true);
  out.innerHTML = val;

  var allold = old.getElementsByTagName("*");
  if (allold === undefined) return;

  var allnew = out.getElementsByTagName("*");
  if (allnew === undefined) return;

  for (var i = 0, max = Math.min(allold.length, allnew.length); i < max; i++) {
    if (!allold[i].isEqualNode(allnew[i])) {
      out.scrollTop = allnew[i].offsetTop;
      return;
    }
  }
}

var editorInitialized = false;
var editor;

function initEditor() {
  if (editorInitialized) {
    return editor;
  }

  $("#in").removeClass("uninitialized");
  editor = ace.edit("in");
  editor.setTheme("ace/theme/chome");
  editorInitialized = true;
  return editor;
}

function setEditorValue(value) {
  editor.setValue(value);
}

var MODE_MAP = {
  "javascript": /\.js$/i,
  "markdown":   /\.(md|markdown|rst)$/i,
  "html":       /\.html?$/i,
  "css":        /\.css$/i,
  "yaml":       /\.(yaml|yml)$/,
  "xml":        /\.(svg|xml)$/,
  "json":       /\.json$/
};

function getModeFromFileName(fileName) {
  var mode = "text";
  for(m in MODE_MAP) {
    v = MODE_MAP[m];
    if (v.exec(fileName)) {
      mode = m;
      break;
    }
  }
  return mode;
}

function setEditorMode(editor, fileName) {
  editor.getSession().setMode("ace/mode/" + getModeFromFileName(fileName));
  return editor;
}

function canPreviewFile(fn) {
  var mode = getModeFromFileName(fn);
  if (/markdown|xml|html|text/i.exec(mode) && !/^\/editor/i.exec(currentPath)) {
    return true;
  } else {
    return false;
  }
}

function isMarkdownFile(fn) {
  var mode = getModeFromFileName(fn)
  if (/markdown/i.exec(mode)) {
    return true;
  }
  return false;
}

var currentPath = "";
var currentDirectory = "";

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

function selectFile(callback) {
  var form = $('<form><input type="file" name="test"></form>', { css: { 'display': 'none' }});
  var input = $(form.children()[0]);

  input.on('change', function(e) {
    if (this.files.length === 1) {
      var file = this.files[0];
      callback(file.name, file)
    }
  });

  // Trigger file selector (only works on newer browsers...)
  input.click();
}

var preview = true;
var canPreview = false;
