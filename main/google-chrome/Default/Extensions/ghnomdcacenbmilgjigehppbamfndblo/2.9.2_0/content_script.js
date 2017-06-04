(function() {
  try {
    urls = {};
    anchors = document.getElementsByTagName('a');
    for (i=0; i<anchors.length; i++) {
      a = anchors[i];
      title = a.title;
      if (title == null || title == '') {
        title = a.textContent;
      }
      // we'll take the longer title
      if (!urls[a.href] || urls[a.href].length < title.length) {
        urls[a.href] = title;
      }
    }   
    return {"urls": urls};
  } catch(e) {
    return {"error": e.message + "\n" + e.stack, "urls" : {}};
  }
})();
