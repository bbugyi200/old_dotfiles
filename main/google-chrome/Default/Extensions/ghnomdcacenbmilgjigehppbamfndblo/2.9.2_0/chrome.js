$(document).ready(function() {
  
  // kick it off
  chrome.tabs.getSelected(null, function(tab) {
    url = tab.url;
    camelizer.onLoad(url);
  });

});



var ABTester = {
  // currently only FF has a test, but this function needs to be in all browsers.
  getTests: function(callback) {
    callback(null);
  }
};


var camelAsync = {
  
  get: function(url, data, callback, type) {
    type = (typeof type === 'undefined') ? 'json' : type;
    $.get(url, data, function(){}, type).always(function(data, textStatus, jqXHR) {
      if (textStatus == "error") {
        status = data.status
        data = null;
      } else {
        status = jqXHR.status;
      }
      out = {"response":data, "status": status};
      callback(out);
    });
  }

}

var camelizerStore = {
  
  getMulti: function(dic, callback) {
    this.getMultiSync(dic, callback);
  },

  getMultiSync: function(dic, callback) {
    // seems chrome can hate us here... so we'll hope we're catching something in chrome and call the callback on the default values.
    // bug: https://code.google.com/p/chromium/issues/detail?id=174436&q=sst&colspec=ID%20Pri%20M%20Iteration%20ReleaseBlock%20Cr%20Status%20Owner%20Summary%20Modified
    chrome.storage.sync.get(dic, function(data) {
      if (typeof data == "undefined") {
        console.log("Chrome storage _sync_ get returned an undefined response! trying non-sync get.");
        camelizerStore.getMultiLocal(dic, callback);
      } else {
        callback(data);
      }
    });
  },
  
  getMultiLocal: function(dic, callback) {
    chrome.storage.local.get(dic, function(data) {
      if (typeof data == "undefined") {
        console.log("Chrome storage get returned an undefined response! executing callback with default values.");
        callback(dic);
      } else {
        callback(data);
      }
    });
  },

  set: function(key, val) {
    obj = {};
    obj[key] = val;
    // call local set first, because it seems if sync set fails first, local set won't work after.
    chrome.storage.local.set(obj);
    chrome.storage.sync.set(obj, function() {
      if (chrome.runtime.lastError) {
        console.log(chrome.runtime.lastError);
      }
    });
  }

};


var injectedScripts = {
  
  getAsin: function(url, callback) {
    chrome.tabs.getSelected(null, function(tab) {
      chrome.tabs.executeScript(tab.id, {runAt: "document_end", file: "scrape_asin.js"}, function(response) {
        var resp = response[0];
        if (!resp) {
          callback(null);
        } else {
          callback(resp.asin);
        }
        return true;
      });
    });
  }

};


function bindChartLinks() {
  $("a[target=_blank]").each(function(index, a) {
    a = $(this);
    if (!a.hasClass("boundTabLink")) {
      a.addClass("boundTabLink");
      a.on("click", function(e) {
        addTracking(this);
      });
    }
  });
}

function browser() {
  return "chrome";
}

function add_ga_script() {
  // these are useful for debugging at times.
  // var script = "ga.js";
  // var script = "https://ssl.google-analytics.com/u/ga_debug.js";
  var script = "https://ssl.google-analytics.com/ga.js";
  var len = $('script[src^="'+script+'"]').length;
  if (len == 0) {
    (function() {
      var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
      ga.src = script;
      var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
    })();
  }
}


// callback passed an array of locale strings
var getAcceptLanguages = function(callback) {
  chrome.i18n.getAcceptLanguages(function(ar) {
    callback(ar);
  });
}

