var tempCache = {
  
  cache: {},

  get: function(key) {
    var raw = this.cache[key];
    if (typeof raw == "undefined") {
      return null;
    }
    var now = parseInt(new Date().getTime());
    var max = parseInt(raw.ts)+30000;

    if (max > now) {
      return raw.val;
    } else {
      delete this.cache[key];
      return null;
    }
  },

  set: function(key, val) {
    this.cache[key] = {
      val: val,
      ts: new Date().getTime()
    }
  },

  clear: function() {
    this.cache = {};
  }

}
