(function() {
  window.options = {
    defaults: {
      count: '15',
      manager: 'true'
    },
    save: function(options) {
      var key, _results;
      _results = [];
      for (key in options) {
        _results.push(localStorage[key] = options[key]);
      }
      return _results;
    },
    load: function() {
      var key, options;
      options = this.shallowClone(this.defaults);
      for (key in options) {
        options[key] = localStorage[key] != null ? localStorage[key] : options[key];
      }
      return options;
    },
    shallowClone: function(obj) {
      var clone, key;
      clone = {};
      for (key in obj) {
        clone[key] = obj[key];
      }
      return clone;
    }
  };

}).call(this);
