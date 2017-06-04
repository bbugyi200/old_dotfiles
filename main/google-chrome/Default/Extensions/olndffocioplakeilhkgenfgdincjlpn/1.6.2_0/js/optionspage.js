(function() {
  var load, save;

  save = function() {
    var options;
    options = {
      count: parseInt($('#count').val()),
      manager: $('#manager').prop('checked')
    };
    return window.options.save(options);
  };

  load = function() {
    var options;
    options = window.options.load();
    $('#count').val(options.count);
    return $('#manager').prop('checked', options.manager === 'true');
  };

  $(function() {
    load();
    return $('#save').on('click', function(e) {
      e.preventDefault();
      return save();
    });
  });

}).call(this);
