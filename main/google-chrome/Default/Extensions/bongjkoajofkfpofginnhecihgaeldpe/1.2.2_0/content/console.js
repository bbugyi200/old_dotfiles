/* Copyright (c) Appteligent, 2012 All Rights Reserved */
(function(pkg) {
    var consoleFns = ["info", "log", "dir", "debug", "warn", "error", "trace",
                      "time", "timeEnd", "group", "groupEnd", "groupCollapsed"],
        settings = pkg.settings || {};

    pkg.console = {};

    consoleFns.forEach(function(fn) {
        if(typeof console[fn] === 'function') {
            if(fn in ["warn", "error"]) {
                pkg.console[fn] = console[fn].bind(console);
            } else {
                pkg.console[fn] = function() {
                    if(settings.vr_enable_debugging_output) {
                        console[fn].apply(console, arguments);
                    }
                };
            }
        }
    });

})(this.VR || (this.VR = {}));