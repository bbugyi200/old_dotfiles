/* Copyright (c) Appteligent, 2012 All Rights Reserved */
(function(pkg) {
    var console = pkg.console || console,
        settings = pkg.settings,
        gebi = document.getElementById.bind(document);

    if(!settings) {
        console.error("[options>init] No settings object! Aborted.");
        return;
    }

    document.addEventListener("DOMContentLoaded", function() {
        var vr_enable_debugging_output = gebi('vr_enable_debugging_output'),
            vr_forget_if_seconds_left = gebi('vr_forget_if_seconds_left'),
            vr_forget_after_days = gebi('vr_forget_after_days');

        settings.onLoaded(function() {
            vr_enable_debugging_output.checked = settings.vr_enable_debugging_output;
            vr_enable_debugging_output.addEventListener('click', function() {
                settings.vr_enable_debugging_output = vr_enable_debugging_output.checked;
            });

            vr_forget_if_seconds_left.value = settings.vr_forget_if_seconds_left;
            vr_forget_if_seconds_left.addEventListener('change', function() {
                if(!vr_forget_if_seconds_left.checkValidity())
                    return;

                settings.vr_forget_if_seconds_left = Number(vr_forget_if_seconds_left.value);
            });

            vr_forget_after_days.value = settings.vr_forget_after_days;
            vr_forget_after_days.addEventListener('change', function() {
                if(!vr_forget_after_days.checkValidity())
                    return;

                settings.vr_forget_after_days = Number(vr_forget_after_days.value);
            });
        });
    });

})(this.VR || (this.VR = {}));