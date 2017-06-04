/**
 * Contains settings constants and provides mechanism to retrieve, update and
 * store them persistently.
 *
 * Exports a Settings object(under 'settings' key), providing the following
 * read/write properties:
 *
 *  o {Boolean} vr_enable_debugging_output=false
 *  o {Number}  vr_forget_if_seconds_left=5
 *  o {Number}  vr_forget_after_days=7
 *
 *  Settings are retrieved from the local store upon initialization and stored
 *  back when changed.
 *
 * @author vtomilin
 *
 * Copyright (c) Appteligent, 2012 All Rights Reserved
 */
(function(pkg) {
    var storage = chrome.storage.local,
        defaults = {
            vr_enable_debugging_output: false,
            vr_forget_if_seconds_left: 5,
            vr_forget_after_days: 7
        },
        observers = [],
        settingsLoaded = false,
        settings = {
            get vr_enable_debugging_output() {
                return defaults.vr_enable_debugging_output;
            },
            set vr_enable_debugging_output(enable) {
                defaults.vr_enable_debugging_output = !!enable;
                storeSettings();
            },
            get vr_forget_if_seconds_left() {
                return defaults.vr_forget_if_seconds_left;
            },
            set vr_forget_if_seconds_left(seconds) {
                seconds = Number(seconds);
                if(seconds < 0) {
                    return;
                }

                defaults.vr_forget_if_seconds_left = seconds;
                storeSettings();
            },
            get vr_forget_after_days() {
                return defaults.vr_forget_after_days;
            },
            set vr_forget_after_days(days) {
                days = Number(days);
                if(days < 0) {
                    return;
                }

                defaults.vr_forget_after_days = days;
                storeSettings();
            },

            /**
             * Adds a "once" callback, if not already added.
             */
            onLoaded: function(callback) {
                if(typeof callback !== 'function'
                        || observers.indexOf(callback) !== -1)
                    return;
                if(!settingsLoaded) {
                    observers.push(callback);
                } else {
                    try {
                        callback();
                    } catch(error) {
                        console.error("[settings>onLoaded] callback threw an",
                                      "exception", error);
                    }
                }
            }
        };

    function storeSettings() {
        storage.set(defaults, function() {
            if(chrome.runtime.lastError) {
                console.error("[settings>storage.set] Failed to store settings",
                              "to persistent storage:",
                              chrome.runtime.lastError.message);
                return;
            }
        });
    }

    storage.get(defaults, function(items) {
        if(chrome.runtime.lastError) {
            console.error("[settings>storage.get] Failed to retrieve settings",
                          "from persistent storage:",
                          chrome.runtime.lastError.message);
            return;
        }

        Object.keys(items).forEach(function(k) {
            defaults[k] = items[k];
        });

        observers.forEach(function(observer) {
            try {
                observer();
            } catch(error) {
                console.error('[settings.onLoaded] observer callback raised',
                              'an exception', error);
            }
        });

        while(observers.length > 0)
            observers.pop();
    });

    // Monitor storage for external changes
    chrome.storage.onChanged.addListener(function(changes, areaName) {
        if (areaName !== "local") {
            return;
        }

        // Apply changes
        Object.keys(changes).forEach(function(key) {
            var change = changes[key];
            if (settings.hasOwnProperty(key)
                    && (change.newValue !== settings[key])) {
                // Bypass storing
                defaults[key] = change.newValue;
            }
        });
    });

    pkg.settings = settings;
})(this.VR || (this.VR = {}));
