/* Copyright (c) Appteligent, 2012 All Rights Reserved */
(function(pkg) {
    var trackers = [],
        settings = pkg.settings,
        console = pkg.console || console,
        cleanupVideos = pkg.cleanupVideos || function() {
            console.warn("[main] No video cleanup function defined!");
        };

    chrome.extension.onConnect.addListener(function(port) {
        var url = port.name;

        console.info("[main>onConnect] attached to %s", url || "unknown URL");

        if(chrome.extension.inIncognitoContext
                && !chrome.extension.isAllowedIncognitoAccess()) {
            console.warn("[main>onConnect] Not tracking video %s in " +
                         "incognito mode because incognito access is not " +
                         "granted for this extension", url);
            return;
        }

        if(url) {
            pkg.newTracker(port, function(tracker) {
                trackers.push(tracker);

                port.onDisconnect.addListener(function() {
                    var index = trackers.indexOf(tracker);
                    if(index != -1) {
                        delete trackers[index];
                    }

                    console.info("[main>onDisconnect] detached from %s", url);
                });
            });
        }
    });

    settings.onLoaded(function() {
        cleanupVideos();
    });

    if(chrome.alarms) {
        chrome.alarms.onAlarm.addListener(function(alarm) {
            if(alarm.name !== 'cleanupVideos') {
                return;
            }

            cleanupVideos();
        });

        chrome.alarms.create("cleanupVideos", {
            periodInMinutes: 60
        });
    } else {
        console.warn('[main] chrome.alarms API is not available');
    }
})(this.VR || (this.VR = {}));
