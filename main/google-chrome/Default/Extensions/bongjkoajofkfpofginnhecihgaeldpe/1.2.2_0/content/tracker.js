/* Copyright (c) Appteligent, 2012 All Rights Reserved */
(function(pkg) {
    var vidRx = /(?:v=|\/)([A-Za-z0-9\-_]{11})(?:.*#a?t=(\d+)s?)?/,
        storage = pkg.newChromeStorage(),
        console = pkg.console || console,
        settings = pkg.settings,
        TrackerProto = {
            onReady: function(playerType) {
                var offSecs = this.offSecs;

                console.info("[onReady] Player type " + playerType +
                             ", initial offset " + (offSecs||0) + " second(s)");

                // We direct content script to position the video if we have
                // offSecs property set
                if(offSecs) {
                    this.port.postMessage({name: "seekTo", data: offSecs});
                    console.info("[onReady] Told content script to resume from " +
                                 offSecs + " second(s)");
                }
            },
            onTimeupdate: function(offsetSeconds) {
                var video = this.video;
                video.offsetSeconds = Math.floor(offsetSeconds);
                video.lastAccessed = Date.now();
                storage.set(this.videoId, video, function(err) {
                    if(err) {
                        console.error("[onTimeupdate>storage.set] Error: %s", err);
                        cleanup(); // video will be updated next time this is called
                    }
                console.debug("[DBG] time updated tick.. ");
                });
            },
            onEnded: function() {
                var vid = this.videoId;
                storage.remove(vid, function(err) {
                    if(err) {
                        console.error("[onEnded>storage.remove] Error: %s", err);
                        return;
                    }

                    console.info("[onEnded] Video " + vid +
                                 " ended, removed from tracked videos list");
                });
            }
        };

    /**
     * @constructor
     * Creates an instance of Tracker object with given worker object.
     *
     * @param {Function} callback
     * @param {Object} port
     * @return {TrackerProto} tracker object
     */
    function newTracker(port, callback) {
        var url = port.name,
            vidInfo = vidRx.exec(url),
            tracker,
            vidId,
            offSecs,
            trackerProps = {
                port: { get: function() {return port;}},
                videoId: { get: function() {return vidId;}}
            };

        if(!vidInfo || vidInfo.length !== 3) {
            console.error("[newTracker] Worker's url(%s) does not match regexp(%s)",
                          url, vidRx);
            return null;
        }

        // Figure video id from the worker.url, pre-fetch video information
        // If url matched, then vidInfo[1] contains video id, and vidInfo[2] may
        // have initial time offset in seconds.
        vidId = vidInfo[1];
        offSecs = vidInfo[2] || 0;

        storage.get(vidId, function(err, vid) {
            var video,
                onReadyHandshook = false;

            if(err) {
                console.error("[newTracker>storage.get] Error: %s", err);
                return;
            }

            video = vid[vidId];

            if(video) {
                if(video.offsetSeconds && !offSecs) {
                    trackerProps.offSecs = {value: video.offsetSeconds};
                }
            } else {
                console.info("[newTracker] new video created: %s", vidId);
                video = {lastAccessed: Date.now(), offsetSeconds: offSecs};
            }

            trackerProps.video = { get: function() { return video; } };

            tracker = Object.create(TrackerProto, trackerProps);

            // Bind to events
            port.onMessage.addListener(function(msg) {
                switch(msg.name) {
                case "ready":
                    if(onReadyHandshook) return;
                    onReadyHandshook = true;
                    tracker.onReady(msg.data);
                    console.info("[tracker>ready] %s", vidId);
                    break;
                case "timeupdate":
                    tracker.onTimeupdate(msg.data);
                    console.info("[tracker>timeupdate] %s: %is", vidId, msg.data);
                    break;
                case "ended":
                    tracker.onEnded();
                    console.info("[tracker>ended] %s", vidId);
                    break;
                default:
                    console.warn("[main>onMessage] unexpected message " +
                                 "received: %s", msg.name);
                }
            });

            // Let other party know that the tracker is ready
            if(offSecs === 0)
                port.postMessage({name: "ready", data: video});

            callback(tracker);
        });
    }

    /**
     * Removes videos, which are older than configured number of days
     */
    function cleanup() {
        var cutoffTimeMillis =
                Date.now() - settings.vr_forget_after_days * 24 * 3600 * 1000;

        storage.get(null, function(err, vids) {
            var vidsToErase = [];
            if(err) {
                console.error("[cleanup] Failed to get all videos");
                return;
            }


            vidsToErase = Object.keys(vids).filter(function(k) {
                if(k.indexOf('vr_') === 0) {
                    // Ignore settings, keys, whose values begin with 'vr_'
                    return false;
                }

                return vids[k].lastAccessed < cutoffTimeMillis;
            });

            if(vidsToErase.length === 0)
                return;

            storage.remove(vidsToErase, function(err) {
                if(err) {
                    console.error("[cleanup>storage.remove] Failed to remove",
                                  vidsToErase.length, "videos");
                    return;
                }

                console.info("[cleanup>storage.remove] Removed ",
                             vidsToErase.length, "videos");
            });
        });
    }

    pkg.newTracker = newTracker;
    pkg.cleanupVideos = cleanup;
})(this.VR || (this.VR = {}));
