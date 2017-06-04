/* Copyright (c) Appteligent, 2012 All Rights Reserved */
(function(pkg) {
	var YT_PLAYER_APIs = ['seekTo', 'getCurrentTime', 'getDuration',
                          'getPlayerState', 'getVideoLoadedFraction',
                          'getVideoUrl'],
		console = pkg.console || console,
		settings = pkg.settings,
		trackerTimerId,
		lastTrackedTime = 0,
		trackingIntervalMillis = 1000,
		initialOffsetSeconds = 0,
		port,
		playerInitWaitTimerId,
		playerInitWaitTimerIntervalMillis = 250,
        trackUrl = window.location.href,
		HTML5PlayerAdapter = {
			seekTo : function(offsetSeconds, dummy) {
				this.player.currentTime = offsetSeconds;
			},
			getCurrentTime : function() {
				return this.player.currentTime;
			},
			getDuration : function() {
				return this.player.duration;
			},
			getPlayerState : function() {
				return this.player.ended ? 0 : this.player.paused ? 2
						: this.player.networkState === 2 ? 3 : !this.player.ended
								&& !this.player.paused ? 1 : -1;
			},
			pauseVideo : function() {
				this.player.pause();
			}
        },
        player = getPlayer();

	/**
	 * Attempts to locate and return initialized YT player object. Returns null
	 * if not found.
	 *
	 * @returns {Object} player object or null
	 * @private
	 */
	function getPlayer() {
		var player = findFlashPlayer(),
			html5Player;

		if (!player) {
			// See if there is an HTML5 player
			html5Player = document.getElementsByTagName('video')[0];
			if (html5Player && html5Player.readyState >= 1/* Have meta data */) {
				player = Object.create(HTML5PlayerAdapter, {
					player : {
						get : function() {
							return html5Player;
						}
					},
					isHTML5 : {
						get : function() {
							return true;
						}
					}
				});
			}
		} else {
			if (player.getVideoLoadedFraction() === 0)
				return null; // Flash player is not ready yet
		}

		return player;
	}

    /**
     * Scans current DOM tree for embed elements, finds one, which implements
     * YouTube player API and returns it, otherwise returns null
     * @returns {object|null}
     */
    function findFlashPlayer() {
        var players = document.getElementsByTagName('embed'),
            player = null;

        // If there is a single embed element, chances are that we are looking
        // at one, embedded in an iframe in which case we can't rely on its being
        // named or id'd
        if(players.length === 1) {
            player = players[0];
            return isYouTubePlayer(player) ? player : null;
        }

        // we are looking for an embed element, which has 'name' and 'id'
        // attributes set to 'movie_player'
        Array.prototype.some.call(players, function(p) {
            if(p.name === 'movie_player' &&
                    p.id === 'movie_player'  &&
                    isYouTubePlayer(p)) {
                player = p;
            }
            return  player !== null;
        });

        return player;
    }

    /**
     * Checks given object to see if it supports API of a YouTube player. Returns
     * true if so.
     *
     * @param {object} p
     * @returns {boolean}
     */
    function isYouTubePlayer(p) {
        return p && YT_PLAYER_APIs.every(function(f) {
            return typeof p[f] === 'function';
        });
    }

	function seekTo(player, offsetSeconds) {
		player.seekTo(offsetSeconds, true);
		// check if it worked
		return Math.abs(player.getCurrentTime() - offsetSeconds) <= 1;
	}

	function _seek(player, offsetSeconds) {
		if (seekTo(player, offsetSeconds)) {
			// check if it worked
			console.info("[seek] Advanced the player's " + "position to %is",
					offsetSeconds);
			initialOffsetSeconds = 0;
		} else {
			initialOffsetSeconds = offsetSeconds;
			console.info("[seek] Could not advance player's position to",
						 offsetSeconds, "remembered it for later");
		}
	}

	function setupPlayer(player, playerType) {
		port = chrome.extension.connect({
			name : window.location.href
		});

		port.onDisconnect.addListener(function() {
			console.warn("[port.onDisconnect] Background script unexpectedly "
					+ "disconnected the content script", port.name);
			trackTimeUpdate(false);
		});

		port.onMessage.addListener(function(msg) {
			var vid;
            try {
                if (msg.name === "seekTo") {
                    _seek(player, msg.data);
                } else if (msg.name === "ready") {
                    vid = msg.data;
                    console.info("[setupPlayer>ready] Received ready notification "
                            + " from the tracker, initial position(%is)",
                            vid.offsetSeconds);
                    // See if need to advance the position
                    if (vid.offsetSeconds > player.getCurrentTime()) {
                        _seek(player, vid.offsetSeconds);
                    }
                }
            } catch(error) {
                // If we get an exception out of seek attempt, it is likely
                // because the player became unusable, hence try to re-get it
                trackTimeUpdate(false);
                port.disconnect();
                waitForPlayer();
            }
		});

		port.postMessage({
			name : "ready",
			data : playerType || "flash"
		});

		trackTimeUpdate(true);
	}

	function trackTimeUpdate(track) {
		if (track && trackerTimerId) {
			// Already tracking
		} else if (track) {
			trackerTimerId = setInterval(
                function() {
                    var p = player || getPlayer(),
                        curTime,
                        secondsLeft;

                    // Make sure the video we're tracking is still the same
                    // as new YouTube interface now reuses the same player
                    // for different videos
                    if(window.location.href !== trackUrl) {
                        trackUrl = window.location.href;

                        trackTimeUpdate(false); // Just in case

                        port.disconnect();

                        // Re-get the player in case if switched from flash to
                        // html5 or vice-versa
                        p = player = getPlayer();
                        if(!p) {
                            waitForPlayer();
                            return;
                        }

                        // And load the new one
                        setupPlayer(p, 'youtube');

                        return;
                    }

                    if (p.getPlayerState() === 0/* Ended */) {
                        port.postMessage({
                            name : "ended"
                        });
                        trackTimeUpdate(false);
                        return;
                    }

                    if (initialOffsetSeconds) {
                        _seek(p, initialOffsetSeconds);
                    }

                    curTime = p.getCurrentTime();
                    secondsLeft = p.getDuration() - curTime;

                    if (secondsLeft <= settings.vr_forget_if_seconds_left) {
                        console.info("[yt>trackTimeUpdate] Ended tracking",
                                     "video because it has only",
                                     secondsLeft, "seconds left");
                        port.postMessage({
                            name : "ended"
                        });
                        trackTimeUpdate(false);
                    } else {
                        if (curTime !== lastTrackedTime) {
                            lastTrackedTime = curTime;

                            port.postMessage({
                                name : "timeupdate",
                                data : curTime
                            });
                        }
                    }
                }, trackingIntervalMillis);
		} else {
            if(trackerTimerId)
			    clearInterval(trackerTimerId);

			trackerTimerId = undefined;
			lastTrackedTime = 0;
		}
	}

	function waitForPlayer() {
        player = getPlayer();
		// This bit waits for YouTube player to initialize
		if (player) {
            trackUrl = window.location.href;
			console.info("[onload] Player found", player.getCurrentTime());
			setupPlayer(player, "youtube");
		} else if(!playerInitWaitTimerId) {
			console.info("[onload] There's no player yet, waiting...");
			// Wait for page scripts to initialize the YT player
			playerInitWaitTimerId = setInterval(function() {
				player = getPlayer();

				if (player) {
                    trackUrl = window.location.href;
					clearInterval(playerInitWaitTimerId);
					playerInitWaitTimerId = null;
					console.info("[onload>mutationObserver] Player found");
					setupPlayer(player, "youtube");
					console.info("[onload>mutationObserver] Player initialized");
				}
			}, playerInitWaitTimerIntervalMillis);
		}
	}

	waitForPlayer();
})(this.VR || (this.VR = {}));
