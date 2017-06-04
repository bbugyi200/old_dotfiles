// Copyright 2011 Google Inc. All Rights Reserved.

/**
 * @fileoverview Google Analytics for Google Dictionary extension.
 * @author sadovsky@google.com (Adam Sadovsky)
 */

'use strict';

var _gaq = _gaq || [];
_gaq.push(['_setAccount', 'UA-23514435-1']);
// Sample 10% of events to reduce the load on Google Analytics.
_gaq.push(['_setSampleRate', '10']);
// Note, we do not call _trackPageview since Chrome arbitrarily stops and starts
// background pages, rendering the pageview count meaningless and overwhelming
// the Analytics service.

(function() {
  var ga = document.createElement('script');
  ga.type = 'text/javascript';
  ga.async = true;
  ga.src = 'https://ssl.google-analytics.com/ga.js';
  var s = document.getElementsByTagName('script')[0];
  s.parentNode.insertBefore(ga, s);
})();
