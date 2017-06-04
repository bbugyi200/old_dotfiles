var getMatches = function(str, regex) {
  var out = null;
  if (regex.test(str))
    out = regex.exec(str);
  
  return(out);
};

var qsToObj = function(str) {
  var nvpair = {};
  if (str != "") {
    var pairs = str.split('&');
    $.each(pairs, function(i, v){
      var pair = v.split('=');
      nvpair[pair[0]] = pair[1];
    });
    return nvpair;
  } else {
    return {};
  }
}

var camelHump = function(url) {
  this.content_url = url;
  this.is_retailer = false;
  this.asin = null;
  this.asins_ar = [];
  this.asins_hash = {};
  this.domain = null;
  this.locale = null;
  this.request_url = null;
  this.asins_scrape_error = null;
  this.retailer_name = "";
  
  this.isValid = function() {
    return(this.request_url);
  };
  
  this.detectRetailer = function() {
    if (this.isAmazon()) {
      this.retailer_name = "Amazon";
      this.runAmazon();
    } else {
      return(null);
    }
    this.is_retailer = true;
    return this.getRequestUrl();
  };
  
  this.getRequestUrl = function() {
    if (this.is_retailer) {
      var camel_url = "https://" + this.domain 
      if (this.asin)
        camel_url += "/chromelizer/" + this.asin + "?locale=" + this.locale + "&ver=5&url=" + escape(this.content_url);
      this.request_url = camel_url;
      return this.request_url;
    }
  };

  this.isAmazon = function()
  {
    url = this.content_url;
    
    // duplicating logic from main.js for some reason
    var domains = [
      /https?:\/\/(?:.*\.)?amazon\.com/,
      /https?:\/\/(?:.*\.)?amazon\.co\.uk/,
      /https?:\/\/(?:.*\.)?amazon\.fr/,
      /https?:\/\/(?:.*\.)?amazon\.de/,
      /https?:\/\/(?:.*\.)?amazon\.ca/,
      /https?:\/\/(?:.*\.)?amazon\.co\.jp/,
      /https?:\/\/(?:.*\.)?amazon\.cn/,
      /https?:\/\/(?:.*\.)?amazon\.es/,
      /https?:\/\/(?:.*\.)?amazon\.it/
    ];
  
    for (var i=0; i<domains.length; i++) {
      regex = domains[i];
      if (url.match(regex)) {
        return true;
      }
    }
    return false;
  };
  
  this.extractAmazonAsin = function(str) {
    regexs = new Array(
      /ASIN\.1=([A-Z0-9]{10,13})(\/|$|\?|\%|\ )?/i,
      /ASIN=([A-Z0-9]{10,13})(\/|$|\?|\%|\ )?/i,
      /dp\/([A-Z0-9]{10,13})(\/|$|\?|\%|\ )?/i,
      /dp\/product\-description\/([A-Z0-9]{10,13})(\/|$|\?|\%|\ )?/i,
      /product\/([A-Z0-9]{10,13})(\/|$|\?|\%|\ )?/i,
      /.*?offer\-listing\/([A-Z0-9]{10,13})(\/|$|\?|\%|\ )?/i,
      /product\-reviews\/([A-Z0-9]{10,13})(\/|$|\?|\%|\ )?/i,
      /dp\/premier\/([A-Z0-9]{10,13})(\/|$|\?|\%|\ )?/i,
      /d\/.*?\/.*?\/([A-Z0-9]{10,13})(\/|$|\?|\%|\ )?/i
     );

    if (str.match(/\/gp\/slredirect\/redirect\.html/)) {
      // this is an ad on an amazon page.
      return false;
    }
    for (i=0; i<regexs.length; i++) {
      regex = regexs[i];
      var asin = getMatches(str, regex);
      if (asin) {
        return(asin[1]);
      }
    }
    return null;
  };
  
  this.injectGetAsin = function(callback) {
    if (this.retailer_name == "Amazon") {
      this.injectGetAmazonAsin(callback);
    } else {
      callback();
    }
  };
  
  this.injectGetAmazonAsin = function(callback) {
    injectedScripts.getAsin(this.content_url, function(asin) {
      if (typeof asin != "undefined" && asin != null) {
        this.asin = asin;
      }
      callback();
    }.bind(this));
  };

  this.runAmazon = function()
  {
    subdomain = this.content_url.substring(0, this.content_url.indexOf("/", 8));
    
    if (!subdomain)
      return(false);
    
    subdomain = (m = subdomain.match(new RegExp("\.([a-z,A-Z]{2,6})$") )) ? m[1] : false;
    
    if (!subdomain)
      return(false);
    
    if (subdomain == "com")
      subdomain = "US";
    else
    {
      subdomain = subdomain.split(".");
      subdomain = subdomain[subdomain.length - 1].toUpperCase();
    }
    
    this.domain = subdomain.toLowerCase() + ".camelcamelcamel.com";
    this.locale = subdomain;
    this.asin = this.extractAmazonAsin(this.content_url);
  };

  this.isWishlist = function() {
    if (this.isAmazon()) {
      regexes = [
        /\/registry\/wishlist/,
        /\/gp\/registry.*?type=wishlist/
      ]
      for (var i=0; i<regexes.length; i++) {
        regex = regexes[i];
        if (this.content_url.match(regex)) {
          return true;
        }
      }
    }
    return false;
  },

  this.add_url_list = function(urls) {
    this.asins_hash = {};
    this.asins_ar = [];
    this.asins_titles = {};
    for(url in urls) {
      h = new camelHump(url);
      h.detectRetailer();
      asin = h.asin;
      title = urls[url];
      if (asin && this.asins_hash[asin] == null && !url.match(/\/product\-reviews\//) && !url.match(/\/review\//)) {
        this.asins_hash[asin] = url;
        this.asins_ar.push(asin);
        this.asins_titles[asin] = title;
      }
    }
  };

  this.domainStripped = function() {
    if (this.domain.substring(0,3) == "us.") {
      return this.domain.replace(/^us\./, '');
    }
    return this.domain;
  };
  
};

if (typeof exports != "undefined") {
  exports.camelHump = function newCamelHump(url) {
    return new camelHump(url);
  }
}
