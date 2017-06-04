var camelizerList = {
  asins: [],
  loaded: false,

  init: function(camelhump) {
    camelizer.tabs_locked = true;
    camelizer.lockPage();
    this.asins = camelhump.asins_ar;
    this.urls = camelhump.asins_hash;
    this.camelhump = camelhump;
    this.camelhump.detectRetailer();
    this.lookup();
    this.results = {};
  },

  lookup: function() {
    var data = {"asins": this.asins, 
      "locale": this.camelhump.locale, 
      "page_asin": this.camelhump.asin,
      "asins_scrape_error": this.camelhump.asins_scrape_error,
      "json": true
    }
    $('.tabs li').removeClass('active');
    $('#camelizerListLink').addClass('active');
    camelizer.displayLoading("");
    $('#camelizerDisplayList').hide();
    
    var key = "asinlist:"+this.camelhump.content_url;
    var cachedResponse = tempCache.get(key);

    if (cachedResponse) {
      this.response = cachedResponse.response;
      $('#camelizerDisplayLoading').hide();
      if (cachedResponse.status == 200) {
        this.loadResults();
      } else {
        camelizer.displayError();
      }

    } else {
      var that = this;
      camelAsync.get('https://'+this.camelhump.domain + '/camelizer/list', data, function(response) {
        that.response = response.response;
        var key = "asinlist:"+that.camelhump.content_url;
        tempCache.set(key, response);
        $('#camelizerDisplayLoading').hide();
        if (response.status == 200) {
          that.loadResults();
        } else {
          if (response.status == 503) {
            camelizer.displayError(i18n.t("t_service_temporarily_unavailable"));
          } else {
            camelizer.displayError();
          }
        }
      }, 'json');
    }
  },

  loadResults: function() {
    $('#camelizerDisplayList').show();
    var container = $('#listsContainer');
    container.text('');
    var root_domain = this.camelhump.content_url.replace(/(https?:\/\/.*?)\/.*/, '$1');
    var base_path = this.camelhump.content_url.split("").reverse().join("").replace(/.*?(\/.*)/, '$1').split("").reverse().join("");
    var i = 0;
    var data = this.response;
    
    camelizer.logged_in = data.logged_in;
    _gaq.push(['_setCustomVar', 4, "Farmer", (camelizer.logged_in === true ? "Yes" : "No")]);
    camelizer.login = data.login;
    camelizer.showLogin();
    var a, url, title, ul3 = null;
    
    if (data.asin) {
      a = $("<a>", {class: "visited", href: "#", title: data.title});
      a.text(data.title);
      a.on("click", {asin: data.asin, url: this.camelhump.content_url}, this.linkClick);
      container.append($("<h5>").append(a));
    }
    var ul = $("<ul>", {class: "productList"});
    var ul2 = $("<ul>", {class: "productList second"});
    if (data.product_count > 25) {
      ul.addClass("first");
    }
    var product_added_count = 0;
    $.each(data.products, function(category, products) {
      active_ul = ul;
      if (product_added_count > 25) {
        active_ul = ul2;
      }
      active_ul.append($("<li>", {class: "category"}).text(category));
      $.each(products, function(i, product) {
        product_added_count ++;
        url = this.urls[product.asin];
        if (url) {
          if (!url.match(/http(s|):\/\//)) {
            if (url.substr(0,1) == '/') {
              url = root_domain + url;
            } else {
              url = base_path + url;
            }
          }
        }
        a = $("<a>", {href: url, title: product.full_title}).text(product.title);
        a.on("click", {asin: product.asin, url: url}, this.linkClick);
        active_ul.append($("<li>").append(a));
      }.bind(this));
    }.bind(this));
    
    if (data.missing_asins && data.missing_asins.length > 0) {
      ul3 = $("<ul>").append($("<li>", {class: "category"}).text(i18n.t('t_we_re_not_tracking_but_will_be_soon')));
      var link_count = 0;
      $.each(data.missing_asins, function(i, asin) {
        title = this.camelhump.asins_titles[asin];
        if (!title) {
          title = asin;
        }
        ul3.append($("<li>").text(title));
      }.bind(this));
    }
    
    container.append(ul, ul2, ul3);

    this.loaded = true;
    camelizer.tabs_locked = false;
    camelizer.unlockPage();
    camelizer.trackPage('products_list');
  },

  linkClick: function(e) {
    e.preventDefault();
    _gaq.push(['_trackEvent', "productsOnThisPageItemClick", e.data.asin]);
    $(e.target).addClass('visited');
    camelizer.start(e.data.url);
    return false;
  }

};
