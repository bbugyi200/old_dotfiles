var tp_to_int = { "1m":1, "3m":2, "6m":3, "1y":4, "all":5 };
var R_PRODUCT_LOADED = 0;
var R_ERROR = 1;
var R_NOT_SUPPORTED = 2;
var R_PRODUCT_NOT_FOUND = 3;
var _gaq = _gaq || [];
var emailTipOver = false;
var priceTipOver = false;
var initialChartHeight = 343;
var ffWideView = false;

var camelizer = {
  tabs_locked: true,
  page_locked: false,
  logged_in: false,
  page_tracked: false,
  primary_page_load: true,
  last_pt: null,
  
  lockPage: function() {
    this.page_locked = true;
  },

  unlockPage: function() {
    this.page_locked = false;
    if (this.afterPageLockUrl) {
      this.onLoad(this.afterPageLockUrl);
    }
  },

  createNewContext: function(url) {
    var key = "camelizer_humps:"+url;
    var ctx = tempCache.get(key);
    if (ctx) {
      this.context = ctx;
    } else {
      this.context = new camelHump(url);
      this.context.detectRetailer();
      tempCache.set(key, this.context);
    }
  },
  
  getFileName: function() {
    var file_name = [];
    $('input[name=cpf]:checked').each(function(index, obj) {
      file_name.push($(obj).val())
    });
    return file_name;
  },

  constructChartURL: function() {
    var context, url, file_name, data, domain = null;
    context = this.context;
    if (context.isAmazon()) {
      file_name = this.getFileName();
      if (file_name.length == 0) {
        file_name = ["amazon"];
        $("#pt_amazon").attr("checked", "checked");
        console.log("no price types selected!");
      }
      file_name = file_name.join('-') + ".png"
    }
    var width = $('#camelizerChart').width();
    if (width < 550 || width > 600) {
      width = $('body').width() -20;
    }
    data = { 
      "force": 1,
      "desired": "true",
      "legend": 0,
      "ilt": 1,
      "tp": $('input[name=tp]:radio:checked').val(),
      "zero": ($('#close_up_view:checked').length == 0 ? 1 : 0),
      "fo": $('#remove_extreme_values:checked').val(),
      "h": this.desiredChartHeight(),
      "w": width,
      "lang": i18n.textdomain()
    }
    domain = context.domain;
    domain = domain.split(".").reverse().slice(0,2);
    domain = domain.reverse().join('.');
    url = 'https://charts.' + domain + '/' + context.locale.toLowerCase() + '/' + context.asin + '/' + file_name + '?' + $.param(data);
    return url;
  },

  desiredChartHeight: function() {
    var h = initialChartHeight;
    if (ffWideView) {
      $("#camelizerUrlParent").css("top", h + "px");
      h -= (3*19); // 3 legend rows of space always for the wide view.
    } else {
      rows = $(".legend_row").filter(function() {
        return ($(this).css('display') == 'table-row');
      }).length;
      h -= (rows*19);
    }
    return h;
  },

  saveFormSettings: function() {
    formSettings = {}
    formSettings["date_range"] = $('input[name=tp]:radio:checked').attr('id');
    formSettings["close_up_view"] = ($('#close_up_view:checked').length == 1 ? true : false);
    formSettings["remove_extreme_values"] = ($('#remove_extreme_values:checked').length == 1 ? true : false);
    if (camelizer.context.isAmazon()) {
      formSettings["pt_amazon"] = ($('#pt_amazon:checked').length == 1 ? true : false);
      formSettings["pt_new"] = ($('#pt_new:checked').length == 1 ? true : false);
      formSettings["pt_used"] = ($('#pt_used:checked').length == 1 ? true : false);
    }
    $.each(formSettings, function(key, val) {
      camelizerStore.set(key, val);
    });
  },

  ensureSelectedNotDisabled: function(callback) {
    camelizerStore.getMulti({"date_range":null}, function(data) {
      is_disabled = $('#'+data.date_range).button("option", "disabled");
      if (is_disabled && $('#'+data.date_range+':checked').length == 1) {
        // set date range to all
        $('#toggle_tp_5').attr('checked', 'checked');
        $('#camelizer_radio label').removeClass('ui-state-active');
        $('label[for="toggle_tp_5"]').addClass('ui-state-active');
      } else if (!camelizer.saveDefaults && $('#'+data.date_range).button('option', 'disabled') == false) {  
        // use case: first loads a product where default is disabled, then clicks on other products, and then on a product where their default can be enabled.
        //  check that saveDefaults is false because if they are still on the first product and change the time-period, we don't want to show their existing default.
        val = tp_to_int[$('#'+data.date_range).val()];
        $('#'+data.date_range).attr('checked', 'checked');
        $('#camelizer_radio label').removeClass('ui-state-active');
        $('label[for="'+data.date_range+'"]').addClass('ui-state-active');
      }
      callback();
    });
  },

  checkTimePeriods: function() {
    $('input[name=tp]').button('enable');
    created_at = Date.parse(context.product_data.created_at)/1000; 
    now = new Date().getTime()/1000;
    days_to_ids = {"30":"1", "91":"2", "182":"3", "365":"4"}
    for(days in days_to_ids) {
      if (now-(days*86400) < created_at) {
        $('#toggle_tp_'+days_to_ids[days]).button("disable");
      }
    }
  },

  updateChart: function(saveDefaults) {
    this.lockPage();
    camelizer.tabs_locked = true;
    this.saveDefaults = saveDefaults
    context = camelizer.context;
    this.checkTimePeriods();
    this.ensureSelectedNotDisabled(function() {
      if (camelizer.saveDefaults) {
        camelizer.saveFormSettings();
      }
      camelizer.finishWatchForm();
      url = camelizer.constructChartURL();
      if (context.product_data != null) {
        $("#camelizerWatchTab a").text(context.product_data.link_phrase);
      }
      
      $('#camelizerChart').one('error', function() { // handle server side errors for chart rendering - don't just leave the spinner going forever.
        $('.throbber').css('visibility', 'hidden');
        camelizer.hideAllDisplays();
        $("#camelizerDisplayError").show();
        camelizer.tabs_locked = false;
        camelizer.unlockPage();
      });

      $('#camelizerChart').one('load', function() {
        camelizer.chartLoaded();
      });
      
      if (url == $('#camelizerChart').attr('src')) {
        camelizer.chartLoaded();
      } else {
        $('#camelizerChart').attr('src', url);
      }
    });
  },

  chartLoaded: function() {
    $('.throbber').css('visibility', 'hidden');
    camelizer.hideAllDisplays();
    $("#camelizerDisplayChart").show();
    $("#productPageLink").hide();
    $("#camelProductPageLink a").attr("href", context.product_data.product_url.replace(/(.*?)\?.*/, "$1"));
    if(camelizer.context.asin != camelizer.first_asin) {
      var text = i18n.t("t_view_product_on_retailer", {"retailer": context.retailer_name});
      $('#footerProductPageLink').attr('href', context.content_url).text(text);
      $('#productPageLink').show();
    }
    if (camelizer.context.asins_ar.length > 1) {
      $('#camelizerListLink').show();
    }
    camelizer.tabs_locked = false;
    camelizer.unlockPage();
  },

  onLoad: function(url) {
    if (this.page_locked) {
      this.afterPageLockUrl = url;
      return false;
    }
    
    startI18n(function() {
      var working_url = url;
      this.lockPage();
      this.tabs_locked = true;
      this.afterPageLockUrl = null;
      wishlistController.scraped = false;
      wishlistController.scrape_ok = false;
      wishlistController.checked_imported = false
      wishlistController.imported_wishlists = [];
      wishlistController.wishlists = [];
      camelizerList.loaded = false;
      
      $("#footer").width($("body").width());
      this.hideAllDisplays();
      wishlistController.scraped = false;
      wishlistController.checked_imported = false;
      this.createNewContext(working_url);
      this.context.injectGetAsin(function() {
        this.first_asin = this.context.asin;
        if (!this.boundCommon) {
          bindCommon(function() {
            this.processUrl(working_url);
          }.bind(this));
        } else {
          this.processUrl(working_url);
        }
      }.bind(this));
    
    }.bind(this));

  },

  processUrl: function(url) {
    if (url.substring(0,4) == "http") {
      $("#camelizerListLink").show();
      getAsins(this.context, function(hump) {
        var key = "camelizer_humps:"+url;
        tempCache.set(key, hump);
        this.context = hump;
        if(hump.isWishlist()) {
          wishlistController.start();
        } else if (hump.asin == null && (hump.asins_ar.length > 0 || hump.asins_scrape_error != null)) {
          camelizerList.init(hump);
        } else {
          camelizerList.loaded = false;
          this.start(url);
        }
      }.bind(this));
    } else {
      camelizer.displayNotSupported();
      camelizer.tabs_locked = true;
      camelizer.unlockPage();
    }

  },
    
  start: function(url) {
    this.lockPage();
    this.tabs_locked = true;
    wishlistController.hideErrors();
    $('.tabs li').removeClass('active');
    $('#camelizerChartLink').addClass('active');
    this.createNewContext(url);
    if (this.context.request_url){
      this.displayLoading("");
      this.initiateProductLookup();
    } else {
      this.context.product_data = {"result_code": R_NOT_SUPPORTED};
      this.updateDisplay();
      this.tabs_locked = true;
      this.unlockPage();
    }
  },
  
  initiateProductLookup: function() {
    var key = "data:"+this.context.asin;
    if (tempCache.get(key)) {
      camelizer.context.product_data = tempCache.get(key);
      this.updateDisplay();
    } else {
      this.getSecrets(function(secrets) {
        camelAsync.get(camelizer.context.getRequestUrl(), secrets, function(data) {
          try {
            if (data.status == 503) {
              camelizer.displayError(i18n.t("t_service_temporarily_unavailable"));
            } else {
            
              if (data.response.result_code == R_PRODUCT_LOADED && data.response.asin != camelizer.context.asin) {
                throw new Error("returned asin doesn't match");
              }
              var key = "data:"+camelizer.context.asin;
              tempCache.set(key, data.response);
              camelizer.context.product_data = data.response;
              camelizer.logged_in = data.response.a
              _gaq.push(['_setCustomVar', 4, "Farmer", (camelizer.logged_in === true ? "Yes" : "No")]);
              camelizer.login = data.response.login
              camelizer.showLogin();
              camelizer.updateDisplay();
              camelizer.trackPage('chart');
            }
          } catch (e) {
            console.log(e);
            console.log(e.message);
            console.log(e.stack);
            camelizer.context.product_data = {"result_code":1};
            camelizer.updateDisplay();
          }
        });
      });
    }
  },
  
  trackPage: function(initial_page) {
    if (!camelizer.page_tracked) {
      _gaq.push(['_setAccount', 'UA-10042935-22'],["_setDomainName", "none"]);
      _gaq.push(['_setAllowHash', false]);
      ABTester.getTests(function(results) {
        if (results) {
          var custom_start = 7;
          $.each(results, function(key, value) {
            _gaq.push(['_setCustomVar', custom_start, key, value]);
            custom_start ++;
          });
        }
        if (camelizer.context && camelizer.context.locale) {
          _gaq.push(['_setCustomVar', 3, "Locale", camelizer.context.locale.toLowerCase()]);
        }
        _gaq.push(['_setCustomVar', 4, "Farmer", (camelizer.logged_in === true ? "Yes" : "No")],
        ['_setCustomVar', 6, "firefoxWideView", ffWideView],
        ['_trackPageview', '/'+initial_page]);
        camelizer.page_tracked = true;
        add_ga_script();
      });
    } else if(camelizer.primary_page_load == false) {
      camelizer.primary_page_load = true;
      _gaq.push(['_trackEvent', "secondaryPageLoad", initial_page]);
    }
  },

  hideAllDisplays: function() {
    $("#pleaseSigninToAmazon").hide();
    $("#wishlistImporter").hide();
    $("#noWishlists").hide();
    $("#camelizerNoProduct").hide();
    $("#camelizerDisplayLoading").hide();
    $("#camelizerDisplayError").hide();
    $("#camelizerDisplayChart").hide();
    $("#camelizerDisplayList").hide();
    $("#camelizerDisplayNotSupported").hide();
    $("#camelizerDisplayProductNotFound").hide();
  },
  
  updateDisplay: function() {
    ctx = this.context;
    switch (ctx.product_data.result_code)
    {
    case R_PRODUCT_LOADED:
      if (typeof ctx.product_data.not_trackable_error != "undefined") {
        camelizer.displayError(ctx.product_data.not_trackable_error);
      } else {
        camelizer.displayLoading(i18n.t("t_product_data_downloaded_loading_chart_image"));
        camelizer.updateChart(false);
      }
    break;
    
    case R_ERROR:
      camelizer.displayError();
    break;
    
    case R_NOT_SUPPORTED:
      camelizer.displayNotSupported(ctx);
    break;
    
    case R_PRODUCT_NOT_FOUND:
      camelizer.displayProductNotFound(ctx);
    break;
    
    default:
      camelizer.displayError();
    break;
    
    }
  },
  
  
  displayError: function(msg) {
    if (typeof msg == "undefined" || !msg) {
      msg = "An error has occurred."
    }
    camelizer.hideAllDisplays();
    $("#camelizerDisplayError").show();
    $("#camelizerErrorText").text(msg);
    camelizer.tabs_locked = false;
    camelizer.unlockPage();
  },
  
  displayLoading: function(msg) {
    camelizer.hideAllDisplays();
    $("#camelizerDisplayLoading").show();
    camelizer.setLoadingText(msg ? msg : i18n.t("t_loading"));
  },
  
  displayNotSupported: function(ctx) {
    camelizer.hideAllDisplays();
    $("#camelizerDisplayNotSupported").show();
    $("#camelizerListLink").hide();
    camelizer.tabs_locked = true;
    camelizer.unlockPage();
    camelizer.trackPage("not_supported");
  },
  
  displayProductNotFound: function(ctx) {
    camelizer.hideAllDisplays();
    $("#camelizerDisplayProductNotFound").show();
    camelizer.tabs_locked = false;
    camelizer.unlockPage();
  },
  
  setLoadingText: function(msg) {
    $("#camelizerLoadingText").text(msg);
  },

  finishWatchForm: function() {
    $("#legend tbody tr[id^=legend_price]").hide();
    if (this.logged_in === true) {
      $(".watchEmail").hide();
      $("#watchNote").show();
    } else {
      $("#watchNote").hide();
      $(".watchEmail").show();
      camelizerStore.getMulti({"email":null}, function(data) {
        $("#emailPriceWatch").val(data.email);
      });
    }
    $("#legend input[type=submit]").hide();
    $("#legend .delete").hide();
    create_update = "create";
    length = 0;
    for (key in this.context.product_data.prices) {
      length += 1;
      $("#legend_"+key).show();

      val = this.context.product_data.prices[key];
      $("#"+key+"_current").text(price(val));
      
      low = this.context.product_data.lowest_pricing[key];
      if (low != null) {
        $("#"+key+"_low").text(price(low.price) + " (" + low.created_at  + ")");
      } else {
        $("#"+key+"_low").text("-");
      }
      high = this.context.product_data.highest_pricing[key];
      if (high != null) {
        $("#"+key+"_high").text(price(high.price) + " (" + high.created_at  + ")");
      } else {
        $("#"+key+"_high").text("-");
      }

      camel = this.context.product_data.camels[key];
      if (camel) {
        $("input[name="+key+"]").val(toMoney(camel.price/100));
        create_update = "update";
        $("#legend_"+key+" .delete").show();
      } else {
        $("input[name="+key+"]").val("");
      }
    }
    plural = length > 1 ? "es" : "";
    $("#"+create_update+"_price_watch"+plural).show();
    $(".currency_symbol").text(this.context.product_data.currency_symbol);
    if (this.context.isAmazon()) {
      $("#graph_icon").show();
    }
  },

  saveSecretKeys: function() {
    if (this.logged_in === false) {
      data = {}
      for (price_type in this.context.product_data.camels) {
        camel = this.context.product_data.camels[price_type];
        data[price_type] = camel.secret_key;
      }
      key = this.context.asin +":"+ this.context.locale.toLowerCase();
      camelizer.context.secret_keys = data;
      camelizerStore.set(key, data);
    }
  },

  getSecrets: function(getSecretsCallback) {
    key = this.context.asin +":"+ this.context.locale.toLowerCase();
    dic = {};
    dic[key] = null;
    camelizerStore.getMulti(dic, function(data) {
      camelizer.context.secret_keys = data[key];
      getSecretsCallback({"secret_keys": data[key]});
    });
  },

  showLogin: function() {
    $(".loginLink").attr("href", "https://" + camelizer.context.domainStripped() + "/login");
    $(".signupLink").attr("href", "https://" + camelizer.context.domainStripped() + "/signup");
    if (this.logged_in == true) {
      if (this.context.isAmazon()) {
        $("#addWishlistLink").show();
      }
      $(".signupLogin").hide();
      
      var anchor = $("<a>", {target: '_blank', href: 'https://' + camelizer.context.domainStripped() + "/user/" + this.login}).text(this.login)
      var loggedin_dom = i18n.t("t_signed_in_as", {"username": anchor}, {"return": "dom"});
      $("#loggedInAs").text("");
      $("#loggedInAs").append(loggedin_dom);
      $("#loggedInAs").show();
    } else {
      $("#addWishlistLink").hide();
      $(".signupLogin").show();
      $("#loggedInAs").hide();
    }
  },

  toggleRow: function(checkbox) {
    tr = $(checkbox).closest("tr");
    if (checkbox.checked) {
      tr.removeClass("unchecked"); 
    } else {
      tr.addClass("unchecked");
    }
  },

  confirm: function(msg, callback) {
    $("#confirmBox").show();
    $("#camelizerContentBox, #footer").css("opacity", 0.2);
    $("#confirmMessage").text(msg);
    y = ($("body").height()-$("#confirmBox").outerHeight())/2;
    x = ($("body").width()-$("#confirmBox").outerWidth())/2;
    $("#confirmBox").css({"top":y+"px", "left":x+"px"});
    // remove all event handlers
    $("#confirmOK, #confirmCancel").unbind();
    $("#confirmOK").focus();
    $("#confirmOK").one("click", function(e) {
      $("#confirmBox").hide();
      $("#camelizerContentBox, #footer").css("opacity", 1);
      callback(true);
    });
    $("#confirmCancel").one("click", function(e) {
      $("#confirmBox").hide();
      $("#camelizerContentBox, #footer").css("opacity", 1);
      callback(false);
    });
  }

}

$(document).ready(function() {
  // it seems this DOES NOT fire reliably in the case of a downgrade.... or I'm too stupid to figure out why it wasn't working. Anyway, to ensure things work, best not have any code here!
  // to simulate a downgrade, drag an xpi of an existing installed xpi on to firefox.
});

function bindCommon(bindCommonCallback) {

  if (camelizer.boundCommon) {
    return;
  }

  camelizer.boundCommon = true;

  $('#camelizer_radio').buttonset();
  
  $('input[name=tp]:radio').change(function(e) {
    _gaq.push(['_trackEvent', "dateRangeButtonClick", $(e.target).val()]);
    $(this).closest('.controlbox').find('.throbber').css('visibility', 'visible');
    $("#camelizer_radio label").removeClass("ui-state-active");
    tp = $('input[name=tp]:radio:checked').val();
    $("label[for=\""+$(this).attr('id')+"\"]").addClass("ui-state-active");
    camelizer.updateChart(true);   
  });
    
  $('.controlbox input[type="checkbox"]').change(function(e) {
    $(this).closest('.controlbox').find('.throbber').css('visibility', 'visible');
    _gaq.push(['_trackEvent', "chartOptionsClick", $(e.target).attr("id")+"_"+(e.target.checked ? "on" : "off")]);
    camelizer.updateChart(true);   
  });
  
  $('#legend input[type="checkbox"]').change(function(e) {
    camelizer.toggleRow(e.currentTarget);
    if ($('#legend input[type="checkbox"]:checked').length != 0 && camelizer.last_pt != $(this).attr("id")) {
      $('#legend_throbber').css('visibility', 'visible');
      price_type_str = $("#legend input[type=\"checkbox\"]:checked").map(function() {
        return this.id.replace(/pt_/,'');
      }).get().join(", ");
      _gaq.push(['_trackEvent', "chartPriceTypeClick", price_type_str]);
      camelizer.updateChart(true);   
      camelizer.last_pt = null;
    } else {
      camelizer.last_pt = $(this).attr("id");
    }
  });

  $('#camelizerListLink a').click(function(e) {
    _gaq.push(['_trackEvent', "tabsClick", "productsOnThisPageClicked"]);
    return doTab(e, function() {
      $("#productPageLink").hide();
      if (camelizerList.loaded) {
        $('#camelizerDisplayList').show();
      } else {
        camelizer.displayLoading("");
        camelizerList.init(camelizer.context);
      }
    });
  });
  
  $('#camelizerChartLink a').click(function(e) {  
    _gaq.push(['_trackEvent', "tabsClick", "chartClicked"]);
    return doTab(e, function() {
      if (camelizer.context.asin) {
        camelizer.updateDisplay();
        if(camelizer.context.asin != camelizer.first_asin) {
          $('#productPageLink').show();
        }
      } else{
        camelizer.hideAllDisplays();
        $("#camelizerNoProduct").show();
        return false;
      }
    });
  });
  
  $("#addWishlistLink a").click(function(e) {
    _gaq.push(['_trackEvent', "tabsClick", "wishlistClicked"]);
    return doTab(e, function() {
      wishlistController.start();
    });
    return false;
  });


  $("#camelizerNoProduct a").click(function(e) {
    $("#camelizerListLink a").click();
    return false;
  });

  $("#priceWatchForm").submit(function(e) {
    e.preventDefault();
    _gaq.push(['_trackEvent', "priceWatchForm", "clickedSubmit"]);
    $("#watchFormErrors").hide();
    $("#watchFormSaveError").hide();
    form = $(e.target);
    if (!numberEntered(form)) {
      return false;
    }
    if (!valueEntered(form)) {
      return false;
    }
    if (!emailEntered()) {
      return false;
    }
    _gaq.push(['_trackEvent', "priceWatchForm", "submittedOK"]);
    
    $("#watchSaving").show();
    url = "https://" + camelizer.context.domain + "/camelizer/track?";
    data = {"asin": camelizer.context.asin,
      "locale": camelizer.context.locale.toUpperCase(),
      "secret_keys": camelizer.context.secret_keys
      }
    url += $("#priceWatchForm").serialize() + "&" + $.param(data);
    camelAsync.get(url, null, function(data) {
      $("#watchSaving").hide();
      if (data.status == 200) {
        var start_count = 0;
        var start_camels = camelizer.context.product_data.camels;
        for (camel in start_camels) {
          start_count ++;
        }
        var end_count = parseInt(data.response.count);
        if (end_count > start_count) {
          _gaq.push(['_trackEvent', "priceWatchSuccess", "Created"]);
        } else if (end_count < start_count) {
          _gaq.push(['_trackEvent', "priceWatchSuccess", "Deleted"]);
        } else {
          _gaq.push(['_trackEvent', "priceWatchSuccess", "Edited"]);
        }
        camelizer.context.product_data.camels = data.response.camels
        camelizer.saveSecretKeys();
      } else {
        show_watch_error();
        $("#watchFormSaveError").show();
      }
      camelizer.finishWatchForm();
    });
    return false;
  });

  $(".delete").click(function(e) {
    e.preventDefault();
    img = $(this);
    camelizer.confirm(i18n.t("t_are_you_sure_you_want_to_delete_this_price_watch"), function(result) {
      if (result == true) {
        img.closest("td").find("input").val("");
        $("#priceWatchForm").submit();
      }
    });
    return false;
  });
  
  bindChartLinks();

  bindToolTips();

  bindEmailTip();
  
  // get saved form settings if there are any. if none, set defaults.
  dic = {"pt_amazon": true, "pt_new": true, "pt_used": true, "close_up_view": true, "remove_extreme_values": true, 'date_range': 'toggle_tp_5'};
  camelizerStore.getMulti(dic, function(data) {
    $.each(dic, function(key, val) {
      res = data[key];
      if (key == 'date_range') {
        $('#'+res).prop('checked', res);
        val = tp_to_int[$('#'+res).val()];
        $('label[for="toggle_tp_'+val+'"]').addClass('ui-state-active');
      } else {
        $('#'+key).prop('checked', res);
        if (key.substr(0,3) == "pt_") {
          camelizer.toggleRow(document.getElementById(key));
        }
      }
    });
    camelizer.boundCommon = true;
    if (typeof bindCommonCallback != "undefined") {
      bindCommonCallback();
    }
  });
  
};


function price(cents) {
  if (cents == null) {
    return $("#nullPriceText").text();
  }
  str = camelizer.context.product_data.currency_symbol;
  return str + toMoney(cents/100);
}


// input a dollar amount
toMoney = function(n) {
  locale = camelizer.context.locale.toLowerCase();
  if (locale == "jp") {
    n = (n*100).toFixed();
  }
  if (locale == "es" || locale == "de" || locale == "fr" || locale == "it")   {
    decimal_sep = ",";
    thousands_sep = ".";
  } else if (locale == "fr") {
    decimal_sep = ",";
    thousands_sep = " ";
  } else {
    decimal_sep = ".";
    thousands_sep = ",";
  }
  c = 2
  d = decimal_sep;
  t = thousands_sep;
  sign = (n < 0) ? '-' : '',

  //extracting the absolute value of the integer part of the number and converting to string
  i = parseInt(n = Math.abs(n).toFixed(c)) + '', 

  j = ((j = i.length) > 3) ? j % 3 : 0; 
  str = sign + (j ? i.substr(0, j) + t : '') + i.substr(j).replace(/(\d{3})(?=\d)/g, "$1" + t) + (c ? d + Math.abs(n - i).toFixed(c).slice(2) : ''); 
  len = str.length;
  str = str.substring(len-3, len) == ".00" ? str.substring(0, len-3) : str;
  if (locale == "jp") {
    return str.replace(/(.*)\..*/, "$1");
  }
  return str;
}


function localizedDollarsToCents(dollars) {
  locale = camelizer.context.locale.toLowerCase();
  if (locale == "jp") {
    return parseInt(dollars.replace(/,/, '').replace(/(.*)\..*/, "$1"));
  }
  if (locale == "es" || locale == "de" || locale == "fr" || locale == "it")   {
    decimal_sep = ",";
  } else {
    decimal_sep = ".";
  }
  var dec_pos = dollars.indexOf(decimal_sep);
  if (dec_pos == -1) {
    dollars += decimal_sep + "00";
  }
  var ar = dollars.split(decimal_sep);
  var dollar_cents = (ar[0].replace(/[^0-9]/g,'')*100);
  var cents = ar[1];
  if (cents.length == 1) {
    cents += "0";
  }
  cents = parseInt(cents);
  cents = dollar_cents + cents;
  return cents.toString();
}



function valueEntered() {
  $("#watchFormPriceError").hide();
  inputs = $("#priceWatchForm").find("input.price");
  ok_values = 0;
  bad_values = 0;
  for(i=0;i<inputs.length;i++) {
    input = $(inputs[i]);
    input.removeClass("invalid");
    if (input.val() != "") {
      current = camelizer.context.product_data.prices[input.attr("name")];
      var val_in_cents = localizedDollarsToCents(input.val());
      if (current == null || val_in_cents < current) {
        ok_values ++;
      } else {
        bad_values ++;
        input.addClass("invalid");
      }
    } else {
      camel = camelizer.context.product_data.camels[input.attr("name")];
      if (typeof camel != "undefined") {
        ok_values ++;
      }
    }
  }
  if (ok_values > 0 && bad_values == 0) {
    return true;
  }
  show_watch_error("watchFormPriceError");
  return false;
}

var watchFormErrorTimeout = null;

function show_watch_error(id) {
  clearTimeout(watchFormErrorTimeout);
  $("#watchFormErrors").show();
  $("#"+id).show();
  bottom = $("#legend").outerHeight() + 7;
  $("#watchFormErrors").css("bottom", bottom+"px");
  watchFormErrorTimeout = setTimeout(function() {
    $("#watchFormErrors").hide();
  }, 10000);
}

function numberEntered() {
  $("#watchFormNumberError").hide();
  inputs = $("#priceWatchForm").find("input.price");
  for(i=0;i<inputs.length;i++) {
    input = $(inputs[i]);
    input.removeClass("invalid");
    if (input.val() != "") {
      if(!input.val().match(/^[0-9\.,\s]*$/)) {
        show_watch_error("watchFormNumberError");
        input.addClass("invalid");
        return false;
      }
    }
  }
  return true;
}

function emailEntered() {
  $("#watchFormEmailError").hide();
  $("#emailPriceWatch").removeClass("invalid");
  if (camelizer.logged_in === true) {
    return true;
  }
  if ($("#emailPriceWatch").val() == "") {
    show_watch_error("watchFormEmailError");
    $("#emailPriceWatch").addClass("invalid");
    return false;
  } else {
    camelizerStore.set("email", $("#emailPriceWatch").val());
  }
  return true;
}

function doTab(e, callback) {
  e.preventDefault();
  if ($(e.target).closest('li').hasClass('active') || camelizer.tabs_locked) {
    return false;
  }
  camelizer.hideAllDisplays();
  $('.tabs li').removeClass('active');
  $(e.target).closest('li').addClass('active');
  callback(e);
  return false;
}

function bindToolTips() {
  
  $("#iconLink").tooltip({
    tip: "#iconTip",
    position: "top left",
    offset: [-1, 25]
  });

  $('input.price').tooltip({
    tip: "#tooltip",
    position: "top left",
    offset: [0, 60],
    events: {
      def:     "mouseover,mouseout",
      input:   ",",
      widget:  "focus mouseover,blur mouseout",
      tooltip: "mouseover,mouseout"
    },
    onBeforeShow: function(e, pos) {
      input = $(e.target);
      price_type = input.attr("name");
      base = camelizer.context.product_data.last_price[price_type];
      if (base == null) {
        for (pt in camelizer.context.product_data.last_price) {
          base = camelizer.context.product_data.last_price[pt];
          if (base) {
            break;
          }
        }
      }
      if (!base) {
        // don't show suggested prices if we know nothing about the price
        return false;
      }
      if (camelizer.context.locale.toLowerCase() == "jp") {
        $("#discount_minimum").text("1");
      }
      discounts = new Array(1, 0.9, 0.75, 0.5, 0.25, 0.1);
      that = this;
      $("#tooltip tbody td").each(function(index, td) {
        if (index != 0) {
          discount = discounts[index-1]
          if (index == 1) {
            suggest_amt = base-discount;
            discount_str = "$0.01";
          } else {
            suggest_amt = base*discount;
            discount_str = ((1-discount)*100).toFixed(0)+"%";
          }
          var a = $("<a>", {"href": "#", "class": "discount "+discount_str, "rel": (suggest_amt/100).toFixed(2)}).text(price(suggest_amt)).click(function(e) {
            _gaq.push(['_trackEvent', "watchSuggestAmountClicked", $(e.target).attr("class")]);
            e.preventDefault();
            input.val(toMoney($(e.target).attr("rel")));
            that.getTip().hide();
            return false;
          });
          $(td).text("").append(a);
        }
      });
    }
  });

  $("input.price").focus(function(e) {
    $(e.target).data("tooltip").hide();
    $(e.target).data("tooltip").show();
  });
  
  $("#tooltip .tooltip").mouseenter(function(e) {
    priceTipOver = true;
  });

  $("#tooltip .tooltip").mouseleave(function(e) {
    priceTipOver = false;
    if (!$("input.price").is(":focus")) {
      $("input.price").data("tooltip").hide();    
    }
  });

  $('input.price').blur(function(e) {
    if (!priceTipOver) {
      $(e.target).data("tooltip").hide();    
    }
  });

  // to deal with widths when user has zoomed their browser.
  $("#footer").width($("body").width());


}

function bindEmailTip() {
  $('#emailPriceWatch').tooltip({
    tip: "#emailTip",
    position: "top center",
    offset: [0, 45],
    events: {
      def:     "mouseover,mouseout",
      input:   "focus,",
      widget:  "focus mouseover,blur mouseout",
      tooltip: "mouseover,mouseout"
    },
    onShow: function(e) {
      $("#tooltip").hide();
    }
  });

  $("#emailTip .tooltip").mouseenter(function(e) {
    emailTipOver = true;
  });

  $("#emailTip .tooltip").mouseleave(function(e) {
    emailTipOver = false;
    if (!$("#emailPriceWatch").is(":focus")) {
      $('#emailPriceWatch').data("tooltip").hide();    
    }
  });

  $('#emailPriceWatch').blur(function(e) {
    if (!emailTipOver) {
      $('#emailPriceWatch').data("tooltip").hide();    
    }
  });
}

function addTracking(a) {
  _gaq.push(['_trackEvent', "camelizerClickLinkOut", a.href, a.id]);
  if (!$(a).hasClass("no_url_tracking")) {
    var url = a.href;
    var qs = a.search;
    if (qs && qs.substring(0,1) == "?") {
      qs = qs.substring(1,qs.length);
    }
    var existingVals = qsToObj(qs);
    var newVals = {
      "utm_campaign": browser()+"_ext",
      "utm_source": a.id,
      "utm_medium": "camelizer"
    }
    $.extend(newVals, existingVals);
    qs = $.param(newVals);
    a.href = a.protocol + "//" + a.host + a.pathname + "?" + qs;  
  }
}


function MissingTranslationError(key) {
  this.message = "Missing key '"+key+"'";
  this.name = "MissingTranslationError";
  this.key = key;
  this.toString = function() {
    return this.name + ":" + this.message;
  };
}

// i18n_data var is defined in i18n_data.js
var i18n = null;
var i18nLoaded = false;

var startI18n = function(callback) {
  
  if (i18nLoaded) {
    callback();
  } else {
    i18nLoaded = true;

    getAcceptLanguages(function(ar) {
      var ok = {"en": true, "es": true, "fr": true, "de": true, "it": true};
      var selected = "en";
      for (var i=0; i<ar.length; i++) {
        var short_locale = ar[i].substring(0,2);
        if (ok[short_locale]) {
          selected = short_locale;
          break;
        }
      }
      
      $("html").attr({
        "xml:lang": selected,
        "lang": selected
      });
        
      i18n = new Jed({
        locale_data : i18n_data, 
        "domain" : selected,
        "missing_key_callback": function(key) {
          throw new MissingTranslationError(key);
        }
      });
        
      i18n.parseHtml();
      callback();
  
    });

  }
  
}

// it's important that this only get's called once, because it sets el.text(), which other JS (Jquery UI) later alters.
Jed.prototype.parseHtml = function() {
  var el, key, str = null;
  
  $("[data-i18n]").each(function(i, el) {
    el = $(el);
    key = el.attr("data-i18n");
    str = i18n.t(key, el);
    el.text(str);
  });
  
  var attrs = ["title", "value"];
  var attr = null;
  for (var i=0; i<attrs.length; i++) {
    attr = attrs[i];
    $("[data-i18n-attr-"+attr+"]").each(function(i, el) {
      el = $(el);
      key = el.attr("data-i18n-attr-"+attr);
      str = i18n.t(key, el);
      el.attr(attr, str);
    });
  }


}

// 1. falls back to en locale if a key is missing in another locale.
// 2. interpolates post translation
Jed.prototype.t = function(key, el, options) { // el is either an html element or an options hash containing interpolations.
  var defaults = {"return": "string"}
  options = (typeof options == "undefined" ? defaults : options);

  try {
    var str = this.gettext(key);  
  } catch (e) {
    if (e instanceof MissingTranslationError && this.textdomain() != "en") {
      var initial_domain = this.textdomain();
      this.textdomain("en");
      var out = this.t(key, el);
      this.textdomain(initial_domain);
      return out;
    }
    // on production we possible don't want to throw an error here? Log to web?
    throw e;
  }
  
  if (str.indexOf("%{") > -1) {
    var matches = str.match(/%{(.*?)}/g);
    if (matches) {
      if (options.return == "string") {
        var interpolations = [];
        for(var i = 0; i<matches.length; i++) {
          var match = matches[i];
          var key = match.substring(2, match.length-1);
          var value = this.resolve_interpolation(el, key);
          interpolations.push(value);
          str = str.replace(match, value);
        }
      } else {
        // rather cumbersome, but this means we never have to do .html() for anything.
        var out = $("<span>");
        var ar = str.split(/%{(.*?)}/);
        for (var i = 0; i<ar.length; i++) {
          t = ar[i];
          if (i%2 == 0) {
            var span = $("<span>").text(t);
          } else {
            var value = this.resolve_interpolation(el, t);
            var span = $("<span>", {"data-interpolate": t}).append(value);
          }
          out.append(span);
        }
        return out;
      }
    }
  }
  return str;
}

Jed.prototype.resolve_interpolation = function(el, key) {
  if (el instanceof jQuery) { // a little hacky
    var value = el.attr("data-i18n-" + key);
  } else {
    var value = el[key];
  }
  if (!value) {
    throw new Error("interpolation not found for " + key + " in " + str);
  } else {
    return value;
  }
}
