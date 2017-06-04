wishlistController = {
  scraped: false,
  scrape_ok: false,
  checked_imported: false,
  imported_wishlists: [],
  wishlists: [],

  start: function() {
    camelizer.tabs_locked = true;
    camelizer.lockPage();
    $('.tabs li').removeClass('active');
    $("#addWishlistLink").addClass('active');
    camelizer.hideAllDisplays();
    if (this.scraped == true && this.checked_imported == true && this.scrape_ok == true) {  
      $("#wishlistImporter").show();
      camelizer.tabs_locked = false;
      camelizer.unlockPage();
    } else {
      camelizer.displayLoading();
      this.getWishlists(function(response) {
        wishlistController.wishlists = response;
        wishlistController.scraped = true;
        wishlistController.drawForm();   
      });
      this.getImportedWishlists();
    }
  },
  
  getWishlists: function(callback) {
    var amazon_base = camelizer.context.content_url.replace(/.*?\/\/(.*?)\/.*/, "$1");
    var json_url = "https://" + amazon_base + "/gp/navigation/ajax/dynamic-menu.html?wishlistItems=wishlist&weblabs=53154%3AT1";
    
    camelAsync.get(json_url, {}, function(response) {
      wishlists = [];
      
      if (response.status != 200)
        return callback({"other_scrape_error": true});
      
      var str = response.response;
      var json = JSON.parse(str);
      var items = json["wishlistItems"];
      
      if (!items["template"])
        return callback({"signin_link": "https://" + amazon_base + "/gp/navigation/redirector.html/ref=sign-in-redirect?"});
      
      try
      {
        items = items["template"]["data"]["items"][0]["items"];
        
        if (!items)
          throw "error";
        
        for (var i = 0; i < items.length; i++)
        {
          var item = items[i];
          var privacy, url, name, count, id, data = null;
          name = item["text"];
          count = parseInt(item["subtext"] || 0);
          url = "https://" + amazon_base + item["url"];
          id = url.replace(/.*?\/gp\/registry\/wishlist\/([a-z0-9]*)[\/?$]?.*/i, "$1");
          
          if (!id || item["url"] == "/gp/registry/wishlist/ref=nav_wishlist_lists_seemore")
            continue;
          
          privacy = true;
          data = {"url": url, "name": name, "items": count, "privacy": privacy, "id": id};
          wishlists.push(data);
        }
        
        wishlistController.scrape_ok = true;
        return callback(wishlists);
      }
      catch (e)
      {
        return callback({"other_scrape_error": true});
      }
    }, "text");
    
    return true;
  },

  getImportedWishlists: function() {
    camelAsync.get("https://" + camelizer.context.domain + "/camelizer/imported_wishlists", {"locale":camelizer.context.locale}, function(response) {
      if (response.status == 200) {
        camelizer.logged_in = response.response.a
        _gaq.push(['_setCustomVar', 4, "Farmer", (camelizer.logged_in === true ? "Yes" : "No")]);
        if (camelizer.logged_in == false) {
          return camelizerList.init(camelizer.context);
        }
        wishlistController.checked_imported = true;
        camelizer.login = response.response.login
        camelizer.showLogin();
        $(".currency_symbol").text(response.response.currency_symbol);
        wishlistController.imported_wishlists = response.response.imported_wishlists;
        wishlistController.drawForm();
      } else {
        $("#camelizerDisplayLoading").hide();
        $("#wishlistImporter").hide();
        camelizer.displayError();
      }
    });
  },

  drawForm: function() {
    if (this.scraped == false || this.checked_imported == false) {  
      // wait for both the async responses.
      return false;
    }
    camelizer.trackPage('wishlists');
    camelizer.tabs_locked = false;
    camelizer.unlockPage();
    $("#camelizerDisplayLoading").hide();
    $("#addWishlistLink").show().addClass('active');

    if (typeof this.wishlists.signin_link != "undefined") {
      $("#wishlistImporter").show();
      $("#wishlists_form").hide();
      $("#pleaseSigninToAmazon").show();
      $("#signinToAmazonLink").attr("href", this.wishlists.signin_link);
      return;
    } else if (typeof this.wishlists.no_wishlist != "undefined") {
      $("#wishlists_form").hide();
      $("#pleaseSigninToAmazon").hide();
      $("#noWishlists").show();
      return;
    } else if (typeof this.wishlists.other_scrape_error != "undefined") {
      // should do something to let us know via the backend!
      $("#pleaseSigninToAmazon").hide();
      $("#wishlistImporter").show();
      $("#wishlists_form").hide();
      camelizer.displayError();
      return;
    }
    

    $("#wishlistImporter").show();
    $("#wishlists_form").show();
    $(".wishlist_submit").hide();
    $("#wishlists").text("");
    if (this.wishlists.length == 0) {
      $("#no_wishlists").show();
    } if (this.wishlists.length == 1) {
      $("#wishlist_submit").show();
    } else {
      $("#wishlists_submit").show();
    }
    $.each(this.wishlists, function(index, wishlist) {
      wishlistController.drawRow(wishlist);
    });
  },

  drawRow: function(wishlist) {
    var wishlist_table = $("#wishlists");
    var edit, del = $("<td>");
    var row = $("<tr>");
    var link = $("<td>").append($("<a>", {href: wishlist.url, target: '_blank', class: 'no_url_tracking amazon_wishlist_link'}).text(wishlist.name));
    
    /*
    if (parseInt(wishlist.items) == 1) {
      var item_text = i18n.t("t_item");
    } else {
      var item_text = i18n.t("t_items", {"count": wishlist.items});
    }
    */
    
    var items = $("<td>").addClass('wishlist_items');
    var privacy = $("<td>");
    if (typeof this.imported_wishlists[wishlist.id] == "undefined") {
      if (wishlist.privacy == "private") {
        privacy = $("<span>").text("(" + i18n.t("t_wishlist_" + wishlist.privacy) + ") " + i18n.t("t_wishlist_note"));
        privacy = $("<td>").addClass('wishlist_privacy').append(privacy);
        checkbox = $("<td>");
      } else {
        checkbox = $("<td>").append($("<input>", {id: 'wishlistCheckbox_'+wishlist.id, type: 'checkbox', name: 'wishlists[]', value: wishlist.url, checked: 'checked'}));
      }
    } else {
      imported_wishlist_id = this.imported_wishlists[wishlist.id];
      checkbox = $("<td>").append($("<img>", {src: 'accept.png', alt: 'Imported', title: 'Imported', class: 'imported'}));
      edit = $("<td>").append($("<a>", {href: 'https://' + camelizer.context.domainStripped() + '/wishlists/wishlist/'+imported_wishlist_id, target: '_blank', id: 'editWishlistLink', class: 'editWishlistLink'}).text(i18n.t("t_edit")));
      del_link = $("<a>", {rel: imported_wishlist_id, href: '#'}).click(function(e) {
        return wishlistController.deleteClick(e, $(this));
      }).append($("<img>", {src: 'delete.png', alt: 'delete'}));
      del = $("<td>").append(del_link, $("<img>", {src: 'loading.gif', style: 'display:none;', class: 'throbber'}));
    }
    row.append(checkbox, link, items, privacy, edit, del);
    wishlist_table.append(row);
    bindChartLinks();
    this.addWishlistTracking();
  },
  
  deleteClick: function(e, anchor) {
    e.preventDefault();
    _gaq.push(['_trackEvent', "wishlistDeleteImportedClick", "clicked"]);
    wishlistController.hideErrors();
    var throbber = anchor.closest("td").find(".throbber");
    throbber.show();
    camelAsync.get("https://" + camelizer.context.domain + "/camelizer/delete_wishlist", {"id":anchor.attr("rel")}, function(response) {
      throbber.hide();
      if (response.status == 200) {
        wishlistController.imported_wishlists = response.response.imported_wishlists;
        wishlistController.drawForm();
      } else {
        wishlistController.showError("wishlistDeleteError");
      }
    });
    return false;
  },

  addWishlists: function(e) {
    this.hideErrors();
    if (!this.wishlistSelected() || !this.priceTypeSelected() || !this.validRelativeValue()) {
      return false;
    }
    _gaq.push(['_trackEvent', "addWishlistsFormSubmit", "clicked"]);
    $("#wishlistFormThrobber").show();
    data = $("#wishlists_form").serialize();
    camelAsync.get("https://" + camelizer.context.domain + "/camelizer/add_wishlists", data, function(response) {
      $("#wishlistFormThrobber").hide();
      data = response.response;
      if (data.login == false) {
        camelizer.login = "";
        camelizer.logged_in = false;
        camelizer.showLogin();
      }
      if (typeof data.error_msg != "undefined") {
        wishlistController.showCustomError(data.error_msg);
      } else if (response.status == 200) {
        if (data.error_count != 0) {
          wishlistController.showError("wishlistImportError");
        } else {
          if (data.imported_count == 1) {
            $("#wishlistImportSuccessMessage").show();
          } else {
            $("#wishlistsImportSuccessMessage").show();
          }
        }
        wishlistController.imported_wishlists = data.imported_wishlists;
        wishlistController.drawForm();
      } else {
        wishlistController.showError("wishlistImportError");
      }
    });
  },

  wishlistSelected: function() {
    if ($("table#wishlists input[type=checkbox]:checked").length == 0) {
      this.showError("wishlistSelectedError");
      return false;
    }
    return true;
  },

  priceTypeSelected: function() {
    if ($("#wishlistPriceTypes input[type=checkbox]:checked").length == 0) {
      this.showError("wishlistPriceTypeError");
      return false;
    }
    return true;
  },

  validRelativeValue: function() {
    if(!$("#relative_value").val().match(/^[0-9\.,\s]*$/)) {
      this.showError("relativeValueError");
      return false;
    }
    return true;
  },

  hideErrors: function() {
    $(".flash").hide();
    $("#wishlistErrors div").hide();
  },
  
  showError: function(id) {
    $("#wishlistErrors").show();
    $("#"+id).show();
  },

  showCustomError: function(msg) {
    $("#wishlistErrors").show();
    $("#wishlistCustomError").text(msg).show();
  },

  addWishlistTracking: function() {
    $("#wishlists_form input[type=checkbox]").change(function(e) {
      _gaq.push(['_trackEvent', "wishlistFormCheckboxChange", $(this).attr("id") + "_" + (e.target.checked ? "on" : "off")]);
    });

    $("#wishlists_form select").change(function(e) {
      _gaq.push(['_trackEvent', "wishlistFormSelectChange", $(this).attr("id") + "_" + $(this).val()]);
    });
  }

};

$(document).ready(function() {
  
  $("#wishlists_form").submit(function(e) {
    e.preventDefault();
    wishlistController.addWishlists(e);
    return false;
  });

  $("#relative_to").change(function(e) {
    if ($(this).val() == "static_price") {
      $("#operation").hide();
    } else {
      $("#operation").show();
    }
  });
  
});
