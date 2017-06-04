(function() {
  $(function() {
    var $doc, $groups, $list, $manager, dateString, defaultFaviconUrl, deleteBookmark, faviconPrefix, focusIndex, getBookmarks, isBookmarklet, isToday, lastDate, months, options, weekdays;
    weekdays = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
    months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
    faviconPrefix = 'https://www.google.com/s2/favicons?domain=';
    defaultFaviconUrl = 'default.png';
    options = window.options.load();
    lastDate = null;
    focusIndex = -1;
    $doc = $(document);
    $manager = $('#manager');
    $groups = $('#groups');
    $list = null;
    dateString = function(d) {
      return "" + weekdays[d.getDay()] + ", " + months[d.getMonth()] + " " + (d.getDate());
    };
    isToday = function(d) {
      var today;
      today = new Date();
      return d.getDate() === today.getDate() && d.getMonth() === today.getMonth() && d.getFullYear() === today.getFullYear();
    };
    isBookmarklet = function(url) {
      return url.toLowerCase().indexOf('javascript') === 0;
    };
    getBookmarks = function() {
      return chrome.bookmarks.getRecent(parseInt(options.count), function(results) {
        $groups.empty();
        return $.each(results, function(i, result) {
          var $close, $group, $groupTitle, $icon, $link, $title, dateTime, domain, faviconUrl, listItem, titleText,
            _this = this;
          dateTime = new Date(result.dateAdded);
          if (!lastDate || dateTime.getDate() !== lastDate.getDate() || dateTime.getMonth() !== lastDate.getMonth() || dateTime.getFullYear() !== lastDate.getFullYear()) {
            lastDate = dateTime;
            $group = $('<li class="group"></li>');
            $groupTitle = $('<h2></h2>');
            $group.append($groupTitle);
            titleText = dateString(dateTime);
            if (isToday(dateTime)) {
              titleText += ' - Today';
            }
            $groupTitle.text(titleText);
            $list = $('<ul></ul>');
            $group.append($list);
            $groups.append($group);
          }
          listItem = $('<li></li>');
          $list.append(listItem);
          $link = $('<a href="#" class="link"></a>');
          listItem.append($link);
          $link.attr('id', result.id);
          $link.attr('href', result.url);
          $link.attr('title', dateTime.toString());
          $link.attr('tabindex', 0);
          $link.on('click', function(e) {
            e.preventDefault();
            if (isBookmarklet(result.url)) {
              chrome.tabs.update({
                url: result.url
              });
            } else {
              chrome.tabs.create({
                url: result.url
              });
            }
            return window.close();
          });
          faviconUrl = defaultFaviconUrl;
          if (!isBookmarklet(result.url)) {
            domain = result.url.match(/.+:\/\/.+?\//);
            domain = domain != null ? domain : result.url;
            faviconUrl = faviconPrefix + domain;
          }
          $icon = $('<img />');
          $link.append($icon);
          setTimeout(function() {
            return $icon.attr('src', faviconUrl);
          }, 1);
          $title = $('<span class="title">&nbsp;</span>');
          $link.append($title);
          if (result.title.length > 0) {
            $title.text(result.title);
          }
          $close = $('<a class="delete icon-remove-sign"></a>');
          $link.append($close);
          return $close.on('click', function(e) {
            e.preventDefault();
            e.stopImmediatePropagation();
            return deleteBookmark($link);
          });
        });
      });
    };
    deleteBookmark = function(link) {
      var id;
      if (confirm('Are you sure you want to delete this bookmark?')) {
        id = link.attr('id');
        return chrome.bookmarks.remove(id, function() {
          if (link.siblings().length === 0) {
            return link.parents('.group').slideUp(function() {
              $(this).remove();
              return getBookmarks();
            });
          } else {
            return link.slideUp(function() {
              $(this).remove();
              return getBookmarks();
            });
          }
        });
      }
    };
    $manager.bind('click', function() {
      return chrome.tabs.create({
        url: 'chrome://bookmarks/'
      });
    });
    $manager.toggle(options.manager === 'true');
    $doc.bind('click', function() {
      if (focusIndex === -1) {
        focusIndex = $('a.link').index($('a.link:first'));
      }
      return $('a.link').eq(focusIndex).focus();
    });
    $doc.bind('keyup', function(e) {
      var keyCode;
      keyCode = e.keyCode || e.which;
      if (keyCode === 9) {
        return e.preventDefault();
      }
    });
    $doc.bind('keydown', function(e) {
      var focused, lastIndex, links;
      links = $('a.link');
      focused = $('a.link:focus');
      lastIndex = links.index($('a.link:last'));
      if (focused.length > 0) {
        focusIndex = links.index(focused);
      }
      if (e.which === 40 || e.which === 9) {
        e.preventDefault();
        focusIndex = focusIndex !== lastIndex ? focusIndex += 1 : 0;
        return links.eq(focusIndex).focus();
      } else if (e.which === 38) {
        e.preventDefault();
        focusIndex = focusIndex !== 0 ? focusIndex -= 1 : lastIndex;
        return links.eq(focusIndex).focus();
      } else if (e.which === 46 || e.which === 8) {
        e.preventDefault();
        return deleteBookmark(links.eq(focusIndex));
      }
    });
    return getBookmarks();
  });

}).call(this);
