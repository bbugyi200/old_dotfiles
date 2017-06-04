(function () {
    var tablist = [];
    var current_tab = 0;
    var total = 0;
    var status = 'waiting';
    var keyword = '';
    var portname = "Search Plus Chrome Extension";
    var tmpSendResponse = function () { };

    function init() {
        tablist = [];
        current_tab = 0;
        total = 0;
        status = 'waiting';
        keyword = '';
    }

    function isValidURL(url) {
        var NP = ["https://chrome.google.com"];
        for (var i = 0; i < NP.length; ++i) {
            var prefix = NP[i];
            if (url.substr(0, prefix.length) == prefix) {
                return false;
            }
        }
        var P = ["http://", "https://"];
        for (var i = 0; i < P.length; ++i) {
            var prefix = P[i];
            if (url.substr(0, prefix.length) == prefix) {
                return true;
            }
        }
        return false;
    }
    function isValidTab(tab) {
        if (!isValidURL(tab.url))
            return false;
        if (tab.status != "complete")
            return false;
        return true;
    }

    // mutually
    function findres(sendResponse) {
        if (current_tab >= total) {
            status = 'waiting';
            sendResponse({ finished: true, progress: 1, tab: tab, result: false });
            return;
        }
        var tab = tablist[current_tab];
        setting = SettingLoader.load();
        var code = "(function(x, y){"
                    + "var port = chrome.extension.connect({name:\""
                    + portname
                    + "\"});"
                    + "var res = window.find(\""
                    + keyword
                    + "\", "
                    + setting['enable_case_sensitive'] // case sensitive
                    + ", false, true, false, true, false);"
                    + "scrollTo(x, y);" // + "setTimeout((function(x_,y_){return function(){scrollTo(x_,y_);};})(x,y),0);"
                    + "port.postMessage({type:\"checked\", result:res, tabid:"
                    + tab.id
                    + "});"
                    + "})(document.body.scrollLeft, document.body.scrollTop);";
        tmpSendResponse = sendResponse;
        chrome.tabs.executeScript(tab.id, { code: code });
    }

    function find(sendResponse, keyword_) {
        init();
        keyword = keyword_;
        chrome.windows.getAll({ populate: true }, function (windows) {
            for (var i = 0; i < windows.length; ++i) {
                var w = windows[i];
                for (var j = 0; j < w.tabs.length; ++j) {
                    var t = w.tabs[j];
                    if (isValidTab(t)) {
                        tablist.push(t);
                        total++;
                    }
                }
            }
            findres(sendResponse);
        });
    }

    var SettingLoader = {
        key: "setting",
        load: function () {
            var tmp = JSON.parse(localStorage.getItem(this.key));
            if (!tmp) return {};
            return tmp;
        },
        save: function (obj) {
            localStorage.setItem(this.key, JSON.stringify(obj));
        }
    };
    var setting = SettingLoader.load();

    // for Connection
    chrome.extension.onRequest.addListener(
        function (request, sender, sendResponse) {
            setting = SettingLoader.load();
            if (request.type == "get_setting") {
                sendResponse({ setting: setting });
            } else if (request.type == "find") {
                if (request.keyword.length == 0)
                    return sendResponse({ finished: true });
                if (status != 'waiting')
                    return sendResponse({ finished: true });
                status = 'finding';
                find(sendResponse, request.keyword);
            } else if (request.type == "findres") {
                findres(sendResponse);
            } else if (request.type == "findignore") {
                current_tab++;
                findres(sendResponse);
            } else if (request.type == 'popup') {
                OpenPopupWindow();
            } else if (request.type == 'get_all_tabs') {
                var tabs = [];
                chrome.windows.getAll({ populate: true }, function (windows) {
                    for (var i = 0; i < windows.length; ++i) {
                        var w = windows[i];
                        for (var j = 0; j < w.tabs.length; ++j) {
                            var t = w.tabs[j];
                            if (isValidTab(t)) {
                                tabs.push(t);
                            }
                        }
                    }
                    sendResponse({ tabs: tabs });
                });
            } else if (request.type == 'collect_into_window') {
                var tablist = request.tablist;
                chrome.windows.create({}, function (window) {
                    chrome.windows.get(window.id, { populate: true }, function (window) {
                        var noneed = window.tabs[0].id;
                        chrome.tabs.move(
                            tablist,
                            {
                                windowId: window.id,
                                index: 0
                            }
                        );
                        chrome.tabs.remove(noneed);
                    });
                });
            }
        }
    );

    chrome.extension.onConnect.addListener(function (port) {
        if (port.name == portname) {
            port.onMessage.addListener(function (msg) {
                if (msg.type == 'checked') {
                    var tab = tablist[current_tab];
                    current_tab++;
                    var finished = current_tab >= tablist.length;
                    var title = tab.title;
                    var url = tab.url;
                    // toLowerCase()
                    var setting = SettingLoader.load();
                    // match filter
                    if (setting['enable_case_sensitive']) {
                        result = msg.result || title.indexOf(keyword) != -1 || url.indexOf(keyword) != -1;
                    } else {
                        result = msg.result || title.toLowerCase().indexOf(keyword.toLowerCase()) != -1 || url.toLowerCase().indexOf(keyword.toLowerCase()) != -1;
                    }
                    tmpSendResponse({ finished: finished, progress: current_tab / total, tab: tab, result: result });
                }
            });
        }
    });

    var w = null;
    function OpenPopupWindow() {
        if (w != null && !w.closed) {
            w.close();
        }
        w = window.open(
            chrome.extension.getURL("html/popup.html"),
            'search_plus_chrome_extension',
            "width=516,height=600");
    }

    var setting = SettingLoader.load();
    if (setting['enable_popup_with_startup']) {
        OpenPopupWindow();
    }
})();