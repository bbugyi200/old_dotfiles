(function () {
    var setting = {};
    chrome.extension.sendRequest({
        type: 'get_setting'
    }, function (response) {
        setting = response.setting;
    });
    // keydown event
    window.addEventListener("keydown", function (e) {
        if (setting['enable_shortcut']) {
            var f1 = setting['ctrl'] == e.ctrlKey;
            var f2 = setting['shift'] == e.shiftKey;
            var f3 = setting['alt'] == e.altKey;
            var f4 = setting['key'] == e.keyCode;
            if (f1 && f2 && f3 && f4) {
                chrome.extension.sendRequest({ type: 'popup' });
            }
        }
    });
})();