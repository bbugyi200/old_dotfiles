
// localize
var Locale = {};
function SetLocale(name) {
    var text = chrome.i18n.getMessage(name);
    $('locale[name="' + name + '"]').each(function () {
        $(this).text(text);
    });
    $('title[name="' + name + '"]').each(function () {
        $(this).text(text);
    });
    $('input[phlocale="' + name + '"]').each(function () {
        $(this).attr('placeholder', text);
    });
    $('option[locname="' + name + '"]').each(function () {
        $(this).text(text);
    });
    Locale[name] = text;
}
function SetLocales(arr) {
    for (var i = 0; i < arr.length; ++i) {
        SetLocale(arr[i]);
    }
}
SetLocales([
            'lang',
            'extName',
            'extDesc',
            'RecentSearch',
            'InputWordsHere',
            'QuickOption',
            'CaseSensitiveSearch',
            'SearchInputHint',
	        "SortBy",
	        "SortByDefaultOrder",
	        "SortByTitle",
	        "SortByURL",
	        "SortByOpenedTime",
	        "Operate",
	        "CloseSelectedTabs",
	        "CollectSelectedTabsIntoASingleWindow",
	        "InsertATextToTheTitleOfSelectedTabs",
	        "InjectCodeToTheContentOfSelectedTabs",
	        "ConfirmCloseTabs",
	        "AreYouSureYouWantToCloseSelectedTabs",
	        "Cancel",
	        "CloseTabs",
	        "InsertATextToTheTitleOfSelectedTabs",
	        "InputTextHere",
	        "ToTheBeginningOfTitle",
	        "ToTheEndOfTitle",
	        "Apply",
	        "InjectCode",
	        "InputCodeHere",
	        "Select",
	        "Deselect",
	        "SelectAllTabs",
	        "DeselectAllTabs",
	        "SelectTheTabsIncludeASpecificSubstring",
	        "DeselectTheTabsIncludeASpecificSubstring",
	        "GetAllTabs",
	        "PrevResult",
	        "NextResult",
	        "Title",
            "TheSettingsAreSavedAutomatically",
            "Note",
            "Keyboard",
            "Behavior",
            "OpenThePopupWindowOnStartingGoogleChrome",
            "KeyboardSettings",
            "BehaviorSettings",
            "Settings",
            "ShortcutToOpenThePopupWindow",
            "SearchPlusSettings",
            "SearchSettings",
            "OtherSettings",
            "Enabled",
            "Disabled",
            'PleaseTypeKeysCTRLSHIFTF',
            'GetSelectedTabsCaptures',
            'CloseA',
	        "ConfirmCapturesImages",
	        "ItWillTakeALittleTime",
	        "Capture",
            "OpenAllCaptureImages"
        ]);