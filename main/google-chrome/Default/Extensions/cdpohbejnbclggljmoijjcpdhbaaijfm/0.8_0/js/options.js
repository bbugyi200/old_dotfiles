/// <reference path="../js/jquery.js"/>
require([
    "jquery"
], function () {
    require([
        "bootstrap",
        "bootstrap-typeahead",
        "bootstrap-tooltip",
        "bootstrap-popover",
        "localize"
    ], function () {
        $(window).ready(function () {
            var setting = {};
            function LoadSetting() {
                setting = JSON.parse(localStorage.getItem('setting'));
                if (!setting)
                    setting = {};
            }
            function SaveSetting() {
                localStorage.setItem('setting', JSON.stringify(setting));
            }

            function ChangeActive(type) {
                $('#setting_tabs > .tabbable li').removeClass('active');
                $('#setting_tabs > .tab-content > div').removeClass('active');
                $('#' + type + '_tab').addClass('active');
                $('#' + type + '_content').addClass('active');
            }
            function KeyboardSettingActive() {
                ChangeActive('keyboard');
            }
            function BehaviorSettingActive() {
                ChangeActive('behavior');
            }
            function DisableShortcut() {
                setting['enable_shortcut'] = false;
                setting['ctrl'] = false;
                setting['shift'] = false;
                setting['alt'] = false;
                setting['key'] = 0;
                SaveSetting();
                $('#key1')
                    .val(Locale['Disabled'])
                    .attr('disabled', '');
                $('#btn1')
                    .removeClass('btn-success')
                    .addClass('btn-danger')
                    .html('<i class="icon-white icon-remove"></i>&nbsp;'+Locale['Disabled']);
            }
            function EnableShortcut() {
                $('#key1')
                    .val(KeyEventToString(setting['ctrl'], setting['shift'], setting['alt'], setting['key']))
                    .attr('placeholder', Locale['PleaseTypeKeysCTRLSHIFTF'])
                    .removeAttr('disabled');
                $('#btn1')
                    .addClass('btn-success')
                    .removeClass('btn-danger')
                    .html('<i class="icon-white icon-ok"></i>&nbsp;'+Locale['Enabled']);
            }
            function IsAlnum(c) {
                return (c >= "A" && c <= "Z") || (c >= "0" && c <= "9");
            }
            function KeyEventToString(ctrlKey, shiftKey, altKey, keyCode) {
                var ctrl = ctrlKey ? "CTRL + " : "";
                var shift = shiftKey ? "SHIFT + " : "";
                var alt = altKey ? "ALT + " : "";
                var ex = ctrl + shift + alt;
                var c = String.fromCharCode(keyCode);
                return (ex != "" && IsAlnum(c)) ? ex + c : (IsAlnum(c) ? c : '');
            }
            function InputShortcutKey(e) {
                var text = KeyEventToString(e.ctrlKey, e.shiftKey, e.altKey, e.keyCode);
                if (text != '') {
                    setting['enable_shortcut'] = true;
                    setting['ctrl'] = e.ctrlKey;
                    setting['shift'] = e.shiftKey;
                    setting['alt'] = e.altKey;
                    setting['key'] = e.keyCode;
                    SaveSetting();
                }
                // view
                $('#key1').val(text);
                return false;
            }
            function ShortcutToggle() {
                if (setting['enable_shortcut'])
                    DisableShortcut();
                else
                    EnableShortcut();
            }
            function EnableCaseSensitive() {
                $('#enable_case_sensitive_input').attr('checked', 'checked');
            }
            function DisableCaseSensitive() {
                $('#enable_case_sensitive_input').removeAttr('checked');
            }
            function CaseSensitiveToggle() {
                if (setting['enable_case_sensitive'])
                    DisableCaseSensitive();
                else
                    EnableCaseSensitive();
                setting['enable_case_sensitive'] = !setting['enable_case_sensitive'];
                SaveSetting();
            }
            function EnableOpenPopupOnStart() {
                $('#enable_popup_with_startup_input').attr('checked', 'checked');
            }
            function DisableOpenPopupOnStart() {
                $('#enable_popup_with_startup_input').removeAttr('checked');
            }
            function CaseOpenPopupOnStartToggle() {
                if (setting['enable_popup_with_startup'])
                    DisableOpenPopupOnStart();
                else
                    EnableOpenPopupOnStart();
                setting['enable_popup_with_startup'] = !setting['enable_popup_with_startup'];
                SaveSetting();
            }

            // events
            $('#keyboard_tab').click(KeyboardSettingActive);
            $('#behavior_tab').click(BehaviorSettingActive);
            $('#btn1').click(ShortcutToggle);
            $('#key1').keydown(InputShortcutKey);
            $('#enable_case_sensitive_input').click(CaseSensitiveToggle);
            $('#enable_popup_with_startup_input').click(CaseOpenPopupOnStartToggle);

            // view

            function Init() {
                LoadSetting();
                ShortcutInit();
                CaseSensitiveInit();
                OpenPopupOnStartInit();
            }
            function ShortcutInit() {
                if (setting['enable_shortcut'])
                    EnableShortcut();
                else
                    DisableShortcut();
            }
            function CaseSensitiveInit() {
                if (setting['enable_case_sensitive'])
                    EnableCaseSensitive();
                else
                    DisableCaseSensitive();
            }
            function OpenPopupOnStartInit() {
                if (setting['enable_popup_with_startup'])
                    EnableOpenPopupOnStart();
                else
                    DisableOpenPopupOnStart();
            }
            Init();
        });
    });
});