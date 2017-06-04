/// <reference path="../js/jquery.js"/>
require([
    'bootstrap',
    'bootstrap-typeahead',
    'bootstrap-tooltip',
    'bootstrap-popover',
    'bootstrap-button',
    'bootstrap-modal',
    'easing',
    'jszip',
    'localize'
], function () {
    var last;
    $.event.add(window, 'load', function () {
        var hist = [];
        var histsize = 500;
        var histindex = 0;
        var hash;
        var orgtablist = [];
        var setting = {};

        function HashIndexInit() {
            if (!isNaN(parseInt(hash))) {
                histindex = parseInt(hash);
            }
            if (histindex + 1 < hist.length) {
                $('.previous_word').removeClass('disabled');
                $('.previous_word > a').unbind('click').click(function () {
                    if ($(this).hasClass('disabled'))
                        return;
                    location.hash = '#' + (histindex + 1);
                    init(true);
                });
                $('.previous_word > a').attr({ href: location.hash });
            } else {
                $('.previous_word').addClass('disabled');
                $('.previous_word > a').unbind('click');
            }
            if (histindex - 1 >= 0) {
                $('.next_word').removeClass('disabled');
                $('.next_word > a').unbind('click').click(function () {
                    if ($(this).hasClass('disabled'))
                        return;
                    location.hash = '#' + (histindex - 1);
                    init(true);
                });
                $('.next_word > a').attr({ href: location.hash });
            } else {
                $('.next_word').addClass('disabled');
                $('.next_word > a').unbind('click');
            }
        }

        function LoadHistory() {
            hist = localStorage.getItem('hist');
            hist = hist ? JSON.parse(hist) : [];
        }
        function SaveHistory() {
            localStorage.setItem('hist', JSON.stringify(hist));
        }
        function PushHistory(keyword) {
            while (histsize > 0 && hist.length >= histsize) {
                hist.shift();
            }
            if (hist[0] == keyword) {
                return;
            }
            hist.unshift(keyword);
            SaveHistory();
            LoadHistory();
            $('#search_input').typeahead({ source: hist })
            UpdateRecentSearch();
        }


        function LoadSetting() {
            setting = JSON.parse(localStorage.getItem('setting'));
            if (!setting)
                setting = {};
        }
        function SaveSetting() {
            localStorage.setItem('setting', JSON.stringify(setting));
        }
        //
        // init
        function InitSearchStart() {
            $('#wrapper1').show();
            $('#progress_bar1_wrapper')
            .removeClass('progress-success')
            .addClass('progress-info')
            .addClass('progress-striped')
            .addClass('active');
            $('#progress_bar1').css({ webkitTransitionDuration: '0' }).css({ width: '0' });
            $('#result').empty();
            $('#result_number').text('0');
            result_number = 0;
        }

        //
        function CreateResultLineTitle(tab) {
            return '<td class="title">'
                + '<a href="#" class="is_result_line" tabindex="1">'
                + tab.title
                + '</a>'
                + '</td>';
        }
        function CreateResultLine(tab, checked) {
            var elm = $(CreateResultLineTitle(tab))
                .unbind('click').click(GetGoToFunc(tab));
            var tr = ''
                + '<tr>'
                + '<td class="number">'
                + '<input type="checkbox" tabid="'
                + tab.id
                + '" '
                + (checked === true ? 'checked' : '')
                + '>'
                + '</td>'
                + '</tr>'
                + '';
            return $(tr).append(elm);
        }

        // Search
        function GoTo(tab) {
            chrome.tabs.update(tab.id, { selected: true });
        }
        function GetGoToFunc(tab) {
            return function () {
                GoTo(tab);
                return false;
            };
        }

        function addResultLine(tab, checked) {
            var elm = $(CreateResultLine(tab, checked))
                    .css({
                        opacity: '0.0'
                    })
                    .animate({ opacity: '1.0' });
            $(elm).popover({ placement: 'top', title: 'URL', content: '<img width="16" height="16" src="' + tab.favIconUrl + '">&nbsp;' + tab.url, trigger: '' });
            $(elm).hover((function (elm) {
                return function () {
                    var t = setTimeout(function () {
                        $(elm).popover('show');
                    }, 300);
                    $(this).data('timeout', t);
                };
            })(elm), (function (elm) {
                return function () {
                    $(elm).popover('hide');
                    clearTimeout($(this).data('timeout'));
                };
            })(elm));
            $('#result').append(elm);
            $('#result_number').text('' + (++result_number));
        }

        function SearchUpdateProgress(response) {
            if (response.result) {
                addResultLine(response.tab);
                orgtablist.push(response.tab);
            }

            if (!response.finished) {
                var w = parseInt(response.progress * 100);
                $('#progress_bar1').queue([]).animate(
                    {
                        width: w + '%'
                    },
                    10, null, function () {
                        SearchResRequest();
                    }
                );
            } else {
                $('#search_button').button('reset');
                $('#progress_bar1').queue([]).animate(
                    {
                        width: '100%'
                    },
                    10, null,
                    function () {
                        $('#progress_bar1_wrapper')
                            .removeClass('progress-info')
                            .removeClass('progress-striped')
                            .removeClass('active')
                            .addClass('progress-success');
                    }
                );
            }
        }
        function SearchResRequest() {
            var timeout = setTimeout(function () {
                chrome.extension.sendRequest({ type: 'findignore' }, function (response) {
                    SearchUpdateProgress(response);
                });
            }, 100);
            chrome.extension.sendRequest({ type: 'findres' }, function (response) {
                clearTimeout(timeout);
                SearchUpdateProgress(response);
            });
        }
        function ReSearch() {
            orgtablist = [];
            InitSearchStart();
            keyword = hist[histindex];
            $('#search_input').val(keyword);
            chrome.extension.sendRequest({ type: 'find', keyword: keyword }, function (response) {
                SearchUpdateProgress(response);
            });
        }
        function Search() {
            orgtablist = [];
            hash = 0;
            hashindex = 0;
            location.hash = '#0';
            $('#search_button').button('loading');
            $('#search_input').tooltip('hide');
            Init()
            InitSearchStart();
            var keyword = $('#search_input').val();
            PushHistory(keyword);
            last = keyword;
            chrome.extension.sendRequest({ type: 'find', keyword: keyword }, function (response) {
                SearchUpdateProgress(response);
            });
        }
        function GetAllTabs() {
            orgtablist = [];
            result_number = 0;
            $('#result').empty();
            chrome.extension.sendRequest({ type: 'get_all_tabs' }, function (response) {
                orgtablist = response.tabs;
                for (var i = 0; i < orgtablist.length; ++i)
                    addResultLine(orgtablist[i]);
            });
        }

        function swap(arr, b, c) {
            arr[b] = arr.splice(c, 1, arr[b])[0];
        }
        function GetComparatorFunc(sortby) {
            if (sortby == 'default') {
                return function (arr, x, y) {
                    return arr[x] != orgtablist[x] && arr[y] == orgtablist[x];
                };
            } else if (sortby == 'title') {
                return function (arr, x, y) {
                    return arr[x].title > arr[y].title;
                };
            } else if (sortby == 'url') {
                return function (arr, x, y) {
                    return arr[x].url > arr[y].url;
                }
            } else if (sortby == 'opened_time') {
                return function (arr, x, y) {
                    return arr[x].id > arr[y].id;
                }
            } else {
                return function (arr, x, y) {
                    return false;
                }
            }
        }
        function SortTabsSub(sortby) {
            var tmp = [];
            for (var i = 0; i < orgtablist.length; ++i)
                tmp.push(orgtablist[i]);
            var checkedlist = [];
            $('#result input:checkbox:checked').each(function () {
                checkedlist.push(parseInt($(this).attr('tabid')));
            });
            for (var i = 0; i < tmp.length; ++i) {
                tmp[i].checked = (checkedlist.indexOf(tmp[i].id) != -1);
            }
            var comparator = GetComparatorFunc(sortby);
            for (var i = 0; i < tmp.length; ++i) {
                for (var j = i + 1; j < tmp.length; ++j) {
                    if (comparator(tmp, i, j)) {
                        swap(tmp, i, j);
                    }
                }
            }
            return tmp;
        }
        function SortTabs(sortby) {
            var tmp = SortTabsSub(sortby);
            tablist = tmp;
            $('#result').empty();
            result_number = 0;
            for (var i = 0; i < tablist.length; ++i) {
                addResultLine(tablist[i], tablist[i].checked);
            }
        }

        function GetSelectedTablist() {
            var tablist = [];
            $('#result input:checkbox:checked').each(function () {
                tablist.push(parseInt($(this).attr('tabid')));
            });
            return tablist;
        }

        function InsertTextToTitle(text, place) {
            var tablist = GetSelectedTablist();
            var code;
            if (place == 'beginning')
                code = 'document.title = "' + text + '"+document.title;';
            else if (place == 'end')
                code = 'document.title += "' + text + '";';
            for (var i = 0; i < tablist.length; ++i) {
                chrome.tabs.executeScript(tablist[i], { code: code });
            }
            ReSearch();
        }

        function truncate(str, length) {
            if (str.length > length) {
                var tstr = str.substring(0, length) + '...';
                return tstr;
            }
            return str;
        }
        function UpdateRecentSearch() {
            $('#show_history_ul').empty();
            for (var i = 0; i < Math.min(50, hist.length); ++i) {
                var line = document.createElement('li');
                $(line)
                    .html('<a href="#' + i + '">' + truncate(hist[i], 10) + '</a>')
                    .click(
                        (function (ind) {
                            return function () {
                                location.hash = '#' + ind;
                                histindex = ind;
                                $('#search_input').val(hist[ind]);
                                init(true);
                            };
                        })(i)
                        );
                $('#show_history_ul').append(line);
            }
        }

        function InitEventsOnce() {
            $('.typeahead').ready(function () {
                $('.tooltip').ready(function () {
                    $('#search_input')
                        .keydown(function (e) {
                            if ($('ul.typeahead').css('display') == 'none') {
                                if (e.which == 13) {
                                    if ($('#search_input').val() == '')
                                        return;
                                    Search();
                                }
                            }
                        })
                        .keyup(function () {
                            $('#search_button').attr({ disabled: this.value == "" });
                        })
                        .typeahead({
                            source: hist
                        })
                        .tooltip({ trigger: 'focus', title: Locale['SearchInputHint'] });
                    $('#search_button')
                        .unbind('click')
                        .click(function () {
                            Search();
                        });
                    $('.move_to_top')
                        .unbind('click')
                        .click(function () {
                            $('html,body').animate({ scrollTop: 0 }, 'fast');
                            return false;
                        });
                    $('.move_to_down')
                        .unbind('click')
                        .click(function () {
                            $('html,body').animate({ scrollTop: $('html').height() }, 'fast');
                            return false;
                        });
                });
            });

            // sort events
            $('#sort_by_default').unbind('click').click(function () {
                SortTabs('default');
            });
            $('#sort_by_title').unbind('click').click(function () {
                SortTabs('title');
            });
            $('#sort_by_url').unbind('click').click(function () {
                SortTabs('url');
            });
            $('#sort_by_opened_time').click(function () {
                SortTabs('opened_time');
            });
            // operate
            $('#collect_into_window').unbind('click').click(function () {
                var tablist = GetSelectedTablist();
                if (tablist.length == 0)
                    return true;
                chrome.extension.sendRequest({ type: 'collect_into_window', tablist: tablist });
            });
            $('#select_all').unbind('click').click(function () {
                $('#result input:checkbox').attr('checked', true);
            });
            $('#select_all_by_number').click(function () {
                $('#select_all').click();
                return false;
            });
            $('#deselect_all').unbind('click').click(function () {
                $('#result input:checkbox').removeAttr('checked');
            });
            $('#select_by_substring').click(function () {
                $('#modal_select_by_substring').modal('show');
                setTimeout(function () {
                    $('#select_by_substring_input').focus();
                }, 300);
            });
            $('#select_by_substring_apply').click(function () {
                var key = $('#select_by_substring_input').val();
                if (key == '') return;
                $('#result tr').each(function () {
                    var f = $('.number > input:checkbox', this).attr('checked') != 'checked';
                    if (f) {
                        var text = $('.title', this).text();
                        if (text.indexOf(key) != -1) {
                            $('.number > input:checkbox', this).attr('checked', 'checked');
                        }
                    }
                });
                $('#modal_select_by_substring').modal('hide');
            });
            $('#select_by_substring_input').keydown(function (e) {
                if (isEnterKey(e)) {
                    $('#select_by_substring_apply').click();
                }
            });
            $('#deselect_by_substring').click(function () {
                $('#modal_deselect_by_substring').modal('show');
                setTimeout(function () {
                    $('#deselect_by_substring_input').focus();
                }, 300);
            });
            $('#deselect_by_substring_apply').click(function () {
                var key = $('#deselect_by_substring_input').val();
                if (key == '') return;
                $('#result tr').each(function () {
                    var f = $('.number > input:checkbox', this).attr('checked') == 'checked';
                    if (f) {
                        var text = $('.title', this).text();
                        if (text.indexOf(key) != -1) {
                            $('.number > input:checkbox', this).removeAttr('checked');
                        }
                    }
                });
                $('#modal_deselect_by_substring').modal('hide');
            });
            $('#deselect_by_substring_input').keydown(function (e) {
                if (isEnterKey(e)) {
                    $('#deselect_by_substring_apply').click();
                }
            });

            $(window).keydown(function () {
                setTimeout(function () {
                    var f = false;
                    $('body .modal').each(function () {
                        if ($(this).css('display') == 'block')
                            f = true;
                    });
                    if (f) return;
                    if (!$(document.activeElement).hasClass('modal_element')) {
                        setTimeout(function () {
                            var f = !$(document.activeElement).hasClass('is_result_line')
                            && !$(document.activeElement).hasClass('si-input');
                            if (f)
                                $('#result').find('.is_result_line').first().focus();
                        });
                    }
                });
            });

            //
            $('#insert_text_to_title').click(function () {
                var tablist = GetSelectedTablist();
                if (tablist.length == 0)
                    return;
                $('#modal_insert_text_to_title').modal('show');
                setTimeout(function () {
                    $('#insert_text_to_title_text').focus();
                }, 300);
            });
            $('#insert_text_to_title_apply').click(function () {
                var text = $('#insert_text_to_title_text').val();
                var place = $('#insert_text_to_title_place').val();
                InsertTextToTitle(text, place);
                $('#modal_insert_text_to_title').modal('hide');
            });
            $('#insert_text_to_title_text').keydown(function (e) {
                if (isEnterKey(e)) {
                    $('#insert_text_to_title_apply').click();
                }
            });

            $('#inject_code_to_tabs').click(function () {
                var tablist = GetSelectedTablist();
                if (tablist.length == 0)
                    return;
                $('#modal_inject_code').modal('show');
            });
            $('#inject_code_apply').click(function () {
                var code = $('#inject_code_input').val();
                var tablist = GetSelectedTablist();
                if (tablist.length == 0)
                    return;
                for (var i = 0; i < tablist.length; ++i) {
                    chrome.tabs.executeScript(tablist[i], { code: code });
                }
                $('#modal_inject_code').modal('hide');
            });

            // get_captures
            var tab_captures = [];
            $('#captures_album').hide();
            $('#captures_album_backdrop').click(function () {
                if ($('#captures_pic_backdrop').hasClass('hide')) {
                    $('#captures_album_backdrop').hide();
                    $('#captures_album').hide();
                    $('#captures_album_controller').hide();
                }
            });
            $('#captures_pic_backdrop').click(function () {
                $('#captures_pic_backdrop').hide();
                $('#captures_pic').hide();
                $(window).resize();
            });
            $('#captures_pic').click(function () {
                $('#captures_pic_backdrop').hide();
                $('#captures_pic').hide();
                $(window).resize();
            });
            var album_inner_left = 0;
            var album_inner_width = 0;
            var album_inner_cnt = 0;
            var flag = false;
            window.onmousewheel = function (e) {
                if ($('#captures_album').css('display') == 'none')
                    return true;
                var d = parseInt(e.wheelDelta);
                if (d > 0)
                    album_inner_left += 60;
                else
                    album_inner_left -= 60;
                $('#captures_album_inner').queue([]).animate({ left: album_inner_left }, { duration: 'slow', easing: jQuery.easeInSine });
                $('#album_nav_bar').queue([]).animate({ left: -(album_inner_left / album_inner_width) * $(window).width() }, { duration: 'slow', easing: jQuery.easeInSine });
                return false;
            };
            $('#captures_open_all').click(function () {
                for (var i = 0; i < tab_captures.length; ++i) {
                    if (tab_captures[i].ok) {
                        setTimeout((function (ind) {
                            return function () {
                                window.open(tab_captures[ind].data, '_blank');
                            };
                        })(i), 0);
                    }
                }
            });
            $('#captures_album_close ').click(function () {
                if ($('#captures_pic_backdrop').hasClass('hide')) {
                    $('#captures_album_backdrop').hide();
                    $('#captures_album').hide();
                }
            });
            $('#get_captures').click(function () {
                $('#modal_captures_album button').each(function () {
                    $(this).removeAttr('disabled');
                });
                $('#modal_captures_album').modal('show');
            });
            $('#modal_captures_album_capture').click(function () {
                $('#modal_captures_album button').each(function () {
                    $(this).attr('disabled', 'disabled');
                });
                if (flag) return;
                flag = true;
                album_inner_left = 0;
                tab_captures = [];
                var tablist = GetSelectedTablist();
                if (tablist.length == 0)
                    return;
                var src = [];
                $('#captures_album_inner').empty();
                chrome.windows.getAll({ populate: true }, function (windows) {
                    var active_window = 0;
                    for (var i = 0; i < windows.length; ++i) {
                        if (windows[i].active)
                            active_window = i;
                    }
                    for (var i = 0; i < windows.length; ++i) {
                        chrome.windows.update(windows[i].id, {});
                        for (var j = 0; j < windows[i].tabs.length; ++j) {
                            if (windows[i].tabs[j].active) {
                                (function (a, b) {
                                    src.push(windows[a].tabs[b].id);
                                })(i, j);
                            }
                        }
                    }
                    var end_flag = 0;
                    function get_image_func(t) {
                        if (t >= tablist.length) {
                            return function () {
                                end_flag = 1;
                            };
                        }
                        return function () {
                            chrome.tabs.get(tablist[t], function (tab) {
                                var wid = parseInt(tab.windowId);
                                var tid = parseInt(tab.index);
                                chrome.tabs.update(tab.id, { selected: true }, function (tab) {
                                    chrome.tabs.getSelected(wid, function () {
                                        setTimeout(function () {
                                            var timeout1 = setTimeout(function () {
                                                tab_captures.push({ tabid: t, ok: false, data: null });
                                                setTimeout(get_image_func(t + 1), 0);
                                            }, 150);
                                            chrome.tabs.captureVisibleTab(wid, {}, function (data) {
                                                tab_captures.push({ tabid: t, ok: true, data: data });
                                                setTimeout(get_image_func(t + 1), 0);
                                                clearTimeout(timeout1);
                                            });
                                        }, 100);
                                    });
                                });
                            });
                        };
                    }
                    setTimeout(get_image_func(0), 0);
                    var timer1 = setInterval(function () {
                        if (end_flag == 1) {
                            var cnt = 0;
                            var width_sum = 0;
                            for (var i = 0; i < tablist.length; ++i) {
                                (function (capture) {
                                    if (!capture.ok)
                                        return;
                                    var img = $(document.createElement('img'))
                                        .attr('src', capture.data)
                                        .click(function () {
                                            // show pic
                                            window.open(capture.data, '_blank');
                                        });
                                    var item = $(document.createElement('div'))
                                        .addClass('item')
                                        .css({ opacity: 1 })
                                        .append(img);
                                    $(img)
                                        .hover(function () {
                                            $(item).animate({ paddingTop: '30px' }, 'fast');
                                        }, function () {
                                            $(item).animate({ paddingTop: '0px' }, 'fast');
                                        });
                                    $('#captures_album_inner').append(item);
                                    cnt++;
                                })(tab_captures[i]);
                            }
                            if (cnt == 0)
                                return;
                            album_inner_cnt = 0;
                            album_inner_width = cnt * 200;
                            $('#captures_album_inner').css({ left: 0 });
                            $('#album_nav_bar').css({ left: 0 });
                            $('#captures_album_inner').width(cnt * 200);
                            $('#captures_album_inner').height(240);
                            $('#album_nav_bar').width(parseInt($(window).width() / cnt));
                            for (var i = 0; i < src.length; ++i) {
                                chrome.tabs.update(src[i], { selected: true });
                            }
                            $('#captures_album_backdrop').show();
                            $('#captures_album').show();
                            $('#captures_album_controller').show();
                            $('#modal_captures_album').modal('hide');
                            flag = false;
                            clearInterval(timer1);
                            setTimeout(function () {
                                $('#captures_album_inner > div').each(function () {
                                    var i = $('img', this);
                                    i.css('margin-top', '-' + parseInt(i.height() / 2) + 'px');
                                });
                            }, 0);
                        }
                    }, 50);
                });
            });

            $('#close_tabs').click(function () {
                var tablist = GetSelectedTablist();
                if (tablist.length == 0)
                    return;
                $('#modal_close_confirm').modal('show');
            });
            $('#close_tabs_command').click(function () {
                $('#modal_close_confirm').modal('hide');
                var tablist = GetSelectedTablist();
                if (tablist.length == 0)
                    return true;
                chrome.tabs.remove(tablist, function () {
                    setTimeout(function () {
                        keyword = last;
                        Search();
                    }, 1000);
                });
            });
            $('#get_all_tabs').click(function () {
                GetAllTabs();
            });
            function EnableCaseSensitive() {
                $('#enable_case_sensitive_input').attr('checked', 'checked');
            }
            function DisableCaseSensitive() {
                $('#enable_case_sensitive_input').removeAttr('checked');
            }
            function CaseSensitiveToggle() {
                LoadSetting();
                if (setting['enable_case_sensitive'])
                    DisableCaseSensitive();
                else
                    EnableCaseSensitive();
                setting['enable_case_sensitive'] = !setting['enable_case_sensitive'];
                SaveSetting();
            }
            function CaseSensitiveInit() {
                LoadSetting();
                if (setting['enable_case_sensitive'])
                    EnableCaseSensitive();
                else
                    DisableCaseSensitive();
            }
            $('#enable_case_sensitive_input')
                .ready(function () {
                    $('#enable_case_sensitive_input')
                        .click(function () {
                            CaseSensitiveToggle();
                        });
                    CaseSensitiveInit();
                });

            // enter apply
            function isEnterKey(e) {
                return e.which == 13;
            }

            $('#outer_wrapper').show('fast', function () {
                setTimeout(function () {
                    $('#search_input').focus();
                    $(window).resize(function resize() {
                        $('body').height($(window).height());
                        $('#outer_wrapper').height($(window).height());
                        $('#search_input').tooltip('show');
                    });
                    $(window).resize();
                }, 300);
                //$('#search_input').focus();
            });
        }

        // events
        function InitEvents(search_flag) {
            $('#wrapper1').hide();
        }

        function Init(search_flag) {
            HashIndexInit();
            InitEvents(search_flag);
        }

        function init(search_flag) {
            // local storage
            hist = [];
            histsize = 500;
            histindex = 0;
            hash = location.hash.substring(1);
            LoadHistory();

            $('#search_input')
                        .typeahead({
                            source: hist
                        })
            UpdateRecentSearch();

            var result_number = 0;

            Init(search_flag);
            if (search_flag === true) {
                ReSearch();
            }
        }

        init();
        InitEventsOnce();

        // social net
        (function () {
            $('#stage3').before(''
            + '<div class="container">'
                + '<hr />'
            + '</div>'
            + '<div class="container">'
                + '<iframe src="https://www.facebook.com/plugins/like.php?href=https%3A%2F%2Fchrome.google.com%2Fwebstore%2Fdetail%2Fcdpohbejnbclggljmoijjcpdhbaaijfm&amp;send=false&amp;layout=standard&amp;width=450&amp;show_faces=false&amp;action=like&amp;colorscheme=light&amp;font=verdana&amp;height=35"'
                    + ' scrolling="no" frameborder="0" style="border: none; overflow: hidden; width: 450px;'
                    + ' height: 35px;" allowtransparency="true"></iframe>'
            + '</div>');
        })();
        (function () {
            window.___gcfg = { lang: Locale['lang'] };
            var po = document.createElement('script'); po.type = 'text/javascript'; po.async = true;
            po.src = 'https://apis.google.com/js/plusone.js';
            var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
        })();
    });
});