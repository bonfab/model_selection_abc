﻿var FRHeader = (function ($, document) {


    var $headerContainer = $('div.header-compact').first();

    var FRHeaderResources = (function () {

        $('body').addClass('v2 ibar-frontiers');
        $headerContainer.addClass('v2 ibar-frontiers ibar-frontiers');

        $headerContainer.on('click', 'a[data-header-tracking]', function () {
            var data = $(this).data('header-tracking');
            event(data.category, data.action, data.label);
        });

        manageHashNavigation(false, window.location.hash);


        //-->> jQuery plug-in for smartDropDowns..
        (function ($) {

            var headerSmartDropDownsCounter = 0;

            $.fn.headerSmartDropDown = function (options) {

                if (!this || !this.length) {
                    return this;
                }

                // These are the defaults.
                var settings = $.extend({
                    align: "left",
                    type: "back", // breadcrum or back
                    doubleColumnLevel: false, // or number of level it should become double
                    offsetTop: 0,
                    offsetLeft: 0,
                    arrowOffsetLeft: 20,
                    arrowOffsetTop: 0,
                    data: [{ name: "No data set.", url: "" }],
                    firstLevelName: "FIRST LEVEL",
                    initialLevelPrefix: "All in ",
                    initialUrl: undefined, // All in ... link on the first level
                    dataLinkUrlDomain: '',
                    includeHtmlTestIds: false
                }, options);

                if ($.isFunction(settings.data)) {
                    settings.data = settings.data();
                }

                function resizeMenu(id) {

                    var h = $(window).height();
                    var diff = $("#ssd-dropdown-" + id).offset().top - $(document).scrollTop();

                    var lastContainer = $("#ssd-dropdown-" + id + " .ssd-dropdown_container:last");
                    var variableHeight = 55; // with crum layer

                    if (lastContainer.siblings(".ssd-crum").size() === 0) {
                        // without crum layer
                        variableHeight = 19;
                    }

                    var newheight = (h - diff - 7) - variableHeight;
                    // console.log("height: " + (h-diff-7));
                    lastContainer.css("height", newheight);

                    var dc = $("#ssd-dropdown-" + id + " .ssd-dropdown_container:last")[0];
                    if (dc.offsetHeight < dc.scrollHeight) {
                        // Scrolling
                        $("#ssd-dropdown-" + id + "  .ssd-menu-list li").addClass("with-scroll");
                    } else {
                        // Not Scrolling
                        $("#ssd-dropdown-" + id + "  .ssd-menu-list li").removeClass("with-scroll");
                        $(dc).css("height", "auto");
                    }
                }

                function hideSmartDropDown(e) {

                    // if the click was outside of the current dropdown, hide and reset the dropdown.
                    if ($(e.target).parents("#ssd-dropdown-" + id).size() != 0) return;

                    $("#ssd-dropdown-" + id + " .ssd-dropdown_container").css("height", "auto");
                    $("#ssd-dropdown-" + id).css("height", "auto");
                    $("#ssd-dropdown-" + id).hide();
                    $("#ssd-dropdown-indicator-" + id).hide();
                    $("#ssd-dropdown-" + id + "-tr-1").html("");
                    //$('.dropdown-trigger.highlight').removeClass('highlight');
                    createNextLevel(initial_size, settings.firstLevelName, settings.data, settings.initialUrl, $(this));
                }

                function bindEvents() {

                    $(document).on('click', hideSmartDropDown);

                    $(document).on('touchstart', hideSmartDropDown);
                }


                /* This method adds a level to the dropdown. This method is called each time the level is changed and once when initialized. */
                function createNextLevel(type, name, children, url, element, path) {

                    var isFirstLevel = (path == undefined) ? true : false; // The path is undefined for the first level

                    // Each level is represented by a TD
                    var td = $(document.createElement("td"));
                    // The TD contains a div as a container (used for scrolling)
                    var container = $(document.createElement("div"));
                    container.addClass("ssd-dropdown_container");
                    container.attr("id", "sdd-dropdown-" + id + "-container-level-" + (isFirstLevel ? 1 : path.length));
                    if (type == "big") {
                        container.addClass("ssd-dropdown_container_big");
                    } else {
                        container.addClass("ssd-dropdown_container_small");
                    }

                    // The path is undefined for the first level
                    // Initialize the path with 
                    if (isFirstLevel) {
                        path = Array();
                        path.push(settings.firstLevelName);
                    }

                    // Prepare the list depending on the double column setting
                    // 1 UL if 1 column, 2 ULs in 2 DIVs for 2 columns
                    if (settings.doubleColumnLevel != false && settings.doubleColumnLevel <= path.length) {
                        // 2 columns
                        var leftCol = $(document.createElement("div"));
                        leftCol.addClass("column-left");
                        var rightCol = $(document.createElement("div"));
                        rightCol.addClass("column-right");
                        var ul1 = $(document.createElement("ul"));
                        ul1.addClass("ssd-menu-list");
                        ul1.addClass("ssd-dropdown_ul");
                        var ul2 = $(document.createElement("ul"));
                        ul2.addClass("ssd-menu-list");
                        ul2.addClass("ssd-dropdown_ul");
                        leftCol.append(ul1);
                        rightCol.append(ul2);
                        container.append(leftCol);
                        container.append(rightCol);
                    } else {
                        // 1 column
                        var ul1 = $(document.createElement("ul"));
                        ul1.addClass("ssd-menu-list");
                        ul1.addClass("ssd-dropdown_ul");
                        container.append(ul1);
                    }
                    tr.append(td);
                    td.append(container);

                    // Render Path depending on type
                    if (settings.type == "breadcrum") {
                        // Render Path
                        if (path != undefined && path.length > 1) {
                            var backpath = $(document.createElement("div"));
                            backpath.addClass("ssd-crum");

                            for (var k = 0; k < (path.length - 1); k++) {
                                var link = $(document.createElement("a"));
                                link.attr("id", "ssd-crum-" + id + "-" + (k + 1));
                                link.attr("ssd-returnto", k + 1);
                                //link.html(path[k]);
                                link.html('<span class="ssd-crum-oneline">' + path[k] + '</span>');
                                var index = k;
                                link.click(function () {
                                    // What happens when the user clicks on the appropriate breadcrum item

                                    element.checkDuplicateEvent('clear');
                                    if ($(this).checkDuplicateEvent())
                                        return;
                                    var returnto = $(this).attr("ssd-returnto");
                                    var container1 = $("#sdd-dropdown-" + id + "-container-level-" + returnto);
                                    var positionToGo = -1 * (container1.position().left);

                                    $("#ssd-container-" + id).animate({ "margin-left": positionToGo + "px" }, 400, function () {
                                        container1.parent().nextAll().remove();
                                        $("#ssd-dropdown-" + id + " .ssd-dropdown_container:last").css("height", "auto");
                                        $("#ssd-dropdown-" + id).css("height", "auto");
                                        resizeMenu(id);

                                    });
                                    //remove from path
                                    var toBeRemovedNo = path.length - returnto;
                                    path.splice(path.length - toBeRemovedNo, toBeRemovedNo);
                                    if (settings.doubleColumnLevel != false && settings.doubleColumnLevel > path.length) {
                                        if (settings.align == "right") {
                                            $("#ssd-dropdown-" + id).animate({ "width": "200px", "margin-left": -150 + settings.offsetLeft + "px" }, 400);
                                        } else {
                                            $("#ssd-dropdown-" + id).animate({ "width": "200px" }, 400);
                                        }

                                    }

                                });

                                link.append('<img src="' + window.FRHeaderTaxonomyData.Navigation.FrontiersJournalAPIUrl + '/Areas/Header/Content/Images/crum-arrow.png">');
                                backpath.append(link);
                            }
                            backpath.append('<span class="ssd-crum-current ssd-crum-oneline level-' + path.length + '">' + name + '</span>');
                            container.before(backpath);

                        }
                    } else {
                        if (path != undefined && path.length > 1) {
                            var backpath = $(document.createElement("div"));
                            backpath.addClass("ssd-crum");
                            backpath.addClass("ssd-back-btn-container");
                            var link = $(document.createElement("a"));

                            link.attr("id", "ssd-crum-" + id);
                            link.attr("ssd-returnto", path.length - 1);
                            link.html('<img src="' + window.FRHeaderTaxonomyData.Navigation.FrontiersJournalAPIUrl + '/Areas/Header/Content/Images/nav-arrow-left.png">back');
                            link.append('<img src="' + window.FRHeaderTaxonomyData.Navigation.FrontiersJournalAPIUrl + '/Areas/Header/Content/Images/crum-arrow.png">');
                            link.click(function () {
                                // What happens when the user clicks back

                                element.checkDuplicateEvent('clear');
                                if ($(this).checkDuplicateEvent())
                                    return;
                                var returnto = $(this).attr("ssd-returnto");
                                var container1 = $("#sdd-dropdown-" + id + "-container-level-" + returnto);
                                var positionToGo = -1 * (container1.position().left);
                                //console.log(container1.position().left);
                                $("#ssd-container-" + id).animate({ "margin-left": positionToGo + "px" }, 400, function () {
                                    container1.parent().nextAll().remove();
                                    $("#ssd-dropdown-" + id + " .ssd-dropdown_container:last").css("height", "auto");
                                    $("#ssd-dropdown-" + id).css("height", "auto");
                                    resizeMenu(id);
                                });
                                //remove from path
                                var toBeRemovedNo = path.length - returnto;
                                path.splice(path.length - toBeRemovedNo, toBeRemovedNo);
                                if (settings.doubleColumnLevel != false && settings.doubleColumnLevel > path.length) {
                                    if (settings.align == "right") {
                                        $("#ssd-dropdown-" + id).animate({ "width": "200px", "margin-left": -150 + settings.offsetLeft + "px" }, 400);// Replace  194 with 150 => new calculation implemented.
                                    } else {
                                        $("#ssd-dropdown-" + id).animate({ "width": "200px" }, 400);
                                    }

                                }
                            });
                            backpath.append(link);
                            backpath.append('<span class="ssd-crum-current ssd-crum-oneline">' + name + '</span>');
                            container.before(backpath);
                        }
                    }

                    if (url != undefined && url != null && url.length) {

                        var item = children[j];
                        var li1 = $(document.createElement("li"));
                        li1.addClass();
                        var a = $(document.createElement("a"));
                        a.addClass("sdd-link");
                        if (settings.includeHtmlTestIds == true) {
                            a.attr('data-test-id', (settings.initialLevelPrefix + name).toLowerCase().replace(/ /g, '_'));
                        }

                        if (url.indexOf('javascript:') == 0 || url.indexOf('http:') == 0 || url.indexOf('https:') == 0) {
                            a.attr('href', url);
                        } else {
                            a.attr('href', settings.dataLinkUrlDomain + url);
                        }

                        a.html(settings.initialLevelPrefix + name);
                        li1.html(a);
                        ul1.append(li1);
                    }

                    // Render list of current level
                    for (var j = 0; j < children.length; j++) {

                        var item = children[j];
                        var li1 = $(document.createElement('li'));
                        li1.addClass();

                        if (item.type == 'label') {
                            li1.html('<span class="ssd-label">' + item.label + '</span>');
                            li1.addClass('ssd-label-li');
                        } else if (!item.c || !item.c.length) {
                            var a = $(document.createElement('a'));
                            a.addClass('sdd-link');

                            if (item.url && item.url.length && (item.url.indexOf('javascript:') == 0 || item.url.indexOf('http:') == 0 || item.url.indexOf('https:') == 0)) {
                                a.attr('href', item.url);
                            } else {
                                a.attr('href', settings.dataLinkUrlDomain + item.url);
                            }

                            if (item.dataTracking && item.dataTracking.length) {
                                a.attr('data-header-tracking', item.dataTracking);
                            }
                            if (settings.includeHtmlTestIds == true) {
                                a.attr('data-test-id', item.n.toLowerCase().replace(/ /g, '_'));
                            }

                            a.html(item.n);
                            li1.html(a);
                        } else {

                            li1.html('<img src="' + window.FRHeaderTaxonomyData.Navigation.FrontiersJournalAPIUrl + '/Areas/Header/Content/Images/nav-arrow-right-v2.png" />' + item.n);
                            li1.attr("ssd-data-id", item.id);
                            if (settings.includeHtmlTestIds == true) {
                                li1.attr('data-test-id', item.n.toLowerCase().replace(/ /g, '_'));
                            }
                            var children2 = item.c;
                            li1.data("ssd-children", children2);
                            li1.data("ssd-url", item.url);
                            li1.data("ssd-dataTracking", item.dataTracking);
                            li1.click(function () {

                                element.checkDuplicateEvent('clear');
                                if ($(this).checkDuplicateEvent())
                                    return;
                                var children = $(this).data("ssd-children");
                                var url2 = $(this).data("ssd-url");
                                path.push($(this).text());
                                var size = "small";
                                if (settings.doubleColumnLevel != false && settings.doubleColumnLevel <= path.length) {
                                    size = "big";
                                }

                                createNextLevel(size, $(this).text(), children, url2, $(this), path);
                                if ($(this).data("ssd-dataTracking") && $(this).data("ssd-dataTracking").length) {
                                    var data = JSON.parse($(this).data("ssd-dataTracking"));
                                    FRHeaderResources.event(data.category, data.action, data.label);
                                }
                            });

                        }
                        if (settings.doubleColumnLevel != false && settings.doubleColumnLevel <= path.length) {
                            if (j % 2 == 0) {
                                ul1.append(li1);
                            } else {
                                ul2.append(li1);
                            }
                        } else {
                            ul1.append(li1);
                        }

                    }
                    tr.append(td);
                    td.append(container);

                    var moveto = (-1 * container.position().left);

                    if (settings.doubleColumnLevel != false && settings.doubleColumnLevel <= path.length) {

                        if (settings.align == "right") {
                            $("#ssd-dropdown-" + id).animate({ "width": "400px", "margin-left": (-350 + settings.offsetLeft + "px") }, 400);
                        } else {

                            $("#ssd-dropdown-" + id).animate({ "width": "400px" }, 400);

                        }
                    } else {

                        $("#ssd-dropdown-" + id).css("width", "200px");
                        if (settings.align == "right") {
                            $("#ssd-dropdown-" + id).css("margin-left", -150 + settings.offsetLeft + "px");
                        }

                    }

                    if (isFirstLevel) {
                        moveto = 0;
                    }

                    $("#ssd-container-" + id).animate({ "margin-left": moveto + "px" }, 400, function () {
                        $("#ssd-dropdown-" + id + " .ssd-dropdown_container").css("height", "20px");
                        $("#ssd-dropdown-" + id + " .ssd-dropdown_container:last").css("height", "auto");
                        resizeMenu(id);
                    });


                } // END OF createNextLevel


                // Get new ID
                var id = headerSmartDropDownsCounter + 1;
                headerSmartDropDownsCounter = id;

                // Create html of the dropdown
                var indicator = $(document.createElement("div"));
                indicator.attr("id", "ssd-dropdown-indicator-" + id);
                indicator.addClass("ssd-dropdown-indicator");

                var dropdown = $(document.createElement("div"));
                dropdown.attr("id", "ssd-dropdown-" + id);
                dropdown.addClass("ssd-dropdown");

                if (settings.align == "right") {
                    dropdown.addClass("right");
                    indicator.addClass("indicator-right");
                }

                var margin_left = dropdown.css("margin-left");

                if (margin_left && margin_left.length) {
                    margin_left = margin_left.replace(/[^-\d\.]/g, '');
                }

                var new_margin_left = 1 * (margin_left) + 1 * (settings.offsetLeft);
                dropdown.css("margin-left", new_margin_left);

                var dropdown_inner_container = $(document.createElement("div"));
                dropdown_inner_container.attr("id", "ssd-container-" + id);
                dropdown_inner_container.addClass("ssd-container");

                var table = $(document.createElement("table"));
                var tr = $(document.createElement("tr"));
                tr.attr("id", "ssd-dropdown-" + id + "-tr-" + 1);

                dropdown.append(dropdown_inner_container);
                dropdown_inner_container.append(table);
                table.append(tr);

                // Determine if the mode is small or big (1 or 2 columns)
                var initial_size = "small";

                if (settings.doubleColumnLevel != false && settings.doubleColumnLevel <= 1) {
                    initial_size = "big";
                }

                // insert html behind button
                var button = this;
                button.attr("ssd-id", id);
                button.after(indicator);
                button.after(dropdown);
                dropdown.data("ssd-trigger", button);

                // Create the first level of the menu
                createNextLevel(initial_size, settings.firstLevelName, settings.data, settings.initialUrl);

                // set the initial position of the dropdown
                var topPosition = button.position().top + button.outerHeight();

                $("#ssd-dropdown-indicator-" + id)
                    .css("top", button.outerHeight() + settings.arrowOffsetTop)
                    .css("left", settings.arrowOffsetLeft);

                $("#ssd-dropdown-" + id)
                    .css("top", topPosition + 5 + settings.offsetTop);

                // set the trigger to show the menu    
                button.on('click', function () {

                    $("body").click();

                    //$(this).addClass('highlight');

                    var topPosition = $(this).position().top + $(this).outerHeight();
                    $(".ssd-dropdown, .ssd-dropdown-indicator").hide();
                    var id = $(this).attr("ssd-id");

                    $("#ssd-dropdown-indicator-" + id)
                        .css("top", $(this).outerHeight() + settings.arrowOffsetTop)
                        .css("left", settings.arrowOffsetLeft)
                        .show();

                    $("#ssd-dropdown-" + id)
                        .css("top", topPosition + settings.offsetTop)
                        .show();

                    resizeMenu(id);
                    return false;
                });

                // Trigger Resize Logic
                $(window).resize(function () {

                    if (!$("#ssd-dropdown-" + id).is(":visible")) return;

                    resizeMenu(id);
                });

                bindEvents();


                return this.each(function () {
                    // Do something to each element here.
                });
            };

        }(jQuery));


        //-->> jQuery plug-in for tool-tips..
        if (typeof $.fn.tipTip === 'undefined') {
            (function ($) {
                $.fn.tipTip = function (options) {
                    var defaults = {
                        activation: "hover",
                        keepAlive: false,
                        maxWidth: "200px",
                        edgeOffset: 3,
                        defaultPosition: "bottom",
                        delay: 400,
                        fadeIn: 200,
                        fadeOut: 200,
                        attribute: "title",
                        content: false, // HTML or String to fill TipTIp with
                        enter: function () { },
                        exit: function () { }
                    };
                    var opts = $.extend(defaults, options);

                    // Setup tip tip elements and render them to the DOM
                    if ($("#tiptip_holder").length <= 0) {
                        var tiptip_holder = $('<div id="tiptip_holder" style="max-width:' + opts.maxWidth + ';"></div>');
                        var tiptip_content = $('<div id="tiptip_content"></div>');
                        var tiptip_arrow = $('<div id="tiptip_arrow"></div>');
                        $("body").append(tiptip_holder.html(tiptip_content).prepend(tiptip_arrow.html('<div id="tiptip_arrow_inner"></div>')));
                    } else {
                        var tiptip_holder = $("#tiptip_holder");
                        var tiptip_content = $("#tiptip_content");
                        var tiptip_arrow = $("#tiptip_arrow");
                    }

                    return this.each(function () {
                        var org_elem = $(this);
                        if (opts.content) {
                            var org_title = opts.content;
                        } else {
                            var org_title = org_elem.attr(opts.attribute);
                        }
                        if (org_title != "") {
                            if (!opts.content) {
                                org_elem.removeAttr(opts.attribute); //remove original Attribute
                            }
                            var timeout = false;

                            if (opts.activation == "hover") {
                                org_elem.hover(function () {
                                    active_tiptip();
                                }, function () {
                                    if (!opts.keepAlive) {
                                        deactive_tiptip();
                                    }
                                });
                                if (opts.keepAlive) {
                                    tiptip_holder.hover(function () { }, function () {
                                        deactive_tiptip();
                                    });
                                }
                            } else if (opts.activation == "focus") {
                                org_elem.focus(function () {
                                    active_tiptip();
                                }).blur(function () {
                                    deactive_tiptip();
                                });
                            } else if (opts.activation == "click") {
                                org_elem.click(function () {
                                    active_tiptip();
                                    return false;
                                }).hover(function () { }, function () {
                                    if (!opts.keepAlive) {
                                        deactive_tiptip();
                                    }
                                });
                                if (opts.keepAlive) {
                                    tiptip_holder.hover(function () { }, function () {
                                        deactive_tiptip();
                                    });
                                }
                            }

                            function active_tiptip() {
                                opts.enter.call(this);
                                tiptip_content.html(org_title);
                                tiptip_holder.hide().removeAttr("class").css("margin", "0");
                                tiptip_arrow.removeAttr("style");

                                var top = parseInt(org_elem.offset()['top']);
                                var left = parseInt(org_elem.offset()['left']);
                                var org_width = parseInt(org_elem.outerWidth());
                                var org_height = parseInt(org_elem.outerHeight());
                                var tip_w = tiptip_holder.outerWidth();
                                var tip_h = tiptip_holder.outerHeight();
                                var w_compare = Math.round((org_width - tip_w) / 2);
                                var h_compare = Math.round((org_height - tip_h) / 2);
                                var marg_left = Math.round(left + w_compare);
                                var marg_top = Math.round(top + org_height + opts.edgeOffset);
                                var t_class = "";
                                var arrow_top = "";
                                var arrow_left = Math.round(tip_w - 12) / 2;

                                if (opts.defaultPosition == "bottom") {
                                    t_class = "_bottom";
                                } else if (opts.defaultPosition == "top") {
                                    t_class = "_top";
                                } else if (opts.defaultPosition == "left") {
                                    t_class = "_left";
                                } else if (opts.defaultPosition == "right") {
                                    t_class = "_right";
                                }

                                var right_compare = (w_compare + left) < parseInt($(window).scrollLeft());
                                var left_compare = (tip_w + left) > parseInt($(window).width());

                                if ((right_compare && w_compare < 0) || (t_class == "_right" && !left_compare) || (t_class == "_left" && left < (tip_w + opts.edgeOffset + 5))) {
                                    t_class = "_right";
                                    arrow_top = Math.round(tip_h - 13) / 2;
                                    arrow_left = -12;
                                    marg_left = Math.round(left + org_width + opts.edgeOffset);
                                    marg_top = Math.round(top + h_compare);
                                } else if ((left_compare && w_compare < 0) || (t_class == "_left" && !right_compare)) {
                                    t_class = "_left";
                                    arrow_top = Math.round(tip_h - 13) / 2;
                                    arrow_left = Math.round(tip_w);
                                    marg_left = Math.round(left - (tip_w + opts.edgeOffset + 5));
                                    marg_top = Math.round(top + h_compare);
                                }

                                var top_compare = (top + org_height + opts.edgeOffset + tip_h + 8) > parseInt($(window).height() + $(window).scrollTop());
                                var bottom_compare = ((top + org_height) - (opts.edgeOffset + tip_h + 8)) < 0;

                                if (top_compare || (t_class == "_bottom" && top_compare) || (t_class == "_top" && !bottom_compare)) {
                                    if (t_class == "_top" || t_class == "_bottom") {
                                        t_class = "_top";
                                    } else {
                                        t_class = t_class + "_top";
                                    }
                                    arrow_top = tip_h;
                                    marg_top = Math.round(top - (tip_h + 5 + opts.edgeOffset));
                                } else if (bottom_compare | (t_class == "_top" && bottom_compare) || (t_class == "_bottom" && !top_compare)) {
                                    if (t_class == "_top" || t_class == "_bottom") {
                                        t_class = "_bottom";
                                    } else {
                                        t_class = t_class + "_bottom";
                                    }
                                    arrow_top = -12;
                                    marg_top = Math.round(top + org_height + opts.edgeOffset);
                                }

                                if (t_class == "_right_top" || t_class == "_left_top") {
                                    marg_top = marg_top + 5;
                                } else if (t_class == "_right_bottom" || t_class == "_left_bottom") {
                                    marg_top = marg_top - 5;
                                }
                                if (t_class == "_left_top" || t_class == "_left_bottom") {
                                    marg_left = marg_left + 5;
                                }
                                tiptip_arrow.css({ "margin-left": arrow_left + "px", "margin-top": arrow_top + "px" });
                                tiptip_holder.css({ "margin-left": marg_left + "px", "margin-top": marg_top + "px" }).attr("class", "tip" + t_class);

                                if (timeout) {
                                    clearTimeout(timeout);
                                }
                                timeout = setTimeout(function () { tiptip_holder.stop(true, true).fadeIn(opts.fadeIn); }, opts.delay);
                            }

                            function deactive_tiptip() {
                                opts.exit.call(this);
                                if (timeout) {
                                    clearTimeout(timeout);
                                }
                                tiptip_holder.fadeOut(opts.fadeOut);
                            }

                        }
                    });
                };
            })(jQuery);
        }


        //-->> jQuery plug-in to handle duplicate events..
        if (typeof $.fn.checkDuplicateEvent === 'undefined') {
            (function ($) {
                var methods = {
                    check: function () {
                        var $this = (this).first();
                        if ($this.data('disabled')) return true;
                        $this.data('disabled', true);
                        return false;
                    },
                    clear: function () {
                        return this.data('disabled', false);
                    }
                };
                $.fn.checkDuplicateEvent = function (method) {
                    if (methods[method]) {
                        return methods[method].apply(this);
                    } else {
                        return methods.check.apply(this);
                    }
                };
            })(jQuery);
        }


        //-->> jQuery extension method for cross-domain Ajax calls..
        if (typeof $.ajaxCrossDomainAPI === 'undefined') {
            $.extend({
                ajaxCrossDomainAPI: function (options) {

                    var settings = $.extend({
                        url: '',
                        data: {},
                        type: 'GET',
                        success: function () { }
                    }, options);

                    if (settings.data && settings.data.cookies && settings.data.cookies.length) {

                        settings.data.cookies = getEncodedCookies(settings.data.cookies);
                    }

                    if ($.browser && $.browser.msie && parseInt($.browser.version, 10) < 10 && window.XDomainRequest) { //Checking for IE browser. 

                        if (document.location.protocol == 'http:') {
                            settings.url = settings.url.replace('https:', 'http:');
                        }

                        var xDomainRequest = new window.XDomainRequest();
                        xDomainRequest.open(settings.type, settings.url);
                        xDomainRequest.onprogress = function () { };
                        xDomainRequest.ontimeout = function () { };
                        xDomainRequest.onerror = function () { };
                        xDomainRequest.onload = function () { settings.success(eval('json = ' + xDomainRequest.responseText)); };
                        xDomainRequest.timeout = 0;
                        setTimeout(function () { xDomainRequest.send(JSON.stringify(settings.data)); }, 500);
                    } else {

                        $.ajax({
                            dataType: 'json',
                            traditional: true,
                            url: settings.url,
                            data: settings.data,
                            type: settings.type
                        })
                            .done(function (responseText) {
                                settings.success(responseText);
                            });
                    }
                }
            });
        }

        function EditPage() {

            if (typeof window.CurrentPageCode === 'undefined' || !window.CurrentPageCode.length) return;

            var editPageUrl = window.FRHeaderTaxonomyData.Navigation.SharepointWebsiteUrl + '/admin/contentEditor.aspx?editPageCode=' + window.CurrentPageCode;

            window.open(editPageUrl, '_blank');
        }

        $(document).on('click', 'a[href^="#"]', function () {

            var $this = $(this);

            if ($this.parents('#journalHome').length <= 0) return; // Limiting manageHashNavigation to V3 journal page only.

            var hashValue = $this.attr('href'); //window.location.hash; //Since click is getting fired before 'window.location.hash' is updated.

            if (!hashValue.length) return;

            manageHashNavigation(true, hashValue);
        });

        //-->> JS methods for Base64 encoding..
        var _keyStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

        function _utf8_encode(string) {
            string = string.replace(/\r\n/g, "\n");
            var utftext = "";

            for (var n = 0; n < string.length; n++) {

                var c = string.charCodeAt(n);

                if (c < 128) {
                    utftext += String.fromCharCode(c);
                } else if ((c > 127) && (c < 2048)) {
                    utftext += String.fromCharCode((c >> 6) | 192);
                    utftext += String.fromCharCode((c & 63) | 128);
                } else {
                    utftext += String.fromCharCode((c >> 12) | 224);
                    utftext += String.fromCharCode(((c >> 6) & 63) | 128);
                    utftext += String.fromCharCode((c & 63) | 128);
                }
            }
            return utftext;
        };

        function encode(input) {
            var output = "";
            var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
            var i = 0;
            input = _utf8_encode(input);

            while (i < input.length) {

                chr1 = input.charCodeAt(i++);
                chr2 = input.charCodeAt(i++);
                chr3 = input.charCodeAt(i++);

                enc1 = chr1 >> 2;
                enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
                enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
                enc4 = chr3 & 63;

                if (isNaN(chr2)) {
                    enc3 = enc4 = 64;
                } else if (isNaN(chr3)) {
                    enc4 = 64;
                }

                output = output + _keyStr.charAt(enc1) + _keyStr.charAt(enc2) + _keyStr.charAt(enc3) + _keyStr.charAt(enc4);
            }
            return output;
        }

        function getCookieValues(name) {

            var cookieValue = document.cookie;
            var cookieStart = cookieValue.indexOf(" " + name + "=");
            if (cookieStart == -1) {
                cookieStart = cookieValue.indexOf(name + "=");
            }
            if (cookieStart == -1) {
                cookieValue = null;
            } else {
                cookieStart = cookieValue.indexOf("=", cookieStart) + 1;
                var cookieEnd = cookieValue.indexOf(";", cookieStart);
                if (cookieEnd == -1) {
                    cookieEnd = cookieValue.length;
                }
                cookieValue = cookieValue.substring(cookieStart, cookieEnd);
            }
            return cookieValue;
        }

        function getAllCookieValues(names) {

            if (!names || !names.length) return '';

            var cookies = [];

            $(names).each(function (i, val) {
                var cookieValue = getCookieValues(val);
                if (cookieValue && cookieValue.length) {
                    cookies.push(val + '=' + cookieValue);
                }
            });

            return cookies.join('; ');
        }

        function getEncodedCookies() {

            var cookieValues = getAllCookieValues([window.FRHeaderTaxonomyData.Navigation.CookieNameLoginRemember, window.FRHeaderTaxonomyData.Navigation.CookieNameLogin, window.FRHeaderTaxonomyData.Navigation.CookieNameImpersonate]);
            var cookieValuesEncoded = encode(cookieValues);
            return encodeURIComponent(cookieValuesEncoded);
        }

        function generateCookie(cookieName, cookieValue) {

            var siteDomain = window.FRHeaderTaxonomyData.Navigation.SiteDomain;
            document.cookie = cookieName + '=' + cookieValue + ';domain=' + siteDomain + ';path=/';
        }

        function getCookie(cookieName) {

            cookieName = cookieName + "=";
            var cookies = document.cookie.split(';');
            for (var i = 0; i < cookies.length; i++) {
                var c = $.trim(cookies[i]);
                if (c.indexOf(cookieName) == 0) {
                    return c.substring(cookieName.length, c.length);
                }
            }
            return '';
        }

        function getCacheTimeStamp() {

            var cacheVersionInCookie = getCookie('FIS_CacheVersion');

            if (cacheVersionInCookie.length) {
                return cacheVersionInCookie;
            } else {
                var ibarCacheVersion = getTimeStamp();
                generateCookie('FIS_CacheVersion', ibarCacheVersion);
                return ibarCacheVersion;
            }
        }

        function refreshCacheTimeStamp() {

            generateCookie('FIS_CacheVersion', getTimeStamp());
        }

        function getTimeStamp() {

            var dateNow = new Date();
            return Date.parse(dateNow.toGMTString()) + dateNow.getMilliseconds();
        }

        function highlightCurrentMenu() {

            if (typeof window.CurrentIBarMenu === 'undefined' || !window.CurrentIBarMenu.length) {
                return;
            }

            if ($('body').hasClass('v2')) {

                $headerContainer.find('.' + window.CurrentIBarMenu).addClass('highlight');
            } else {

                $headerContainer.find('.' + window.CurrentIBarMenu).find('a').addClass('highlight');

                if (window.CurrentIBarMenu == 'journala2z') {
                    $headerContainer.find('.about').find('a').addClass('highlight');
                }
            }
        }

        function fixHtmlPlaceholderForIE() {

            // Checking for IE browser. 
            if (!$.browser || !$.browser.msie || parseInt($.browser.version, 10) >= 10) {
                return;
            }

            $headerContainer.find('input:text').each(function () {

                var $self = $(this);

                if ($self.val() == "" || $self.val() == null) {
                    $self.val($self.attr('placeholder'));
                }

                if ($self.attr('placeholder') != '' && $self.val() == $self.attr('placeholder')) {
                    $self.addClass("hasPlaceholder");
                }

                $self.focus(function () {
                    if ($self.attr('placeholder') != '' && $self.val() == $self.attr('placeholder')) {
                        $self.val('').removeClass('hasPlaceholder');
                    }
                })
                    .blur(function () {
                        if ($self.attr('placeholder') != '' && ($self.val() == '' || $self.val() == $self.attr('placeholder'))) {
                            $self.val($self.attr('placeholder')).addClass('hasPlaceholder');
                        }
                    });

                $headerContainer.find('form').submit(function () {
                    $self.find('.hasPlaceholder').each(function () { $self.val(''); });
                });

            });
        }

        function upperCaseFirstLetter(string) {
            return string.charAt(0).toUpperCase() + string.slice(1);
        }

        function goToFrontiersSearch(control) {

            var query = $.trim($(control).val());

            if (!query.length) return;

            if (window.FRHeaderProfileData.IsSearchEnabled) {//go to new search page
                window.location.href = window.FRHeaderTaxonomyData.Navigation.HomeUIUrl + '/search?query=' + encodeURIComponent(query) + '&ht=1';
            }
            else {
                window.location.href = window.FRHeaderTaxonomyData.Navigation.SharepointWebsiteUrl + '/SearchServer.aspx?sq=' + encodeURIComponent(query) + '&ht=1';
            }
        }

        function manageHashNavigation(isHashChange, hashValue) {

            if (!hashValue || !hashValue.length) return;

            hashValue = hashValue.substr(1); //-->>Removing leading hash(#).

            if (hashValue.substr(0, 1) == '_') {
                hashValue = hashValue.substr(1); //-->>Removing leading underscore, if any.
            }

            var navigateToHash = function () {

                var $element = $('body').find('[id="' + hashValue + '"]');

                if (!$element || !$element.length) return;

                if ($element.closest('#journalHome').length <= 0) return; //-->>Limiting manageHashNavigation to V3 journal page only.

                if ($element.hasClass('reset-hash-position')) return; //-->>Limiting if hash reset already handled for the element.

                window.location.hash = '_' + hashValue; //-->>To prevent default hash jump. 

                var hashScrollTop = parseInt($element.offset().top) - parseInt($('body').css('padding-top'));

                $('html, body').animate({ scrollTop: hashScrollTop }, 100);
            };

            var timeOut = isHashChange ? 100 : 500;
            setTimeout(navigateToHash, timeOut);
        }

        function loadStyle(url) {

            var link = document.createElement('link');
            link.type = 'text/css';
            link.rel = 'stylesheet';
            link.onload = function () { };
            link.href = url;
            document.getElementsByTagName('head')[0].appendChild(link);
        }

        function loadScript(url, callback) {

            if (!url || !(typeof url === 'string')) {
                return;
            };

            var script = document.createElement('script');

            //if this is IE8 and below, handle on-load differently
            if (typeof document.attachEvent === "object") {
                script.onreadystatechange = function () {
                    if (script.readyState === 'loaded' || script.readyState === 'complete') {
                        if (callback) {
                            callback(); //once the script is loaded, run the callback
                        }
                    };
                };
            } else {
                //this is not IE8 and below, so we can actually use on-load
                script.onload = function () {
                    if (callback) {
                        callback(); //once the script is loaded, run the callback
                    };
                };
            };

            //create the script and add it to the DOM
            script.src = url;
            script.type = 'text/javascript';
            document.getElementsByTagName('head')[0].appendChild(script);
        }

        function event(category, action, opt_label, opt_value, opt_noninteraction) {
            //-->> Google Analytics
            if (window._gaq) {
                if (opt_value != 'undefined' && opt_noninteraction != 'undefined') {
                    window._gaq.push(['_trackEvent', category, action, opt_label, opt_value, opt_noninteraction]);
                } else {
                    window._gaq.push(['_trackEvent', category, action, opt_label]);
                }
                return;
            }

            //-->> Google Analytics Universal
            if (window.ga) {
                //alert('category:'+ category + ',action:' + action + ',opt_label:' + opt_label);
                window.ga('send', 'event', category, action, opt_label);
                return;
            }
        }

        function trackOutboundLink(category, action, opLabel, url) {

            if (window.ga != undefined && window.ga.hasOwnProperty('loaded') && window.ga.loaded === true) {
                window.ga('send', 'event', category, action, opLabel, {
                    'hitCallback':
                    function () {
                        document.location = url;
                    }
                });
            } else {
                document.location = url;
            }
        }

        return {
            loadStyle: loadStyle,
            loadScript: loadScript,
            getCookies: getEncodedCookies,
            getTimeStamp: getCacheTimeStamp,
            refreshCacheTimeStamp: refreshCacheTimeStamp,
            highlightCurrentMenu: highlightCurrentMenu,
            fixHtmlPlaceholderForIE: fixHtmlPlaceholderForIE,
            upperCaseFirstLetter: upperCaseFirstLetter,
            goToFrontiersSearch: goToFrontiersSearch,
            event: event,
            trackOutboundLink: trackOutboundLink,
            EditPage: EditPage
        };

    })();


    var FRHeaderLoggedOut = (function () {

        function fill() {

            render();
            bindEvents();
        }

        function render() {

            $headerContainer.find('.logged-user').addClass('hidden');
            $headerContainer.find('.search-icon-container').removeClass('hidden');
            $headerContainer.find('.unlogged-user').removeClass('hidden');
        }

        function bindEvents() {

            // Login Button
            $(document).on('click', function (e) {

                var $target = $(e.target) || $(e.srcElement);
                var $popover = $target.closest('.popover-login');
                var $popoverTrigger = $target.closest('.popover-login-trigger');

                if ($popoverTrigger.length) {
                    e.preventDefault();
                    $headerContainer.find('.popover-login').fadeToggle('fast', 'swing');
                    $headerContainer.find('.popover-login [name="txtLoginEmail"]').focus();
                } else if (!$popover.length) {
                    $headerContainer.find('.popover-login').hide();
                }
            });

            // Login Info Button
            $(document).on('click', function (e) {

                var $target = $(e.target) || $(e.srcElement);
                var $popover = $target.closest('.popover-login-info');
                var $popoverTrigger = $target.closest('.popover-login-info-trigger');

                if ($popoverTrigger.length) {
                    e.preventDefault();
                    $headerContainer.find('.popover-login-info').fadeToggle('fast', 'swing');
                } else if (!$popover.length) {
                    $headerContainer.find('.popover-login-info').hide();
                }
            });

            // Social login
            $headerContainer.find('.third-party-button-wrapper a').on('click', function () {

                var openIdType = $(this).data('type');

                window.location.href = window.FRHeaderTaxonomyData.Navigation.SharepointWebsiteUrl + "/Registration/Register.aspx?openIdType=" + openIdType;
            });
        }

        return { fill: fill };

    })();


    var FRHeaderLoggedIn = (function () {

        function fill() {

            render();
            bindEvents();
            fillNavigationUrls();

            //renderNotifications();
            renderImpactMenus();
            renderProfileMenus();
            if (FRHeaderProfileData.Profile.IsMyFrontiersActive) {
                bindMyFrontiersBetaSmartDropdowns();
            }
            else
                bindMyFrontiersSmartDropdowns();


            $headerContainer.find('[title]').tooltip();
        }

        function render() {

            $headerContainer.find('.unlogged-user').addClass('hidden');
            $headerContainer.find('.logged-user').removeClass('hidden');
            $headerContainer.find('.search-icon-container').removeClass('hidden');
        }

        function bindEvents() {

            //ibar dropdowns
            $(document).on('click', function (e) {

                var $target = $(e.target) || $(e.srcElement);
                var $popover = $target.closest('.popover-profile');
                var $popoverTrigger = $target.closest('.popover-profile-trigger');

                if ($popoverTrigger.length) {
                    e.preventDefault();
                    $headerContainer.find('.popover-profile').fadeToggle('fast', 'swing');

                } else if (window.FRHeaderProfileData.IsProfileDropdownEnabled === true) {

                    var $popoverUsernameTrigger = $target.closest('.username');

                    if ($popoverUsernameTrigger.length) {
                        e.preventDefault();
                        $headerContainer.find('.popover-profile').fadeToggle('fast', 'swing');
                    } else if (!$popover.length) {
                        $headerContainer.find('.popover-profile').hide();
                    }

                } else if (!$popover.length) {
                    $headerContainer.find('.popover-profile').hide();
                }
            });

            //Click event tracking
            $(document).on('click', function (e) {
                if (e.target.innerText == "Suggest Topic") {
                    FRHeaderResources.event('JP_MH_Action', 'link', 'click_header_suggest_topic');
                }
                if (e.target.innerText == "Submit Manuscript") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_my_frontiers_submit_ms');
                }
                if (e.target.innerText == "Research Topic Management") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_my_frontiers_rt_mgmt');
                }
                if (e.target.innerText == "Digital Editorial Office") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_my_frontiers_deo');
                }
                if (e.target.innerText == "My Submissions") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_my_frontiers_my_sub');
                }
                if (e.target.innerText == "Participate in Frontiers") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_my_frontiers_participate');
                }
                if (e.target.innerText == "Invoices") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_my_frontiers_invoices');
                }
                if (e.target.innerText == "How to Participate") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_Participate_in_Frontiers_how_to');
                }
                if (e.target.innerText == "Recommend Frontiers") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_Participate_in_Frontiers_recommend');
                }
                if (e.target.innerText == "Research Topic Guidelines") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_Participate_in_Frontiers_rt_guidelines');
                }
                if (e.target.innerText == "Suggest Editor") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_Participate_in_Frontiers_suggest_editor');
                }
                if (e.target.innerText == "Apply as Frontiers Editor") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_Participate_in_Frontiers_apply_as_editor');
                }
                if (e.target.innerText == "Give Feedback") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_Participate_in_Frontiers_give_feedback');
                }
                if (e.target.innerText == "Contact Us") {
                    FRHeaderResources.event('Header_Action', 'link', 'click_header_Participate_in_Frontiers_contact_us');
                }
            });

            // impact
            $(document).on('click', function (e) {

                var $target = $(e.target) || $(e.srcElement);
                var $popover = $target.closest('.popover-impact');
                var $popoverTrigger = $target.closest('.popover-impact-trigger');

                if ($popoverTrigger.length) {
                    e.preventDefault();

                    var popoverArrowRight = ($headerContainer.find('.myhome-container').outerWidth() / 2) +
                        $headerContainer.find('.notification-container').outerWidth() +
                        $headerContainer.find('.username-container').outerWidth() +
                        $headerContainer.find('.profile-container').outerWidth() - 10;

                    var popoverRight = (popoverArrowRight - $headerContainer.find('.impact-popover').outerWidth()) + 225;

                    if (popoverRight > 0) {
                        $headerContainer.find('.impact-popover').css('right', popoverRight);
                    }

                    $headerContainer.find('.popover-impact em').css('right', popoverArrowRight + 'px');
                    $headerContainer.find('.popover-impact').fadeToggle('fast', 'swing');
                } else if (!$popover.length) {
                    $headerContainer.find('.popover-impact').hide();
                }
            });
        }
       

        function fillNavigationUrls() {

            $headerContainer.find('a.myhome').attr('href', window.FRHeaderProfileData.Profile.Navigation.MyHomeUrl);

            $headerContainer.find('a.username')
                .text(window.FRHeaderProfileData.Profile.User.FullName);

            if (window.FRHeaderProfileData.IsProfileDropdownEnabled === false) {
                $headerContainer.find('a.username').attr('href', window.FRHeaderProfileData.Profile.Navigation.UserProfileUrl);
            }

            $headerContainer.find('img.profile-pic')
                .attr('src', window.FRHeaderProfileData.Profile.Navigation.UserThumbnailUrl)
                .on('error', function () {
                    this.onerror = null;
                    this.src = window.FRHeaderTaxonomyData.Navigation.FrontiersJournalAPIUrl + '/Areas/Header/Content/Images/default_profile_32.jpg';
                });
        }        

        function renderImpactMenus() {

            if (typeof window.FRHeaderProfileData.Profile === 'undefined') return;

            if (!window.FRHeaderProfileData.Profile.ImpactMenus &&
                !window.FRHeaderProfileData.Profile.ImpactMenus.length) {
                return;
            }

            $headerContainer.find('a.popover-impact-disabled').attr('href', window.FRHeaderProfileData.Profile.Navigation.LoopImpactUrl);
            return;
        }

        function renderProfileMenus() {

            if (!window.FRHeaderProfileData.Profile || !window.FRHeaderProfileData.Profile.User) {
                return;
            }

            var lstProfileMenus = [];

            if (window.FRHeaderProfileData.IsProfileDropdownEnabled == true) {

                lstProfileMenus = [
                    { n: 'Profile', url: window.FRHeaderProfileData.Profile.Navigation.UserProfileUrl },
                    { n: 'Settings & Privacy', url: window.FRHeaderProfileData.Profile.Navigation.UserProfileSettingsUrl },
                    { n: 'Help Center', url: window.FRHeaderProfileData.Profile.Navigation.FrontiersHelpCenterUrl },
                    { n: 'Logout', url: window.FRHeaderProfileData.Profile.Navigation.LoopLogoutUrl }
                ];
            }
            else {
                lstProfileMenus = [
                    { n: 'View Profile', url: window.FRHeaderProfileData.Profile.Navigation.UserProfileUrl },
                    { n: 'Publications', url: window.FRHeaderProfileData.Profile.Navigation.UserProfilePublicationsUrl },
                    { n: 'Network', url: window.FRHeaderProfileData.Profile.Navigation.UserProfileNetworkUrl },
                    { n: 'Settings', url: window.FRHeaderProfileData.Profile.Navigation.UserProfileSettingsUrl },
                    { n: 'Logout', url: window.FRHeaderProfileData.Profile.Navigation.LogoutUrl }
                ];
            }

            $headerContainer.find('.popover-profile-trigger')
                .headerSmartDropDown({
                    type: 'back',
                    offsetLeft: -20,
                    arrowOffsetLeft: 12,
                    align: 'right',
                    initialUrl: null,
                    firstLevelName: '',
                    initialLevelPrefix: '',
                    doubleColumnLevel: false,
                    data: lstProfileMenus
                });

            if (window.FRHeaderProfileData.IsProfileDropdownEnabled == true) {
                $headerContainer.find('a.username')
                    .headerSmartDropDown({
                        type: 'back',
                        offsetLeft: 45,
                        arrowOffsetLeft: 45,
                        align: 'right',
                        initialUrl: null,
                        firstLevelName: '',
                        initialLevelPrefix: '',
                        doubleColumnLevel: false,
                        data: lstProfileMenus
                    });
            }

            var profileMyHomeMenu = { 'n': 'My Home', 'url': window.FRHeaderProfileData.Profile.Navigation.MyHomeUrl };
            var profileImpactMenus = { 'n': 'Impact', 'url': window.FRHeaderProfileData.Profile.Navigation.LoopImpactUrl };         
            var mobileViewProfileMenus = new Array().concat(profileMyHomeMenu, profileImpactMenus, lstProfileMenus);;

            $headerContainer.find('.profile-pic.responsive')
                .headerSmartDropDown({
                    type: 'back',
                    offsetLeft: -170,
                    firstLevelName: '',
                    arrowOffsetLeft: 12,
                    doubleColumnLevel: false,
                    data: mobileViewProfileMenus
                });
        }



        function bindMyFrontiersSmartDropdowns() {

            if (!window.FRHeaderProfileData.Profile.MyFrontiersMenus ||
                !window.FRHeaderProfileData.Profile.MyFrontiersMenus.length > 0) {
                return;
            }

            var unreadMailCount = '';
            var myFrontiersDesktopMenus;
            var myFrontiersMobileMenus;
            var myjournalMenus = window.FRHeaderProfileData.Profile.MyFrontiersMenus;
            var myOfficeMenus = window.FRHeaderProfileData.Profile.OfficeMenus;
            var OfficeMenuBeta = window.FRHeaderProfileData.Profile.OfficeBetaMenus;
            var officeMenuLabel = { 'type': 'label', 'label': 'Office' };
            var officeMenuBetaLabel = { 'type': 'label', 'label': 'Office (Beta)' };
            var impersonatedRole = window.FRHeaderProfileData.Profile.ImpersonatedRole;

            if (window.FRHeaderProfileData.Profile.HasUnReadMailCount) {
                unreadMailCount = '<b>' + window.FRHeaderProfileData.Profile.UnReadMailCount + '</b>';
            }

            var profileMessages = { 'n': 'Messages ' + unreadMailCount, 'url': '/Mail/IbarMail.aspx?op=1', 'dataTracking': '{"category":"Header_Action","action":"link", "label": "click_header_my_frontiers_message" }' };

            if (myOfficeMenus.length > 0 && OfficeMenuBeta.length > 0) {
                myFrontiersDesktopMenus = new Array().concat(profileMessages, myjournalMenus, officeMenuLabel, myOfficeMenus, officeMenuBetaLabel, OfficeMenuBeta);
            }

            else if (myOfficeMenus.length > 0) {
                myFrontiersDesktopMenus = new Array().concat(profileMessages, myjournalMenus, officeMenuLabel, myOfficeMenus);
            } else if (impersonatedRole && impersonatedRole.length) {
                var backToOfficeDesktopMenu = { n: 'Back to ' + impersonatedRole, url: '/Admin/EOF/ToEof.aspx' };
                myFrontiersDesktopMenus = new Array().concat(profileMessages, myjournalMenus, officeMenuLabel, backToOfficeDesktopMenu);
            } else {
                myFrontiersDesktopMenus = new Array().concat(profileMessages, myjournalMenus);
            }

            $headerContainer.find('.myfrontiers-container a')
                .headerSmartDropDown({
                    type: 'back',
                    align: 'right',
                    offsetLeft: 49,
                    arrowOffsetLeft: 88,
                    initialUrl: null,
                    firstLevelName: '',
                    initialLevelPrefix: '',
                    doubleColumnLevel: false,
                    data: myFrontiersDesktopMenus,
                    dataLinkUrlDomain: window.FRHeaderTaxonomyData.Navigation.SharepointWebsiteUrl
                });

            if (myOfficeMenus.length > 0 && OfficeMenuBeta.length > 0) {
                myFrontiersMobileMenus = new Array().concat(profileMessages, myjournalMenus, officeMenuLabel, myOfficeMenus, officeMenuBetaLabel, OfficeMenuBeta);
            }
            else if (myOfficeMenus.length > 0) {
                myFrontiersMobileMenus = new Array().concat(profileMessages, myjournalMenus, officeMenuLabel, myOfficeMenus);
            }

            else if (impersonatedRole && impersonatedRole.length) {
                var backToOfficeMenu = { n: 'Back to ' + impersonatedRole, url: '/Admin/EOF/ToEof.aspx' };
                myFrontiersMobileMenus = new Array().concat(profileMessages, myjournalMenus, officeMenuLabel, backToOfficeMenu);
            } else {
                myFrontiersMobileMenus = new Array().concat(profileMessages, myjournalMenus);;
            }

            $headerContainer.find('#myoffice')
                .headerSmartDropDown({
                    type: 'back',
                    offsetLeft: -120,
                    firstLevelName: '',
                    arrowOffsetLeft: 64,
                    doubleColumnLevel: false,
                    data: myFrontiersMobileMenus,
                    dataLinkUrlDomain: window.FRHeaderTaxonomyData.Navigation.SharepointWebsiteUrl
                });
        }


        function bindMyFrontiersBetaSmartDropdowns() {

            $(".myfrontiersbeta-container").removeClass("hidden");
            $("#myfrontierssponsive").removeClass("hidden");
            $(".myfrontiers-container").addClass("hidden");
            $('div.header-compact #myoffice').addClass("hidden");


            $('div.header-compact a.myfrontiersbeta').attr('href', window.FRHeaderProfileData.Profile.Navigation.MyFrontiersUrl);
            $('div.header-compact a.myfrontiersmobile').attr('href', window.FRHeaderProfileData.Profile.Navigation.MyFrontiersUrl);


            if ((!window.FRHeaderProfileData.Profile.OfficeMenus ||
                !window.FRHeaderProfileData.Profile.OfficeMenus.length) > 0 && (!window.FRHeaderProfileData.Profile.ImpersonatedRole && !window.FRHeaderProfileData.Profile.ImpersonatedRole.length)) {
                $(".myofficecontainer").remove();
                return;
            }

            $(".myoffice-container").removeClass("hidden");
            $('div.header-compact #myofficeresponsive').removeClass("hidden");


            var myOfficeMenus = window.FRHeaderProfileData.Profile.OfficeMenus;
            var OfficeMenuBeta = window.FRHeaderProfileData.Profile.OfficeBetaMenus;
            var myFrontiersDesktopMenus;
            var myFrontiersMobileMenus;
            var myjournalMenus = window.FRHeaderProfileData.Profile.MyFrontiersMenus;

            var officeMenuLabel = { 'type': 'label', 'label': 'Office' };
            var officeMenuBetaLabel = { 'type': 'label', 'label': 'Office (Beta)' };
            var impersonatedRole = window.FRHeaderProfileData.Profile.ImpersonatedRole;


            if (myOfficeMenus.length > 0 && OfficeMenuBeta.length > 0) {
                myFrontiersDesktopMenus = new Array().concat(officeMenuLabel, myOfficeMenus, officeMenuBetaLabel, OfficeMenuBeta);
            }

            else if (myOfficeMenus.length > 0) {
                myFrontiersDesktopMenus = new Array().concat(officeMenuLabel, myOfficeMenus);
            } else if (impersonatedRole && impersonatedRole.length) {
                var backToOfficeDesktopMenu = { n: 'Back to ' + impersonatedRole, url: '/Admin/EOF/ToEof.aspx' };
                myFrontiersDesktopMenus = new Array().concat(officeMenuLabel, backToOfficeDesktopMenu);
            }

            $headerContainer.find('.myoffice-container a')
                .headerSmartDropDown({
                    type: 'back',
                    align: 'right',
                    offsetLeft: 5,
                    arrowOffsetLeft: 46,
                    initialUrl: null,
                    firstLevelName: '',
                    initialLevelPrefix: '',
                    doubleColumnLevel: false,
                    data: myFrontiersDesktopMenus,
                    dataLinkUrlDomain: window.FRHeaderTaxonomyData.Navigation.SharepointWebsiteUrl
                });

            if (myOfficeMenus.length > 0 && OfficeMenuBeta.length > 0) {
                myFrontiersMobileMenus = new Array().concat(officeMenuLabel, myOfficeMenus, officeMenuBetaLabel, OfficeMenuBeta);
            }
            else if (myOfficeMenus.length > 0) {
                myFrontiersMobileMenus = new Array().concat(officeMenuLabel, myOfficeMenus);
            }

            else if (impersonatedRole && impersonatedRole.length) {
                var backToOfficeMenu = { n: 'Back to ' + impersonatedRole, url: '/Admin/EOF/ToEof.aspx' };
                myFrontiersMobileMenus = new Array().concat(officeMenuLabel, backToOfficeMenu);
            } else {
                myFrontiersMobileMenus = myjournalMenus;;
            }

            $headerContainer.find('#myofficeresponsive')
                .headerSmartDropDown({
                    type: 'back',
                    offsetLeft: -144,
                    firstLevelName: '',
                    arrowOffsetLeft: 46,
                    doubleColumnLevel: false,
                    data: myFrontiersMobileMenus,
                    dataLinkUrlDomain: window.FRHeaderTaxonomyData.Navigation.SharepointWebsiteUrl
                });
        }

        return { fill: fill };

    })();


    var FRHeaderBootstrapper = (function () {

        $(function () {

            init();
        });

        function init() {

            if (window.FRHeaderProfileData.Profile &&
                window.FRHeaderProfileData.Profile.LoginUserId &&
                window.FRHeaderProfileData.Profile.LoginUserId > 0) {
                FRHeaderLoggedIn.fill();
                FRHeaderMessages.fill();
            } else {
                FRHeaderLoggedOut.fill();
            }

            bindEvents();
            populateTaxonomyDropdown();
            highlightCurrentMenu();
            fixHtmlPlaceholderForIE();
            manageHashNavigation(false, window.location.hash);
            $headerContainer.find('[title]').tooltip();
            resetSearchBox();
            setHeaderLogo();
        }

        function bindEvents() {

            $headerContainer.find('.search-icon-container').on('click', function () {
                FRHeaderResources.event('Header_Action', 'link', 'click_header_search');
                if (window.FRHeaderProfileData.IsSearchEnabled) {//go to new search page
                    window.location.href = window.FRHeaderTaxonomyData.Navigation.SearchUIUrl + "/search?origin=" + encodeURIComponent(window.location.href);
                }
                else {
                    $headerContainer.find('.search-bar-container').removeClass('hide');
                    $headerContainer.find('.search-bar-container').show();
                    $headerContainer.find('.search-icon-container').hide();
                    $headerContainer.find('.header-search-field').focus();
                    resizeSearchBar();
                    positionSearchIconInSearchBar();
                }
                
            });

            $headerContainer.find('.header-search-btn').on('click', function () {
                goToFrontiersSearch($(this).prev('.header-search-field'));

            });

            $headerContainer.find('.header-search-field').on('keypress', function (event) {

                if (event.keyCode != 13) return;
                goToFrontiersSearch(this);
                event.preventDefault();

            });

            $(document).on('click', 'a[href^="#"]', function () {

                var $this = $(this);

                if ($this.parents('#journalHome').length <= 0) return; // Limiting manageHashNavigation to V3 journal page only.

                var hashValue = $this.attr('href'); //window.location.hash; //Since click is getting fired before 'window.location.hash' is updated.

                if (!hashValue.length) return;

                manageHashNavigation(true, hashValue);
            });

            // This is to remove the search bar
            $('body').click(function (e) {

                if ($(e.target).hasClass('search-icon') || $(e.target).parent().hasClass('search-bar-container')) {
                    // Do nothing, we're clicking the icon and we already have a callback for that
                } else {

                    if (!$headerContainer.find('.search-bar-container').is(':visible')) return;
                    $headerContainer.find('.search-bar-container').toggle();
                    $headerContainer.find('.search-icon-container').toggle();
                    $headerContainer.find('.header-search-field').val('');
                }
            });

            // Terrible trick to fit the search in here
            $(window).resize(function () {
                resizeSearchBar();
                positionSearchIconInSearchBar();
            });

            //Google Analytics
            bindGoogleAnalytics();
        }

        function bindGoogleAnalytics() {
            $headerContainer.find('.byjournal').on('click', function () {
                FRHeaderResources.event('Header_Action', 'link', 'click_header_journal_nav');
            });

            $headerContainer.find('.popover-profile-trigger').on('click', function () {
                FRHeaderResources.event('Header_Action', 'link', 'click_header_user');
            });

            $headerContainer.find('.myfrontiers-container a').on('click', function () {
                FRHeaderResources.event('Header_Action', 'link', 'click_header_my_frontiers');
            });


        }

        function highlightCurrentMenu() {

            if (typeof window.CurrentIBarMenu === 'undefined' || !window.CurrentIBarMenu.length) {
                return;
            }

            $headerContainer.find('.' + window.CurrentIBarMenu).parent().addClass('highlight');
        }

        function fixHtmlPlaceholderForIE() {

            // Checking for IE browser. 
            if (!navigator || !navigator.userAgent || navigator.userAgent.toUpperCase().indexOf('MSIE') < 0) {
                return;
            }

            $headerContainer.find('input[placeholder]').each(function () {

                var $self = $(this);

                if ($self.val() == "" || $self.val() == null) {
                    $self.val($self.attr('placeholder'));
                }

                if ($self.attr('placeholder') != '' && $self.val() == $self.attr('placeholder')) {
                    $self.addClass("hasPlaceholder");
                }

                $self.focus(function () {
                    if ($self.attr('placeholder') != '' && $self.val() == $self.attr('placeholder')) {
                        $self.val('').removeClass('hasPlaceholder');
                    }
                })
                    .blur(function () {
                        if ($self.attr('placeholder') != '' && ($self.val() == '' || $self.val() == $self.attr('placeholder'))) {
                            $self.val($self.attr('placeholder')).addClass('hasPlaceholder');
                        }
                    });

                $headerContainer.find('form').submit(function () {
                    $self.find('.hasPlaceholder').each(function () {
                        $self.val('');
                    });
                });

            });
        }

        function goToFrontiersSearch(control) {

            var query = $.trim($(control).val());

            if (!query.length) return;

            FRHeaderResources.event('SEARCH_Action', 'link', 'search-field-ibar');
            if (window.FRHeaderProfileData.IsSearchEnabled) {//go to new search page
                window.location.href = window.FRHeaderTaxonomyData.Navigation.HomeUIUrl + '/search?query=' + encodeURIComponent(query) + '&ht=1';
            }
            else {
                window.location.href = window.FRHeaderTaxonomyData.Navigation.SharepointWebsiteUrl + '/SearchServer.aspx?sq=' + encodeURIComponent(query) + '&ht=1';
            }
        }

        function manageHashNavigation(isHashChange, hashValue) {

            if (!hashValue || !hashValue.length) return;

            hashValue = hashValue.substr(1); //-->>Removing leading hash(#).

            if (hashValue.substr(0, 1) == '_') {
                hashValue = hashValue.substr(1); //-->>Removing leading underscore, if any.
            }

            var navigateToHash = function () {

                var $element = $('body').find('[id="' + hashValue + '"]');

                if (!$element || !$element.length) return;

                if ($element.closest('#journalHome').length <= 0) return; //-->>Limiting manageHashNavigation to V3 journal page only.

                if ($element.hasClass('reset-hash-position')) return; //-->>Limiting if hash reset already handled for the element.

                window.location.hash = '_' + hashValue; //-->>To prevent default hash jump. 

                var hashScrollTop = parseInt($element.offset().top) - parseInt($('body').css('padding-top'));

                $('html, body').animate({ scrollTop: hashScrollTop }, 100);
            };

            var timeOut = isHashChange ? 100 : 500;
            setTimeout(navigateToHash, timeOut);
        }

        function populateTaxonomyDropdown() {

            if (!window.FRHeaderTaxonomyData ||
                !window.FRHeaderTaxonomyData.Taxonomy ||
                !window.FRHeaderTaxonomyData.Taxonomy.c ||
                !window.FRHeaderTaxonomyData.Taxonomy.c.length) {

                $headerContainer.find('.journalAZ').removeClass('hidden');
                return;
            }

            $headerContainer.find('.journal-drop').removeClass('hidden');

            var journalTaxonomyInfo = window.FRHeaderTaxonomyData.Taxonomy.c;
            var journalAtoZ = {
                'n': 'Journals A-Z',
                'url': window.FRHeaderTaxonomyData.Navigation.JournalsAtoZUrl
            };
            var journalBySubject = {
                'type': 'label',
                'label': 'By Subject'
            };


            journalTaxonomyInfo.unshift(journalAtoZ, journalBySubject);

            $headerContainer.find('.byjournal')
                .headerSmartDropDown({
                    align: 'left',
                    type: 'breadcrum',
                    offsetLeft: -9,
                    arrowOffsetLeft: 65,
                    doubleColumnLevel: 3,
                    firstLevelName: 'Journals',
                    initialLevelPrefix: 'All in ',
                    data: journalTaxonomyInfo,
                    dataLinkUrlDomain: window.FRHeaderTaxonomyData.Navigation.FrontiersJournalUIUrl,
                    includeHtmlTestIds: true
                });

            var mobileViewJournalMenus = [
                {
                    'n': 'Home',
                    'url': window.FRHeaderTaxonomyData.Navigation.HeaderLogoUrl,
                    'dataTracking': '{"category":"Header_Action","action":"link", "label": "click_header_home" }'
                },
                {
                    'n': 'About',
                    'url': window.FRHeaderTaxonomyData.Navigation.AboutFrontiersUrl,
                    'dataTracking': '{"category":"Header_Action","action":"link", "label": "click_header_about" }'
                },
                {
                    'n': 'Journals',
                    'url': '',
                    'c': journalTaxonomyInfo,
                    'dataTracking': '{ "category": "Header_Action", "action": "link", "label": "click_header_journal_nav" }'
                },
                {
                    'n': 'Research Topics',
                    'url': window.FRHeaderTaxonomyData.Navigation.ResearchTopicUrl,
                    'dataTracking': '{ "category": "Header_Action", "action": "link", "label": "click_header_rt" }'
                },
                { 'n': 'Submit', 'url': window.FRHeaderTaxonomyData.Navigation.SubmissionUrl, 'dataTracking': '{"category":"Header_Action","action":"link", "label": "click_header_submit" }' }
            ];

            $headerContainer.find('.journal-dropdown-responsive')
                .headerSmartDropDown({
                    type: 'back',
                    offsetLeft: -9,
                    arrowOffsetLeft: 5,
                    firstLevelName: '',
                    doubleColumnLevel: false,
                    data: mobileViewJournalMenus,
                    dataLinkUrlDomain: window.FRHeaderTaxonomyData.Navigation.FrontiersJournalUIUrl,
                    includeHtmlTestIds: true
                });
        }

        function getLateralContainersWidth() {

            var occWidth = 0;

            $headerContainer.find('.col-xs-6').each(function () {
                if ($(this).is(":visible")) {
                    occWidth += $(this).width();
                }
            });

            return occWidth;
        }

        function resetSearchBox() {

            $headerContainer.find('.search-bar-container').hide();
            $headerContainer.find('.search-icon-container').show();
            resizeSearchBar();
            positionSearchIconInSearchBar();
        }

        function resizeSearchBar() {

            var $headerSearchField = $headerContainer.find('.header-search-field');

            if (!$headerSearchField.length || !$headerSearchField.is(":visible")) return;

            var marginRight = 73;
            var newWidth = $('body').width() - getLateralContainersWidth() - marginRight;

            $headerSearchField.width(newWidth);
        }

        function positionSearchIconInSearchBar() {

            var $headerSearchField = $headerContainer.find('.header-search-field');

            if (!$headerSearchField.length) return;

            var rightCorner = $headerSearchField.position().left + $headerSearchField.width();
            var backspace = -15;
            var $headerSearchButton = $headerContainer.find('.header-search-btn');
            var headerSearchButtonPosition = $headerSearchButton.position();

            headerSearchButtonPosition.left = Math.floor(rightCorner - backspace);
            $headerSearchButton.css('left', headerSearchButtonPosition.left);
        }

        function setHeaderLogo() {
            $headerContainer.find('.header-logo').attr('src', window.FRHeaderTaxonomyData.Navigation.FrontiersJournalAPIUrl + '/Areas/Header/Content/Images/thin-header-logo.png');
        }

    })();


    var FRHeaderMessages = (function () {
        function fill() {
            var impersonatedRole = window.FRHeaderProfileData.Profile.ImpersonatedRole;
            if (impersonatedRole && impersonatedRole.length)
                return;

            GetUserMessage();
        }

        function updateIsShown(message) {
            if (!message.IsBlocking) {
                $.ajaxCrossDomainAPI({
                    type: 'POST',
                    data: { cookies: [window.FRHeaderTaxonomyData.Navigation.CookieNameLoginRemember, window.FRHeaderTaxonomyData.Navigation.CookieNameLogin, window.FRHeaderTaxonomyData.Navigation.CookieNameImpersonate] },
                    url: window.FRHeaderTaxonomyData.Navigation.FrontiersJournalAPIUrl + '/Header/UpdateIsShown?messageId=' + message.MessageId,
                });
            }
        }

        function GetUserMessage() {
            $.ajaxCrossDomainAPI({
                type: 'POST',
                data: { cookies: [window.FRHeaderTaxonomyData.Navigation.CookieNameLoginRemember, window.FRHeaderTaxonomyData.Navigation.CookieNameLogin, window.FRHeaderTaxonomyData.Navigation.CookieNameImpersonate] },
                url: window.FRHeaderTaxonomyData.Navigation.FrontiersJournalAPIUrl + '/Header/GetMessages',
                success: function (message) {
                    if (message != null && message.MessageId > 0) {
                        renderMessagePopUp(message);
                        updateIsShown(message);
                    }
                }
            });
        }

        //function renderMessagePopUp(message) {
        //    var messagePopUpTemplate = [
        //        '<div class="modal in message-modal-container"  id="myMessageModal" {ISBLOCKING_DATA_ATTR} tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="false">',
        //        '<div class="modal-dialog">',
        //        '<div class="modal-content">',
        //        '<div class="modal-header clearfix">',
        //        '<h4 class="modal-title" id="myModalLabel">{MESSAGETITLE}</h4>',
        //        '</div><div class="modal-body">{MESSAGECONTENT}</div>',
        //        '<div class="modal-footer">',
        //        '<button type="button" class="btn btn-default btn-primary" data-dismiss="modal">{PRIMARYBUTTONTEXT}</button>',
        //        '{SECONDARYBUTTON}',
        //        '</div></div></div></div>'
        //    ].join('');
        //    var secondaryButtonTemplate = '<button type="button" class="btn btn-default lighter btn-secondary" data-href="{SECONDARYBUTTONLINK}">{SECONDARYBUTTONTEXT}</button>';

        //    var messagePopUpFinalHtml = '';
        //    var secondaryButtonHtml = '';

        //    if (message.IsSecondaryButtonVisible) {
        //        secondaryButtonHtml += secondaryButtonTemplate
        //            .replace(/{SECONDARYBUTTONLINK}/g, message.SecondaryButtonLink)
        //            .replace(/{SECONDARYBUTTONTEXT}/g, message.SecondaryButtonTitle)
        //    }

        //    messagePopUpFinalHtml += messagePopUpTemplate
        //        .replace(/{ISBLOCKING_DATA_ATTR}/g, message.IsBlocking ? 'data-backdrop="static" data-keyboard="false"' : '')
        //        .replace(/{MESSAGETITLE}/g, message.Title)
        //        .replace(/{MESSAGECONTENT}/g, message.Content)
        //        .replace(/{PRIMARYBUTTONTEXT}/g, message.PrimaryButtonTitle)
        //        .replace(/{SECONDARYBUTTON}/g, secondaryButtonHtml);
        //    var $messageFinalHtml = $(messagePopUpFinalHtml);
        //    $messageFinalHtml.find('.btn-primary').on('click', function () {
        //        updateUserMessage(true, message.MessageId);
        //        $headerContainer.find('.message-modal-container').modal('hide');
        //    });
        //    $messageFinalHtml.find('.btn-secondary').on('click', function () {
        //        updateUserMessage(false, message.MessageId);
        //        var href = $(this).data('href');

        //        if (!href || !href.length) return;

        //        window.open(href, "_blank");
        //    })
        //    $headerContainer.append($messageFinalHtml);
        //    $headerContainer.find('.message-modal-container').modal('show');
        //    if (!message.IsBlocking)
        //        $('.modal-backdrop.in').css({ opacity: 0.0 });
        //    else
        //        $('.modal-backdrop.in').css({ opacity: 0.7 });
        //}

        function renderMessagePopUp(message) {
            var messagePopUpTemplate = [
                '<div class="popup-user-message" id="myMessageModal" data-test-id="user_message_modal">',
                '<div class="overlay" data-test-id="user_message_overlay"></div >',
                '<div class="popup-container" data-test-id="user_message_popup_container">',
                '<h4 data-test-id="user_message_header"> <<messageTitle>> </h4>',
                '<div class="user-message-container" data-test-id="user_message_container"> <<messageContent>>',
                '</div>',
                '<div class="actions-user-message" data-test-id="user_message_action_container">',
                message.IsSecondaryButtonVisible ? '<div class="button outline-medgrey02 cancel btn-secondary" data-test-id="user_message_secondary_button" data-href="<<secondaryButtonLink>>"> <<secondaryButtonText>> </div>' : '',
                '<div class="button flat-maincolor accept btn-primary" data-test-id="user_message_primary_button"> <<primaryButtonTitle>> </div>',
                '</div>',
                '</div>',
                '</div >'
            ].join('');

            var messageFinalHtml = getTokenReplacedText(messagePopUpTemplate, message);
            var $messageFinalHtml = $(messageFinalHtml);
            $messageFinalHtml.find('.btn-primary').on('click', function () {
                updateUserMessage(true, message.MessageId);
                closeUserMessageModal();
            });
            $messageFinalHtml.find('.btn-secondary').on('click', function () {
                updateUserMessage(false, message.MessageId);
                var href = $(this).data('href');

                if (!href || !href.length) return;

                window.open(href, "_blank");
            });

            $messageFinalHtml.find('.overlay').on('click', function () {
                if (!message.IsBlocking) {
                    closeUserMessageModal();
                }
            });

            var browser = getBrowserDetails();
            var browserName = browser.name.trim();
            if (browserName === 'IE' || browserName === 'MSIE') {
                $messageFinalHtml.find('.popup-container').addClass('terms-condition-modal-msie');
                $messageFinalHtml.find('.user-message-container').addClass('terms-condition-modal-msie-user-message-container');

            }
            //$(document).on('keydown', function (event) {
            //    if (event.keyCode === 27) { // ESC
            //        if (!message.IsBlocking) {
            //            closeUserMessageModal();
            //        }
            //    }
            //});

            $headerContainer.append($messageFinalHtml);
        }

        function getBrowserDetails() {
            var ua = navigator.userAgent, tem, M = ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\/))\/?\s*(\d+)/i) || [];
            if (/trident/i.test(M[1])) {
                tem = /\brv[ :]+(\d+)/g.exec(ua) || [];
                return { name: 'IE ', version: (tem[1] || '') };
            }
            if (M[1] === 'Chrome') {
                tem = ua.match(/\bOPR\/(\d+)/)
                if (tem != null) { return { name: 'Opera', version: tem[1] }; }
            }
            M = M[2] ? [M[1], M[2]] : [navigator.appName, navigator.appVersion, '-?'];
            if ((tem = ua.match(/version\/(\d+)/i)) != null) { M.splice(1, 1, tem[1]); }
            return {
                name: M[0],
                version: M[1]
            };
        }

        function closeUserMessageModal() {
            $('.popup-user-message').addClass('collapse-popup');
            $("#myMessageModal").remove();
            //$('html').css('overflow-y', 'auto');
        }

        function getTokenReplacedText(input, message) {

            var tokens = {
                '<<messageTitle>>': message.Title,
                '<<messageContent>>': message.Content,
                '<<secondaryButtonLink>>': message.SecondaryButtonLink,
                '<<secondaryButtonText>>': message.SecondaryButtonTitle,
                '<<primaryButtonTitle>>': message.PrimaryButtonTitle
            };
            tokenReplacedText = input.replace(/<<messageTitle>>|<<messageContent>>|<<secondaryButtonLink>>|<<secondaryButtonText>>|<<primaryButtonTitle>>/gi, function (matched) {
                return tokens[matched];
            });
            return tokenReplacedText;
        }

        function updateUserMessage(isPrimary, messageId) {
            $.ajaxCrossDomainAPI({
                type: 'POST',
                data: { cookies: [window.FRHeaderTaxonomyData.Navigation.CookieNameLoginRemember, window.FRHeaderTaxonomyData.Navigation.CookieNameLogin, window.FRHeaderTaxonomyData.Navigation.CookieNameImpersonate] },
                url: window.FRHeaderTaxonomyData.Navigation.FrontiersJournalAPIUrl + '/Header/UpdateUserMessage?isPrimary=' + isPrimary + '&messageId=' + messageId,
                success: function () {
                    //console.log('User message updated');
                }
            });
        }

        return { fill: fill };
    })();

    return {
        trackOutboundLink: FRHeaderResources.trackOutboundLink,
        EditPage: FRHeaderResources.EditPage
    };


})($, document);

if (typeof window.EditPage === 'undefined') {

    //-->>EditPage function which navigates to CONFIG>ContentEditor..
    var EditPage = FRHeader.EditPage;
}


