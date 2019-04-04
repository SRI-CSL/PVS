/**
 * @module BasicDisplay
 * @version 2.1
 * @description Renders a basic digital display.
 *              This module provide APIs for changing the look and feel of
 *              the rendered text, including: cursors, background color, font, size, alignment.
 * @author Paolo Masci, Patrick Oladimeji, Henrique Pacheco
 * @date May 12, 2017
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses BasicDisplay
 // The following configuration assumes the pvsio-web demo is stored in a folder within pvsio-web/examples/demo/
 require.config({
     baseUrl: "../../client/app",
     paths: {
         d3: "../lib/d3",
         lib: "../lib"
     }
 });
 require(["widgets/BasicDisplay"], function (BasicDisplay) {
      "use strict";
      var device = {};
      device.disp = new BasicDisplay("disp", {
        top: 100, left: 120, height: 24, width: 120
      }, {
        displayKey: "disp"
      });
      device.disp.render( { disp: 10.5 }); // the display renders 10.5
 });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, document */

define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3");
    var Widget = require("widgets/Widget"),
        StateParser = require("util/PVSioStateParser"),
        Speaker  = require("widgets/TextSpeaker"),
        property = require("util/property");

    /**
     * @function <a name="BasicDisplay">BasicDisplay</a>
     * @description Constructor.
     * @param id {String} The ID of the display.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 0, left: 0, width: 200, height: 80 }.
     * @param opt {Object} Options:
     *          <li>align (String): text alignment (available options are "left", "right", anc "center". Default is "center")</li>
     *          <li>backgroundColor (String): background display color (default is black, "#000")</li>
     *          <li>borderWidth (Number): border width (default is 0, i.e., no border, unless option borderColor has been specified -- in this case, the border is 2px)</li>
     *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid" (default is "none")</li>
     *          <li>borderColor (String): border color, must be a valid HTML5 color (default color used in the widget is "black")</li>
     *          <li>blinking (Bool): true means the display is blinking (default is false, i.e., not blinking)</li>
     *          <li>displayKey (string): name of the state attribute defining the display content. This information will be used by the render method. Default is the ID of the display.</li>
     *          <li>fontSize (Number): font size (default is 0.8 * coords.height)</li>
     *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
     *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
     *          <li>format (String): sets the display format. When this option is set to "mm:ss", the display value represents seconds, and format to be displayed is mm:ss</li>
     *          <li>inverted (Bool): if true, the text has inverted colors,
     *              i.e., fontColor becomes backgroundColor, and backgroundColor becomes fontColor (default is false)</li>
     *          <li>letterSpacing (Number): spacing between characters, in pixels (default is 0)</li>
     *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
     *          <li>visibleWhen (string): boolean expression indicating when the display is visible. The expression can use only simple comparison operators (=, !=) and boolean constants (true, false). Default is true (i.e., always visible).</li>
     * @memberof module:BasicDisplay
     * @instance
     */
    function BasicDisplay(id, coords, opt) {
        opt = opt || {};
        coords = coords || {};
        opt.displayKey = opt.displayKey || id;
        opt.auditoryFeedback = (opt.auditoryFeedback) ?
            ((opt.auditoryFeedback.toString() === "enabled" || opt.auditoryFeedback.toString() === "true") ? "enabled" : "disabled") : "disabled";
        opt.visibleWhen = opt.visibleWhen || "true";
        this.id = property.call(this, id);
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 200;
        this.height = coords.height || 80;

        this.borderColor = opt.borderColor || "inherit";
        this.borderWidth = (opt.borderColor) ? ((opt.borderWidth) ? parseFloat(opt.borderWidth) : 2) : 0;
        this.borderStyle = (opt.borderColor) ? ((opt.borderStyle) ? opt.borderStyle : "solid") : "none";

        this.fontSize = opt.fontsize || opt.fontSize || this.height * 0.8;
        this.fontFamily = opt.fontfamily || opt.fontFamily || "sans-serif";
        this.font = [this.fontSize, "px ", this.fontFamily];
        this.smallFont = [(this.fontSize * 0.7), "px ", this.fontFamily];
        this.letterSpacing = opt.letterSpacing;
        this.align = opt.align || "center";
        this.backgroundColor = opt.backgroundColor || "black";
        this.fontColor = opt.fontColor || "white";
        this.cursor = opt.cursor || "default";
        if (opt.inverted) {
            var tmp = this.backgroundColor;
            this.backgroundColor = this.fontColor;
            this.fontColor = tmp;
        }
        this.blinking = opt.blinking || false;
        this.textBaseline = "middle";
        var elemClass = id + " displayWidget" + " noselect ";
        if (this.blinking) { elemClass += " blink"; }
        opt.position = opt.position || "absolute";
        opt.borderRadius = opt.borderRadius || "2px";
        opt.opacity = opt.opacity || 1;
        this.format = opt.format;
        this.div = d3.select(this.parent)
                        .append("div").style("position", opt.position)
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width",(this.width + this.borderWidth) + "px").style("height", (this.height + this.borderWidth) + "px")
                        .style("margin", 0).style("padding", 0).style("border-radius", opt.borderRadius).style("opacity", opt.opacity)
                        .style("background-color", this.backgroundColor)
                        .style("border-width", this.borderWidth + "px")
                        .style("border-style", this.borderStyle)
                        .style("border-color", this.borderColor)
                        .style("display", "none").attr("id", id).attr("class", elemClass);
        this.div.append("span").attr("id", id + "_span").attr("class", id + "_span")
                        .attr("width", this.width).attr("height", this.height)
                        .style("margin", 0).style("padding", 0)
                        .style("vertical-align", "top").style("border-radius", opt.borderRadius);//.style("opacity", opt.opacity);
        this.div.append("canvas").attr("id", id + "_canvas").attr("class", id + "_canvas")
                        .attr("width", this.width - this.borderWidth).attr("height", this.height - this.borderWidth)
                        .style("margin", 0).style("padding", 0).style("border-radius", opt.borderRadius)//.style("opacity", opt.opacity)
                        .style("vertical-align", "top");
        var x2 = this.left + this.width;
        var x3 = this.top + this.height;
        this.div.attr("coords", this.left + "," + this.top + "," + x2 + "," + x3)
                .style("cursor", this.cursor);
        this.txt = ""; // txt is used to store the current text rendered on the display
        this.displayKey = property.call(this, opt.displayKey);
        this.auditoryFeedback = property.call(this, opt.auditoryFeedback);
        this.visibleWhen = property.call(this, opt.visibleWhen);
        this.example = opt.example || "test"; // example is used in the prototype builder to demonstrate the font style of the display
        Widget.call(this, id, "display");
        return this;
    }
    BasicDisplay.prototype = Object.create(Widget.prototype);
    BasicDisplay.prototype.constructor = BasicDisplay;
    BasicDisplay.prototype.parentClass = Widget.prototype;

    /**
     * @function <a name="render">render</a>
     * @param data {Object} JSON object representing the display to be rendered.
     *                      The display value is specified in the attribute <displayKey>
     *                      (the actual value of <displayKey> is instantiated when creating the widget, see constructor's options)
     * @param opt {Object} Override options for the display style, useful to dynamically change the display style during simulations. Available options include:
     *              <li> fontSize (string): the font size of the display
     *              <li> fontColor (string): the font color of the display
     *              <li> backgroundColor (string): the background color of the display
     *              <li> borderWidth (Number): border width (default is 0, i.e., no border, unless option borderColor has been specified -- in this case, the border is 2px)</li>
     *              <li> borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid" (default is "none")</li>
     *              <li> borderColor (String): border color, must be a valid HTML5 color (default color used in the widget is "black")</li>
     *              <li> blinking (Bool): true means the text is blinking
     * @memberof module:BasicDisplay
     * @instance
     */
    BasicDisplay.prototype.render = function (txt, opt) { // todo: refactor param names: txt --> data
        var _this = this;
        function renderln(data, opt) {
            function filltext (data, txt, align) {
                for (var i = 0; i < txt.length; i++) {
                    if (align === "right") {
                        data.context.fillText(txt[txt.length - 1 - i], data.width - _this.letterSpacing * i, data.height / 2);
                    } else if (align === "left") {
                        data.context.fillText(txt[txt.length - 1 - i], _this.letterSpacing * txt.length - _this.letterSpacing * i, data.height / 2);
                    } else { // align === center
                        data.context.fillText(txt[txt.length - 1 - i], _this.letterSpacing * txt.length / 2 + data.width / 2 - _this.letterSpacing * i, data.height / 2);
                    }
                }
            }
            opt = opt || {};
            data.context.clearRect(0, 0, data.width, data.height);
            data.context.fillStyle = opt.backgroundColor || _this.backgroundColor;
            if (data.context.fillStyle !== "transparent") {
                data.context.fillRect(0, 0, data.width, data.height);
            }
            data.context.fillStyle = opt.fontColor || _this.fontColor;
            if (data.align === "left") {
                data.context.textAlign = "start";
                if (_this.letterSpacing) {
                    filltext(data, txt, "left");
                } else {
                    data.context.fillText(data.txt, 0, data.height / 2);
                }
            } else if (data.align === "right") {
                data.context.textAlign = "end";
                if (_this.letterSpacing) {
                    filltext(data, txt, "right");
                } else {
                    data.context.fillText(data.txt, data.width, data.height / 2);
                }
            } else {
                data.context.textAlign = "center";
                if (_this.letterSpacing) {
                    filltext(data, txt, "center");
                } else {
                    data.context.fillText(data.txt, data.width / 2, data.height / 2);
                }
            }
        }

        opt = opt || {};
        opt.auditoryFeedback = opt.auditoryFeedback || this.auditoryFeedback();
        var isEnabled = false;
        var visibleWhen = opt.visibleWhen || this.visibleWhen();
        var expr = StateParser.simpleExpressionParser(visibleWhen);
        if (expr && expr.res) {
            if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                isEnabled = true;
            } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                var str = StateParser.resolve(txt, expr.res.attr);
                if (str) {
                    str = StateParser.evaluate(str);
                    if ((expr.res.binop === "=" && str === expr.res.constant) ||
                         (expr.res.binop === "!=" && str !== expr.res.constant)) {
                             isEnabled = true;
                    }
                }
            }
        }

        if (isEnabled) {
            opt.borderWidth = opt.borderWidth || this.borderWidth;
            opt.borderStyle = opt.borderStyle || this.borderStyle;
            opt.borderColor = opt.borderColor || this.borderColor;
            this.div.style("border-width", opt.borderWidth + "px")
                    .style("border-style", opt.borderStyle)
                    .style("border-color", opt.borderColor);

            txt = txt || "";
            if (typeof txt === "object") {
                // txt in this case is a PVS state that needs to be parsed
                var disp = StateParser.resolve(txt, this.displayKey());
                if (disp) {
                    if (typeof disp === "object") {
                        disp = JSON.stringify(disp);
                    }
                    this.txt = StateParser.evaluate(disp);
                    if (typeof this.txt === "string") {
                        this.txt = this.txt.replace(new RegExp("\"", "g"), "");
                    }
                }
            } else {
                this.txt = txt;
            }
            if (this.format === "mm:ss" && !isNaN(parseFloat(this.txt))) {
                // value represents seconds, and format to be displayed is mm:ss
                var val = parseFloat(this.txt);
                var minutes = Math.floor(val / 60);
                var seconds = Math.floor(val - (minutes * 60));
                var minutes_txt = (minutes < 10)? "0" + minutes : minutes;
                var seconds_txt = (seconds < 10)? "0" + seconds : seconds;
                this.text2speech = minutes + " minutes and " + seconds + " seconds";
                this.txt = minutes_txt + ":" + seconds_txt;
            }
            //read out the display value if audio is enabled for this display widget
            if (opt.auditoryFeedback === "enabled") {
                if (this.format === "mm:ss") {
                    Speaker.speak(this.text2speech);
                } else {
                    Speaker.speak(this.txt.toString());
                }
            }
            this.example = this.txt;
            // set blinking
            var elemClass = document.getElementById(this.id()).getAttribute("class");
            if(elemClass) {
                elemClass = (opt.blinking || this.blinking) ?
                                ((elemClass.indexOf("blink") < 0) ? (elemClass + " blink") : elemClass)
                                : elemClass.replace(" blink", "");
                document.getElementById(this.id()).setAttribute("class", elemClass);
            }
            // render content
            var context = document.getElementById(this.id() + "_canvas").getContext("2d");
            context.textBaseline = this.textBaseline;
            var align = opt.align || this.align;
            context.font = this.font.join("");
            renderln({
                txt: this.txt,
                context: context,
                align: align,
                height: this.height,
                width: this.width - 2 * this.borderWidth
            }, opt);
            d3.select("#" + this.id() + "_canvas").style("display", "block");
            d3.select("#" + this.id() + "_span").style("display", "none");
            return this.reveal();
        }
        return this.hide();
    };

    /**
     * @function <a name="setColors">setColors</a>
     * @description Sets the font color and background color.
     * @param colors {Object} Font color and background color. Attributed of the object are fontColor: (string) and backgroundColor (string).
     * @param opt {Object} Override options for the display style, useful to dynamically change the display style during a simulation. Available options include:
     *              <li> fontSize (string): the font size of the display
     *              <li> fontColor (string): the font color of the display
     *              <li> backgroundColor (string): the background color of the display
     *              <li> blinking (Bool): true means the text is blinking
     * @memberof module:BasicDisplay
     * @instance
     */
    BasicDisplay.prototype.setColors = function (colors, opt) {
        colors = colors || {};
        opt = opt || {};
        opt.auditoryFeedback = opt.auditoryFeedback || "disabled";
        this.fontColor = colors.fontColor || this.fontColor;
        this.backgroundColor = colors.backgroundColor || this.backgroundColor;
        if (colors.opacity) {
            this.div.style("opacity", colors.opacity);
        }
        return this.render(this.txt, opt);
    };
    /**
     * @function <a name="invertColors">invertColors</a>
     * @description Inverts the colors of the display (as in a negative).
     * @memberof module:BasicDisplay
     * @instance
     */
    BasicDisplay.prototype.invertColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        var elemIsBlinking = (document.getElementById(this.id()).getAttribute("class").indexOf("blink") >= 0);
        return this.render(this.txt, { blinking: elemIsBlinking });
    };
    /**
     * @function <a name="invertGlyphiconColors">invertGlyphiconColors</a>
     * @description Inverts the colors of the glyphicond rendered with the display widget.
     * @memberof module:BasicDisplay
     * @instance
     */
    BasicDisplay.prototype.invertGlyphiconColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        var elemIsBlinking = (document.getElementById(this.id()).getAttribute("class").indexOf("blink") >= 0);
        return this.renderGlyphicon(this.txt, { blinking: elemIsBlinking });
    };
    BasicDisplay.prototype.renderSample = function (opt) {
        opt = opt || {};
        var txt = opt.txt || this.example;
        return this.render(txt, { visibleWhen: "true" });
    };

    /**
     * @function <a name="renderGlyphicon">renderGlyphicon</a>
     * @description Renders a glyphicon.
     * @param icon (String) Name of the glyphicon to be rendered, e.g., glyphicon-time. Glyphicon names are those of the bootstrap library (https://getbootstrap.com/docs/3.3/components/)
     * @param opt {Object} Override options. Same options available for the render() method.
     * @memberof module:BasicDisplay
     * @instance
     */
    BasicDisplay.prototype.renderGlyphicon = function (icon, opt) {
        opt = opt || {};
        this.txt = icon;
        var span = document.getElementById(this.id() + "_span");
        if (opt.blinking || this.blinking) {
            span.setAttribute("class", "glyphicon " + icon + " blink");
        } else {
            span.setAttribute("class", "glyphicon " + icon);
        }
        span.style.color = opt.fontColor || this.fontColor;
        span.style.backgroundColor = opt.backgroundColor || this.backgroundColor;
        span.style.borderRadius = "2px";
        span.style.width = this.width;
        span.style.height = this.height;
        span.style.fontSize = 0.7 * this.height + "px";
        d3.select("#" + this.id() + "_canvas").style("display", "none");
        d3.select("#" + this.id() + "_span").style("display", "block");
        this.reveal();
        return this;
    };


    /**
     * @function <a name="renderMultiline">renderMultiline</a>
     * @description Renders an array of display elements. Useful for displaying menus.
     * @example var disp = new BasicDisplay("disp");
                disp.renderMultiline([ res.bagsval0 + " ml",
                                       res.bagsval1 + " ml",
                                       res.bagsval2 + " ml",
                                       res.bagsval3 + " ml",
                                       res.bagsval4 + " ml",
                                       res.bagsval5 + " ml",
                                       res.bagsval6 + " ml",
                                       res.bagsval7 + " ml",
                                       res.bagsval8 + " ml",
                                       res.bagsval9 + " ml" ], { selected: 0, direction: "inverted" });
     * @param opt {Object} Override options
     *           <li>blinking (bool) Whether the widget is blinking (default: false, i.e., not blinking)</li>
     *           <li>direction (String) Whether the display elements are rendered from top to bottom,
     *               or from bottom to top. Default direction is top to bottom. Use "inverted" to render from bottom to top.</li>
     *           <li>selected (Number) A number representing the index of the selected display element.</li>
     * @memberof module:BasicDisplay
     * @instance
     */
    BasicDisplay.prototype.renderMultiline = function (txt, opt) {
        function clearContext(context, width, height) {
            context.save();
            context.fillStyle = opt.backgroundColor;
            context.fillRect(0, 0, width, height);
            context.restore();
        }
        function renderln(data, opt) {
            opt = opt || {};
            data.context.fillStyle = (opt.inverted) ? opt.fontColor : opt.backgroundColor;
            data.context.fillRect(0, data.y, data.width, data.height);
            data.context.fillStyle = (opt.inverted) ? opt.backgroundColor : opt.fontColor;
            var y_offset = data.y || 0;
            if (data.align === "left") {
                data.context.textAlign = "start";
                data.context.fillText(data.txt, 0, data.height / 2 + y_offset);
            } else if (data.align === "right") {
                data.context.textAlign = "end";
                data.context.fillText(data.txt, data.width, data.height / 2 + y_offset);
            } else {
                data.context.textAlign = "center";
                data.context.fillText(data.txt, data.width / 2, data.height / 2 + y_offset);
            }
        }
        opt = opt || {};
        opt.backgroundColor = opt.backgroundColor || this.backgroundColor;
        opt.fontColor = opt.fontColor || this.fontColor;
        this.txt = txt;
        var context = document.getElementById(this.id() + "_canvas").getContext("2d");
        clearContext(context, this.width, this.height);
        context.textBaseline = this.textBaseline;
        var align = opt.align || this.align;
        if (typeof txt === "object" && txt.length) {
            var fontSize = opt.menuFontSize || (this.height / txt.length);
            var newFont = [ fontSize ].concat(this.font.slice(1));
            context.font = newFont.join("");
            var i = 0;
            for (i = 0; i < txt.length; i++) {
                var offset = (opt.direction === "inverted") ? txt.length - i - 1 : i;
                renderln({
                    txt:  txt[i],
                    context: context,
                    align: align,
                    width: this.width,
                    height: (this.height / txt.length),
                    x: 0,
                    y: (offset * (this.height / txt.length))
                }, { inverted: (+opt.selected === i) ? true : false });
            }
        }
        // set blinking
        var elemClass = document.getElementById(this.id()).getAttribute("class");
        if (opt.blinking || this.blinking) {
            if (elemClass.indexOf("blink") < 0) {
                elemClass = elemClass + " blink";
            }
        } else {
            elemClass = elemClass.replace(" blink", "");
        }
        document.getElementById(this.id()).setAttribute("class", elemClass);
        d3.select("#" + this.id() + "_canvas").style("display", "block");
        d3.select("#" + this.id() + "_span").style("display", "none");
        this.reveal();
        return this;
    };

    /**
     * @function <a name="hide">hide</a>
     * @description Hides the widget
     * @memberof module:BasicDisplay
     * @instance
     */
    BasicDisplay.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    /**
     * @function <a name="reveal">reveal</a>
     * @description Makes the widget visible
     * @memberof module:BasicDisplay
     * @instance
     */
    BasicDisplay.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    /**
     * @function <a name="move">move</a>
     * @description Changes the position of the widget according to the coordinates given as parameter.
     * @param data {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
     * @memberof module:BasicDisplay
     * @instance
     */
    BasicDisplay.prototype.move = function (data) {
        data = data || {};
        if (data.top) {
            this.top = data.top;
            this.div.style("top", this.top + "px");
        }
        if (data.left) {
            this.left = data.left;
            this.div.style("left", this.left + "px");
        }
        return this;
    };

    /**
     * @function <a name="toJSON">toJSON</a>
     * @description Returns a serialised version of the widget in JSON format.
     *              This is useful for saving/loading a specific instance of the widget.
     *              In the current implementation, the following attributes are included in the JSON object:
     *              <li> type (string): widget type, i.e., "display" in this case
     *              <li> id (string): the unique identifier of the widget instance
     *              <li> fontSize (string): the font size of the display
     *              <li> fontColor (string): the font color of the display
     *              <li> backgroundColor (string): the background color of the display
     *              <li> displayKey (string): the name of the state attribute defining the display content.
     *              <li> visibleWhen (string): a booloan expression defining when the condition under which the widget is visible
     *              <li> auditoryFeedback (string): whether display readback is enabled
     * @returns JSON object
     * @memberof module:BasicDisplay
     * @instance
     */
    BasicDisplay.prototype.toJSON = function () {
        return {
            type: this.type(),
            id: this.id(),
            displayKey: this.displayKey(),
            auditoryFeedback: this.auditoryFeedback(),
            visibleWhen: this.visibleWhen(),
            fontSize: this.fontSize,
            fontColor: this.fontColor,
            backgroundColor: this.backgroundColor
        };
    };
    /**
    * Updates the location and size of the widget according to the given position and size
     */
    BasicDisplay.prototype.updateLocationAndSize = function (pos, opt) {
        opt = opt || {};
        if (opt.imageMap) {
            BasicDisplay.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
        }
        this.top = pos.y || 0;
        this.left = pos.x || 0;
        this.width = pos.width || 200;
        this.height = pos.height || 80;
        // this.fontsize = this.height * 0.9;
        // this.font = [this.fontsize, "px ", this.fontfamily];
        // this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
        d3.select("div." + this.id()).style("left", this.left + "px").style("top", this.top + "px")
            .style("width", this.width + "px").style("height", this.height + "px").style("font-size", this.fontSize + "px");
        d3.select("div." + this.id()).select("span").attr("width", this.width + "px").attr("height", this.height + "px"); // used for glyphicon
        d3.select("div." + this.id()).select("canvas").attr("width", this.width + "px").attr("height", this.height + "px"); // used for standard text and numbers
        return this.render(this.example, opt);
    };
    BasicDisplay.prototype.updateStyle = function (data) {
        data = data || {};
        this.fontSize = data.fontSize || this.fontSize;
        this.font = [this.fontSize, "px ", this.fontFamily];
        this.smallFont = [(this.fontSize * 0.7), "px ", this.fontFamily];
        this.fontColor = data.fontColor || this.fontColor;
        this.backgroundColor = data.backgroundColor || this.backgroundColor;
        return this;
    };
    /**
     * @function <a name="remove">remove</a>
     * @description Removes the div elements of the widget from the html page -- useful to programmaticaly remove widgets from a page.
     * @memberof module:TouchscreenButton
     * @instance
     */
    BasicDisplay.prototype.remove = function () {
        BasicDisplay.prototype.parentClass.remove.apply(this);
        d3.select("div." + this.id()).remove();
    };

    module.exports = BasicDisplay;
});
