/**
 * @module CursoredDisplay
 * @desc Renders a cursor display
 * @author Patrick Oladimeji, Paolo Masci
 * @date Mar 13, 2012
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, document */

define(function (require, exports, module) {
    "use strict";
    module.exports = function (el, width, height, opt) {
        opt = opt || {};
        width = width || 200;
        height = height || 80;
        var font = (height * 0.5) + "px " + (opt.font || "sans-serif"),
            smallFont = (height * 0.4) + "px " + (opt.font || "sans-serif"),
            align = opt.align || "center",
            black = (opt.inverted) ? '#fff' : "#000",
            white = (opt.inverted) ? "#000" : '#fff',
            textBaseline = "middle";

        function fontheight(font) {
            var r = font.match(/\d+/g)[0];
            return parseFloat(r);
        }

        function decRadius() {
            return fontheight(smallFont) / 8;
        }

        var context = document.getElementById(el).getContext("2d");
        context.font = font;
        context.align = align;
        context.fillStyle = black;
        context.textBaseline = textBaseline;
        /**
         * clears the canvas context to prepare for drawing
         */
        function clearContext() {
            context.save();
            context.fillStyle = black;
            context.fillRect(0, 0, width, height);
            context.restore();
        }

        function drawCircle(context, x, y, r, fillStyle) {
            context.save();
            context.fillStyle = fillStyle;
            context.beginPath();
            context.arc(x, y, r, 0, Math.PI * 2, true);
            context.closePath();
            context.stroke();
            context.fill();
            context.restore();
        }

        return {
            renderNumber: function (numstr, cursorpos) {
                clearContext();
                var th = 28,
                    x,
                    y,
                    pad = 2;
                var centerx = width / 2,
                    centery = height / 2,
                    txtmeasure;
                var frac = numstr.split(".")[1],
                    whole = numstr.split(".")[0];
                //pad the string if necessary
                var i;
                if (cursorpos >= whole.length - 1) {
                    for (i = cursorpos - (whole.length - 1); i > 0; i--) {
                        whole = "0".concat(whole);
                    }
                } else if (cursorpos < 0) {
                    frac = frac || "";
                    for (i = Math.abs(cursorpos) - frac.length; i > 0; i--) {
                        frac = frac.concat("0");
                    }
                }
                if (frac !== undefined && frac.length > 0) {
                    drawCircle(context, centerx, centery, decRadius(), white);

                    x = centerx + pad + decRadius();
                    context.textAlign = "left";
                    context.fillStyle = white;
                    context.font = smallFont;
                    th = fontheight(context.font);
                    y = centery + (th * 0.5);
                    //draw the fraction bit
                    frac.split("").forEach(function (d, index) {
                        if (cursorpos === (index + 1) * -1) {
                            context.save();
                            //draw a cursor and then the number
                            txtmeasure = context.measureText(d);
                            context.fillRect(x, y - th, txtmeasure.width, th);
                            context.fillStyle = black;
                            context.fillText(d, x, centery);
                            x += txtmeasure.width + pad;
                            context.restore();
                        } else {
                            txtmeasure = context.measureText(d);
                            context.fillText(d, x, centery);
                            x += txtmeasure.width + pad;
                        }
                    });
                }
                context.font = font;
                context.textAlign = "right";
                context.fillStyle = white;
                x = centerx - decRadius() - pad;
                th = fontheight(context.font);
                y = centery + (th * 0.5);
                //draw the whole bit in reverse aligning to the right
                whole.split("").reverse().forEach(function (d, index) {
                    if (d === "_" && index < whole.length - cursorpos) { d = "0"; }
                    if (cursorpos === index) {
                        context.save();
                        //draw a cursor and then the number
                        txtmeasure = context.measureText(d);
                        context.fillRect(x - txtmeasure.width, y - th, txtmeasure.width, th);
                        context.fillStyle = black;
                        context.fillText(d, x, centery);
                        x -= (txtmeasure.width + pad);
                        context.restore();
                    } else {
                        txtmeasure = context.measureText(d);
                        context.fillText(d, x, centery);
                        x -= (txtmeasure.width + pad);
                    }
                });
                return this;
            },
            renderText: function (txt) {
                clearContext();
                var pad = 2,
                    centerx = width / 2,
                    centery = height / 2,
                    txtmeasure = context.measureText(txt);
                context.font = font;
                context.fillStyle = white;
                context.textAlign = align;
                if (align === "center") {
                    context.fillText(txt, centerx, centery);
                } else if (align === "left") {
                    context.fillText(txt, pad, centery);
                } else {
                    context.fillText(txt, width - txtmeasure.width + pad, centery);
                }
                return this;
            },
            width: function (w) {
                if (w !== undefined) {
                    width = w;
                    return this;
                }
                return width;
            },
            height: function (h) {
                if (h !== undefined) {
                    height = h;
                    return this;
                }
                return height;
            },
            font: function (f) {
                if (f) {
                    font = f;
                    return this;
                }
                return font;
            },
            smallfont: function (f) {
                if (f) {
                    smallFont = f;
                    return this;
                }
                return smallFont;
            }

        };

    };
});
