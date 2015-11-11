/**
 * manages rendering tcas view
 * @author Patrick Oladimeji
 * @date 3/18/14 13:53:20 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3, require, $, brackets, window, MouseEvent */
define(function (require, exports, module) {
    "use strict";
    var StateParser = require("util/PVSioStateParser"),
        d3 = require("d3/d3");
    
    var h = 1200, w = 1200;
    var angleOffset = Math.PI / 2, nmi = 1852, nmiToPixel = 32;
    var colorMap = {"yellow": "rgba(255,255,0,0.5)", "transparent": "rgba(255,255,255,0)"};
    var maxRange = 180;
    function distance (relPosition) {
        return Math.sqrt(relPosition.x * relPosition.x + relPosition.y * relPosition.y);
    }
    
    function drawIntruder(pos) {
        
    }
    
    function drawCross(canvas, pos) {
        var size = 5;
        canvas.save();
        canvas.fillStyle = "#ddd";
        canvas.strokeStyle = "#ddd";
        canvas.beginPath();
        canvas.moveTo(pos.x, pos.y - size);
        canvas.lineTo(pos.x, pos.y + size);
        canvas.moveTo(pos.x - size, pos.y);
        canvas.lineTo(pos.x + size, pos.y);
        canvas.stroke();
        canvas.restore();
    }
    
    function drawCircle(c, pos, rad) {
        c.save();
        c.beginPath();
        c.lineWidth = 2;
        c.strokeStyle = "#ddd";
        c.arc(pos.x, pos.y, rad, 0, 360 * Math.PI / 180);
        c.stroke();
        c.closePath();
        c.restore();
    }
    
    function drawDiamond(c, pos, color) {
        
    }
    
    function drawSelf(canvas, pos) {
        drawCross(canvas, pos);
    }
    
    function drawText (text, x, y, c) {
        c.fillStyle = "white";
        c.font = "normal 10px Arial";
        c.fillText(text, x, y);
    }

    function drawTick(radius, angle, center, c, tickLength) {
        angle += angleOffset;
        tickLength = tickLength || 10;
        var x1 = center.x + radius * Math.cos(angle),
            x2 = center.x + (radius - tickLength) * Math.cos(angle),
            y1 = center.x + radius * Math.sin(angle),
            y2 = center.x + (radius - tickLength) * Math.sin(angle);
        c.beginPath();
        c.lineWidth = 4;
        c.moveTo(x1, y1);
        c.lineTo(x2, y2);
        c.stroke();
    }
    
    function drawRangeBands(bands, ownPos, c) {
        var color;
        bands.forEach(function (band) {
            color = band.color.toLowerCase();
            if (color === "none") {
                color = "transparent";
            }
            
//            band.range.forEach(function (angle) {
//                c.save();
//                c.strokeStyle = colorMap[color];
//                drawTick(160, angle, ownPos, c);
//                c.restore();
//            });
            //draw an arc for each begining and end of band -- the normalisation factor 0.4 is due to the TCAS display which goes from 0 to 6 and from -6 to 0, and also forms an incomplete circle
            var first = band.range[0] * 0.4,
                last = band.range[band.range.length - 1] * 0.4;
            
            c.save();
            c.strokeStyle = colorMap[color];
            c.lineWidth = 20;
            c.beginPath();
            c.arc(ownPos.x, ownPos.y, 175, first, last);
            c.stroke();
            c.restore();
        });
    }
    
    
    function translate (pos, trans) {
        pos.x += trans.x;
        pos.y += trans.y;
        return pos;
    }
    
    function normalisePos (pos) {
        pos.x = (StateParser.evaluate(pos.x) * nmiToPixel / nmi);
        pos.y = (StateParser.evaluate(pos.y) * nmiToPixel / nmi * -1); // svg y axis is flipped wrt the y Cartesian axis
        return pos;
    }
    
    
    function render(state) {
        console.log(state);
        //create the canvas if it does not already exist
        var canvasEl = d3.select("canvas");
        if (canvasEl.empty()) {
            canvasEl = d3.select("#canvas").append("canvas").attr("width", w + "px").attr("height", h + "px");
        }
        var canvas = canvasEl.node().getContext("2d");
        canvas.clearRect(0, 0, w, h);
        canvas.fillStyle = "black";
        canvas.fillRect(0, 0, w, h);
        canvas.drawImage(document.getElementById("tcas_deck"), 0, 0);
        if (!state.si || !state.so) {
            return;
        }
        var center = {x: w / 2, y: h / 2};

        var ipos = translate(normalisePos(state.si), center);
        var opos = translate(normalisePos(state.so), center);
        var iPosRel = { x: (ipos.x - opos.x) , y: (ipos.y - opos.y) };
        var ownPos = { x: center.x, y: center.y + 2 * nmiToPixel };
        //draw the intruder position (if within a given range)
        if (distance(iPosRel) < maxRange) {
            drawCircle(canvas, {x: ownPos.x + iPosRel.x, y: ownPos.y + iPosRel.y}, 5);
        }
        //draw the own position
        drawSelf(canvas, ownPos);
        var bands = state.trkBand.map(function (band) {
            return {color: band.color, range: band.range.map(function (d) {
                return StateParser.evaluate(d);
            })};
        });
        drawRangeBands(bands, center, canvas);
        var needleAngle = Math.atan(StateParser.evaluate(state.vo.y) / StateParser.evaluate(state.vo.x));
        canvas.strokeStyle = "rgba(255,255,255,0.7)";
        drawTick(180, needleAngle, center, canvas, 170);
    }
    
    module.exports = {
        render: render
    };
});
