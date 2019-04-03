/**
 * @author Paolo Masci
 * @date 2016/04/25
 */

function displayKeyCode(charCode) {
    if (charCode === 8) return "backspace"; //  backspace
    if (charCode === 9) return "tab"; //  tab
    if (charCode === 13) return "enter"; //  enter
    if (charCode === 16) return "shift"; //  shift
    if (charCode === 17) return "ctrl"; //  ctrl
    if (charCode === 18) return "alt"; //  alt
    if (charCode === 19) return "pause/break"; //  pause/break
    if (charCode === 20) return "caps lock"; //  caps lock
    if (charCode === 27) return "escape"; //  escape
    if (charCode === 32) return "space bar";
    if (charCode === 33) return "page up"; // page up, to avoid displaying alternate character and confusing people
    if (charCode === 34) return "page down"; // page down
    if (charCode === 35) return "end"; // end
    if (charCode === 36) return "home"; // home
    if (charCode === 37) return "left arrow"; // left arrow
    if (charCode === 38) return "up arrow"; // up arrow
    if (charCode === 39) return "right arrow"; // right arrow
    if (charCode === 40) return "down arrow"; // down arrow
    if (charCode === 45) return "insert"; // insert
    if (charCode === 46) return "delete"; // delete
    if (charCode === 91) return "left window"; // left window
    if (charCode === 92) return "right window"; // right window
    if (charCode === 93) return "select key"; // select key
    if (charCode === 96) return "numpad 0"; // numpad 0
    if (charCode === 97) return "numpad 1"; // numpad 1
    if (charCode === 98) return "numpad 2"; // numpad 2
    if (charCode === 99) return "numpad 3"; // numpad 3
    if (charCode === 100) return "numpad 4"; // numpad 4
    if (charCode === 101) return "numpad 5"; // numpad 5
    if (charCode === 102) return "numpad 6"; // numpad 6
    if (charCode === 103) return "numpad 7"; // numpad 7
    if (charCode === 104) return "numpad 8"; // numpad 8
    if (charCode === 105) return "numpad 9"; // numpad 9
    if (charCode === 106) return "multiply"; // multiply
    if (charCode === 107) return "add"; // add
    if (charCode === 109) return "subtract"; // subtract
    if (charCode === 110) return "decimal point"; // decimal point
    if (charCode === 111) return "divide"; // divide
    if (charCode === 112) return "F1"; // F1
    if (charCode === 113) return "F2"; // F2
    if (charCode === 114) return "F3"; // F3
    if (charCode === 115) return "F4"; // F4
    if (charCode === 116) return "F5"; // F5
    if (charCode === 117) return "F6"; // F6
    if (charCode === 118) return "F7"; // F7
    if (charCode === 119) return "F8"; // F8
    if (charCode === 120) return "F9"; // F9
    if (charCode === 121) return "F10"; // F10
    if (charCode === 122) return "F11"; // F11
    if (charCode === 123) return "F12"; // F12
    if (charCode === 144) return "num lock"; // num lock
    if (charCode === 145) return "scroll lock"; // scroll lock
    if (charCode === 186) return ";"; // semi-colon
    if (charCode === 187) return "="; // equal-sign
    if (charCode === 188) return ","; // comma
    if (charCode === 189) return "-"; // dash
    if (charCode === 190) return "."; // period
    if (charCode === 191) return "/"; // forward slash
    if (charCode === 192) return "`"; // grave accent
    if (charCode === 219) return "["; // open bracket
    if (charCode === 220) return "\\"; // back slash
    if (charCode === 221) return "]"; // close bracket
    if (charCode === 222) return "'"; // single quote
    return "key " + String.fromCharCode(charCode);
}
function uniKeyCode(event) {
    if (event.keyCode === 27) {//  escape
        document.getElementById("keyCode").value = "Disabled";
        document.getElementById("keyName").value = "";
    } else {
        document.getElementById("keyCode").value = event.keyCode;
        document.getElementById("keyName").value = displayKeyCode(event.keyCode);
    }
    event.preventDefault();
    event.stopPropagation();
}
function colorNameToHex(color) {
    var colors = {"aliceblue":"#f0f8ff","antiquewhite":"#faebd7","aqua":"#00ffff","aquamarine":"#7fffd4","azure":"#f0ffff",
    "beige":"#f5f5dc","bisque":"#ffe4c4","black":"#000000","blanchedalmond":"#ffebcd","blue":"#0000ff","blueviolet":"#8a2be2","brown":"#a52a2a","burlywood":"#deb887",
    "cadetblue":"#5f9ea0","chartreuse":"#7fff00","chocolate":"#d2691e","coral":"#ff7f50","cornflowerblue":"#6495ed","cornsilk":"#fff8dc","crimson":"#dc143c","cyan":"#00ffff",
    "darkblue":"#00008b","darkcyan":"#008b8b","darkgoldenrod":"#b8860b","darkgray":"#a9a9a9","darkgreen":"#006400","darkkhaki":"#bdb76b","darkmagenta":"#8b008b","darkolivegreen":"#556b2f",
    "darkorange":"#ff8c00","darkorchid":"#9932cc","darkred":"#8b0000","darksalmon":"#e9967a","darkseagreen":"#8fbc8f","darkslateblue":"#483d8b","darkslategray":"#2f4f4f","darkturquoise":"#00ced1",
    "darkviolet":"#9400d3","deeppink":"#ff1493","deepskyblue":"#00bfff","dimgray":"#696969","dodgerblue":"#1e90ff",
    "firebrick":"#b22222","floralwhite":"#fffaf0","forestgreen":"#228b22","fuchsia":"#ff00ff",
    "gainsboro":"#dcdcdc","ghostwhite":"#f8f8ff","gold":"#ffd700","goldenrod":"#daa520","gray":"#808080","green":"#008000","greenyellow":"#adff2f",
    "honeydew":"#f0fff0","hotpink":"#ff69b4",
    "indianred ":"#cd5c5c","indigo":"#4b0082","ivory":"#fffff0","khaki":"#f0e68c",
    "lavender":"#e6e6fa","lavenderblush":"#fff0f5","lawngreen":"#7cfc00","lemonchiffon":"#fffacd","lightblue":"#add8e6","lightcoral":"#f08080","lightcyan":"#e0ffff","lightgoldenrodyellow":"#fafad2",
    "lightgrey":"#d3d3d3","lightgreen":"#90ee90","lightpink":"#ffb6c1","lightsalmon":"#ffa07a","lightseagreen":"#20b2aa","lightskyblue":"#87cefa","lightslategray":"#778899","lightsteelblue":"#b0c4de",
    "lightyellow":"#ffffe0","lime":"#00ff00","limegreen":"#32cd32","linen":"#faf0e6",
    "magenta":"#ff00ff","maroon":"#800000","mediumaquamarine":"#66cdaa","mediumblue":"#0000cd","mediumorchid":"#ba55d3","mediumpurple":"#9370d8","mediumseagreen":"#3cb371","mediumslateblue":"#7b68ee",
    "mediumspringgreen":"#00fa9a","mediumturquoise":"#48d1cc","mediumvioletred":"#c71585","midnightblue":"#191970","mintcream":"#f5fffa","mistyrose":"#ffe4e1","moccasin":"#ffe4b5",
    "navajowhite":"#ffdead","navy":"#000080",
    "oldlace":"#fdf5e6","olive":"#808000","olivedrab":"#6b8e23","orange":"#ffa500","orangered":"#ff4500","orchid":"#da70d6",
    "palegoldenrod":"#eee8aa","palegreen":"#98fb98","paleturquoise":"#afeeee","palevioletred":"#d87093","papayawhip":"#ffefd5","peachpuff":"#ffdab9","peru":"#cd853f","pink":"#ffc0cb","plum":"#dda0dd","powderblue":"#b0e0e6","purple":"#800080",
    "red":"#ff0000","rosybrown":"#bc8f8f","royalblue":"#4169e1",
    "saddlebrown":"#8b4513","salmon":"#fa8072","sandybrown":"#f4a460","seagreen":"#2e8b57","seashell":"#fff5ee","sienna":"#a0522d","silver":"#c0c0c0","skyblue":"#87ceeb","slateblue":"#6a5acd","slategray":"#708090","snow":"#fffafa","springgreen":"#00ff7f","steelblue":"#4682b4",
    "tan":"#d2b48c","teal":"#008080","thistle":"#d8bfd8","tomato":"#ff6347","turquoise":"#40e0d0",
    "violet":"#ee82ee",
    "wheat":"#f5deb3","white":"#ffffff","whitesmoke":"#f5f5f5",
    "yellow":"#ffff00","yellowgreen":"#9acd32"};
    if (typeof color === "string" && colors[color.toLowerCase()]) {
        return colors[color.toLowerCase()];
    }
    return color;
}
function rgb2hex(col){
    if (typeof col === "string") {
        var rgb = col.match(/^rgba?[\s+]?\([\s+]?(\d+)[\s+]?,[\s+]?(\d+)[\s+]?,[\s+]?(\d+)[\s+]?/i);
        return (rgb && rgb.length === 4) ?
                 ("#" + ("0" + parseInt(rgb[1],10).toString(16)).slice(-2) +
                        ("0" + parseInt(rgb[2],10).toString(16)).slice(-2) +
                        ("0" + parseInt(rgb[3],10).toString(16)).slice(-2)) : col;
    }
    return col;
}
function dimColor(col, amt) {
    if (typeof col === "string") {
        amt = amt || 48;
        col = rgb2hex(colorNameToHex(col)).slice(1) || "0";
        var num = parseInt(col,16);

        var r = (num >> 16) + amt;
        r = (r > 255) ? 255 : ((r < 0) ? 0 : r);

        var b = ((num >> 8) & 0x00FF) + amt;
        b = (b > 255) ? 255 : ((b < 0) ? 0 : b);

        var g = (num & 0x0000FF) + amt;
        g = (g > 255) ? 255 : ((g < 0) ? 0 : g);

        return "#" + (g | (b << 8) | (r << 16)).toString(16);
    }
    return col;
}
