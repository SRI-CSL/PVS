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
