/**
 * @description Reads out what is displayed on the user interface. The module uses speakGenerator.js, which is a port of the eSpeak speech synthesizer from C++ to JavaScript using Emscripten. Enables text-to-speech on the web using only JavaScript and HTML5. View a working demo at http://projects.mattytemple.com/speak-js/.
Options

You can also specify some options with calling speak(), by doing

  `speak('hello world', { option1: value1, option2: value2 .. })`
available options are:

-amplitude: How loud the voice will be (default: 100)
-pitch: The voice pitch (default: 50)
-speed: The speed at which to talk (words per minute) (default: 175)
-voice: Which voice to use (for a non-default voice, requires you to build speak.js to include the proper data. See Language Support below) (default: en/en-us)
-wordgap: Additional gap between words in 10 ms units (default: 0)
-noWorker: Do not use a web worker
For example `speak('hello world', { pitch: 100 })` will talk in a very high-pitched voice.

You then need to call speak() with the voice option that tells it to use the right voice for your language. For example, for French this should work:

  `speak('boulanger', { voice: 'fr' })`
 *
 * @author Patrick Oladimeji
 * @date 2/24/14 21:18:04 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, speak */
define(function (require, exports, module) {
    "use strict";
    var speaking = false, allowOverlap = true;

    module.exports = {
        speak: function (message) {
            //skip successive speak calls if overlap is disabled
            if (allowOverlap) {
                speak(message);
            } else {
                if (!speaking) {
                    speaking = true;
                    speak(message, null, function () {
                        speaking = false;
                    });
                }
            }
        },
        allowOverlap: function (val) {
            allowOverlap = val;
        }
    };
});
