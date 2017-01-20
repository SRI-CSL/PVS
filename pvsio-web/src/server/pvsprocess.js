/**
Copyright (c) 2012

Patrick Oladimeji
This file is part of pvsio-web.

pvsio-web is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

pvsio-web is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * javascript nodejs lib for communicating with pvs process
 * @author hogfather
 * @date Jul 27, 2012 12:54:38 AM
 * @project JSLib
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global require, module, process, __dirname */

var util = require("util"),
    path = require("path"),
    logger = require("tracer").console();
var procWrapper = require("./processwrapper");
module.exports = function () {
    "use strict";
    var pvs = procWrapper();
    var o                                   = {},
        output                              = [],
        readyString                         = "<PVSio>",
        garbageCollector                    = [";;; GC:", ";;; Finished GC"],
        wordsIgnored                        = ["", "==>", readyString].concat(garbageCollector),
        filename,
        processReady                        = false,
        workspaceDir                        = path.join(__dirname, "../../examples/");
    var _silentMode = false; // used to turn off log messages when restarting pvsio
	/**
	 * get or set the workspace dir. this is the base directory of the pvs source code
	 * @param {String} dir
	 * @return {String} the current workspace directory
	 */
	o.workspaceDir = function (dir) {
		if (dir) {
			//logger.log(dir);
			dir = dir.substr(-1) !== "/" ? (dir + "/") : dir;
			workspaceDir = dir;
			console.log("Workspace directory: " + workspaceDir + "\n");
            process.chdir(workspaceDir);
			return o;
		}
		return workspaceDir;
	};

	function filterLines(lines) {
		return lines.filter(function (d) {
			return wordsIgnored.indexOf(d.trim()) < 0;
		});
	}

	function arrayToOutputString(lines) {
		return lines.join("").replace(/,/g, ", ").replace(/\s+\:\=/g, ":=").replace(/\:\=\s+/g, ":=");
	}
	/**
        This function returns a function for processing stream data that is returned by the ouput stream of a process.
    */
	function processDataFunc() {
		var res = [];
		return function (data, cb) {
			var lines = data.split("\n");
			if (readyString === lines[lines.length - 1].trim()) {
				if (cb && typeof cb === "function") {
					lines.pop();//get rid of last line
					res = res.concat(filterLines(lines));
					cb(arrayToOutputString(res));
					res = [];
					return true;
				}
			} else {
				res = res.concat(filterLines(lines));
			}
			return false;
		};
	}

    o.removeFile = function (file, cb) {
        var np = path.normalize(file);

        if (np.indexOf(path.dirname(workspaceDir)) === 0) {
            pvs.exec({command: "rm -rf \"" + np + "\"", callBack: cb});
        } else {
            var error = ("cannot delete a folder outside the context of the current project");
            logger.error(error);
            logger.error(util.format("path: %s, workspace: %s, normalised Path: %s", file, workspaceDir, np));
            cb(error);
        }
    };
	/**
	 * starts the pvs process with the given sourcefile, then registers a data processor with the processwrapper.
     * The data processor function is used internally to match a command sent to the process with the corresponding
     * callback.
	 * @param {String} filename source file to load with pvsio
	 * @param {function} callback to call when processis ready or process exited
	 */
	o.start = function (file, callback) {
		filename = file;
        function onDataReceived(data) {
			// this shows the original PVSio output
            console.log(data.trim());
            if (!processReady) {
                var lines = data.split("\n").map(function (d) {
                    return d.trim();
                });
                var lastLine = lines[lines.length - 1];
                //copy lines into the output list ignoring the exit string, the startoutput string '==>'
                //and any blank lines
                output = output.concat(lines.filter(function (d) {
                    return wordsIgnored.indexOf(d) < 0;
                }));

                if (lastLine.indexOf(readyString) > -1) {
                    //last line of the output is the ready string
                    callback({type: "processReady", data: output});
                    processReady = true;
                    pvs.dataProcessor(processDataFunc());
                }
            }
            output = [];
		}

		function onProcessExited(err) {
			processReady = false;
			var msg;
            if (_silentMode) {
                msg = "";
            } else {
                if (err) {
                    if (err.code === "ENOENT") {
                        msg = "\n\n\n---------------------------------------------------\n\n\nError: PVS executable files are not on your PATH.\nPlease add the PVS executable files (pvs, pvsio and proveit) to your PATH.\n\nA way to do this is to create symbolic links to those files, and place the symbolic links in /usr/bin. For instance, if PVS is installed in /opt/PVS/pvs, the following commands executed in a Terminal window create the required symbolic links (note that you need to specify absolute paths):\n\nsudo ln -s /opt/PVS/pvs /usr/bin/pvs\nsudo ln -s /opt/PVS/pvsio /usr/bin/pvsio\nsudo ln -s /opt/PVS/proveit /usr/bin/proveit\n\n\n---------------------------------------------------";
                    } else {
                        msg = (err.code) ? "pvsio process exited with error code " + err.code + ".\n" + output.join("")
									: "pvsio process exited with error code " + err + ".\n" + output.join("");
                    }
                } else { msg = "pvsio process exited cleanly.\n" + output.join(""); }
                logger.error(msg);
            }
            _silentMode = false;
			callback({type: "processExited", data: msg, code: err});
		}

		if (process.env.PORT) { // this is for the PVSio-web version installed on the heroku cloud -- !!NB!! the pvsio script needs to be manually updated to point to /app/PVS (the relocate script does not work with heroku)
		    pvs.start({processName: "/app/PVS/pvsio", args: [filename],
				onDataReceived: onDataReceived,
				onProcessExited: onProcessExited});
		} else if (process.env.pvsdir) {
		    pvs.start({processName: path.join(process.env.pvsdir, "pvsio"), args: [filename],
				onDataReceived: onDataReceived,
				onProcessExited: onProcessExited});
        } else {
		    pvs.start({processName: "pvsio", args: [filename],
				onDataReceived: onDataReceived,
				onProcessExited: onProcessExited});
		}

		logger.info("\n-------------------------------------\nPVSio process started with theory "
                    + filename + "\n-------------------------------------");
        console.log("\nProcess context is " + o.workspaceDir() + "\n");
		return o;
	};

	/**
	 * sends a command to the pvsio process. This method returns immediately. The result of the command
	 * will be by the 'on data' event of the process standard output stream
	 * @param {string} command the command to send to pvsio
	 */
	o.sendCommand = function (command, callback) {
        console.log(command);
		pvs.sendCommand(command, callback);
		return o;
	};

	/**
	 * closes the pvsio process
     * @param {string} signal The signal to send to the kill process. Default is 'SIGTERM'
	 */
	o.close = function (signal, silentMode) {
        _silentMode = silentMode;
		signal = signal || 'SIGTERM';
		pvs.kill(signal);
		return o;
	};
	return o;
};
