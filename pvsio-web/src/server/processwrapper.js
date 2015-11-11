/**
Copyright (c) 2012

Patrick Oladimeji
This file is part of pvsio-web.

pvsio-web is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

pvsio-web is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * 
 * A generic wrapper for spawning a process, sending messages to a process and shutting down a process 
 * 
 * @author Patrick Oladimeji
 * @date Dec 2, 2012 : 10:28:48 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global require, process, module */
var spawn = require("child_process").spawn,
    logger = require("tracer").console(),
	exec = require("child_process").exec;

module.exports = function () {
    "use strict";
    var proc, _dataProcessor;
    var o = {}, cbQueue = [];
    
    /**
     * Starts an interactive process with the specified parameters. The opt variable should contain
     * at least the name of the process to start
     * @param opt - parameters for starting up the process. May contain the following properties
     *      processName: String
     *      args: Array
     *      onDataReceived: function to be called when the process sends something to its stdout
            onErrorReceived: function to be called when the process encounters an error
     *      onProcessExited: function to be called when the process exits
     *  
     * @returns {___anonymous325_326}
     */
    o.start = function (opt) {
        if (opt) {
            try {
                proc = spawn(opt.processName, opt.args, {uid: process.getuid(), gid: process.getgid()});

                proc.stdout.setEncoding('utf8');
                proc.stderr.setEncoding("utf8");

                proc.stdout.on("data", function (data) {
                    var f = o.dataProcessor();
                    //call any callback and forward the data to onDataReceived if it exists
                    if (f) {
                        if (f(data, cbQueue[0])) {
                            cbQueue.shift();
                        }
                    }
                    if (opt.onDataReceived) { opt.onDataReceived(data); }
                });
                if (opt.onErrorReceived) {
                    proc.stderr.on("data", opt.onErrorReceived);
                }
                if (opt.onProcessExited) {
                    proc.on('close', opt.onProcessExited);
                    proc.on('error', opt.onProcessExited);
                }
            } catch (err) {
                logger.log(err);
            }
        }
        return o;
    };
    /**
	 * A function that is parses the data stream and forwards the result to a callback function as appropriate.
	 * This would typically be when a specified token is found in the output stream.
	 * The function takes a string and a callback function as parameter and should return true if the callback function
	 * has been invoked
	 */
	o.dataProcessor = function (f) {
		if (f) {
			_dataProcessor = f;
			return o;
		}
		return _dataProcessor;
	};
    /**
     * execute a self terminating process and calls the callback with the output of the stdout
     * @param {String} opt A space separated string containing the process to execute and any arguments
     * @returns {___anonymous364_365}
     */
    o.exec = function (opt) {
        exec(opt.command, opt.callBack);
        return o;
    };
    /**
     * stop the process spawned
     * @returns {___anonymous364_365}
     */
    o.kill = function (signal) {
        signal = signal || 'SIGTERM';
        if (proc) {
            proc.kill(signal);
            proc.stdout.destroy();
            proc.stdin.destroy();
            proc.stderr.destroy();
            proc = undefined;
        }
        return o;
    };
	
	function write(msg, cb) {
		cbQueue.push(cb);
		var ok = proc.stdin.write(msg);
		if (!ok) {
			//wait for drain event before trying to write the command again
			proc.stdin.once("drain", (function (m) {
				write(m);
			}(msg)));
		}
	}
		
    /**
     * send a command to the running process
     * @param command
     * @returns {___anonymous364_365}
     */
    o.sendCommand = function (command, cb) {
		write(command, cb);
        return o;
    };
    return o;
};