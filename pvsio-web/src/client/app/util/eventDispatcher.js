/**
 * @author hogfather
 * @date Feb 29, 2012
 * @project JSNumberEntry
 */

/**
 * adapted from javascript the good parts
 * @param that
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define*/
define(function (require, exports, module) {
    "use strict";
    module.exports = function (that) {
        var registry = {};

        var fire = function (event) {
            // Fire an event on an object. The event can be either  a string
            // containing the name of the event or an  object containing a type
            // property containing the  name of the event. Handlers registered by
            // the 'on' method that match the event name will be invoked.
            var array, func, handler, i, type = typeof event === 'string' ? event
                : event.type;
            // If an array of handlers exist for this event, then  loop through it
            // and execute the handlers in order.
            if (registry.hasOwnProperty(type)) {
                array = registry[type];
                for (i = 0; i < array.length; i++) {
                    handler = array[i];
                    // A handler record contains a method and an optional  array
                    // of parameters. If the method is a name, look up the
                    // function.
                    func = handler.method;
                    if (typeof func === 'string') {
                        func = this[func];
                    }
                    // Invoke a handler. If the record contained  parameters, then
                    // pass them. Otherwise, pass the event object.
                    func.apply(this, handler.parameters || [ event ]);
                }//end for
            }//end if
            return this;
        };
        that.fire = fire;

        var on = function (type, method, parameters) {
            // Register an event. Make a handler record. Put it  in a handler
            // array, making one if it doesn't yet exist for this type.
            var handler = {	method : method, parameters : parameters};
            if (registry.hasOwnProperty(type)) {
                registry[type].push(handler);
            } else {
                registry[type] = [ handler ];
            }

            return this;
        };

        that.addListener = on;

        var removeListener = function (type, method) {
            var array, i, handler;
            if (registry.hasOwnProperty(type)) {
                array = registry[type];
                for (i = 0; i < array.length; i++) {
                    handler = array[i];
                    if (method === handler.method) {
                        array.splice(i, 1);
                    }
                }//end for
            }
            //do nothing if this event type has no listeners
            return this;
        };
        that.removeListener = removeListener;

        var clearListeners = function () {
            registry = {};
            return this;
        };
        that.clearListeners = clearListeners;

        return that;
    };
});
