
define(function (require, exports, module) {

    var ScreenCollection = require("plugins/pimPrototyper/ScreenCollection");

    return function() {
        describe("the screen collection", function() {
			var collection;

			/**
			* @param {ScreenCollection} collection Collection being watched
             * @param {string} event Name of the event to listen for
             * @return {function} The function being spied on
             * @private
             */
			function spyOnEvent(collection, event) {
                var handler = {
                    handler: function(){}
                };

                spyOn(handler, "handler");

                collection.on(event, handler.handler);
                return handler.handler;
            }

            beforeEach(function() {
                collection = new ScreenCollection();
            });

            afterEach(function() {
                collection.reset(null);
            });

            it("correctly sets the selected screen", function() {
                var s = new Backbone.Model();
				collection.setSelected(s);
                expect(collection.getSelected()).toBe(s);
            });

            it("emits an event when the selected screen is set", function() {
				var s = new Backbone.Model();
				var handler = spyOnEvent(collection, "selectionChanged");
				collection.setSelected(s);
                expect(handler.calls.count()).toEqual(1);
            });

            it("changes the selected screen if the currently selected screen is removed", function() {
				var s = new Backbone.Model();
				collection.add(s);
				collection.setSelected(s);
				collection.remove(s);
				expect(collection.getSelected()).not.toBe(s);
            });
        });
    };
});
