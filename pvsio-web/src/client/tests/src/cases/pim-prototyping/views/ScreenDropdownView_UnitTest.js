
define(function (require, exports, module) {

    var ScreenDropdownView = require("plugins/pimPrototyper/forms/ScreenDropdownView"),
        ScreenCollection = require("plugins/pimPrototyper/ScreenCollection");

    return function() {
        describe("the screen dropdown view", function () {

            function createCollection(modelData) {
                modelData = modelData || [];
                return new ScreenCollection(modelData);
            }

            function createView(collection) {
                collection = collection || createCollection();
                return new ScreenDropdownView({ collection: collection });
            }

            function hasElement(selector) {
                var viewEl = createView().render().$el;
                var el = viewEl.find(selector);
                expect(el.length).toBeGreaterThan(0);
            }

            /**
            * @param {ScreenDropdownView} view View being watched
             * @param {string} event Name of the event to listen for
             * @return {function} The function being spied on
             * @private
             */
            function spyOnEvent(view, event) {
                var handler = {
                    handler: function(){}
                };

                spyOn(handler, "handler");

                view.on(event, handler.handler);
                return handler.handler;
            }

            it("renders a dropdown list", function () {
                hasElement(".btn-screen-dropdown");
            });

            it("lists all the screens in the model", function () {
                var collection = createCollection(["a", "b", "c"]);
                var view = createView(collection);
                var count = view.render().$el.find(".screen-dropdown li").length;
                expect(count).toEqual(collection.length);
            });

            it("updates the screen list when the collection is removed from", function () {
                var collection = createCollection(["a", "b", "c"]);
                var view = createView(collection);
                view.render();
                collection.remove(collection.last());
                var count = view.$el.find(".screen-dropdown li").length;
                expect(count).toEqual(collection.length);
            });

            it("emits an event when a screen in the list is clicked", function () {
                var collection = createCollection(["a", "b", "c"]);
                var view = createView(collection);
                view.render();
                var f = spyOnEvent(view, "screenSelected");
                view.$el.find("li a")[0].click();
                expect(f).toHaveBeenCalled();
            });
        });
    };
});
