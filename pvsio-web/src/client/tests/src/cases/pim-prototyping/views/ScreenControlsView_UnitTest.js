
define(function (require, exports, module) {

    var ScreenControlsView = require("plugins/pimPrototyper/forms/ScreenControlsView");
    var ScreenCollection = require("plugins/pimPrototyper/ScreenCollection");

    return function() {
        describe("the screen controls view", function() {

            function createCollection(modelData) {
                modelData = modelData || [];
                return new ScreenCollection(modelData);
            }

            function createView(collection) {
                collection = collection || createCollection();
                return new ScreenControlsView({ collection: collection });
            }

            function hasElement(selector) {
                var viewEl = createView().render().$el;
                var el = viewEl.find(selector);
                expect(el.length).toBeGreaterThan(0);
            }


            /**
            * @param {ScreenControlsView} view View being watched
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

            it("renders a dropdown list", function() {
                hasElement(".btn-screen-dropdown");
            });

            it("renders a settings button", function() {
                hasElement(".btn-screen-options");
            });

            it("renders a delete button", function() {
                hasElement(".btn-screen-delete");
            });

            it("renders a change image button", function() {
                hasElement(".btn-screen-image");
            });

            it("renders an add button", function() {
                hasElement(".btn-screen-add");
            });

            it("updates the screen displayed as selected when the selection changes", function() {
                var collection = createCollection(["a", "b", "c"]);
                var view = createView(collection);
                var toSelect = collection.last();
                var name = "test";
                toSelect.set("name", name);
                collection.setSelected(toSelect);
                expect(view.$el.find(".btn-screen-dropdown_label").text()).toEqual(name);
            });

            it("disables the buttons if no screen is selected", function() {
                var collection = createCollection(["a", "b", "c"]);
                var view = createView(collection);
                collection.setSelected(null);
                expect(view.$el.find(".btn-screen-options").attr("disabled")).not.toBeUndefined();
                expect(view.$el.find(".btn-screen-image").attr("disabled")).not.toBeUndefined();
                expect(view.$el.find(".btn-screen-delete").attr("disabled")).not.toBeUndefined();
            });

            it("enables the buttons if a screen is selected", function() {
                var collection = createCollection(["a", "b", "c"]);
                var view = createView(collection);
                collection.setSelected(collection.last());
                expect(view.$el.find(".btn-screen-options").attr("disabled")).toBeUndefined();
                expect(view.$el.find(".btn-screen-image").attr("disabled")).toBeUndefined();
                expect(view.$el.find(".btn-screen-delete").attr("disabled")).toBeUndefined();
            });

            it("emits an event when the change image button is clicked", function() {
                var collection = createCollection(["a", "b", "c"]);
                var view = createView(collection);
                collection.setSelected(collection.last());
                var f = spyOnEvent(view, "changeImageClicked");
                view.$el.find(".btn-screen-image").click();
                expect(f).toHaveBeenCalled();
            });
        });
    };
});
