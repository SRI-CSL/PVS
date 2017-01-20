
define(function (require, exports, module) {

    var EditScreenView = require("plugins/pimPrototyper/forms/EditScreenView"),
        Screen = require("plugins/pimPrototyper/Screen");

    return function() {
        describe("the edit screen dialog view", function() {
            var view;
            var emptyScreenData;

            /**
             * @param {string} event Name of the event to listen for
             * @param {string} fieldValue Value to give the screen name input field
             * @return {function} The function being spied on
             * @private
             */
            function spyOnEvent(event, fieldValue) {
                var $el = view.render().$el;
                $el.find("input[name='screenName']").val(fieldValue);

                var handler = {
                    handler: function(){}
                };

                spyOn(handler, "handler");

                view.on(event, handler.handler);
                return handler.handler;
            }

            beforeAll(function() {
                var screen = new Screen();
                emptyScreenData = {
                    model: screen
                };
            });

            afterEach(function() {
                view.remove();
            });

            it("provides an input box for the screen name", function() {
                view = new EditScreenView(emptyScreenData);
                var count = view.render().$el.find("input[name='screenName']").length;
                expect(count).toEqual(1);
            });

            it("displays the current screen name in the input box", function() {
                var screen = new Screen({ name: "this is a test" });
                view = new EditScreenView({ model: screen });
                var value = view.render().$el.find("input[name='screenName']").value;
                expect(value).toEqual(screen.name);
            });

            it("provides a check box for setting the initial screen", function() {
                view = new EditScreenView(emptyScreenData);
                var count = view.render().$el.find("input[name='isInitial'][type='checkbox']").length;
                expect(count).toEqual(1);
            });

            it("does not emit an 'ok' event if no name input is provided", function() {
                view = new EditScreenView(emptyScreenData);
                var handler = spyOnEvent("ok", null);
                view.$el.find(".btn-create").click();
                expect(handler.calls.count()).toEqual(0);
            });

            it("emits an 'ok' event when the create button is clicked if a name is provided", function() {
                view = new EditScreenView(emptyScreenData);
                var handler = spyOnEvent("ok", "test");
                view.$el.find(".btn-create").click();
                expect(handler.calls.count()).toEqual(1);
            });

            it("does not emit an 'ok' event when the cancel button is clicked", function() {
                view = new EditScreenView(emptyScreenData);
                var handler = spyOnEvent("ok", "test");
                view.$el.find(".btn-cancel").click();
                expect(handler.calls.count()).toEqual(0);
            });

            it("emits a 'cancel' event when the cancel button is clicked", function() {
                view = new EditScreenView(emptyScreenData);
                var handler = spyOnEvent("cancel", "test");
                view.$el.find(".btn-cancel").click();
                expect(handler.calls.count()).toEqual(1);
            });
        });
    };
});
