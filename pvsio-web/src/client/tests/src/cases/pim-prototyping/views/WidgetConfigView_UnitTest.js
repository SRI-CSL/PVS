
define(function (require, exports, module) {

    var WidgetConfigView = require("plugins/pimPrototyper/forms/WidgetConfigView"),
        ScreenCollection = require("plugins/pimPrototyper/ScreenCollection"),
        Widget = require("plugins/pimPrototyper/PIMWidget");

    return function() {
        describe("the widget config dialog view", function() {
            var view;

            function spyOnEvent(event, fieldValue) {
                var $el = view.render().$el;
                $el.find("input[name='name']").val(fieldValue);

                var handler = {
                    handler: function(){}
                };

                spyOn(handler, "handler");

                view.on(event, handler.handler);
                return handler.handler;
            }

            function createCollection(modelData) {
                modelData = modelData || [];
                return new ScreenCollection(modelData);
            }

            function createView(data) {
                data = data || {};
                if (data.screenCollection == null) {
                    data.screenCollection = createCollection();
                }
                return new WidgetConfigView(data);
            }

            afterEach(function() {
                view.remove();
            });

            it("provides an input box for the widget name", function () {
                view = createView();
                var count = view.render().$el.find("input[name='name']").length;
                expect(count).toEqual(1);
            });

            it("provides a dropdown list for the target screen", function () {
                view = createView();
                var count = view.render().$el.find(".btn-screen-dropdown").length;
                expect(count).toEqual(1);
            });

            it("populates the form with data from the provided widget", function () {
                var collection = createCollection(["a", "b", "c"]);
                var targetScreen = collection.last();
                var screenName = "test";
                targetScreen.set("name", screenName);
                var widgetName = "testwidget";
                var widget = new Widget(1, {}, { name: widgetName, targetScreen: targetScreen });

                view = createView({ widget: widget, screenCollection: collection });
                view.render();

                expect(view.$el.find("input[name='name']")[0].value).toEqual(widgetName);
                expect(view.$el.find(".btn-screen-dropdown_label")[0].textContent).toEqual(screenName);
            });

            it("does not emit an 'ok' event if no input is provided", function () {
                view = createView();
                var handler = spyOnEvent("ok", null);
                view.$el.find(".btn-create").click();
                expect(handler.calls.count()).toEqual(0);
            });

            it("emits an 'ok' event when the create button is clicked if a name is provided", function () {
                view = createView();
                var handler = spyOnEvent("ok", "test");
                view.$el.find(".btn-create").click();
                expect(handler.calls.count()).toEqual(1);
            });

            it("does not emit an 'ok' event when the cancel button is clicked", function () {
                view = createView();
                var handler = spyOnEvent("ok", "test");
                view.$el.find(".btn-cancel").click();
                expect(handler.calls.count()).toEqual(0);
            });

            it("emits a 'cancel' event when the cancel button is clicked", function () {
                view = createView();
                var handler = spyOnEvent("cancel", "test");
                view.$el.find(".btn-cancel").click();
                expect(handler.calls.count()).toEqual(1);
            });
        });
    };
});
