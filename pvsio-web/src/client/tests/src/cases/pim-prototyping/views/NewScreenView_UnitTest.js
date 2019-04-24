
define(function (require, exports, module) {
    
    var NewScreenView = require("plugins/pimPrototyper/forms/NewScreenView");
    
    return function() {
        describe("the new screen dialog view", function() {
            var view;

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
            
            beforeEach(function() {
                view = new NewScreenView();
            });
            
            afterEach(function() {
                view.remove();
            });
            
            it("provides an input box for the screen name", function() {
                var count = view.render().$el.find("input[name='screenName']").length;
                expect(count).toEqual(1);
            });
            
            it("does not emit an 'ok' event if no input is provided", function() {
                var handler = spyOnEvent("ok", null);
                view.$el.find(".btn-create").click();
                expect(handler.calls.count()).toEqual(0);
            });
            
            it("emits an 'ok' event when the create button is clicked if a name is provided", function() {
                var handler = spyOnEvent("ok", "test");
                view.$el.find(".btn-create").click();
                expect(handler.calls.count()).toEqual(1);
            });
            
            it("does not emit an 'ok' event when the cancel button is clicked", function() {
                var handler = spyOnEvent("ok", "test");
                view.$el.find(".btn-cancel").click();
                expect(handler.calls.count()).toEqual(0);
            });
            
            it("emits a 'cancel' event when the cancel button is clicked", function() {
                var handler = spyOnEvent("cancel", "test");
                view.$el.find(".btn-cancel").click();
                expect(handler.calls.count()).toEqual(1);
            });
        });
    };
});
