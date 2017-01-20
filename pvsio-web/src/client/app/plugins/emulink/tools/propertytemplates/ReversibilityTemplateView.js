/**
 * PropertyTemplates plugin.
 * @author Paolo Masci
 * @date Oct 4, 2016
 */
 /*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
 /*jshint unused:false*/
 define(function (require, exports, module) {
     "use strict";
     var d3 = require("d3/d3"),
         formTemplate = require("text!./reversibility/frontend.handlebars"),
         theoremTemplate = require("text!./reversibility/reversibility.handlebars"),
         BaseDialog = require("pvsioweb/forms/BaseDialog"),
         FormUtils = require("plugins/emulink/forms/FormUtils");

     var default_data = { actions: ["act"], reversingAction: "rev", s: "s" };
     var ReversibilityTemplateView = BaseDialog.extend({
         initialize: function (data) {
             d3.select(this.el).attr("class", "overlay").style("top", self.scrollY + "px");
             this.render(data);
             this._data = data;
             this.focus();
         },
         render: function (data) {
             var template = Handlebars.compile(formTemplate);
             this.$el.html(template(data));
             $("body").append(this.el);
             d3.select("#pvs_property").html(Handlebars.compile(theoremTemplate)(default_data));
             return this;
         },
         events: {
             "input #reversingAction": "updateTheorem",
             "input #stateVariable": "updateTheorem",
             "input #transitions": "updateTheorem",
             "click #btnRight": "right",
             "click #btnLeft": "left",
             "keydown .panel": "keypress"
         },
         updateTheorem: function (event) {
             var data = { actions: ["act"], reversingAction: "rev", s: "s" };
             var transitions = d3.select("#ReversibilityTemplate").select("#transitions").node();
             if (transitions && transitions.selectedOptions && transitions.selectedOptions.length > 0) {
                 data.actions = [];
                 for (var i = 0; i < transitions.selectedOptions.length; i++) {
                     if (transitions.selectedOptions[i].value !== "") {
                         data.actions.push(transitions.selectedOptions[i].value);
                     }
                 }
             }
             var reversingAction = d3.select("#ReversibilityTemplate").select("#reversingAction").node();
             if (reversingAction && reversingAction.selectedOptions && reversingAction.selectedOptions.length > 0) {
                 data.reversingAction = d3.select("#ReversibilityTemplate").select("#reversingAction").node().selectedOptions[0].value || data.reversingAction;
             }
             var attributes = d3.select("#ReversibilityTemplate").select("#stateVariable").node();
             if (attributes && attributes.selectedOptions && attributes.selectedOptions.length > 0) {
                 data.s = d3.select("#ReversibilityTemplate").select("#stateVariable").node().selectedOptions[0].value.replace(/\./g, "`") || data.s;
             }
            //  d3.select("#pvs_property").html(Handlebars.compile(theoremTemplate)(data));
            d3.select("#pvs_property").node().value = Handlebars.compile(theoremTemplate)(data);
         },
         right: function (event) {
             var form = this.el;
             if (FormUtils.validateForm(form)) {
                 var selectors = [ "pvs_property" ];
                 var formdata = FormUtils.serializeForm(form, selectors);
                 this.trigger(this._data.buttons[1].toLowerCase(), {data: formdata.labels, el: this.el}, this);
             }
         },
         left: function (event) {
             this.trigger(this._data.buttons[0].toLowerCase(), {el: this.el}, this);
         },
         keypress: function (event) {
             var form = this.el;
             switch(event.which) {
             case 13: //enter pressed
                 this.right(event);
                 break;
             case 27: //esc pressed
                 this.left(event);
                 break;
             default: break;
             }
         }
     });

     module.exports = {
         /**
          * creates a new form view to display questions. Renders two buttons for
          * taking positive or negative responses to the question posed.
          * @param {header: {string}, textLines: {string}} data Data to use to render the form
          */
         create: function (data) {
             return new ReversibilityTemplateView(data);
         }
     };
 });
