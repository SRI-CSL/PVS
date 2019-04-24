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
         formTemplate = require("text!./feedback/frontend.handlebars"),
         theoremTemplate = require("text!./feedback/feedback.handlebars"),
         prooflite_strategy_induction  = require("text!./prooflite_strategy_induction.handlebars"),
         pvs_strategy_induction  = require("text!./pvs_strategy_induction.handlebars"),
         pvs_theorem_induction  = require("text!./pvs_theorem_induction.handlebars"),
         BaseDialog = require("pvsioweb/forms/BaseDialog"),
         FormUtils = require("plugins/emulink/forms/FormUtils");

     var default_data = { actions: ["act"], disp: ["disp"] };
     var theorem_data = {
         theorem_name: "VISIBILITY",
         transition_relation: "trans",
         property: "visibility",
         state: "State",
         guard: "guard"
     };
     var FeedbackTemplateView = BaseDialog.extend({
         initialize: function (data) {
             d3.select(this.el).attr("class", "overlay").style("top", self.scrollY + "px");
             default_data.actions = data.transitions;
             this.render(data);
             this._data = data;
             this.focus();
         },
         render: function (data) {
             var template = Handlebars.compile(formTemplate);
             this.$el.html(template(data));
             $("body").append(this.el);
             d3.select("#pvs_property").html(Handlebars.compile(theoremTemplate)(default_data));
             d3.select("#pvs_theorem").html(Handlebars.compile(pvs_theorem_induction)(theorem_data));
             d3.select("#prooflite_strategy").html(Handlebars.compile(prooflite_strategy_induction, { noEscape: true })({
                 theorem: theorem_data.theorem_name,
                 transition_relation: theorem_data.transition_relation,
                 transition_names: default_data.actions
             }));
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
             var data = { actions: default_data.actions, disp: ["disp"] };
             var transitions = d3.select("#FeedbackTemplate").select("#transitions").node();
             var attributes = d3.select("#FeedbackTemplate").select("#stateVariable").node();
             if (attributes && attributes.selectedOptions && attributes.selectedOptions.length > 0) {
                 data.disp = [];
                 for (var i = 0; i < attributes.selectedOptions.length; i++) {
                     if (attributes.selectedOptions[i].value !== "") {
                         data.disp.push(attributes.selectedOptions[i].value.replace(/\./g, "`"));
                     }
                 }
             }
             d3.select("#pvs_property").node().value = Handlebars.compile(theoremTemplate)(data);
             d3.select("#pvs_theorem").node().value = Handlebars.compile(pvs_theorem_induction, { noEscape: true })(theorem_data);
             d3.select("#prooflite_strategy").node().value = Handlebars.compile(prooflite_strategy_induction, { noEscape: true })({
                 theorem: theorem_data.theorem_name,
                 transition_relation: theorem_data.transition_relation,
                 transition_names: data.actions
             });
         },
         right: function (event) {
             var form = this.el;
             if (FormUtils.validateForm(form)) {
                 var selectors = [ "pvs_property", "pvs_theorem", "prooflite_strategy" ];
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
             return new FeedbackTemplateView(data);
         }
     };
 });
