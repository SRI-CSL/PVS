/**
 * @module PIMTestGenerator
 * @version 1.0
 * @description
 * PIMTestGenerator provides functions to generate abstract test cases from PIM Emuchart models.
 * @author Nathan Robb
 * @date 20/10/2015
 *
 * Presentation Interaction Models have the structure of:
 PIMs = array of PIM
 PIM = {
    states: (array of {
                name: (string) // the state name
        }),
    transitions: (array of {
                start_state: (string), // Initial state
                end_state: (string),	// End state
                i_behaviour: (string)  // Action on transition (Interface behaviour)
        }),
    name: (string),
    pm: (Presentation Model {   // Presentation model for this PIM
                name: (string),
                widgets: (array of widget {
                        name : (string),
                        category : (string),
                        behaviours : (array of string)
                    }),
                components: (array of {
                        name: (string),
                        widgets: (array of widget {
                                name : (string),
                                category : (string),
                                behaviours : (array of string)
                            }),
                        components: (empty array (of Presentation Model),
                        pmr: ((set of ){
                                behaviour (string): operation (string)
                            })
                    })
        }),
    pmr: ((set of ){
                behaviour (string): operation (string)
        }),
    start_state: (State {
                name: (string)  // Starting state of the PIM
        }),
    final_states: (array of {
                name: (string) // Ending states of the PIM
        })
}
 */

/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define */

define(function (require, exports, module) {
    "use strict";

    /**
     * Constructor.
     * @memberof PIMTestGenerator
     */
    function PIMTestGenerator(name) {
        this.modelName = name;
        return this;
    }

    /**
     * Returns a string of abstract tests for the Emuchart.
     * @param pm PM (or PIM) to generate tests for.
     * @returns {string} The test cases for the PM/PIM.
     * @memberof PIMTestGenerator
     */
    PIMTestGenerator.prototype.generateTests = function (pm) {
        /**
         * Generate tests for the presentation model
         * (If the pm is a PIM then generates tests for all its
         * component models, else if PM then generates tests for its
         * widgets).
         * @param pim The presentation interaction model to get component model
         * tests for.
         * @returns Array of AbstractTests {
         *		  state_name: (string),
         *		  widget_name: (string),
         *		  predicates: (array of {
         *				  name: (string),
         *				  args: (array of string)
         *			  })
         *	  }
         */
        function genTests(pim) {
            var pm = pim.pm || pim;
            if (!pm || !pm.widgets || !pm.widgets.forEach) {
                return [];
            }

            var componentTests = [];
            // If a PIM PM is passed in 1st this will be skipped
            pm.widgets.forEach(function(widget) {
                var tPredicates = [], tArgs = [];
                // Add widget predicates
                tPredicates.push({ name: "hasWidget", args: [widget.name] });
                tPredicates.push({ name: "Visible", args: [widget.name] });
                tPredicates.push({ name: "Active", args: [widget.name] });

                if (widget.behaviours.length > 0) {
                    // The behaviour is on this widget
                    tArgs.push(widget.name);
                    // The behaviours this widget has (add all)
                    tArgs.push.apply(tArgs, widget.behaviours.reverse());

                    var pName = "hasBehaviour";

                    // If the category contains Responder or Generator
                    if (~widget.category.indexOf("Responder")) {
                        pName = "resBehaviour";
                    } else if (~widget.category.indexOf("Generator")) {
                        pName = "genBehaviour";
                    }
                    // Add widget behaviour predicate
                    tPredicates.push({ name: pName, args: tArgs });
                }

                componentTests.push({
                    state_name: pm.name,
                    widget_name: widget.name,
                    predicates: tPredicates
                });
            });

            if (!pm.components || !pm.components.forEach) {
                return componentTests;
            }

            pm.components.forEach(function(component) {
                componentTests.push.apply(componentTests, genTests(component));
            });

            return componentTests;
        }


        /**
         * Generate the all the tests for the states within a PM / PIM.
         * @param pm PM / PIM to get state tests for.
         * @returns {string} A string of all the states tests.
         */
        function stateTests(pm) {
            /**
             * Generate the tests for the states within a PM / PIM recursively.
             * @param pm PM / PIM to get state tests for.
             * @returns {Array} if PIM or {object} if PM of state test(s).
             */
            function stateTestsRec(pm) {
                /**
                 * Get the tests for a PM.
                 * @param pm PM to get states from.
                 * @returns object {
                 *		  state_name: (string),
                 *		  widget_name: (string),
                 *		  predicates: (array of {
                 *				  name: (string),
                 *				  args: (array of string)
                 *			  }) or null
                 *	  }
                 */
                function addState(pm) {
                    if (!pm || !pm.widgets || !pm.widgets.forEach ||
                        pm.widgets.length === 0) {
                        return null;
                    }

                    var tPredicates = [];
                    // Add widgets predicates
                    pm.widgets.forEach(function (widget) {
                        // Add to beginning (unshift)
                        tPredicates.unshift({ name: "" });
                        tPredicates.unshift({ name: "Active", args: [widget.name] });
                        tPredicates.unshift({ name: "Visible", args: [widget.name] });
                        tPredicates.unshift({ name: "hasWidget", args: [widget.name] });
                    });

                    return {
                        state_name: pm.name,
                        widget_name: pm.widgets[0].name,
                        predicates: tPredicates
                    };
                }

                if (!pm) {
                    return [];
                }

                if (!pm.components || pm.components.length === 0) {
                    return addState(pm);
                }

                var tests = [];
                pm.components.forEach(function (comp) {
                    tests.push(stateTestsRec(comp));
                });

                return tests;
            }

            /**
             * Transforms a state test into the correct format to display.
             * @param test Test to display.
             * @returns {string} The formatted state test.
             */
            function printStateTest(test) {

                if (!test || !test.predicates || !test.predicates.forEach) {
                    return "";
                }

                var testStr = "";
                testStr += "State(" + test.state_name + ") => ";
            //	testStr += "\n\t";
                test.predicates.forEach(function (p) {
                    // Split condition
                    if (p.name === "") {
                        // Replace with "" if want all on the same line
                        testStr += "\n\t";
                    } else {
                        testStr += p.name + "(" + p.args.join(", ") + ") " + "Λ" + " ";
                    }
                });
                // Remove last '/\ \n\t' -> \n
                testStr = testStr.substring(0, testStr.lastIndexOf("Λ")) + "\n";
                return testStr;
            }

            /**
             * Display the state tests.
             * @param tests Tests to display.
             * @returns {string} A string of all the state.
             * tests for the PM / PIM.
             */
            function printStateTests(tests) {
                if (!tests) {
                    return "Error: no tests. Generated from model.";
                }

                var testsStr = "";
                // If the tests are for a PIM, else for PM
                if (tests.forEach) {
                    tests.forEach(function (test) {
                        if (!test || !test.predicates) {
                            return;
                        }
                        testsStr += printStateTest(test);
                    });
                } else {
                    testsStr += printStateTest(tests);
                }

                return testsStr;
            }

            var tests = stateTestsRec(pm);

            return printStateTests(tests);
        }
        /**
         * Generate the all the tests for behaviours within a PM / PIM.
         * @param tests The generated tests.
         * @param pm PM / PIM to get behaviour tests for.
         * @returns string A string of all the behaviour tests.
         * TODO tidy function
         */
        function behaviourTests(tests, pm) {

            if (!tests || !tests.forEach || !pm) {
                return "Error: no behaviour tests from model";
            }

            var iBehav, sBehav = [], sRespBehav = [], noBehav = [];
            var UniqueList = function () {
                var uList = {};
                this.get = function () {
                    var arr = [];
                    for (var key in uList) {
                        if (uList.hasOwnProperty(key)) {
                            arr.push(JSON.parse(key));
                        }
                    }
                    return arr;
                };
                this.add = function (obj) {
                    uList[JSON.stringify(obj)] = null;
                };
            };
            var uniqueList = new UniqueList();

            // Split the Behaviour based tests into each type of behaviour
            tests.forEach(function (test) {
                if (!test.predicates || !test.predicates.forEach) {
                    return;
                }

                test.predicates.forEach(function (p) {
                    if(p.name === "hasBehaviour" || p.name === "genBehaviour") {
                        p.args.forEach(function (arg, i) {
                            // Skip over Widget name in the args
                            if (i === 0) {
                                return;
                            }
                            var sub = arg.substring(0, 2);
                            if (sub === "I_") {
                                // Is I-Behaviour
                                // If PIM find details in transitions
                                if (pm.pm) {
                                    pm.transitions.forEach(function (tran) {
                                        if (tran.i_behaviour === arg) {
                                            uniqueList.add({
                                                start_state: tran.start_state,
                                                // Set the widget name and the i_behaviour
                                                predicates: [p.args[0], arg],
                                                end_state: tran.end_state
                                            });
                                        }
                                    });
                                }
                            } else if (sub === "S_") {
                                // Is S-Behaviour
                                sBehav.push({
                                    state_name: test.state_name,
                                    interaction: test.widget_name,
                                    gen_behaviour: arg
                                });
                            }
                            // else incorrectly named PIM / PM behaviours
                        });
                    } else if (p.name === "resBehaviour") {
                        // Remove the Widget name from the args
                        p.args.splice(1, p.args.length - 1)
                            .forEach(function (arg) {
                            var sub = arg.substring(0, 2);
                            // Only system behaviours can be resBehaviours
                            if (sub === "S_") {
                                sRespBehav.push({
                                    state_name: test.state_name,
                                    widget_name: test.widget_name,
                                    res_behaviour: arg
                                });
                            }
                        });
                    }
                });

                // Has no behaviour
                if (test.predicates.length === 3) {
                    noBehav.push({
                        state_name: test.state_name,
                        widget_name: test.widget_name
                    });
                }
            });
            // Ensure the iBehaviour tests are unique.
            iBehav = uniqueList.get();

            /**
             * Display the i_behaviour tests.
             * @param tests Ii_behaviour tests to display.
             * @returns {string} A string of all the i_behaviour
             * tests for the PM / PIM.
             */
            function printIBehaviourTests(tests) {
                if (!tests || !tests.forEach) {
                    return "Error: no tests from I behaviours.";
                }

                var testsStr = "";
                tests.forEach(function(test) {
                    testsStr += "State(" + test.start_state + ") " + "Λ" + " ";
                    testsStr += "Interaction(" + test.predicates.join(", ") + ") ";
                    testsStr += "=> State(" + test.end_state + ")\n";
                });
                return testsStr;
            }

            /**
             * Display the S-Behaviour tests that are not responses.
             * @param tests S-Behaviour tests to display.
             * @returns {string} A string of all the S-Behaviour.
             * tests for the PM / PIM.
             */
            function printSBehaviourTests(tests) {
                if (!tests || !tests.forEach) {
                    return "Error: no tests from S behaviours.";
                }

                var testsStr = "";
                tests.forEach(function(test) {
                    testsStr += "State(" + test.state_name + ") " + "Λ" + " ";
                    testsStr += "Interaction(" + test.interaction + ") => ";
                    testsStr += "genBehaviour(" + test.gen_behaviour + ")\n";
                });
                return testsStr;
            }

            /**
             * Display the S-Behaviour tests that are responses.
             * @param tests S-Behaviour tests to display.
             * @returns {string} A string of all the S-Behaviour
             * tests for the PM / PIM.
             */
            function printSRespBehaviourTests(tests) {
                if (!tests || !tests.forEach) {
                    return "Error: no tests from S Resp. behaviours.";
                }

                var testsStr = "";
                tests.forEach(function(test) {
                    testsStr += "State(" + test.state_name + ") => ";
                    testsStr += "respBehaviour(" + test.widget_name + ", " + test.res_behaviour + ")\n";
                });
                return testsStr;
            }

            /**
             * Display the no behaviour tests.
             * @param tests no behaviour tests to display.
             * @returns {string} A string of all the no behaviour
             * tests for the PM / PIM.
             */
            function printNoBehaviourTests(tests) {
                if (!tests || !tests.forEach) {
                    return "Error: no tests from no behaviours.";
                }

                var testsStr = "";
                tests.forEach(function(test) {
                    testsStr += "State(" + test.state_name + ") " + "Λ" + " ";
                    testsStr += "Interaction(" + test.widget_name + ") => ";
                    testsStr += "genBehaviour(NONE) " + "Λ" + " respBehaviour(NONE)\n";
                });
                return testsStr;
            }

            var behaviourTestsStr = [];
            behaviourTestsStr.push(printIBehaviourTests(iBehav));
            behaviourTestsStr.push(printSBehaviourTests(sBehav));
            behaviourTestsStr.push(printSRespBehaviourTests(sRespBehav));
            behaviourTestsStr.push(printNoBehaviourTests(noBehav));
            // Only put a newline after types of tests that actually have tests in them
            return behaviourTestsStr.filter(function(s){ return s; }).join("\n");
        }

        // add test generation
        var tests = "";
        // Get test predicates for the PIM / PM
        var _tests = genTests(pm);
        // Print state tests.
        tests += stateTests(pm.pm || pm) + "\n";
        // Print behaviour tests.
        tests += behaviourTests(_tests, pm);

        return tests;
    };

    /**
     * Returns a string of the Emuchart description tests were generated for.
     * @memberof PIMTestGenerator
     */
    PIMTestGenerator.prototype.printDescriptor = function (name, pm) {
        var ans = "# ---------------------------------------------------------------\n";
        ans += "#  Tests for "+ name +": " + pm.name;
        ans += "\n# ---------------------------------------------------------------\n";
        return ans;
    };

    /**
     * Returns a string of the disclaimer for using this tool.
     * @memberof PIMTestGenerator
     */
    PIMTestGenerator.prototype.printDisclaimer = function () {
        return "\n# ---------------------------------------------------------------\n" +
            "#  Tests generated using PVSio-web PIM Test Generator ver 0.1\n" +
            "#  Tool freely available at http://www.pvsioweb.org" +
            "\n# ---------------------------------------------------------------\n";
    };

    /**
     * Returns a string of abstract tests for the Emuchart to be saved in a file.
     * @memberof PIMTestGenerator
     */
    PIMTestGenerator.prototype.print = function (name, models) {
        var _this = this;

        // Print PIM tests
        var ans = "Tests for Presentation Interaction Models:\n";
        if (!models.pims || models.pims.length === 0 || !models.pims[0].isPIM) {
            ans += "Unable to find any PIMs.\n";
        } else {
            // Foreach pim generate tests
            models.pims.forEach(function (pim) {
                ans += _this.printDescriptor("PIM", pim) + "\n";
                ans += _this.generateTests(pim) + "\n";
            });
        }

        if (models.pms && models.pms.length > 0) {
            // Print PM tests
            ans += "\nTests for Presentation Models:\n";
            models.pms.forEach(function (pm) {
                // Only test the PMs
                ans += _this.printDescriptor("PM", pm) + "\n";
                ans += _this.generateTests(pm) + "\n";
            });
        }

        ans += _this.printDisclaimer() + "\n";

        // Print tests to console (useful if can't get access to saved test file?).
        console.log(ans);

        return { file_name: name + ".tests", res: ans };
    };

    module.exports = PIMTestGenerator;
});
