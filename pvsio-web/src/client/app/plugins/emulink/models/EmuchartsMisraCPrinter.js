/**
 * @author Gioacchino Mauro
 * @date Mer 11 Mag 2016 10:50:09 CEST
 *
 * MISRA C code printer for emucharts models.
 * Emuchart objects have the following structure:
      emuchart = {
                name: (string),
                author: {
                    name: (string),
                    affiliation: (string),
                    contact: (string)
                },
                importings: (array of including and defining pre-processing directives, according to types of variable used),
                constants: (array of {
                                name: (string), // the constant identifier
                                type: (string), // the constant type
                                value: (string) // the constant value (can be undefined)
                            }),
                variables: (array of {
                                name: (string), // the variable identifier
                                type: (string), // the variable type
                                scope: (string) // the variable scope, either local or input or output
                            }),
                states: (array of {
                                name: (string), // the state label
                                id: (string),   // a unique identifier
                            }),
                transitions: (array of {
                                name: (string), // the transition label (it includes trigger name, condition and actions)
                                id: (string),   // a unique identifier
                                source: {
                                    name: (string), // the source state label
                                    id: (string)    // a unique identifier
                                },
                                target: {
                                    name: (string), // the target state label
                                    id: (string)    // a unique identifier
                                },
                                listSources: (array of 
                                    :(string)       // sources state label list in case of homonymous transitions with different source
                                                    // (this array is present only into the first object of the array with the same trigger name)
                                ),
                                listTargets: (array of 
                                    :(string)       // target state label list in case of homonymous transitions with different target
                                                    // (this array is present only into the first object of the array with the same trigger name)
                                ),
                            }),
                initial_transitions: (array of {
                                name: (string),         // the initial transition label
                                id: (string),           // a unique identifier
                                condition: (string),    // It is an optional, in case of multiple initial transitions
                                actions: (string)       // It is an optional, in case of different expression, instead of initializations
                                target: {
                                    name: (string), // the target state label
                                    id: (string)    // a unique identifier
                                },
                            })
      }
 */
define(function (require, exports, module) {
    var makefileTemplate = require("text!plugins/emulink/models/misraC/templates/makefile.handlebars");
    var threadTemplate = require("text!plugins/emulink/models/misraC/templates/thread.handlebars");
    var headerTemplate = require("text!plugins/emulink/models/misraC/templates/header.handlebars");
    var mainTemplate = require("text!plugins/emulink/models/misraC/templates/main.handlebars");
    var doxygenTemplate = require("text!plugins/emulink/models/misraC/templates/doxygen.handlebars");
    var Android_threadTemplate = require("text!plugins/emulink/models/misraC/templates/threadAndroid.handlebars");
    var Android_headerTemplate = require("text!plugins/emulink/models/misraC/templates/headerAndroid.handlebars");
    var EmuchartsParser = require("plugins/emulink/EmuchartsParser");
    var displayNotificationView  = require("plugins/emulink/forms/displayNotificationView");
    var _parser = new EmuchartsParser();
    
    var displayNotification = function (msg, title) {
        title = title || "Notification";
        displayNotificationView.create({
            header: title,
            message: msg,
            buttons: ["Ok"]
        }).on("ok", function (e, view) {
            view.remove();
        });
    };
    var displayError = function (msg) {
        displayNotification(msg, "Compilation Error");
    };

    var machineStateType = "MachineState";
    var initialMachineState = "initialMachineState";
    var predefined_variables = {
        previous_state: { name: "previous_state", type: machineStateType, value: initialMachineState },
        current_state: { name: "current_state", type: machineStateType, value: initialMachineState }
    };
    var declarations = [];
    var string_length = 0;  //MAX stringh length for strings variables
    
    var operatorOverrides = {
        ":="    : "=",
        "AND"   : "&&",
        "OR"    : "||",
        "NOT"   : "!",
        "MOD"   : "fmod",
        "and"   : "&&",
        "or"    : "||",
        "mod"   : "fmod",
        "not"   : "!",
        "="     : "=="
    };
    
    var typeMaps = {
        "Time"  : "Time",    //Iachino: Serve??
        "bool"  : "UC_8",
        "char"  : "UC_8",
        "string": "C_8",
        "int"   : "UI_32",
        "Sint"  : "SI_32",
        "float" : "F_32",
        "double": "D_64"                      
    };
    
    var Ints = ["int", "unsigned int", "long", "unsigned long"];
    
    /**
     * Specific-length equivalents should be typedefd for the specific compile, with respect to MISRA 1998 rule (Rule 13, advisory)
     */   
    function getType(type) {
        if ((type.toLowerCase() === "bool") || (type.toLowerCase() === "boolean")) {
            type = typeMaps.bool;
            // if(!isInArray(declarations, "true")){  //Iachino: remove in case of decision to declare always boolean typedef
            //     declarations.push("#define true 1");
            //     declarations.push("#define false 0");
            //     declarations.push("#define TRUE 1");
            //     declarations.push("#define FALSE 0");
            //     declarations.push("typedef unsigned char " + type + ";");
            // }    
        } else if ((type.toLowerCase() === "char") || (type.toLowerCase() === "unsigned char")) {
            //The type char shall always be declared as unsigned char or signed char
            //see MISRA 1998 rules (Rule 14, required)
            type = typeMaps.char;
            //if(!isInArray(declarations, type)){
            //    declarations.push("typedef unsigned char " + type + ";");
            //}
        } else if (Ints.indexOf(type.toLowerCase()) >= 0) {
            type = typeMaps.int;
            if(!isInArray(declarations, type)){
                declarations.push("typedef unsigned int " + type + ";");
            }
        } else if ((type.toLowerCase() === "signed int") || (type.toLowerCase() === "signed long")) {
            type = typeMaps.Sint;
            if(!isInArray(declarations, type)){
                declarations.push("typedef signed int " + type + ";");
            }
        } else if (type.toLowerCase() === "float") {
            type = typeMaps.float;
            if(!isInArray(declarations, type)){
                declarations.push("typedef float " + type + ";");
            }
        } else if ((type.toLowerCase() === "real") || (type.toLowerCase() === "double") ||
                   (type.toLowerCase() === "itimes") || (type.toLowerCase() === "pausetime") || (type.toLowerCase() === "irates") || (type.toLowerCase() === "ivols")   //ONLY FOR TESTING with Alaris GP model 
                    ) {
            type = typeMaps.double;
            if(!isInArray(declarations, type)){
                declarations.push("typedef double " + type + ";");
            }
        } else if (type.toLowerCase() === "string") {
            type = typeMaps.string;
            if(!isInArray(declarations, type)){
                declarations.push("#include <string.h>");
                declarations.push("typedef char " + type + ";");
            }
        }
        return typeMaps[type] || type;
    }
    
    /**
     * Set a number with the properly value's suffix, useful for parsing declaration's variable, with respect to MISRA 1998 rule (Rule 18, advisory)
     * Parameter is current value
     */
    function setSuffix(v) {
        if (isNumber(v.value)) {
            if ((v.type.toUpperCase() === typeMaps.int) || (v.type.toUpperCase() === typeMaps.Sint)) {
                v.value = v.value + "u";
            } else if ((v.type.toUpperCase() === typeMaps.float) || (v.type.toUpperCase() === typeMaps.double)) {
                if (v.value.indexOf(".") === -1) {
                    v.value = v.value + ".0f";
                } else {
                    v.value = v.value + "f";
                }
            }
        }
        return v.value;
    }
    
    /**
     * Set a number with the properly value's suffix, useful for parsing actions's transations, with respect to MISRA 1998 rule (Rule 18, advisory)
     * Parameters are variable definitions, current value to analize and emucharts structure
     */
    function setSuffixInActions(variable, val, emuchart) {
        emuchart.variables.local.map(function(z) {
            if(variable.val.identifier.val === z.name) {
                if ((z.type === typeMaps.int) || (z.type === typeMaps.Sint)) {
                    val += "u";
                } else if ((z.type === typeMaps.float) || (z.type === typeMaps.double)) {
                    if (val.indexOf(".") === -1){
                        val += ".0f";
                    } else {
                        val += "f";
                    }
                }
            } 
        });
        return val;
    }
    
    /**
     * Change operator sintax from Emucharts to C code
     */
    function getOperator(op, emuchart) {
        return operatorOverrides[op] || op;
    }
    
    /**
     * Checks if a value is in an array
     * Returns a boolean
     */
    function isInArray(array, search) {
        var arrayJoin = array.join();
        return arrayJoin.indexOf(search) >= 0;
    }
    
    /**
     * Checks if a value is a float or a finite number
     * Returns a boolean
     */
    function isNumber(n) {
        return !isNaN(parseFloat(n)) && isFinite(n);
    }
        
    /**
     * Checks if a value is a local variable in the emuchart structure
     * Returns a boolean
     */
    function isLocalVariable(name, emuchart) {
        if (name === predefined_variables.current_state.name ||
                name === predefined_variables.previous_state.name) {
            return true;
        }
        if (emuchart.variables && emuchart.variables.local) {
            var i = 0;
            for (i = 0; i < emuchart.variables.local.length; i++) {
                if (name === emuchart.variables.local[i].name) {
                    return true;
                }
            }
        }
        return false;
    }
    // function isInputVariable(name, emuchart) {
    //     if (emuchart.variables && emuchart.variables.input) {
    //         var i = 0;
    //         for (i = 0; i < emuchart.variables.input.length; i++) {
    //             if (name === emuchart.variables.input[i].name) {
    //                 return true;
    //             }
    //         }
    //     }
    //     return false;
    // }
    // function isOutputVariable(name, emuchart) {
    //     if (emuchart.variables && emuchart.variables.output) {
    //         var i = 0;
    //         for (i = 0; i < emuchart.variables.output.length; i++) {
    //             if (name === emuchart.variables.output[i].name) {
    //                 return true;
    //             }
    //         }
    //     }
    //     return false;
    // }
    
    /**
     * Checks if a value is a constant in the emuchart structure
     * Returns a boolean
     */
    function isConstant(name, emuchart) {
        if (emuchart.constants) {
            var i = 0;
            for (i = 0; i < emuchart.constants.length; i++) {
                if (name === emuchart.constants[i].name) {
                    return true;
                }
            }
        }
        return false;
    }
    
    /**
     * Splice method for strings
     * Returns the new string
     */
    function spliceSlice(str, index, count, add) {
        return str.slice(0, index) + (add || "") + str.slice(index + count);
    }
    
    function parseTransition(t, emuchart) {
        function getExpression(expression, emuchart) {
            var complexActions = ["expression", "assignment", "function"];
            if (expression === undefined || expression === null) {
                return "";
            }
            if (expression.type === 'modop') {
                //handling modulo operator in condition forms, it's valid only for integer values
                expression.val = '%';
            }
            /** 
             * Managing modulo operator in expression forms:
             * 1. check if type is 'modop' (and include math.h library so we can use fmod())
             * 2. move 'fmod' string back to the last parenthesis of the expression and introduce a comma to separate two parameters of fmod function
             * 3. right parenthesis (rpar) start with one unit more than left parenthesis (lpar), the algorithm stops when they are equals.
             */
            if (Array.isArray(expression.val)){
                var i, j;
                for (i = 0; i < expression.val.length; i++) {
                    if (expression.val[i].type === 'modop') {
                        if(!isInArray(declarations, "#include <math.h>")) {
                            declarations.push("#include <math.h>");
                        }
                        var lpar = 0;
                        var rpar = 1;
                        for ( j = i; j >= 0; j--) {
                            if (expression.val[j].val === '(') { lpar++; }
                            if (expression.val[j].val === ')') { rpar++; }
                            if (lpar === rpar) {
                                var tmp = {};         // TODO: upgrade the emucharts parser, so we have this builtin operator
                                tmp.type = "builtin";
                                tmp.val = ",";
                                expression.val.splice(j, 0, expression.val[i]);     //moving 'fmod' back to the last parenthesis
                                expression.val.splice(i+1, 1, tmp);                 //adding new comma
                                break;
                            }
                        }
                    }
                }
            }
            if (expression.type === "assignment") {
                var name = expression.val.identifier.val;
                var isstring = false;
                expression.val.expression.val.map(function (v) {
                    if (v.type === "string") {
                        // different treatment for strings assignemnts
                        string_length = Math.max(string_length,v.val.length - 2);
                        v.val =  "strcpy(st->" + name + ", " + v.val + ")";
                        isstring = true;
                    }
                    return string_length;
                });

                expression.val.expression.val.map(function (v) {
                    if (v.type === "identifier"){
                        if (isLocalVariable(v.val, emuchart)) {
                            v.val = "st->" + v.val;
                        } else if (!isConstant(v.val, emuchart)) {
                            //same treatment of LocalVariables, left the prototype intentionally in case of different choice
                            v.val = "st->"+ v.val;
                        }
                    }
                    return;
                });
                if (isLocalVariable(name, emuchart)) {
                    if (isstring) {
                        // In strings case we use strcpy, not a regular assignment
                        return getExpression(expression.val.expression, emuchart);
                    } else {
                        return "st->" + name + " = " +
                            getExpression(expression.val.expression, emuchart);
                    }                
                }
                //same treatment of LocalVariables, left the prototype intentionally in case of different choice
                if (isstring) {
                    return getExpression(expression.val.expression, emuchart);
                } else {
                    return "st->" + name + " = " +
                        getExpression(expression.val.expression, emuchart);
                }  
            } else {
                if (expression.type === 'identifier'){
                    if(isLocalVariable(expression.val, emuchart)) {
                            expression.val = "st->" + expression.val;
                        } else if (!isConstant(expression.val, emuchart)){
                            //same treatment of LocalVariables, left the prototype intentionally in case of different choice
                            expression.val = "st->"+ expression.val;
                        }
                }
                if (Array.isArray(expression.val)) {
                    var res = expression.val.map(function (token) {
                        if (complexActions.indexOf(token.val) > -1) {
                            return getExpression(token.val, emuchart);
                        } else {
                            return getOperator(token.val, emuchart);
                        }
                    });
                    return res.join(" ");
                } else {
                    if (complexActions.indexOf(expression.val) > -1) {
                        return getExpression(expression.val, emuchart);
                    } else {
                        return getOperator(expression.val, emuchart);
                    }
                }
            }
        }        
        var name = t.name;
        var functionBody = _parser.parseTransition(name);
        if (functionBody.res) {
            functionBody = functionBody.res.val;
            var id = functionBody.identifier;
            var condition = functionBody.cond;
            var actions = functionBody.actions;
            if (condition) {
                condition = condition.val.map(function (token) {
                    return getExpression(token, emuchart);
                }).join(" ");
                /** 
                 * Handling logical operators, according to MISRA C rule 34 (The operands of a logical && or || shall be primary expressions)
                 * Adding ') ' and ' (' before and after logical operators
                 * With a flag it checks if it's the first logical operator, in order to add '( ' and ' )' as well to the beginning and to the end of the string
                 **/
                var i;
                var firstTime = true;
                for (i = 0; i < condition.length; i++){
                    if ((condition[i] === '&' ) || (condition[i] === '|')){
                        condition = spliceSlice(condition, i, 0, ') ');
                        condition = spliceSlice(condition, i+4, 0, ' (');
                        i += 4;
                        if (firstTime){
                            condition = spliceSlice(condition, 0, 0, '( ');
                            condition = spliceSlice(condition, condition.length, 0, ' )');
                            i += 4;
                            firstTime = false;
                        }
                    }
                }
            }
            if (actions) {
                actions = actions.val.map(function (a) {
                    a.val.expression.val.map(function(b){
                        if(b.type === "number"){
                            b.val = setSuffixInActions(a, b.val, emuchart);                         
                        }
                    });
                    return getExpression(a, emuchart);
                });
            }
            return {id: id.val, actions: actions, condition: condition, source: t.source, target: t.target };
        } else if (functionBody.err) {
            displayError(functionBody.err);
            return { erroneousLabel: name, parserError: functionBody.err };
        }
    }
    
    function Printer(name) {
        this.modelName = name;
        this.model = {modelName: name, transitions: []};
    }

    Printer.prototype.constructor = Printer;

    Printer.prototype.print_variables = function (emuchart) {
        if (emuchart.variables) {
            this.model.input_variables = emuchart.variables.input.map(function (v) {
                if (v.type.toLowerCase() === "char") {
                    v.value = "\"" + v.value + "\"";
                    v.type = getType(v.type);
                    return v;
                }
                v.type = getType(v.type);
                v.value = setSuffix(v);
                return v;
            });
            this.model.output_variables = emuchart.variables.output.map(function (v) {
                if (v.type.toLowerCase() === "char") {
                    v.value = "\"" + v.value + "\"";
                    v.type = getType(v.type);
                    return v;
                }
                v.type = getType(v.type);
                v.value = setSuffix(v);
                return v;
            });
            this.model.local_variables = emuchart.variables.local.map(function (v) {
                if (v.type.toLowerCase() === "char") {
                    v.value = "\"" + v.value + "\"";
                    v.type = getType(v.type);
                    return v;
                }
                if (v.type.toLowerCase() === "string") {
                    string_length = Math.max(string_length,v.value.length);
                    v.value =  "strcpy(st->" + v.name + ", \"" + v.value + "\")";
                    v.isstring = true;
                    v.type = getType(v.type);
                    return v;
                }
                v.type = getType(v.type);
                v.value = setSuffix(v);
                return v;
            });
        }
    };

    Printer.prototype.print_constants = function (emuchart) {
        this.model.constants = emuchart.constants.map(function (v) {
            if (v.type.toLowerCase() === "char") {
                v.value = "\"" + v.value + "\"";
                v.type = getType(v.type);
                return v;
            }
            else{
                v.type = getType(v.type);
                v.value = setSuffix(v);
            }
            return v;
        });
    };

    Printer.prototype.print_declarations = function (emuchart) {
        /* Adding initial declarations, always useful to manage boolean variables */
        declarations.push("typedef unsigned char " + typeMaps.bool + ";");
        declarations.push("#define true 1");
        declarations.push("#define false 0");
        declarations.push("#define TRUE 1");
        declarations.push("#define FALSE 0");
        if (string_length > 0){
            declarations.push("#define STRING_LENGTH " + string_length);
        }
        this.model.importings = declarations;
        if (emuchart.variables) {
            this.model.structureVar = emuchart.variables.local.map(function (v) {
                v.type = getType(v.type);
                if (v.isstring === true) { 
                    return (v.type + " "+ v.name + "[STRING_LENGTH];");
                }
                return (v.type + " "+ v.name + ";");
            });
        }
        this.model.structureVar.push(machineStateType + " " + predefined_variables.current_state.name + ";  //  Predefined variable for current state.");
        this.model.structureVar.push(machineStateType + " " + predefined_variables.previous_state.name + ";  //  Predefined variable for previous state.");
    };
    
    Printer.prototype.print_transitions = function (emuchart) {
        var transitions = [];
        var functionsName = [];
        emuchart.transitions.forEach(function (t) {
            if (t.name === "") { t.name = "tick"; }
            var parsedTransition  = parseTransition(t, emuchart);
            if (parsedTransition) {
                 if(!isInArray(functionsName, parsedTransition.id)){
                     //first insertion of a new transition  
                     functionsName.push(parsedTransition.id);
                     parsedTransition.listSources = [];
                     parsedTransition.listSources.push(parsedTransition.source.name);
                     parsedTransition.listTargets = [];
                     parsedTransition.listTargets.push(parsedTransition.target.name);
                     transitions.push(parsedTransition);
                 } else {
                     //transition name is already in the list                  
                     var i, tmp;
                     for (i = 0; i < transitions.length; i++) {
                         if (transitions[i].id !== 'undefined') {
                            if (transitions[i].id === parsedTransition.id) {
                                //transitions with the same name are now in an unique list
                                tmp = [];
                                tmp.push(transitions[i]);
                                tmp.push(parsedTransition);
                                transitions[i] = tmp;
                            } else {
                                var j;
                                for (j = 0; j < transitions[i].length; j++) {
                                    //from third case on, scroll through the proper list and push the new transition 
                                    if (transitions[i][j].id === parsedTransition.id) {
                                        tmp = [];
                                        var k;
                                        var v = transitions[i];
                                        for (k = 0; k < v.length; k++) {
                                            tmp.push(v[k]);
                                        }
//                                        transitions[i].map(function (v) {
//                                            tmp.push(v);
//                                            return;
//                                        });
                                        tmp.push(parsedTransition);
                                        transitions[i] = tmp;
                                        break;
                                    }
                                }
                            }
                            
                            //adding current source and target into the proper list, the lists (listSources and listTargest) are in the first element of the proper transition array (transitions[i][0]) 
                            if(!transitions[i].listSources){
                                //control in transitions list
                                if(!isInArray(transitions[i][0].listSources, parsedTransition.source.name) &&
                                   (transitions[i][0].id === parsedTransition.id)) {
                                    //checks if there are different sources in transitions
                                    transitions[i][0].listSources.push(parsedTransition.source.name);
                                }
                            } else {
                                //control in transitions innested list
                                if(!isInArray(transitions[i].listSources, parsedTransition.source.name) &&
                                   (transitions[i].id === parsedTransition.id)){
                                    //checks if there are different sources in transitions
                                    transitions[i].listSources.push(parsedTransition.source.name);
                                }
                            }
                            
                            if(!transitions[i].listTargets) {
                                //control in transitions list
                                if(!isInArray(transitions[i][0].listTargets, parsedTransition.target.name) &&
                                   (transitions[i][0].id === parsedTransition.id)){
                                    //checks if there are different targets in transitions
                                    transitions[i][0].listTargets.push(parsedTransition.target.name);
                                }
                            } else {
                                //control in transitions in nested list
                                if(!isInArray(transitions[i].listTargets, parsedTransition.target.name) &&
                                   (transitions[i].id === parsedTransition.id)){
                                    //checks if there are different targets in transitions
                                    transitions[i].listTargets.push(parsedTransition.target.name);
                                }
                            }
                         }
                     }
                 }
            }
        });
        if (transitions) {
            this.model.transitions = this.model.transitions.concat(transitions);
        }
    };
    
    Printer.prototype.print_initial_transition = function (emuchart) {
        var initial_transitions = emuchart.initial_transitions,
            transitions = [];
        initial_transitions.forEach(function (t) {
            var parsedInit = parseTransition(t, emuchart);
            if (parsedInit) {
                transitions.push(parsedInit);
            }
        });
        if (transitions) {
            this.model.initial_transitions = transitions;
        }
    };
    

    Printer.prototype.print_types = function (emuchart) {
    };

    Printer.prototype.print_states = function (emuchart) {
        this.model.states = emuchart.states;
    };
    
    Printer.prototype.print_descriptor = function (emuchart) {
        this.model.descriptor = 
            "/**---------------------------------------------------------------" +
            "\n*   Model: " + emuchart.name;
        Handlebars.registerHelper('filename', function() {
                return emuchart.name;
        });
        if (emuchart.author) {
            this.model.descriptor += 
                "\n*   Author: " + emuchart.author.name +
                "\n*           " + emuchart.author.affiliation +
                "\n*           " + emuchart.author.contact;
        }
        if (emuchart.description) {
            this.model.descriptor += 
                "\n*  ---------------------------------------------------------------" +
                "\n*   " + emuchart.description;
        }
        this.model.descriptor += 
            "\n*  ---------------------------------------------------------------*/\n";
        this.model.makefile_descriptor = this.model.descriptor.replace(/\*|\//g,'#');
    };
    
    Printer.prototype.print_disclaimer = function (emuchart) {
        this.model.disclaimer = "\n/** ---------------------------------------------------------------\n" +
                    "*   C code generated using PVSio-web MisraCPrinter ver 1.0\n" +
                    "*   Tool freely available at http://www.pvsioweb.org" +
                    "\n*  --------------------------------------------------------------*/\n";
        this.model.makefile_disclaimer = this.model.disclaimer.replace(/\*|\//g,'#');
        this.model.makefile_disclaimer = this.model.makefile_disclaimer.replace(/C code/g, "Makefile");
    };

    Printer.prototype.print = function (emuchart) {
        this.model.transitions = [];
        this.print_variables(emuchart);
        this.print_constants(emuchart);
        this.print_transitions(emuchart);
        this.print_initial_transition(emuchart);
        this.print_states(emuchart);
        this.print_declarations(emuchart);
        this.print_disclaimer(emuchart);
        this.print_descriptor(emuchart);
        
        console.log(this.model);//Iachino: TO debug
        
        var makefile = Handlebars.compile(makefileTemplate)(this.model);
        var thread = Handlebars.compile(threadTemplate)(this.model);
        var header = Handlebars.compile(headerTemplate)(this.model);
        var main = Handlebars.compile(mainTemplate)(this.model);
        var doxygen = Handlebars.compile(doxygenTemplate)(this.model);
        var Android_thread = Handlebars.compile(Android_threadTemplate)(this.model);
        var Android_header = Handlebars.compile(Android_headerTemplate)(this.model);
        declarations = [];
        return {makefile: makefile, thread: thread, header: header, main: main, doxygen: doxygen, Android_thread: Android_thread, Android_header: Android_header};
    };

    module.exports = Printer;
});
