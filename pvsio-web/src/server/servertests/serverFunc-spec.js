/**
 * test suite for testing server functions
 * @author Patrick Oladimeji
 * @date 6/26/14 11:55:50 AM
 */
/*jshint unused: false*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global require, expect, describe, it, __dirname*/

var serverFuncs = require("../serverFunctions"),
    pvsio = require("../pvsprocess"),
    p = pvsio();
var path = require("path"),
    fs = require("fs"),
    exec = require("child_process").exec;

var writeFile = serverFuncs.writeFile,
    stat = serverFuncs.stat,
    //getFolderStructure = serverFuncs.getFolderStructure,
    mkdirRecursive	= serverFuncs.mkdirRecursive,
    createProject = serverFuncs.createProject,
    openProject = serverFuncs.openProject,
    listProjects = serverFuncs.listProjects;

var testid = new Date().getTime(),
    errLog = function (err) { "use strict"; console.log(err); };
describe("sever functions for manipulating file system and projects", function () {
    "use strict";
    it("Testing stat function :this file exists and it is a file", function () {
        stat(path.join(__dirname, "serverFunc-spec.js"))
            .then(function (f) {
                expect(f).toBeDefined();
                expect(f.isDirectory()).toEqual(false);
                expect(f.isFile()).toEqual(true);
                expect(typeof f.isBlockDevice).toEqual("function");
                expect(typeof f.isCharacterDevice).toEqual("function");
                expect(typeof f.isFIFO).toEqual("function");
                expect(typeof f.isSocket).toEqual("function");
            }, errLog);
    });
    
    it("write file works", function (done) {
        writeFile(path.join(__dirname, "test.txt"), "hello")
            .then(function (res) {
                stat(res.path).then(function (f) {
                    expect(f).toBeDefined();
                    expect(f.isFile()).toEqual(true);
                    fs.unlink(res.path, function () {
                        done();
                    });
                }, errLog);
            }, errLog);
    });
    it("mkdirRecursive function works", function (done) {
        var baseDir = path.join(__dirname, "__" + new Date().getTime() + "_folder");
        var testDir = path.join(baseDir, "foo", "bar", "bar");
        mkdirRecursive(testDir, function (err) {
            if (err) {console.log(err); }
            stat(testDir).then(function (f) {
                expect(f).toBeDefined();
                expect(f.isDirectory()).toEqual(true);
                exec("rm -r " + baseDir, function (err) {
                    if (err) { console.log(err); }
                    done();
                });
            });
        });
    });
    
    it("writing a file with specified nested folders (e.g., goo/foo/test.txt works", function (done) {
        writeFile(path.join(__dirname, "__testingfolder_one/two/three/test.txt"), "hello")
            .then(function (res) {
                stat(res.path).then(function (f) {
                    expect(f).toBeDefined();
                    expect(f.isFile()).toEqual(true);
                    exec("rm -r " + path.join(__dirname, "__testingfolder_one"), function (err) {
                        if (err) { console.log(err); }
                        done();
                    });
                }, errLog);
            }, errLog);
    });
    
    it("listProjects() returns a list of projects in public/projects directory, each having a name property", function (done) {
        listProjects().then(function (projects) {
            expect(projects.length).toBeGreaterThan(0);
            expect(projects.every(function (p) {
                return p.name;
            })).toBeTruthy();
            done();
        });
    });
    
    it("openProject contains the right information", function (done) {
        openProject("AlarisGP").then(function (project) {
            expect(project).toBeDefined();
            expect(project.name).toEqual("AlarisGP");
            expect(project.descriptors).toBeDefined();
            expect(project.descriptors.length > 0).toEqual(true);
            done();
        });
    });
    
    it("deleting files work", function (done) {
        var dir = path.join(__dirname, "__testingfolder_one");
        mkdirRecursive(dir, function (err) {
            if (err) {
                expect(err).toBeFalsy();
                done();
            } else {
                p.removeFile(dir, function (err) {
                    if (err) {
                        expect(err).toBeFalsy();
                        done();
                    } else {
                        stat(dir).then(function (f) {
                            expect(f).toBeUndefined();
                            done();
                        }).catch(function (err) {
                            expect(err.errno).toEqual(34);
                            done();
                        });
                    }
                });
            }
        });
    });
});
