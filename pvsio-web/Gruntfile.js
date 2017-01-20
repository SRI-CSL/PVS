/**
 *
 * @author Patrick Oladimeji
 * @date 7/15/14 17:40:45 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global module */
module.exports = function (grunt) {
    "use strict";

    grunt.initConfig({
        pkg: grunt.file.readJSON("package.json"),
        uglify: {
            options: {
                banner: '/*!<%= pkg.name %><%= grunt.template.today("yyyy-mm-dd")%>*/\n'
            },
            build: {
                src: 'src/client/index.js',
                dest: 'dist/<%=pkg.name%>.min.js'
            }
        },
        requirejs: {
            compile: {
                options: {
                    baseUrl: "src/client/app",
                    mainConfigFile: "src/client/index.js",
                    out: "dist/<%=pkg.name%>.min.js",
                    name: "../index",
                    optimize: "none"
                }
            }
        },
        copy: {
            main: {
                files: [
                    {expand: true, cwd: "src/client", src: "**/*", dest: "build"}
                ]
            }
        },
        jshint: {
            all: ["src/client/app/**/*.js", "src/client/tests/**/*.js", "src/server/**/*.js", '!src/server/lib/**/*.js', "!src/client/tests/lib/**/*.js"],
            options: {
                jshintrc: true // search for .jshintrc files relative to the files being linted
            }
        },
        sass: {
            options: {
                sourceMap: true
            },
            dist: {
                files: {
                    "src/client/css/style.css": "src/client/css/scss/style.scss"
                }
            }
        },
        postcss: {
            options: {
                map: {
                    inline: false // save sourcemaps as separate files
                },

                processors: [
                    require('autoprefixer')({browsers: 'last 4 versions'}), // add vendor prefixes
                ]
            },
            dist: {
                src: 'src/client/css/style.css'
            }
        },
        watch: {
            source: {
                files: ["src/client/css/scss/**/*.scss", "src/client/css/scss/**/*.css"],
                tasks: ["sass", "postcss"]
            }
        },
        run: {
            options: {
                    wait: true
                },
                server: {
                    cmd: "./start.sh"
                }
        },
        concurrent: {
            dev: ['run:server', 'watch'],
            options: {
                logConcurrentOutput: true
            }
        }
    });

    grunt.loadNpmTasks("grunt-contrib-uglify");
    grunt.loadNpmTasks("grunt-contrib-requirejs");
    grunt.loadNpmTasks("grunt-contrib-copy");
    grunt.loadNpmTasks("grunt-contrib-jshint");
    grunt.loadNpmTasks("grunt-contrib-watch");
    grunt.loadNpmTasks("grunt-concurrent");
    grunt.loadNpmTasks("grunt-sass");
    grunt.loadNpmTasks("grunt-run");
    grunt.loadNpmTasks('grunt-postcss');
    grunt.registerTask("test", ["jshint:all"]);
    grunt.registerTask("default", ["test"]);
    grunt.registerTask("dev", ["sass", "postcss", "concurrent:dev"]);
};
