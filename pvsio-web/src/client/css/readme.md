This folder contains the CSS for the project. The file `style.css` is compiled from SCSS source files (via the Grunt tasks `sass` and/or `dev`). Any style changes should be made in the appropriate file within the `scss/` directory before recompiling the CSS using the Grunt task. The compilation step also automatically adds browser prefixes to rules when needed.

## SCSS Structure
The scss/ directory contains a number of sub-folders:

```
├── scss
│   ├── components		# Actual styling. Each file should represent some component of the UI - a button, the layout of a section, etc.
│   │   ├── _all.scss	# Any new components should be imported in this file
│   │   └── ...
│   ├── helpers			# Global variables, general mixins, functions, etc.
│   │   ├── _variables.scss
│   │   └── ...
│   ├── legacy			# Contains CSS files that have not yet been converted to SCSS
│   │   └── ...
│   ├── style.scss		# The main SCSS file, which imports all other files
│   └── vendor			# (S)CSS from external libraries
        └── ...
```

For custom-made demos, including the entire `style.css` file may not be necessary - in these situations creating a `.scss` file that just imports the required modules (and linking the compiled version from the demo's HTML) may be a better option.
