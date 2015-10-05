/**
 * Javascript module for laying out and creating user resizable content regions in html page
 * @author Patrick Oladimeji
 * @date 9/1/14 14:01:12 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, require, brackets, document, window, Event*/
(function () {
	"use strict";
	var prefix = "ljs-",
		right = prefix + "right",
		left = prefix + "left",
		center = prefix + "center",
		bottom = prefix + "bottom",
		top = prefix + "top";
	var alignments = [right, left, top, bottom, center];

	function hasClass(el, className) {
		if (el.classList) {
			return el.classList.contains(className);
		} else {
			return new RegExp('(^| )' + className + '( |$)', 'gi').test(el.className);
		}
	}
	
	function addClass(el, className) {
		if (el.classList) {
			el.classList.add(className);
		} else {
			el.className += ' ' + className;
		}
	}
	
	function removeClass(el, className) {
		if (el.classList) {
			el.classList.remove(className);
		} else {
			el.className = el.className.replace(new RegExp('(^|\\b)' + className.split(' ').join('|') + '(\\b|$)', 'gi'), ' ');
		}
	}
	
	function ensureMin(size) {
		return size < 6 ? 6 : size;
	}
	
	function insertResizer(el, where) {
		var resizer = document.createElement("div");
		el.appendChild(resizer);
		addClass(resizer, prefix.concat("resizer"));
		addClass(resizer, where);
		var parent = el.parentElement;
		addClass(parent, "ljs-parent");//used to fix ominous gap between content
		//add event listener
		var children = Array.prototype.map.call(el.parentNode.children, function (c) {
			return c;
		});
		var sibling = children.filter(function (c) {
			return c !== el;
		})[0];
		var mousemove = function (e) {
			var event = new Event("resize");
			
			var pbox = parent.getBoundingClientRect(),
				elbox = el.getBoundingClientRect();
			var width = ensureMin(e.clientX - elbox.left),
				height = ensureMin(e.clientY - elbox.top);
			var percent = width * 100 / pbox.width;
			if (where === right) {
				el.style.width = percent + "%";
				sibling.style.width = (100 - percent)  + "%";
			} else if (where === left) {
				width = ensureMin(elbox.right - e.clientX);
				percent = width * 100 / pbox.width;
				el.style.width = percent + "%";
				sibling.style.width = (100 - percent)  + "%";
			} else if (where === top) {
				height = ensureMin(elbox.bottom - e.clientY);
				percent = height * 100 / pbox.height;
				el.style.height = percent + "%";
				sibling.style.height = (100 - percent) + "%";
			} else {
				percent = height * 100 / pbox.height;
				el.style.height = percent + "%";
				sibling.style.height = (100 - percent) + "%";
			}
			el.dispatchEvent(event);
			sibling.dispatchEvent(event);
			addClass(parent, "ljs-resizing");
			return false;
		};
		
		//sibling should be one
		resizer.onmousedown = function (e) {
            function unregister(e) {
				parent.removeEventListener("mousemove", mousemove);
				removeClass(parent, "ljs-resizing");
				return false;
            }
			parent.addEventListener("mousemove", mousemove);
			document.body.onmouseup = unregister;
			document.body.onmouseleave = unregister;
			return false;
		};
	}
	
	/**
		Runs the layout function on a document element
		This function acts recursively and calls layout on any div elements that have been classed with ljs-container.
		The function looks for direct children of the container element that have been classed as .ljs-right, .ljs-top, .ljs-bottom or .ljs-left
		@param {string} el the html selector for the element to run the layout function on
	*/
	function layout(props) {
		props = props || {};
		var el = props.el || "body", parent;
		var childSelectors = alignments.map(function (a) {
			return el.concat(" .").concat(a);
		}).join(",");
		
		function getChildren() {
			return document.querySelectorAll(childSelectors);
		}
		
		//layout each of the children
		//if the child is aligned to the left it means it can be resized on the right
		Array.prototype.forEach.call(getChildren(), function (c, i) {
			if (hasClass(c, right)) {
				insertResizer(c, left);
			} else if (hasClass(c, left)) {
				insertResizer(c, right);
			} else if (hasClass(c, top)) {
				insertResizer(c, bottom);
			} else if (hasClass(c, bottom)) {
				insertResizer(c, top);
			}
		});
		
		if (props.useFullHeight) {
			parent = document.querySelector(el);
			window.onresize = function (e) {
				parent.style.height = window.innerHeight + "px";
				//fire resize events for the child containers
				Array.prototype.forEach.call(getChildren(), function (c, i) {
					var event = new Event("resize");
					c.dispatchEvent(event);
				});
			};
			parent.style.minHeight = window.innerHeight + "px";
		}
	}
	window.layoutjs = layout;
}());