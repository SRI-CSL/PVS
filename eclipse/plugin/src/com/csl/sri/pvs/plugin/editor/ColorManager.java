package com.csl.sri.pvs.plugin.editor;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

/**
 * @author
 *
 * This class assigns colors to elements of the PVS syntax
 *
 */

public class ColorManager {

	protected Map<RGB, Color> fColorTable = new HashMap<RGB, Color>(10);

	public static final RGB KEYWORD = new RGB(128, 0, 128); // purple

	public static final RGB TYPE = new RGB(128, 0, 0); // maroon

	public static final RGB CONSTANT = new RGB(0, 0, 128); // navy

	public static final RGB OPERATION = new RGB(0, 128, 0); // green

	public static final RGB COMMENT = new RGB(128, 128, 0); // olive

	public static final RGB DEFAULT = new RGB(0, 0, 0); // black

	/**
	 * Return the color that is stored in the color table under the given RGB
	 * value.
	 * 
	 * @param rgb
	 *            the RGB value
	 * @return the color stored in the color table for the given RGB value
	 */

	public void dispose() {
		Iterator e = fColorTable.values().iterator();
		while (e.hasNext())
			((Color) e.next()).dispose();
	}

	public Color getColor(RGB rgb) {
		Color color = (Color) fColorTable.get(rgb);
		if (color == null) {
			color = new Color(Display.getCurrent(), rgb);
			fColorTable.put(rgb, color);
		}
		return color;
	}
}
