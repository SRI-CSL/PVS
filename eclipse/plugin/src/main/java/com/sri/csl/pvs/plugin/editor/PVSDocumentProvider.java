package com.sri.csl.pvs.plugin.editor;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.ui.editors.text.FileDocumentProvider;

/**
 * @author Christelle Scharff and Guillaume Maillard
 * 
 * This class manages
 * 
 */

public class PVSDocumentProvider extends FileDocumentProvider {

	protected IDocument createDocument(Object element) throws CoreException {
		IDocument document = super.createDocument(element);
		if (document != null) {
			IDocumentPartitioner partitioner = new FastPartitioner(
					new PVSPartitionScanner(), new String[] {
							PVSPartitionScanner.PVS_TAG,
							PVSPartitionScanner.PVS_COMMENT });
			partitioner.connect(document);
			document.setDocumentPartitioner(partitioner);
		}
		return document;
	}
}