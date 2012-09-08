package com.sri.csl.pvs.plugin.editor;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

/**
 * @author 
 * 
 * This class configures the PVS editor - scanner, colors and double
 * click strategy
 *
 */

public class PVSConfiguration extends SourceViewerConfiguration {

	private PVSDoubleClickStrategy doubleClickStrategy;

	private PVSScanner scanner;

	private ColorManager colorManager;

	public PVSConfiguration(ColorManager colorManager) {
		this.colorManager = colorManager;
	}

	public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
		return new String[] { IDocument.DEFAULT_CONTENT_TYPE,
				PVSPartitionScanner.PVS_COMMENT,
				PVSPartitionScanner.PVS_TAG };
	}

	public ITextDoubleClickStrategy getDoubleClickStrategy(
			ISourceViewer sourceViewer, String contentType) {
		if (doubleClickStrategy == null)
			doubleClickStrategy = new PVSDoubleClickStrategy();
		return doubleClickStrategy;
	}

	protected PVSScanner getPVSScanner() {
		if (scanner == null) {
			scanner = new PVSScanner(colorManager);
			scanner.setDefaultReturnToken(new Token(new TextAttribute(
					colorManager.getColor(ColorManager.DEFAULT))));
		}
		return scanner;
	}

	public IPresentationReconciler getPresentationReconciler(
			ISourceViewer sourceViewer) {
		PresentationReconciler reconciler = new PresentationReconciler();

		DefaultDamagerRepairer dr = new DefaultDamagerRepairer(
				getPVSScanner());
		reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
		reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

		return reconciler;
	}

}