;;; This file provides the global key-bindings and abbreviations for
;;; most of the commands of PVS.  The pvs-abbreviate function checks
;;; whether the abbreviation clashes with any existing function and will
;;; not do the abbreviation in that case.  Since the users .emacs file
;;; is loaded first, this will catch most such clashes.  If you want to
;;; make modifications to this list, simply copy this file to a
;;; directory in your load-path (normally set in your .emacs file) and
;;; modify it accordingly.

;; HISTORY 
;; 12-Jul-1994		Sam Owre	
;;    Removed key-bindings for C-z C-g and C-h p

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(require 'pvs-utils)

(global-set-key "\C-x\C-z" 'suspend-pvs)
(global-set-key "\C-x\C-c" 'exit-pvs)
(global-set-key "\C-c\C-f" 'find-pvs-file)
(global-set-key "\C-ch"    'help-pvs)
(global-set-key "\C-c\C-hl" 'help-pvs-language)
(global-set-key "\C-c\C-hb" 'help-pvs-bnf)
(global-set-key "\C-c\C-hp" 'help-pvs-prover)
(global-set-key "\C-c\C-hc" 'help-pvs-prover-command)
(global-set-key "\C-c\C-hs" 'help-pvs-prover-strategy)
(global-set-key "\C-c\C-he" 'help-pvs-prover-emacs)
(global-set-key "\C-c\C-hr" 'pvs-release-notes)
(global-set-key "\C-z\C-f" 'pvs-find-lisp-file)


(pvs-abbreviate 'parse 'pa)
(pvs-abbreviate 'typecheck 'tc)
(pvs-abbreviate 'typecheck-importchain 'tci)
(pvs-abbreviate 'typecheck-prove 'tcp)
(pvs-abbreviate 'typecheck-prove-importchain 'tcpi)
(pvs-abbreviate 'prove 'pr)
(pvs-abbreviate 'step-proof 'prs)
(pvs-abbreviate 'redo-proof 'prr)
(pvs-abbreviate 'prove-theory 'prt)
(pvs-abbreviate 'prove-pvs-file 'prf)
(pvs-abbreviate 'prove-importchain 'pri)
(pvs-abbreviate 'prove-importchain-subtree 'pris)
(pvs-abbreviate 'prove-next-unproved-formula 'prnext)
(pvs-abbreviate 'prove-proofchain 'prp)
(pvs-abbreviate 'prove-formulas-theory 'prft)
(pvs-abbreviate 'prove-formulas-pvs-file 'prff)
(pvs-abbreviate 'prove-formulas-importchain 'prfi)
(pvs-abbreviate 'prove-formulas-importchain-subtree 'prfs)
(pvs-abbreviate 'prove-tccs-theory 'prtt)
(pvs-abbreviate 'prove-tccs-pvs-file 'prtf)
(pvs-abbreviate 'prove-tccs-importchain 'prti)
(pvs-abbreviate 'prove-tccs-importchain-subtree 'prts)
(pvs-abbreviate 'prove-untried-theory 'prut)
(pvs-abbreviate 'prove-untried-pvs-file 'pruf)
(pvs-abbreviate 'prove-untried-importchain 'prui)
(pvs-abbreviate 'prove-untried-importchain-subtree 'prus)
(pvs-abbreviate 'prettyprint-theory 'ppt)
(pvs-abbreviate 'prettyprint-pvs-file 'ppf)
(pvs-abbreviate 'prettyprint-declaration 'ppd)
(pvs-abbreviate 'prettyprint-region 'ppr)
(pvs-abbreviate 'prettyprint-expanded 'ppe)
(pvs-abbreviate 'prettyprint-theory-instance 'ppti)
(pvs-abbreviate 'show-tccs 'tccs)
(pvs-abbreviate 'find-pvs-file 'ff)
(pvs-abbreviate 'find-theory 'ft)
(pvs-abbreviate 'view-prelude-file 'vpf)
(pvs-abbreviate 'view-prelude-theory 'vpt)
(pvs-abbreviate 'view-library-file 'vlf)
(pvs-abbreviate 'view-library-theory 'vlt)
(pvs-abbreviate 'new-pvs-file 'nf)
(pvs-abbreviate 'new-theory 'nt)
(pvs-abbreviate 'import-pvs-file 'imf)
(pvs-abbreviate 'import-theory 'imt)
(pvs-abbreviate 'delete-pvs-file 'df)
(pvs-abbreviate 'delete-theory 'dt)
(pvs-abbreviate 'save-some-pvs-files 'ssf)
(pvs-abbreviate 'print-theory 'ptt)
(pvs-abbreviate 'print-pvs-file 'ptf)
(pvs-abbreviate 'print-importchain 'pti)
(pvs-abbreviate 'alltt-theory 'alt)
(pvs-abbreviate 'alltt-pvs-file 'alf)
(pvs-abbreviate 'alltt-importchain 'ali)
(pvs-abbreviate 'alltt-proof 'alp)
(pvs-abbreviate 'latex-theory 'ltt)
(pvs-abbreviate 'latex-pvs-file 'ltf)
(pvs-abbreviate 'latex-proof 'ltp)
(pvs-abbreviate 'latex-importchain 'lti)
(pvs-abbreviate 'latex-theory-view 'ltv)
(pvs-abbreviate 'latex-proof-view 'lpv)
(pvs-abbreviate 'latex-set-linelength 'lts)
(pvs-abbreviate 'list-pvs-files 'lf)
(pvs-abbreviate 'list-theories 'lt)
(pvs-abbreviate 'change-context 'cc)
(pvs-abbreviate 'save-context 'sc)
(pvs-abbreviate 'context-path 'cp)
(pvs-abbreviate 'status-theory 'stt)
(pvs-abbreviate 'status-pvs-file 'stf)
(pvs-abbreviate 'status-importchain 'sti)
(pvs-abbreviate 'status-importbychain 'stb)
(pvs-abbreviate 'status-proof 'sp)
(pvs-abbreviate 'status-proof-theory 'spt)
(pvs-abbreviate 'status-proof-pvs-file 'spf)
(pvs-abbreviate 'status-proof-importchain 'spi)
(pvs-abbreviate 'status-proofchain 'spc)
(pvs-abbreviate 'status-proofchain-theory 'spct)
(pvs-abbreviate 'status-proofchain-pvs-file 'spcf)
(pvs-abbreviate 'status-proofchain-importchain 'spci)
(pvs-abbreviate 'help-pvs 'pvs-help)
(pvs-abbreviate 'help-pvs-bnf 'pvs-help-bnf)
(pvs-abbreviate 'help-pvs-language 'pvs-help-language)
(pvs-abbreviate 'help-pvs-prover 'pvs-help-prover)
(pvs-abbreviate 'help-pvs-prover-command 'pvs-help-prover-command)
(pvs-abbreviate 'help-pvs-prover-strategy 'pvs-help-prover-strategy)
(pvs-abbreviate 'help-pvs-prover-emacs 'pvs-help-prover-emacs)
(pvs-abbreviate 'report-pvs-bug 'pvs-report-bug)
(pvs-abbreviate 'edit-proof 'show-proof)
(pvs-abbreviate 'x-prove 'xpr)
(pvs-abbreviate 'x-step-proof 'xsp)
(pvs-abbreviate 'pvs-load-patches 'load-pvs-patches)
