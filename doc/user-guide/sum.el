(typecheck "sum")
(show-tccs "sum")
(save-excursion
  (set-buffer "sum.tccs")
  (write-file "sum-tccs.tex"))
(if (file-exists-p "pvs-tex.sub")
    (delete-file "pvs-tex.sub"))
(latex-pvs-file "sum")
(copy-file "sum.tex" "sum-nosub.tex" t)
(save-excursion
  (set-buffer (find-file-noselect "pvs-tex.sub" t))
  (erase-buffer)
  (insert "THEORY key 7 {\\textbf{\\large Theory}}
sum 1 2 {\\sum_{i = 0}^{#1} i}")
  (save-buffer))
(latex-pvs-file "sum")
(copy-file "sum.tex" "sum-sub.tex" t)
