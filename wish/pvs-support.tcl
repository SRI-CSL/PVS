wm withdraw .

proc emacs-eval {arg} {
    puts stdout ":pvs-eval $arg :end-pvs-eval"
    gets stdin
}

proc show-proof {prf} {
    catch {destroy .show-proof}
    toplevel .show-proof
    label .show-proof.proof -text $prf -font "-*-new century schoolbook-bold-r-*-*-33-*-*-*-*-*-*-*" -fg yellow -bg purple
    pack .show-proof.proof
}
