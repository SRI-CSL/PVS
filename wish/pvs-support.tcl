wm withdraw .

proc emacs-eval {arg} {
    puts stdout ":pvs-eval $arg :end-pvs-eval"
    gets stdin
}

set proof_x_sep 10
set proof_y_sep 20

proc insert-proofstate {prf path win} {
    global env
    $win create bitmap 0 0 -bitmap @$env(PVSPATH)/wish/sequent.xbm \
	-tags "$path.sequent sequent" -anchor n
    $win create line 0 0 0 0 -tags "$path.totop totop $path.sequpdate"
    $win create line 0 0 0 0 \
	-tags "$path.torule torule $path.sequpdate $path.ruleupdate"
    $win create text 0 0 -text [lindex $prf 1] \
	-tags "$path.rule rule" -anchor n
}

proc insert-proof {prf path win} {
    insert-proofstate $prf $path $win

    set count 1
    foreach sub [lindex $prf 2] {
	insert-proof $sub $path.$count $win
	$win addtag $path.ruleupdate withtag $path.$count.totop
	incr count
    }
}

proc dag-add-item {tag succs} {
}

proc dag-delete-item {tag} {
}

proc dag-layout {top} {
}

proc tree-layout {top} {
}

proc proof-rule {path rule} {
    global $path

    set $path(rule) $rule
}

proc proof-num-children {path kids} {
    global $path

    set $path(kids) $kids
}

proc proof-done {path} {
    global $path

    set $path(done) 1
}

proc proof-show {path} {
    global $path proofwin env

    if [info exists $path(sequent)] {
	show-sequent $path
    } else {
	$proofwin create bitmap 0 0 -bitmap @$env(PVSPATH)/wish/sequent.xbm \
	    -tags "$path.sequent $path sequent [ancestors $path]" -anchor n
	if [info exists $path(rule)] {
	    $proofwin create line 0 0 0 0 \
		-tags "$path.line $path [ancestors $path]"
	    $proofwin create text 0 0 -text [set $path(rule)]
	}
    }

    dag-add-item $path [kids $path [set $path(kids)]]
}

proc update-widths {prf path win} {
    global path_width proof_x_sep

    set bbox [$win bbox $path.rule]
    set width [expr {[lindex $bbox 2] - [lindex $bbox 0]}]

    set kw -$proof_x_sep
    set count 1
    foreach sub [lindex $prf 2] {
	update-widths $sub $path.$count $win
	incr kw $path_width($path.$count)
	incr kw $proof_x_sep
	incr count
    }

    set path_width($path) [expr {$kw>$width?$kw:$width}]
}

proc move-sequent {win path x y} {
    $win coords $path.sequent $x $y
    update-lines $win $path.sequpdate
}

proc move-rule {win path x y} {
    $win coords $path.rule $x $y
    update-lines $win $path.ruleupdate
}

proc update-lines {win tag} {
    foreach line [$win find withtag $tag] {
	foreach tag [$win gettags $line] {
	    if [regexp {^(.*)\.([0-9]+)\.totop} $tag whole parpath kid] {
		set rulebbox [$win bbox $parpath.rule]
		set rulexy [$win coords $parpath.rule]
		set seqxy [$win coords $parpath.$kid.sequent]
		eval $win coords $line \
		    [lindex $rulexy 0] [lindex $rulebbox 3] \
		    $seqxy
		break
	    } elseif [regexp {^(.*)\.torule} $tag whole curpath] {
		set rulexy [$win coords $curpath.rule]
		set seqbbox [$win bbox $curpath.sequent]
		set seqxy [$win coords $curpath.sequent]
		eval $win coords $line \
		    [lindex $seqxy 0] [lindex $seqbbox 3] \
		    $rulexy
		break
	    }
	}
    }
}

proc move-proof {prf path win x y} {
    global path_width proof_x_sep proof_y_sep

    set numkids [llength [lindex $prf 2]]

    move-sequent $win $path $x $y
    set seqbbox [$win bbox $path.sequent]

    move-rule $win $path \
	$x [expr $proof_y_sep+[lindex $seqbbox 3]]
    set rulebbox [$win bbox $path.rule]

    set kw -$proof_x_sep

    for {set i 1} {$i<=$numkids} {incr i} {
	incr kw $path_width($path.$i)
	incr kw $proof_x_sep
    }

    set curx [expr round($x-$kw/2)]

    set count 1
    foreach sub [lindex $prf 2] {
	move-proof $sub $path.$count $win \
	    [expr $curx+$path_width($path.$count)/2] \
	    [expr $proof_y_sep+[lindex $rulebbox 3]]
	incr curx $path_width($path.$count)
	incr curx $proof_x_sep
	incr count
    }
}	

proc update-proof {prf path win} {
    global path_width

    update-widths $prf $path $win

    move-proof $prf $path $win 0 15

    set allbbox [$win bbox all]
    set allbbox [lreplace $allbbox 1 1 [expr [lindex $allbbox 1]-10]]
    $win config -scrollregion $allbbox
    update idletasks
    set winwid [winfo width $win]
    set bboxwid [expr [lindex $allbbox 2]-[lindex $allbbox 0]]
    set margin [expr ($winwid-$bboxwid)/2]
    $win xview [expr -$margin / [lindex [$win config -scrollincrement] 4]]
}

set proof(lastX) 0
set proof(lastY) 0

proc thingDown {w x y} {
    global proof
    $w dtag selected
    $w addtag selected withtag current
    $w raise current
    set proof(lastX) $x
    set proof(lastY) $y

    foreach tag [$w gettags selected] {
	if [regexp {^(.*)\.sequent} $tag whole path] {
	    set proof(update) $path.sequpdate
	    break
	} elseif [regexp {^(.*)\.rule} $tag whole path] {
	    set proof(update) $path.ruleupdate
	    break
	}
    }
}

proc thingMove {w x y} {
    global proof
    $w move selected [expr $x-$proof(lastX)] [expr $y-$proof(lastY)]
    set proof(lastX) $x
    set proof(lastY) $y
    update-lines $w $proof(update)
}

proc clear-message {top} {
    $top.message config -text ""
}

proc show-message {top text} {
    $top.message config -text $text
    after 5000 "clear-message $top"
}

proc gen-ps {top} {
    global proof canvcolors

    set w $top.fr.c

    set bbox [$w bbox all]
    set psfile $proof(directory)$proof(file)_$proof(name).ps

    $w postscript \
	-file $psfile \
	-x [lindex $bbox 0] \
	-width [expr [lindex $bbox 2]-[lindex $bbox 0]] \
	-y [lindex $bbox 1] \
	-height [expr [lindex $bbox 3]-[lindex $bbox 1]] \
	-rotate yes
    show-message $top "Saved PS to $psfile"
}

# For debugging.
proc reshow-proof {} {
    global proof
    show-proof $proof(proof) $proof(name) $proof(file) $proof(directory)
}

proc show-proof {prf name file directory} {
    global proof
    set proof(proof) $prf
    set proof(name) $name
    set proof(file) $file
    set proof(directory) $directory
    catch {destroy .show-proof}
    catch {unset path_width}
    set top [toplevel .show-proof -geometry 400x400]
    pack propagate $top 0
    wm title $top "Proof of $name in $directory$file"
    wm iconname $top "PVS Proof"
    wm minsize $top 100 100
    wm maxsize $top 10000 10000
    set fr [frame $top.fr]
    pack $fr -expand yes -fill both
    set c [canvas $fr.c -xscroll "$fr.hscroll set" -yscroll "$fr.vscroll set"]
    scrollbar $fr.vscroll -relief sunken -command "$c yview"
    scrollbar $fr.hscroll -relief sunken -command "$c xview" -orient horiz
    pack $fr.vscroll -side right -fill y
    pack $fr.hscroll -side bottom -fill x
    pack $c -expand yes -fill both
    insert-proof $prf top $c
    $c delete top.totop
    $c raise sequent
    $c raise rule
    $c bind sequent <Control-1> "thingDown $c %x %y"
    $c bind rule <Control-1> "thingDown $c %x %y"
    $c bind any <Any-ButtonRelease-1> "$c dtag selected"
    bind $c <Any-B1-Motion> "thingMove $c %x %y"
    update-proof $prf top $c
    label $top.message -text ""
    pack $top.message -fill x
    button $top.dismiss -text "Dismiss" -command {destroy .show-proof}
    pack $top.dismiss -side left -padx 2 -pady 2
    button $top.ps -text "Gen PS" -command "gen-ps $top"
    pack $top.ps -side left -padx 2 -pady 2
}
