wm withdraw .

proc emacs-eval {arg} {
    puts stdout ":pvs-eval $arg :end-pvs-eval"
    gets stdin
}

set proof_x_sep 10
set proof_y_sep 20

proc create-dag {win} {
    upvar #0 dag-$win dag

    catch {unset dag}
}

proc dag-add-item {win tag succs} {
    upvar #0 dag-$win dag

    set dag(succs,$tag) $succs

    foreach item [$win find withtag $tag] {
	set dag(idtotag,$item) $tag
	if {![string match *dagline* [$win gettags $item]]} {
	    $win addtag $tag.real withtag $item
	}
    }

    set bbox [$win bbox $tag.real]
    set dag(anchor,$tag) [list [expr -[lindex $bbox 0]] \
			      [expr -[lindex $bbox 1]]]

    # Give these initial values (it doesn't matter what)
    set dag(topx,$tag) 0
    set dag(botx,$tag) 0
    set dag(topy,$tag) 0
    set dag(boty,$tag) 0

    foreach s $succs {
	set lineid \
	    [$win create line 0 0 0 0 \
		 -tags "$s dagline$tag,$s dagline.from$tag dagline.to$s dagline"]
	set dag(linefrom,$lineid) $tag
	set dag(lineto,$lineid) $s
    }
}

proc dag-delete-item {tag} {
    error "You can't delete items yet"
}

proc move-dag-item {win tag x y} {
    upvar #0 dag-$win dag

    set bbox [$win bbox $tag.real]
    set anchor $dag(anchor,$tag)

    set curx [expr [lindex $bbox 0]+[lindex $anchor 0]]
    set cury [expr [lindex $bbox 1]+[lindex $anchor 1]]

    $win move $tag [expr $x-$curx] [expr $y-$cury]

    set dag(topx,$tag) $x
    set dag(topy,$tag) $y

    set dag(botx,$tag) $x
    set dag(boty,$tag) [expr $y+[lindex $bbox 3]-[lindex $bbox 1]]

    update-lines-to $win $tag
    update-lines-from $win $tag
}

proc update-lines-to {win tag} {
    foreach id [$win find withtag dagline.to$tag] {
	update-line $win $id
    }
}

proc update-lines-from {win tag} {
    foreach id [$win find withtag dagline.from$tag] {
	update-line $win $id
    }
}

proc update-line {win id} {
    upvar #0 dag-$win dag

    $win coords $id \
	$dag(botx,$dag(linefrom,$id)) \
	$dag(boty,$dag(linefrom,$id)) \
	$dag(topx,$dag(lineto,$id)) \
	$dag(topy,$dag(lineto,$id))
}

proc dag-layout {top} {
}

proc tree-layout {win top} {
    update-widths $win $top
    move-tree $win $top
}

proc update-widths {win tag} {
    upvar #0 dag-$win dag

    global proof_x_sep

    set bbox [$win bbox $tag.real]
    set width [expr [lindex $bbox 2]-[lindex $bbox 0]]

    set kw -$proof_x_sep
    foreach sub $dag(succs,$tag) {
	update-widths $win $sub
	incr kw $dag(width,$sub)
	incr kw $proof_x_sep
    }

    set dag(width,$tag) [expr {$kw>$width?$kw:$width}]
}

proc move-tree {win tag {x 0} {y 0}} {
    upvar #0 dag-$win dag
    global proof_x_sep
    global proof_y_sep

    move-dag-item $win $tag $x $y

    set bbox [$win bbox $tag.real]

    set kw -$proof_x_sep
    foreach succ $dag(succs,$tag) {
	incr kw $dag(width,$succ)
	incr kw $proof_x_sep
    }

    set curx [expr round($x-$kw/2)]
    foreach succ $dag(succs,$tag) {
	move-tree $win $succ \
	    [expr $curx+$dag(width,$succ)/2] \
	    [expr $proof_y_sep+[lindex $bbox 3]]
	incr curx $dag(width,$succ)
	incr curx $proof_x_sep
    }
}

proc ancestors {path {suffix {}}} {
    if {$path=={top}} {
	return {}
    } else {
	regexp {^(.*)\.([^.]+)$} $path whole pre post
	return [concat $pre$suffix [ancestors $pre $suffix]]
    }
}

proc kids {path nkids} {
    set kids {}
    for {set i 0} {$i<$nkids} {incr i} {
	lappend kids $path.$i
    }
    return $kids
}

proc proof-num-children {path kids} {
    global $path

    set ${path}(kids) $kids
    catch {unset ${path}(rule)}
    catch {unset ${path}(done)}
}

proc proof-rule {path rule} {
    global $path

    set ${path}(rule) $rule
}

proc proof-done {path} {
    global $path

    set ${path}(done) 1
}

proc proof-show {path} {
    global $path proofwin env proof_y_sep

    if [info exists ${path}(sequent)] {
	show-sequent $path
    } else {
	set seq \
	    [$proofwin create bitmap 0 0 \
		 -bitmap @$env(PVSPATH)/wish/sequent.xbm \
		 -tags "$path.sequent $path sequent [ancestors $path .desc]" \
		 -anchor n]
	if [info exists ${path}(rule)] {
	    set bbox [$proofwin bbox $seq]
	    set seqbot [lindex $bbox 3]
	    set linebot [expr $seqbot+$proof_y_sep]
	    $proofwin create line 0 $seqbot 0 $linebot \
		-tags "$path.line $path [ancestors $path .desc]"
	    $proofwin create text 0 $linebot -text [set ${path}(rule)] \
		-tags "$path.rule $path rule [ancestors $path .desc]" \
		-anchor n
	}
    }

    dag-add-item $proofwin $path [kids $path [set ${path}(kids)]]
}

proc layout-proof {} {
    global proofwin

    tree-layout $proofwin top
    center-canvas $proofwin
}

proc center-canvas {win} {
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

proc setup-proof {name file directory} {
    global proof proofwin
    set proof(name) $name
    set proof(file) $file
    set proof(directory) $directory
    catch {destroy .show-proof}
    set top [toplevel .show-proof -geometry 400x400]
    pack propagate $top 0
    wm title $top "Proof of $name in $directory$file"
    wm iconname $top "PVS Proof"
    wm minsize $top 100 100
    wm maxsize $top 10000 10000
    set fr [frame $top.fr]
    pack $fr -expand yes -fill both
    set c [canvas $fr.c -xscroll "$fr.hscroll set" -yscroll "$fr.vscroll set"]
    set proofwin $c
    create-dag $c
    scrollbar $fr.vscroll -relief sunken -command "$c yview"
    scrollbar $fr.hscroll -relief sunken -command "$c xview" -orient horiz
    pack $fr.vscroll -side right -fill y
    pack $fr.hscroll -side bottom -fill x
    pack $c -expand yes -fill both
    label $top.message -text ""
    pack $top.message -fill x
    button $top.dismiss -text "Dismiss" -command {destroy .show-proof}
    pack $top.dismiss -side left -padx 2 -pady 2
    button $top.ps -text "Gen PS" -command "gen-ps $top"
    pack $top.ps -side left -padx 2 -pady 2
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
