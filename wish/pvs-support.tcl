wm withdraw .

proc emacs-eval {arg} {
    puts stdout ":pvs-eval $arg :end-pvs-eval"
    gets stdin
}

proc emacs-evaln {arg} {
    puts stdout ":pvs-evaln $arg :end-pvs-evaln"
}

proc pvs-send {arg} {
    emacs-evaln [format {(pvs-send '%s)} $arg]
}

proc pvs-send-and-wait {arg} {
    emacs-eval [format {(pvs-send-and-wait '%s)} $arg]
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

proc dag-delete-item {win tag suffix} {
    upvar #0 dag-$win dag

    $win delete $tag$suffix
    if {$suffix=={}} {
	if {[info exists dag(destroy,$tag)]} {
	    eval $dag(destroy,$tag)
	}
    }
}

proc dag-add-destroy-cb {win tag cb} {
    upvar #0 dag-$win dag

    if {[info exists dag(destroy,$tag)]} {
	set dag(destroy,$tag) [concat $cb ";" $dag(destroy,$tag)]
    } else {
	set dag(destroy,$tag) $cb
    }
}

proc dag-delete-subtree {win tag {suffix .real}} {
    upvar #0 dag-$win dag

    set succs {}
    catch {set succs $dag(succs,$tag)}

    foreach i $succs {
	dag-delete-subtree $win $i {}
    }

    dag-delete-item $win $tag $suffix
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

proc delete-proof-subtree {path} {
    global proofwin

    catch {move-dag-item $proofwin $path 0 0}

    dag-delete-subtree $proofwin $path
}

proc proof-num-children {path kids} {
    global $path

    set ${path}(kids) $kids
    catch {unset ${path}(rule)}
    catch {unset ${path}(done)}
    catch {unset ${path}(tcc)}
}

proc proof-rule {path rule} {
    global $path

    set ${path}(rule) $rule
}

proc proof-done {path} {
    global $path proofwin

    set ${path}(done) 1

    my-foreground $proofwin $path blue
}

proc proof-tcc {path} {
    global $path

    set ${path}(tcc) 1
}

proc proof-show {path} {
    global $path proofwin env proof_y_sep

    if [info exists ${path}(sequent)] {
	show-sequent $path
    } else {
	set seq \
	    [$proofwin create bitmap 0 0 \
		 -bitmap @$env(PVSPATH)/wish/sequent.xbm \
		 -tags "$path.sequent $path $path.real sequent [ancestors $path .desc]" \
		 -anchor n]
	if [info exists ${path}(rule)] {
	    set bbox [$proofwin bbox $seq]
	    set seqbot [lindex $bbox 3]
	    set linebot [expr $seqbot+$proof_y_sep]
	    $proofwin create line 0 $seqbot 0 $linebot \
		-tags "$path.line $path $path.real [ancestors $path .desc]"
	    $proofwin create text 0 $linebot -text [set ${path}(rule)] \
		-tags "$path.rule $path $path.real rule [ancestors $path .desc]" \
		-anchor n
	}
    }


    dag-add-item $proofwin $path [kids $path [set ${path}(kids)]]
    if [info exists ${path}(done)] {
	my-foreground $proofwin $path blue
    } elseif [info exists ${path}(tcc)] {
	my-foreground $proofwin $path green4
    }
}

proc get-current-sequent {} {
    global proofwin
    upvar #0 dag-$proofwin dag

    set path $dag(idtotag,[$proofwin find withtag current])

    set lisp_path [path-to-lisp-path $path]

    source [lindex [pvs-send-and-wait "(request-sequent '($lisp_path))"] 0]
}

proc show-sequent {path seq_label text} {
    global proofwin pathtolabel

    for {set label 1} {[winfo exists .sequent$label]} {incr label} {
    }

    set pathtolabel($path) $label

    set win .sequent$label.sequent
    set bbox [$proofwin bbox $path.sequent]
    set x [lindex $bbox 2]
    set y [expr round(([lindex $bbox 1]+[lindex $bbox 3])/2)]
    $proofwin create text $x $y -anchor w \
	-tags "$path $path.label $path.label$label label [ancestors $path .desc] label.$label" \
	-text $label
    update-color $path $path.label$label
    
    frame .sequent$label
    toplevel $win -class Sequent
    text $win.text -setgrid true
    $win.text insert end $text
    set height [lindex [split [$win.text index end] .] 0]
    pack $win.text
    button $win.dismiss -text Dismiss -command "destroy $win"
    pack $win.dismiss -side left -padx 2 -pady 2
    bind $win <Destroy> "catch {$proofwin delete $path.label$label}"
    bind $win <Destroy> "+after 1 {catch {destroy .sequent$label}}"
    wm geometry $win 80x$height
    wm iconname $win {PVS sequent}
    wm title $win "Sequent $label ($seq_label)"

    dag-add-destroy-cb $proofwin $path "disable-sequent $win"
}

proc disable-sequent {win} {
    catch {destroy $win}
}

proc path-to-lisp-path {path} {
    lrange [split $path .] 1 end
}

proc my-foreground {win tag color} {
    my-itemconfig $win $tag -foreground $color
    my-itemconfig $win $tag -fill $color
}

proc update-color {path tag} {
    global proofwin $path

    if [info exists ${path}(done)] {
	my-foreground $proofwin $tag blue
	return
    }

    set seqid [$proofwin find withtag $path.sequent]
    set tags [$proofwin gettags $seqid]
    foreach t $tags {
	if {$t=={current-subgoal}} {
	    my-foreground $proofwin $tag firebrick
	    $proofwin addtag current-subgoal withtag $tag
	    return
	}
    }

    if [info exists ${path}(tcc)] {
	my-foreground $proofwin $tag green4
	return
    }
    
    my-foreground $proofwin $tag black
}

proc my-itemconfig {win tag opt val} {
    foreach id [$win find withtag $tag] {
	catch {$win itemconfig $id $opt $val}
    }
}

proc layout-proof {} {
    global proofwin

    tree-layout $proofwin top
    center-canvas $proofwin
}

proc center-canvas {win} {
    set allbbox [$win bbox all]
    set allbbox [lreplace $allbbox 1 1 [expr [lindex $allbbox 1]-10]]
    set allbbox [lreplace $allbbox 3 3 [expr [lindex $allbbox 3]+10]]
    $win config -scrollregion $allbbox
    update idletasks
    set winwid [winfo width $win]
    set bboxwid [expr [lindex $allbbox 2]-[lindex $allbbox 0]]
    set margin [expr ($winwid-$bboxwid)/2]
#    $win xview [expr -$margin / [lindex [$win config -scrollincrement] 4]]
}

proc proof-current {path} {
    global proofwin curpath

    if {[info exists curpath]} {
	$proofwin dtag current-subgoal
	foreach tag [concat $curpath [ancestors $curpath]] {
	    update-color $tag $tag
	}
    }
    $proofwin addtag current-subgoal withtag $path
    foreach tag [ancestors $path] {
	$proofwin addtag current-subgoal withtag $tag
    }
    my-foreground $proofwin current-subgoal firebrick
    my-foreground $proofwin $path orange

    set bbox [$proofwin bbox $path.real]
    regexp {^(.*).c} $proofwin whole fr
    set hscroll $fr.hscroll
    set vscroll $fr.vscroll

    set hget [$hscroll get]
    set vget [$vscroll get]

    set units [lindex [$proofwin config -scrollincrement] 4]

    set allbbox [lindex [$proofwin config -scrollregion] 4]

    set width [winfo width $proofwin]
    set height [winfo height $proofwin]

    set left [expr $units*[lindex $hget 2]+[lindex $allbbox 0]]
    set top [expr $units*[lindex $vget 2]+[lindex $allbbox 1]]
    set right [expr $left+$width]
    set bottom [expr $top+$height]

    if {[lindex $bbox 3]+10>$bottom} {
	$proofwin yview [expr ([lindex $bbox 3]+10-$height-[lindex $allbbox 1])/$units]
    } elseif {[lindex $bbox 1]-10<$top} {
	$proofwin yview [expr ([lindex $bbox 1]-10-[lindex $allbbox 1])/$units]
    }

    if {[lindex $bbox 2]+10>$right} {
	$proofwin xview [expr ([lindex $bbox 2]+10-$width-[lindex $allbbox 0])/$units]
    } elseif {[lindex $bbox 0]-10<$left} {
	$proofwin xview [expr ([lindex $bbox 0]-10-[lindex $allbbox 0])/$units]
    }				      

    set curpath $path
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
    set c [canvas $fr.c -xscroll "$fr.hscroll set" -yscroll "$fr.vscroll set" \
	      -height 1 -width 1]
    set proofwin $c
    create-dag $c
    scrollbar $fr.vscroll -relief sunken -command "$c yview"
    scrollbar $fr.hscroll -relief sunken -command "$c xview" -orient horiz
    pack $fr.vscroll -side right -fill y
    pack $fr.hscroll -side bottom -fill x
    pack $c -expand yes -fill both
    $c bind sequent <1> {get-current-sequent}
    label $top.message -text ""
    pack $top.message -fill x -side bottom
    button $top.dismiss -text "Dismiss" -command {destroy .show-proof}
    pack $top.dismiss -side left -padx 2 -pady 2
    button $top.ps -text "Gen PS" -command "gen-ps $top"
    pack $top.ps -side left -padx 2 -pady 2
    bind $top <Destroy> {pvs-send (stop-displaying-proof)}
    bind $top <Destroy> {+
	foreach kid [winfo children .] {
	    if {[string match .sequent* $kid]} {
		destroy $kid
	    }
	}
    }
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
