#                               -*- Mode: Tcl -*- 
# pvs-support.tcl -- 
# Author          : Carl Witty with mods by Sam Owre
# Created On      : Thu Apr 27 02:27:14 1995
# Last Modified By: Sam Owre
# Last Modified On: Thu May  4 19:04:20 1995
# Update Count    : 14
# Status          : Alpha test


wm withdraw .

set mono 0

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

proc create-dag {win} {
    upvar #0 dag-$win dag

    catch {unset dag}
    set dag(items) {}
    bind $win <Destroy> "+delete-all-dag-items $win"
}

proc dag-bind-move {win suffix modifier button update} {
    upvar #0 dag-$win dag

    $win bind dag-item <$modifier-$button> \
	"dag-motion-click $win %x %y {$suffix}"
    $win bind dag-item <$modifier-B$button-Motion> \
	"dag-motion-drag $win %x %y $update"
}

proc dag-motion-click {win x y suffix} {
    upvar #0 dag-$win dag
    global moving

    catch {unset moving}

    set dag(oldX) $x
    set dag(oldY) $y

    set dag(drag_path) $dag(idtotag,[$win find withtag current])

    $win dtag selected
    $win addtag selected withtag $dag(drag_path)$suffix

    foreach id [$win find withtag selected] {
	catch {
	    set moving($dag(idtotag,$id)) 1
	}
    }	
}

proc dag-motion-drag {win x y update} {
    upvar #0 dag-$win dag
    global moving

    set dx [expr $x-$dag(oldX)]
    set dy [expr $y-$dag(oldY)]

    $win move selected $dx $dy
    set dag(oldX) $x
    set dag(oldY) $y

    foreach tag [array names moving] {
	incr dag(topx,$tag) $dx
	incr dag(topy,$tag) $dy
	incr dag(botx,$tag) $dx
	incr dag(boty,$tag) $dy
    }

    update-lines-to $win $dag(drag_path)
    if {$update=={both}} {
	update-lines-from $win $dag(drag_path)
    }
}

proc dag-add-item {win tag succs linetags} {
    upvar #0 dag-$win dag

    # Warning...this isn't updated by deletes.
    set dag(items) [concat $dag(items) $tag]

    set dag(succs,$tag) $succs

    foreach item [$win find withtag $tag] {
	if {![string match *dagline* [$win gettags $item]]} {
	    set dag(idtotag,$item) $tag
	    $win addtag dag-item withtag $item
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
	# In my opinion, on the X11R6 server, width 0 lines look better
	# than width 1 lines.
	set lineid \
	    [$win create line 0 0 0 0 \
		 -tags "$s dagline$tag,$s dagline.from$tag dagline.to$s dagline $linetags" \
		 -fill [get-option displayforeground] \
		 -width 0]
	set dag(linefrom,$lineid) $tag
	set dag(lineto,$lineid) $s
    }
}

proc delete-all-dag-items {win} {
    upvar #0 dag-$win dag

    foreach i [array names dag] {
	if {[string match destroy,* $i]} {
	    eval $dag($i)
	}
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
    set dag(topy,$tag) [expr $y-1]

    set dag(botx,$tag) $x
    set dag(boty,$tag) [expr $y+1+[lindex $bbox 3]-[lindex $bbox 1]]

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

proc dag-layout {win top} {
    upvar #0 dag-$win dag

    set dag(layout) "dag-layout $win $top"

    foreach item $dag(items) {
	set dag(level,$item) 0
    }

    set maxlev 0

    set change 1
    while {$change} {
	set change 0
	foreach item $dag(items) {
	    set lev $dag(level,$item)
	    foreach succ $dag(succs,$item) {
		if {$dag(level,$succ)<=$lev} {
		    set dag(level,$succ) [expr 1+$lev]
		    set change 1
		    if {$lev==$maxlev} {
			incr maxlev
		    }
		}
	    }
	}
    }

    set y 0
    for {set lev 0} {$lev<=$maxlev} {incr lev} {
	set wd -[get-option xSep $win]
	set ht 0
	foreach item $dag(items) {
	    if {$dag(level,$item)==$lev} {
		set bbox [$win bbox $item.real]
		set w [expr [lindex $bbox 2]-[lindex $bbox 0]]
		set h [expr [lindex $bbox 3]-[lindex $bbox 1]]
		incr wd [get-option xSep $win]
		incr wd $w
		if {$h>$ht} {
		    set ht $h
		}
	    }
	}
	set x [expr -int($wd/2)]
	foreach item $dag(items) {
	    if {$dag(level,$item)==$lev} {
		set bbox [$win bbox $item.real]
		set w [expr [lindex $bbox 2]-[lindex $bbox 0]]
		move-dag-item $win $item [expr $x+int($w/2)] $y
		incr x [get-option xSep $win]
		incr x $w
	    }
	}
	incr y [get-option ySep $win]
	incr y $ht
    }	    
}

proc dagwin-layout {win} {
    upvar #0 dag-$win dag

    eval $dag(layout)
    canvas-set-scroll $win 1
}

proc tree-layout {win top} {
    upvar #0 dag-$win dag

    set dag(layout) "tree-layout $win $top"

    update-widths $win $top
    move-tree $win $top
}

proc update-widths {win tag} {
    upvar #0 dag-$win dag

    set bbox [$win bbox $tag.real]
    set width [expr [lindex $bbox 2]-[lindex $bbox 0]]

    set kw -[get-option xSep $win]
    foreach sub $dag(succs,$tag) {
	update-widths $win $sub
	incr kw $dag(width,$sub)
	incr kw [get-option xSep $win]
    }

    set dag(width,$tag) [expr {$kw>$width?$kw:$width}]
}

proc move-tree {win tag {x 0} {y 0}} {
    upvar #0 dag-$win dag

    move-dag-item $win $tag $x $y

    set bbox [$win bbox $tag.real]

    set kw -[get-option xSep $win]
    foreach succ $dag(succs,$tag) {
	incr kw $dag(width,$succ)
	incr kw [get-option xSep $win]
    }

    set curx [expr round($x-$kw/2)]
    foreach succ $dag(succs,$tag) {
	move-tree $win $succ \
	    [expr $curx+$dag(width,$succ)/2] \
	    [expr [get-option ySep $win]+[lindex $bbox 3]]
	incr curx $dag(width,$succ)
	incr curx [get-option xSep $win]
    }
}


# Support for Proof windows

proc ancestors {path top {suffix {}}} {
    if {$path==$top} {
	return $path$suffix
    } else {
	regexp {^(.*)\.([^.]+)$} $path whole pre post
	return [concat $path$suffix [ancestors $pre $top $suffix]]
    }
}

proc kids {path nkids} {
    set kids {}
    for {set i 0} {$i<$nkids} {incr i} {
	lappend kids $path.$i
    }
    return $kids
}

proc delete-proof-subtree {name theory relpath} {
    set proofwin .proof.u_$theory-$name-i.fr.c
    set path $theory-$name-$relpath
    catch {move-dag-item $proofwin $path 0 0}

    dag-delete-subtree $proofwin $path
}

proc proof-num-children {name theory path kids} {
    set fullpath $theory-$name-$path
    global $fullpath
    set ${fullpath}(kids) $kids
    catch {unset ${fullpath}(rule)}
    catch {unset ${fullpath}(done)}
    catch {unset ${fullpath}(tcc)}
}

proc proof-rule {name theory path rule} {
    set fullpath $theory-$name-$path
    global $fullpath
    set ${fullpath}(rule) $rule
}

proc proof-sequent {name theory path seqlabel sequent} {
    set fullpath $theory-$name-$path
    global $fullpath
    set ${fullpath}(seqlabel) $seqlabel
    set ${fullpath}(sequent) $sequent
}

proc proof-done {name theory path interactive} {
    set fullpath $theory-$name-$path
    global $fullpath
    if {$interactive} {
	set pwin .proof.u_$theory-$name-i.fr.c
    } else {
	set pwin .proof.u_$theory-$name.fr.c
    }
    set ${fullpath}(done) 1

    my-foreground $pwin $fullpath [get-option doneColor]
}

proc proof-tcc {path} {
    global $path

    set ${path}(tcc) 1
}

proc proof-show {name theory path interactive} {
    global env
    set top $theory-$name-top
    set fullpath $theory-$name-$path
    global $fullpath
    if {$interactive} {
	set proofwin .proof.u_$theory-$name-i.fr.c
    } else {
	set proofwin .proof.u_$theory-$name.fr.c
    }
    set seq \
	[$proofwin create bitmap 0 0 \
	     -bitmap @$env(PVSPATH)/wish/sequent.xbm \
	     -tags "$fullpath.sequent $fullpath $fullpath.real sequent [ancestors $fullpath $top .desc]" \
	     -anchor n \
	     -foreground [get-option displayforeground]]
    if [info exists ${fullpath}(rule)] {
	set bbox [$proofwin bbox $seq]
	set seqbot [lindex $bbox 3]
	set ysep [get-option ySep $proofwin]
	# I don't know why it happens, but sometimes get-option returns {}
	# here.  get-option works fine after this function.
	if {$ysep == {}} {
	    set ysep 5
	}
	set linebot [expr $seqbot+$ysep]
	set rule [set ${fullpath}(rule)]
	if {[string index $rule 0] == "+"} {
	    set rule [string range $rule 1 end]
	    set rulebitmap "gray25"
	} else {
	    set rulebitmap ""
	}
	set alen [get-option abbrevLen]
	if $alen {
	    if {[string length $rule]>$alen} {
		if {[regexp {^[^ ]* } $rule whole]} {
		    set rule "$whole...)"
		}
	    }
	}
	set ${fullpath}(rule_abbr) $rule
	set anc [ancestors $fullpath $top .desc]
	$proofwin create line 0 $seqbot 0 $linebot \
	    -tags "$fullpath.line $fullpath $fullpath.real $anc" \
	    -fill [get-option displayforeground]
	$proofwin create text 0 $linebot -text $rule \
	    -font [get-option displayfont] \
	    -tags "$fullpath.rule $fullpath $fullpath.real rule $anc" \
	    -anchor n \
	    -fill [get-option displayforeground] -stipple $rulebitmap
    }

    dag-add-item $proofwin $fullpath [kids $fullpath [set ${fullpath}(kids)]] [ancestors $fullpath $top .desc]
    dag-add-destroy-cb $proofwin $fullpath "global $fullpath; catch {unset $fullpath}"
     if [info exists ${fullpath}(done)] {
 	my-foreground $proofwin $fullpath [get-option doneColor]
     } elseif [info exists ${fullpath}(tcc)] {
 	my-foreground $proofwin $fullpath [get-option tccColor]
     }
}

proc get-full-rule {proofwin top} {
    global pathtorlabel
    upvar #0 dag-$proofwin dag

    set path $dag(idtotag,[$proofwin find withtag current])
    global $path

    if {![string compare [set ${path}(rule)] [set ${path}(rule_abbr)]]} {
	return
    }

    for {set label 1} {[winfo exists .rule$label]} {incr label} {
    }

    if [info exists pathtorlabel($path)] {
	set rulelab $pathtorlabel($path)
	if {! [winfo ismapped .rule$rulelab.rule]} {
	    wm deiconify .rule$rulelab.rule
	} else {
	    wm iconify .rule$rulelab.rule
	    wm deiconify .rule$rulelab.rule
	}
 	return
    }
    set pathtorlabel($path) $label

    set rulewin .rule$label.rule
    set bbox [$proofwin bbox $path.rule]
    set x [lindex $bbox 2]
    set y [expr round(([lindex $bbox 1]+[lindex $bbox 3])/2)]
    $proofwin create text $x $y -anchor w \
	-tags "$path $path.rlabel $path.rlabel$label rlabel [ancestors $path $top .desc] rlabel.$label" \
	-text $label
    update-color $proofwin $path $path.rlabel$label
    
    frame .rule$label
    toplevel $rulewin -class Command
    frame $rulewin.fr
    pack $rulewin.fr -expand yes -fill both
    text $rulewin.fr.text -bd 2 -relief raised
    $rulewin.fr.text insert end [set ${path}(rule)]
    set height [lindex [split [$rulewin.fr.text index end] .] 0]
    set wd 0
    for {set i 1} {$i<=$height} {incr i} {
	set linewd [lindex [split [$rulewin.fr.text index "$i.0 lineend"] .] 1]
	if {$linewd>$wd} {set wd $linewd}
    }
    $rulewin.fr.text config -height $height -width $wd -state disabled -wrap none
    pack $rulewin.fr.text -expand yes -fill both
    button $rulewin.dismiss -text Dismiss -command "destroy $rulewin"
    pack $rulewin.dismiss -side left -padx 2 -pady 2
    bind $rulewin <Destroy> "+catch {$proofwin delete $path.rlabel$label}"
    bind $rulewin <Destroy> "+catch {unset pathtorlabel($path)}"
    bind $rulewin <Destroy> "+after 1 {catch {destroy .rule$label}}"
    wm iconname $rulewin {PVS command}
    wm title $rulewin "Command $label [set ${path}(rule_abbr)]"

    set next [lindex [set dag(succs,$path)] 0]
    dag-add-destroy-cb $proofwin $next "catch {destroy $rulewin}"
}
    

# proc get-current-sequent {proofwin} {
#     upvar #0 dag-$proofwin dag
# 
#     set path $dag(idtotag,[$proofwin find withtag current])
# 
#     set lisp_path [path-to-lisp-path $path]
# 
#     set file [lindex [pvs-send-and-wait "(request-sequent '($lisp_path))"] 0]
#     source $file
#     exec rm -f $file
# }
    

proc undo-to-sequent {proofwin} {
    upvar #0 dag-$proofwin dag

    set path $dag(idtotag,[$proofwin find withtag current])

    set lisp_path [path-to-lisp-path $path]

    set file [lindex [pvs-send-and-wait "(undo-to-sequent '($lisp_path))"] 0]
    source $file
    exec rm -f $file
}

proc show-sequent {proofwin top} {    
    global pathtolabel
    upvar #0 dag-$proofwin dag

    set path $dag(idtotag,[$proofwin find withtag current])
    global $path
    set seq_label [set ${path}(seqlabel)]
    set text [set ${path}(sequent)]

    for {set label 1} {[winfo exists .sequent$label]} {incr label} {
    }

    if [info exists pathtolabel($path)] {
	set seqlab $pathtolabel($path)
	if {! [winfo ismapped .sequent$seqlab.sequent]} {
	    wm deiconify .sequent$seqlab.sequent
	} else {
	    wm iconify .sequent$seqlab.sequent
	    wm deiconify .sequent$seqlab.sequent
	}
	return
    }

    set seqwin .sequent$label.sequent
    set bbox [$proofwin bbox $path.sequent]
    set x [lindex $bbox 2]
    set y [expr round(([lindex $bbox 1]+[lindex $bbox 3])/2)]

    $proofwin create text $x $y -anchor w \
	-tags "$path $path.label $path.label$label label [ancestors $path $top .desc] label.$label" \
	-text $label
    update-color $proofwin $path $path.label$label
    
    frame .sequent$label
    toplevel $seqwin -class Sequent -borderwidth 2
    frame $seqwin.fr
    pack $seqwin.fr -expand yes -fill both
    text $seqwin.fr.text -bd 2 -relief raised -height 2 -width 80 -setgrid true
    set text [string range $text 1 [expr [string length $text]-2]]
    $seqwin.fr.text insert end $text
    set height [lindex [split [$seqwin.fr.text index end] .] 0]
    $seqwin.fr.text config -state disabled
    if {$height>5} {
	scrollbar $seqwin.fr.s -command "$seqwin.fr.text yview"
	$seqwin.fr.text config -yscrollcommand "$seqwin.fr.s set"
	pack $seqwin.fr.s -fill y -side right
	wm minsize $seqwin 80 2
	wm maxsize $seqwin 2000 2000
	if {$height>[get-option maxHeight]} {
	    set height [get-option maxHeight]
	}
    } else {
	wm minsize $seqwin 80 $height
	wm maxsize $seqwin 2000 2000
    }
    pack $seqwin.fr.text -expand yes -fill both
    button $seqwin.dismiss -text Dismiss -bd 2 -command "destroy .sequent$label"
    pack $seqwin.dismiss -side left -padx 2 -pady 2
    button $seqwin.stick -text Stick -bd 2 -command "stick $seqwin $path"
    pack $seqwin.stick -side left -padx 2 -pady 2
    button $seqwin.help -text Help -bd 2 -command "help-sequent"
    pack $seqwin.help -side right -padx 2 -pady 2
    bind $seqwin <Destroy> "catch {$proofwin delete $path.label$label}"
    bind $seqwin <Destroy> "+catch {unset pathtolabel($path)}"
    bind $seqwin <Destroy> "+after 1 {destroy-sequent $seqwin}"
    wm geometry $seqwin 80x$height
    wm iconname $seqwin {PVS sequent}
    wm title $seqwin "Sequent $label ($seq_label)"

    catch {unset sticky_seqs($seqwin)}
    dag-add-destroy-cb $proofwin $path "destroy-sequent $seqwin"
    set pathtolabel($path) $label
}

proc stick {win path} {
    global sticky_seqs $win pathtolabel
    catch {unset pathtolabel($path)}
    set sticky_seqs($win) 1
    pack forget $win.stick
}

proc destroy-sequent {win} {
    global sticky_seqs $win
    if {! [info exists sticky_seqs($win)]} {
	if {[winfo exists $win]} {
	    catch {destroy [winfo parent $win]}
	}
    }
}

proc path-to-lisp-path {path} {
    lrange [split $path .] 1 end
}

proc my-foreground {win tag color} {
    if {[string match @* $color]} {
	if {![string match */* $color]} {
	    global env
	    set color @$env(PVSPATH)/wish/[string range $color 1 end].xbm
	}
	my-config $win line $tag -fill black
	my-config $win line $tag -stipple $color
	my-config $win text $tag -fill black
	my-config $win text $tag -stipple $color
    } else {
	my-config $win bitmap $tag -foreground $color
	my-config $win line $tag -stipple {}
	my-config $win line $tag -fill $color
	my-config $win text $tag -stipple {}
	my-config $win text $tag -fill $color
    }
}

proc update-color {proofwin path tag} {
    global $path curpath

    if [info exists ${path}(done)] {
	my-foreground $proofwin $tag [get-option doneColor]
	return
    }

    if {[info exists curpath]} {
	if {$path==$curpath} {
	    my-foreground $proofwin $tag [get-option currentColor]
	    $proofwin addtag current-subgoal withtag $tag
	    return
	}
    }

    set seqid [$proofwin find withtag $path.sequent]
    set tags [$proofwin gettags $seqid]
    foreach t $tags {
	if {$t=={current-subgoal}} {
	    my-foreground $proofwin $tag [get-option ancestorColor]
	    $proofwin addtag current-subgoal withtag $tag
	    return
	}
    }

    if [info exists ${path}(tcc)] {
	my-foreground $proofwin $tag [get-option tccColor]
	return
    }
    
    my-foreground $proofwin $tag [get-option displayforeground]
}


proc my-config {win type tag opt val} {
    foreach id [$win find withtag $tag] {
	if {[$win type $id]==$type} {
	    $win itemconfig $id $opt $val
	}
    }
}

proc layout-proof {name theory interactive} {
    if {$interactive} {
	set proofwin .proof.u_$theory-$name-i.fr.c
    } else {
	set proofwin .proof.u_$theory-$name.fr.c
    }
    set top $theory-$name-top
    tree-layout $proofwin $top
    canvas-set-scroll $proofwin
}

proc canvas-set-scroll {win {recenter 0}} {
    global tk_version
    set allbbox [$win bbox all]
    set allbbox [lreplace $allbbox 1 1 [expr [lindex $allbbox 1]-10]]
    set allbbox [lreplace $allbbox 3 3 [expr [lindex $allbbox 3]+10]]
    $win config -scrollregion $allbbox
    update idletasks
    set winwid [winfo width $win]
    set bboxwid [expr [lindex $allbbox 2]-[lindex $allbbox 0]]
    set margin [expr ($winwid-$bboxwid)/2]
    if {$recenter} {
	if {$tk_version >= 4.0} {
	    $win xview scroll [expr -$margin] units
	} else {
	    set den [lindex [$win config -scrollincrement] 4]
	    $win xview [expr -$margin / $den]
	}
    }
}

proc proof-current {name theory relpath} {
    global tk_version
    global curpath
    set proofwin .proof.u_$theory-$name-i.fr.c
    set ptop $theory-$name-top
    set path $theory-$name-$relpath

    if {[info exists curpath]} {
	$proofwin delete current-circle
	$proofwin dtag current-subgoal
	set ancs [ancestors $curpath $ptop]
	unset curpath
	foreach tag $ancs {
	    update-color $proofwin $tag $tag
	}
    }
    if {$relpath!={}} {
	foreach tag [ancestors $path $ptop] {
	    $proofwin addtag current-subgoal withtag $tag
	}
	my-foreground $proofwin current-subgoal [get-option ancestorColor]
	my-foreground $proofwin $path [get-option currentColor]

	set bbox [$proofwin bbox $path.real]
	regexp {^(.*).c} $proofwin whole fr
	set hscroll $fr.bottom.hscroll
	set vscroll $fr.vscroll

	set hget [$hscroll get]
	set vget [$vscroll get]
	if {$tk_version >= 4.0} {
	    set units [lindex [$proofwin config -xscrollincrement] 4]
	} else {
	    set units [lindex [$proofwin config -scrollincrement] 4]
	}

	# allbbox is the size of the entire proof tree
	set allbbox [lindex [$proofwin config -scrollregion] 4]

	set width [winfo width $proofwin]
	set height [winfo height $proofwin]

	if {$tk_version >= 4.0} {
	    set hdiff [expr [lindex $allbbox 2] - [lindex $allbbox 0]]
	    set vdiff [expr [lindex $allbbox 3] - [lindex $allbbox 1]]
	    set left [expr [lindex $hget 0]*$hdiff + [lindex $allbbox 0]]
	    set top [expr [lindex $vget 0]*$vdiff + [lindex $allbbox 1]]
	} else {
	    set left [expr $units*[lindex $hget 2]+[lindex $allbbox 0]]
	    set top [expr $units*[lindex $vget 2]+[lindex $allbbox 1]]
	}
	set right [expr $left+$width]
	set bottom [expr $top+$height]

	if {$tk_version >= 4.0} {
	    if {$height >= $vdiff} {
		$proofwin yview moveto 0
	    } elseif {[lindex $bbox 3]+10>$top} {
		set e [expr double([lindex $bbox 3]+10 - [lindex $allbbox 1] - $height)/$vdiff]
		$proofwin yview moveto $e
	    } elseif {[lindex $bbox 1]-10<$bottom} {
		set e [expr double([lindex $bbox 3]+10 - [lindex $allbbox 1] - $height)/$vdiff]
		$proofwin yview moveto $e
	    }
	    if {$width >= $hdiff} {
		$proofwin xview moveto -0.5
	    } elseif {[lindex $bbox 2]+10>$right} {
		set e [expr ([lindex $bbox 2] + 10 - [lindex $allbbox 0] - (.5 * $width))/$hdiff]
		$proofwin xview moveto $e
	    } elseif {[lindex $bbox 0]-10<$left} {
		set e [expr double([lindex $bbox 2]+ 10 - [lindex $allbbox 0] - (.5 * $width))/$hdiff]
		$proofwin xview moveto $e
	    }
	} else {		      
	    if {[lindex $bbox 3]+10>$bottom} {
		set e [expr ([lindex $bbox 3]+10-$height-[lindex $allbbox 1])/$units]
		$proofwin yview $e
	    } elseif {[lindex $bbox 1]-10<$top} {
		set e [expr ([lindex $bbox 1]-10-[lindex $allbbox 1])/$units]
		$proofwin yview $e
	    }
	}

	if {$tk_version >= 4.0} {
	    if {[lindex $bbox 2]+10>$right} {
		set e [expr ([lindex $bbox 2] + 10 - [lindex $allbbox 0] - (.5 * $width))/$hdiff]
		$proofwin xview moveto $e
	    } elseif {[lindex $bbox 0]-10<$left} {
		set e [expr double([lindex $bbox 2]+ 10 - [lindex $allbbox 0] - (.5 * $width))/$hdiff]
		$proofwin xview moveto $e
	    }
	} else {
	    if {[lindex $bbox 2]+10>$right} {
		set e [expr ([lindex $bbox 2]+10-$width-[lindex $allbbox 0])/$units]
		$proofwin xview $e
	    } elseif {[lindex $bbox 0]-10<$left} {
		set e [expr ([lindex $bbox 0]-10-[lindex $allbbox 0])/$units]
		$proofwin xview $e
	    }
	}		      

	set pwid [expr [lindex $bbox 2]-[lindex $bbox 0]]
	set phit [expr [lindex $bbox 3]-[lindex $bbox 1]]

	if {[parse-bool [get-option circleCurrent]]} {
	    $proofwin create oval \
		[expr [lindex $bbox 0]-$pwid/2.8] \
	    [expr [lindex $bbox 1]-$phit/2.8] \
	    [expr [lindex $bbox 2]+$pwid/2.8] \
	    [expr [lindex $bbox 3]+$phit/2.8] \
	    -tags "$path $path.outline current-circle [ancestors $path $ptop .desc]" \
	    -outline [get-option currentColor] \
	    -width 2
    }
    
    set curpath $path
    }
}

proc clear-message {top} {
    if [winfo exists $top] {
	$top.message config -text ""
    }
}

proc show-message {top text} {
    $top.message config -text $text
    after 5000 "clear-message $top"
}

proc gen-ps {top psfile landscape} {
    global canvcolors

    set w $top.fr.c

    set bbox [$w bbox all]

    $w postscript \
	-file $psfile \
	-x [lindex $bbox 0] \
	-width [expr [lindex $bbox 2]-[lindex $bbox 0]] \
	-y [lindex $bbox 1] \
	-height [expr [lindex $bbox 3]-[lindex $bbox 1]] \
	-pageheight 8.5i \
	-rotate [expr {$landscape ? "yes" : "no"}]
    show-message $top "Saved PS to $psfile"
}


proc setup-dag-win {title icon PSname win_name class} {
    global tk_version
    reset-options
    catch {destroy $win_name}
    if {$tk_version >= 4.0} {
	set top [toplevel $win_name -width 400 -height 400 \
		     -class $class -bd 2 -relief raised]
    } else {
	set top [toplevel $win_name -geometry 400x400 \
		     -class $class -bd 2 -relief raised]
    }
    pack propagate $top 0
    wm title $top $title
    wm iconname $top $icon
    wm minsize $top 100 100
    wm maxsize $top 10000 10000
    set fr [frame $top.fr]
    pack $fr -expand yes -fill both
    set sbwidth 15
    frame $fr.bottom -width $sbwidth
    if {$tk_version >= 4.0} {
	set c [canvas $fr.c -height 1 -width 1 -bd 1 -relief sunken \
		   -xscrollcommand "$fr.bottom.hscroll set" \
		   -yscrollcommand "$fr.vscroll set" \
		   -xscrollincrement 1 -yscrollincrement 1 \
		   -highlightthickness 4]
    } else {
	set c [canvas $fr.c -height 1 -width 1 -bd 1 -relief sunken \
		   -xscrollcommand "$fr.bottom.hscroll set" \
		   -yscrollcommand "$fr.vscroll set" \
		   -scrollincrement 1]
    }
    create-dag $c
    if {$tk_version >= 4.0} {
	frame $fr.bottom.right -height 18 -width 18 -bd 3 -relief flat
	scrollbar $fr.vscroll -width $sbwidth -bd 1 -relief sunken \
	    -elementborderwidth 3 -highlightthickness 4 \
	    -command "$c yview"
	scrollbar $fr.bottom.hscroll -width $sbwidth -bd 1 -relief sunken \
	    -elementborderwidth 3 -highlightthickness 4 \
	    -command "$c xview" -orient horiz
    } else {
	frame $fr.bottom.right -height 18 -width 25 -bd 3 -relief flat
	scrollbar $fr.vscroll -width $sbwidth -bd 2 -relief sunken \
	    -command "$c yview"
	scrollbar $fr.bottom.hscroll -width $sbwidth -bd 2 -relief sunken \
	    -command "$c xview" -orient horiz
    }
    pack $fr.bottom -side bottom -fill x
    pack $fr.bottom.right -side right
    if {$tk_version >= 4.0} {
	pack $fr.bottom.hscroll -side bottom -fill x
	pack $fr.vscroll -side right -fill y
	pack $c -expand yes -fill both
    } else {
	pack $fr.bottom.hscroll -side bottom -fill x -padx 4 -pady 4
	pack $fr.vscroll -side right -fill y -padx 4 -pady 4
	pack $c -expand yes -fill both -padx 4 -pady 4
    }
    label $top.message -text ""
    pack $top.message -fill x -side bottom
    if {$tk_version >= 4.0} {
	button $top.dismiss -bd 3 -highlightthickness 2 -relief raised \
	    -padx 4 -pady 4 \
	    -text "Dismiss" -command "destroy $win_name"
    } else {
	button $top.dismiss -bd 3 \
	    -padx 4 -pady 4 \
	    -text "Dismiss" -command "destroy $win_name"
    }
    pack $top.dismiss -side left -padx 4 -pady 2
    if {$tk_version >= 4.0} {
	menubutton $top.ps -bd 3 -highlightthickness 2 -relief raised \
	    -padx 4 -pady 5 \
	    -text "Gen PS" -menu $top.ps.menu
    } else {
	menubutton $top.ps -bd 3 \
	    -padx 4 -pady 5 \
	    -text "Gen PS" -menu $top.ps.menu -relief raised
    }
    menu $top.ps.menu
    $top.ps.menu add command -label Portrait -command "gen-ps $top $PSname 0"
    $top.ps.menu add command -label Landscape -command "gen-ps $top $PSname 1"
    pack $top.ps -side left -padx 4 -pady 2
    if {$tk_version >= 4.0} {
	button $top.help -bd 3 -highlightthickness 2 -relief raised \
	    -padx 4 -pady 4 \
	    -text "Help" -command "help-$class"
    } else {
	button $top.help -bd 3 -relief raised \
	    -padx 4 -pady 4 \
	    -text "Help" -command "help-$class"
    }
    pack $top.help -side right -padx 4 -pady 2
    if {$tk_version >= 4.0} {
	menubutton $top.conf -bd 3 -highlightthickness 2 -relief raised \
	    -padx 4 -pady 5 \
	    -text Config -menu $top.conf.menu
    } else {
	menubutton $top.conf -bd 3 -relief raised \
	    -padx 4 -pady 5 \
	    -text Config -menu $top.conf.menu
    }
    pack $top.conf -side right -padx 4 -pady 2
    menu $top.conf.menu
    $top.conf.menu add cascade -label "Horiz. Separation" \
	-menu $top.conf.menu.sepx
    $top.conf.menu add cascade -label "Vert. Separation" \
	-menu $top.conf.menu.sepy
    menu $top.conf.menu.sepx
    menu $top.conf.menu.sepy
    foreach i {5 10 20 50 100 200} {
	if {$tk_version >= 4.0} {
	    $top.conf.menu.sepx add command -label $i \
		-command "option add Pvs$top*xSep $i; dagwin-layout $c"
	    $top.conf.menu.sepy add command -label $i \
		-command "option add Pvs$top*ySep $i; dagwin-layout $c"
	} else {
	    $top.conf.menu.sepx add command -label $i \
		-command "option add Tk$top*xSep $i; dagwin-layout $c"
	    $top.conf.menu.sepy add command -label $i \
		-command "option add Tk$top*ySep $i; dagwin-layout $c"
	}
    }
    if {$tk_version >= 4.0} {
	$top.conf.menu.sepx add command -label Custom... \
	    -command "make-setter $top x"
	$top.conf.menu.sepy add command -label Custom... \
	    -command "make-setter $top y"
    } else {
	$top.conf.menu.sepx add command -label Custom... \
	    -command "make-setter $top x"
	$top.conf.menu.sepy add command -label Custom... \
	    -command "make-setter $top y"
    }
    set-dag-window-options $win_name
    return $c
}

proc make-setter {top orient} {
    global tk_version
    set win $top.${orient}Sep
    catch {destroy $win}

    toplevel $win -class Setter

    if {$orient=={x}} {
	wm title $win "Horizontal separation"
	wm iconname $win "Horiz sep"
    } else {
	wm title $win "Vertical separation"
	wm iconname $win "Vert sep"
    }

    label $win.lab -text Separation:
    pack $win.lab -side left
    entry $win.ent -width 10 -relief sunken
    if {$tk_version >= 4.0} {
	bind $win.ent <Return> "option add Pvs$top*${orient}Sep \[%W get\]; dagwin-layout $top.fr.c; destroy $win"
    } else {
	bind $win.ent <Return> "option add Tk$top*${orient}Sep \[%W get\]; dagwin-layout $top.fr.c; destroy $win"
    }
    pack $win.ent -side left
    focus $win.ent
}


# Proof Command Support

proc show-proof-commands {commands} {
    show-prover-commands $commands
}

proc show-prover-commands {commands} {
    global tk_version
    set win .prover-commands
    catch {destroy $win}
    reset-options
    toplevel $win -relief flat
    wm maxsize $win 2000 2000
    frame $win.fr
    button $win.fr.dismiss -text Dismiss -bd 3 -command "destroy $win"
    pack $win.fr.dismiss -side left -padx 2 -pady 2
    button $win.fr.help -text Help -bd 3 -command "help-commands-window"
    pack $win.fr.help -side right -padx 2 -pady 2
    pack $win.fr -side bottom -padx 2 -pady 2 -fill x
    if {$tk_version >= 4.0} {
	scrollbar $win.scrollbar -command "$win.text yview" -width 15 \
	    -elementborderwidth 3 -highlightthickness 4 -bd 1 -relief sunken
    } else {
	scrollbar $win.scrollbar -command "$win.text yview" -width 15 \
	    -bd 2 -relief sunken
    }
    if {$tk_version >= 4.0} {
	pack $win.scrollbar -side right -fill y
    } else {
	pack $win.scrollbar -side right -fill y -padx 4 -pady 4
    }
    if {$tk_version >= 4.0} {
	listbox $win.text -selectmode single -bd 1 -relief sunken \
	    -height 25 -width 27 -highlightthickness 4 \
	    -font [get-option displayfont] \
	    -yscrollcommand "$win.scrollbar set"
    } else {
	listbox $win.text -bd 1 -relief sunken -geometry 25x27 \
	    -font [get-option displayfont] \
	    -yscrollcommand "$win.scrollbar set"
    }
    bind $win.text <Enter> "focus %W"
    bind $win.text <Motion> "prover-command-select %W %y"
    if {$tk_version >= 4.0} {
	bind $win.text <Leave> "%W selection clear 0 end"
    } else {
	bind $win.text <Leave> "%W select clear"
    }
    if {$tk_version >= 4.0} {
	bind $win.text <Button-1> {
	    send-command %W %y
	    break
	}
    } else {
	bind $win.text <Button-1> "send-command %W %y"
    }
    bind $win.text <Shift-Button-1> "nop"
    bind $win.text <B1-Motion> "nop"
    bind $win.text <Shift-B1-Motion> "nop"
    bind $win.text <Button-2> "help-command %W %y"
    bind $win.text <B2-Motion> "nop"
    bind $win.text <Button-3> "help-strategy %W %y"
    if {$tk_version >= 4.0} {
	bind $win.text <space> {
	    prover-commands-page-down %y
	    break
	}
    } else {
	bind $win.text <space> "prover-commands-page-down %y"
    }
    bind $win.text d "prover-commands-page-down %y"
    bind $win.text <Delete> "prover-commands-page-up %y"
    bind $win.text u "prover-commands-page-up %y"
    bind $win.text c "send-command %W %y"
    bind $win.text h "help-command %W %y"
    bind $win.text s "help-strategy %W %y"
    if {$tk_version < 4.0} {
	tk_listboxSingleSelect $win.text
    }
    pack $win.text -side left -fill both -expand 1 -padx 4 -pady 4
    foreach cmd $commands {
	$win.text insert end $cmd
    }
    set-prover-commands-options
    wm iconname $win {PVS Prover Commands}
    wm title $win "PVS Prover Commands"
}

proc set-prover-commands-options {} {
    set-prover-commands-windowbackground [get-option windowbackground]
    set-prover-commands-displaybackground [get-option displaybackground]
    set-prover-commands-displayforeground [get-option displayforeground]
    set-prover-commands-activedisplaybackground \
	[get-option activedisplaybackground]
    set-prover-commands-activedisplayforeground \
	[get-option activedisplayforeground]
    set-prover-commands-displayfont [get-option displayfont]
    set-prover-commands-buttonbackground [get-option buttonbackground]
    set-prover-commands-buttonforeground [get-option buttonforeground]
    set-prover-commands-activebuttonbackground \
	[get-option activebuttonbackground]
    set-prover-commands-activebuttonforeground \
	[get-option activebuttonforeground]
    set-prover-commands-troughcolor [get-option troughcolor]
    set-prover-commands-buttonfont [get-option buttonfont]
}

proc set-prover-commands-windowbackground {color} {
    global tk_version
    .prover-commands config -background $color
    .prover-commands.fr config -background $color
    if {$tk_version >= 4.0} {
	.prover-commands.text config -highlightbackground $color
	.prover-commands.text config -highlightcolor $color
	.prover-commands.scrollbar config -highlightbackground $color
    }
}

proc set-prover-commands-displaybackground {color} {
    .prover-commands.text config -background $color
}

proc set-prover-commands-displayforeground {color} {
    .prover-commands.text config -foreground $color
}

proc set-prover-commands-activedisplaybackground {color} {
    .prover-commands.text config -selectbackground $color
}

proc set-prover-commands-activedisplayforeground {color} {
    .prover-commands.text config -selectforeground $color
}

proc set-prover-commands-displayfont {font} {
    .prover-commands.text config -font $font
}

proc set-prover-commands-buttonbackground {color} {
    global tk_version
    if {$tk_version >= 4.0} {
	.prover-commands.scrollbar config -background $color
    } else {
	.prover-commands.scrollbar config -foreground $color
    }
    foreach btn [winfo children .prover-commands.fr] {
	$btn config -background $color
    }
}

proc set-prover-commands-buttonforeground {color} {
    foreach btn [winfo children .prover-commands.fr] {
	$btn config -foreground $color
    }
}

proc set-prover-commands-activebuttonbackground {color} {
    global tk_version
    if {$tk_version >= 4.0} {
	.prover-commands.scrollbar config -activebackground $color
    }
    foreach btn [winfo children .prover-commands.fr] {
	$btn config -activebackground $color
    }
}

proc set-prover-commands-activebuttonforeground {color} {
    foreach btn [winfo children .prover-commands.fr] {
	$btn config -activeforeground $color
    }
}

proc set-prover-commands-troughcolor {color} {
    global tk_version
    if {$tk_version >= 4.0} {
	.prover-commands.scrollbar config -troughcolor $color
	foreach btn [winfo children .prover-commands.fr] {
	    $btn config -highlightbackground $color
	}
    } else {
	.prover-commands.scrollbar config -background $color
    }
}

proc set-prover-commands-buttonfont {font} {
    foreach btn [winfo children .prover-commands.fr] {
	$btn config -font $font
    }
}


proc prover-command-select {win y} {
    global tk_version
    if {$tk_version >= 4.0} {
	$win selection clear 0 end
	$win selection set [$win nearest $y]
    } else {
	$win select from [$win nearest $y]
    }
}

proc prover-commands-page-down {y} {
    global tk_version
    set val [.prover-commands.scrollbar get]
    if {$tk_version >= 4.0} {
	set first [lindex $val 0]
	set last [lindex $val 1]
	set nfirst $last
	set nlast [expr $last + $last - $first]
	if {$nlast > 1.0} {
	    .prover-commands.text yview moveto 1.0
	} else {
	    .prover-commands.text yview moveto $nfirst
	}
	prover-command-select .prover-commands.text $y
    } else {
	set v0 [lindex $val 0]
	set v1 [lindex $val 1]
	set v2 [lindex $val 2]
	set v3 [lindex $val 3]
	set nv3 [min $v3 [expr $v0 - $v1]]
	set cur [lindex [.prover-commands.text curselection] 0]
	set ny [expr $nv3 + $cur - $v2]
	.prover-commands.scrollbar set $v0 $v1 $nv3 [expr $nv3 + $v1 -1]
	.prover-commands.text yview $nv3
	.prover-commands.text select from $ny
    }
}

proc prover-commands-page-up {y} {
    global tk_version
    set val [.prover-commands.scrollbar get]
    if {$tk_version >= 4.0} {
	set first [lindex $val 0]
	set last [lindex $val 1]
	set nfirst [expr $first - ($last - $first)]
	if {$nfirst < 0.0} {
	    .prover-commands.text yview moveto 0.0
	} else {
	    .prover-commands.text yview moveto $nfirst
	}
	prover-command-select .prover-commands.text $y
    } else {
	set v0 [lindex $val 0]
	set v1 [lindex $val 1]
	set v2 [lindex $val 2]
	set v3 [lindex $val 3]
	set nv2 [max 0 [expr $v2 - $v1 + 1]]
	set cur [lindex [.prover-commands.text curselection] 0]
	set ny [expr $cur - $v2 + $nv2]
	.prover-commands.scrollbar set $v0 $v1 $nv2 [expr $nv2 + $v1 - 1]
	.prover-commands.text yview [expr $nv2]
	.prover-commands.text select from $ny
    }
}

proc send-command {win y} {
    set index [$win nearest $y]
    set cmd [$win get $index]
    emacs-evaln "(progn (switch-to-lisp t t) \
                   (goto-char (point-max)) (pvs-prover-any-command \"$cmd\"))"
}

proc help-command {win y} {
    set index [$win nearest $y]
    set cmd [$win get $index]
    emacs-evaln "(help-pvs-prover-command \"$cmd\")"
}

proc help-strategy {win y} {
    set index [$win nearest $y]
    set cmd [$win get $index]
    emacs-evaln "(help-pvs-prover-strategy \"$cmd\")"
}


proc show-declaration {id width height decl} {
    set win .declaration-$id
    catch {destroy $win}
    toplevel $win -relief raised -bd 2
    text $win.text -width $width -height $height
    $win.text insert end $decl
    button $win.dismiss -text Dismiss -command "destroy $win"
    pack $win.text -side top
    pack $win.dismiss -side left -padx 2 -pady 2
    wm iconname $win {PVS Declaration}
    wm title $win "Declaration $id"
}

    

# Theory hierarchy support

proc module-hierarchy {name file directory dag} {
    catch {frame .th-hier}
    # put the u_ in in case $name starts with an uppercase letter
    set thwin .th-hier.u_$name
    set win \
	[setup-dag-win \
	     "Theory hierarchy for $name in $directory$file" \
	     "Theory Hierarchy" \
	     $directory${name}_hier.ps \
	     $thwin \
	     TheoryHierarchy]
    dag-bind-move $win {} Control 1 both
    $win bind :theory <Enter> "module-highlight $win"
    $win bind :theory <Leave> "module-unhighlight $win"
    $win bind :theory <1> "select-theory $win"
    foreach item $dag {
	set th [lindex $item 0]
	set succs [lindex $item 1]
	$win create text 0 0 -text $th -tags "$th $th.real :theory" -anchor n
	dag-add-item $win $th $succs {}
    }
    $win lower dagline
    dag-layout $win $name
    canvas-set-scroll $win 1
}
    
proc select-theory {win} {
    upvar #0 dag-$win dag

    set id [$win find withtag current]
    set item $dag(idtotag,$id)

    emacs-eval "(find-theory \"$item\")"
}

proc module-highlight {win} {
    upvar #0 dag-$win dag
    global tk_version
    global mono

    set id [$win find withtag current]
    set item $dag(idtotag,$id)

    $win dtag :hier_highlight
    $win addtag :hier_highlight withtag $item
    $win addtag :hier_highlight withtag dagline.to$item
    $win addtag :hier_highlight withtag dagline.from$item

    if {$tk_version >= 4.0} {
	if {[winfo depth .]==1 || $mono} {
	    my-foreground $win :hier_highlight @gray
	} else {
	    my-foreground $win :hier_highlight \
		[get-option activedisplayforeground]
	}
    } else {
	if {[tk colormodel .]!={color} || $mono} {
	    my-foreground $win :hier_highlight @gray
	} else {
	    my-foreground $win :hier_highlight \
		[get-option activedisplayforeground]
	}
    }
}

proc module-unhighlight {win} {
    my-foreground $win :hier_highlight [get-option displayforeground]
}


# Called from wish.lisp to set up a proof

proc setup-proof {name theory directory counter interactive} {
    global curpath
    catch {frame .proof}
    if {$interactive} {
	set win .proof.u_$theory-$name-i
    } else {
	set win .proof.u_$theory-$name
    }
    set pw \
	[setup-dag-win \
	     "Proof of $name in $theory" \
	     "PVS Proof" \
	     $directory${theory}_$name.ps \
	     $win \
	     Proof]
    if {$interactive} {
	if {[info exists curpath]} {
	    unset curpath
	}
	set proofwin $pw
    }

    dag-bind-move $pw .desc Control 1 to
    dag-bind-move $pw {} Control 2 both
    if {$interactive} {
	$pw bind sequent <1> "show-sequent $pw $theory-$name-top"
    } else {
	set text "Sequent is only available for interactive proofs"
	$pw bind sequent <1> "show-message $win \"$text\""
    }
    $pw bind rule <1> "get-full-rule $pw $theory-$name-top"
    bind $pw <Destroy> "+pvs-send {(stop-displaying-proof $counter)}"
    bind $pw <Destroy> {+
	foreach kid [winfo children .] {
	    if {[string match .sequent* $kid]} {
		destroy $kid
	    }
	}
    }
}

proc reset-options {} {
    global tk_version
    global mono
    option clear
    if {$tk_version >= 4.0} {
	set pvs [winfo name .]
	option add $pvs.displayfont \
	    -adobe-courier-bold-r-normal--12-120-75-75-m-70-iso8859-1 \
	    startupFile
	option add $pvs.buttonfont \
	    -adobe-courier-bold-r-normal--10-100-75-75-m-60-iso8859-1 \
	    startupFile
	if {[winfo depth .] > 1 && !$mono} {
	    option add $pvs.windowbackground wheat startupFile
	    option add $pvs.displaybackground white startupFile
	    option add $pvs.displayforeground black startupFile
	    option add $pvs.activedisplaybackground mediumslateblue startupFile
	    option add $pvs.activedisplayforeground red startupFile
	    option add $pvs.buttonbackground lightblue startupFile
	    option add $pvs.buttonforeground black startupFile
	    option add $pvs.activebuttonbackground steelblue startupFile
	    option add $pvs.activebuttonforeground white startupFile
	    option add $pvs.troughcolor sienna3 startupFile
	    # sequent colors
	    option add $pvs.currentColor DarkOrchid startupFile
	    option add $pvs.circleCurrent yes startupFile
	    option add $pvs.tccColor green4 startupFile
	    option add $pvs.doneColor blue startupFile
	    option add $pvs.ancestorColor firebrick startupFile
	} else {
	    option add $pvs.windowbackground white startupFile
	    option add $pvs.displaybackground white startupFile
	    option add $pvs.displayforeground black startupFile
	    option add $pvs.activedisplaybackground black startupFile
	    option add $pvs.activedisplayforeground @gray startupFile
	    option add $pvs.buttonbackground white startupFile
	    option add $pvs.buttonforeground black startupFile
	    option add $pvs.activebuttonbackground black startupFile
	    option add $pvs.activebuttonforeground white startupFile
	    option add $pvs.troughcolor black startupFile
	    # sequent colors
	    option add $pvs.currentColor black startupFile
	    option add $pvs.circleCurrent yes startupFile
	    option add $pvs.tccColor black startupFile
	    option add $pvs.doneColor @gray startupFile
	    option add $pvs.ancestorColor black startupFile
    }
	option add $pvs.abbrevLen 35 startupFile
	option add $pvs.maxHeight 30 startupFile
	option add $pvs*proof*xSep 10 startupFile
	option add $pvs*proof*ySep 20 startupFile
	option add $pvs*th-hier*xSep 50 startupFile
	option add $pvs*th-hier*ySep 100 startupFile
    } else {
	option add Tk.displayfont \
	    -adobe-courier-bold-r-normal--12-120-75-75-m-70-iso8859-1 \
	    startupFile
	option add Tk.buttonfont \
	    -adobe-courier-bold-r-normal--10-100-75-75-m-60-iso8859-1 \
	    startupFile
	if {[tk colormodel .]=={color} && !$mono} {
	    option add Tk.windowbackground wheat startupFile
	    option add Tk.displaybackground white startupFile
	    option add Tk.displayforeground black startupFile
	    option add Tk.activedisplaybackground mediumslateblue startupFile
	    option add Tk.activedisplayforeground red startupFile
	    option add Tk.buttonbackground lightblue startupFile
	    option add Tk.buttonforeground black startupFile
	    option add Tk.activebuttonbackground steelblue startupFile
	    option add Tk.activebuttonforeground white startupFile
	    option add Tk.troughcolor sienna3 startupFile
	    # sequent colors
	    option add Tk.currentColor DarkOrchid startupFile
	    option add Tk.circleCurrent yes startupFile
	    option add Tk.tccColor green4 startupFile
	    option add Tk.doneColor blue startupFile
	    option add Tk.ancestorColor firebrick startupFile
	} else {
	    option add Tk.windowbackground white startupFile
	    option add Tk.displaybackground white startupFile
	    option add Tk.displayforeground black startupFile
	    option add Tk.activedisplaybackground black startupFile
	    option add Tk.activedisplayforeground @gray startupFile
	    option add Tk.buttonbackground white startupFile
	    option add Tk.buttonforeground black startupFile
	    option add Tk.activebuttonbackground black startupFile
	    option add Tk.activebuttonforeground white startupFile
	    option add Tk.troughcolor black startupFile
	    # sequent colors
	    option add Tk.currentColor black startupFile
	    option add Tk.circleCurrent yes startupFile
	    option add Tk.tccColor black startupFile
	    option add Tk.doneColor @gray startupFile
	    option add Tk.ancestorColor black startupFile
	}
	option add Tk.abbrevLen 35 startupFile
	option add Tk.maxHeight 30 startupFile
	option add Tk*proof*xSep 10 startupFile
	option add Tk*proof*ySep 20 startupFile
	option add Tk*th-hier*xSep 50 startupFile
	option add Tk*th-hier*ySep 100 startupFile
    }
}

proc get-option {opt {win .}} {
    set cap [string toupper [string range $opt 0 0]][string range $opt 1 end]
    option get $win $opt $cap
}

proc set-dag-window-options {win} {
    set-dag-window-windowbackground $win [get-option windowbackground]
    set-dag-window-displaybackground $win [get-option displaybackground]
    set-dag-window-buttonbackground $win [get-option buttonbackground]
    set-dag-window-buttonforeground $win [get-option buttonforeground]
    set-dag-window-activebuttonbackground $win [get-option activebuttonbackground]
    set-dag-window-activebuttonforeground $win [get-option activebuttonforeground]
    set-dag-window-troughcolor $win [get-option troughcolor]
    set-dag-window-buttonfont $win [get-option buttonfont]
}

proc set-dag-window-windowbackground {win color} {
    global tk_version
    $win config -background $color
    $win.fr config -background $color
    $win.fr.bottom config -background $color
    $win.fr.bottom.right config -background $color
    $win.message config -background $color
    if {$tk_version >= 4.0} {
	$win.fr.c config -highlightbackground $color
	$win.fr.bottom.hscroll config -highlightbackground $color
	$win.fr.vscroll config -highlightbackground $color
    }
}

proc set-dag-window-displaybackground {win color} {
    $win.fr.c config -background $color
}

proc set-dag-window-buttonbackground {win color} {
    global tk_version
    if {$tk_version >= 4.0} {
	$win.fr.vscroll config -background $color
	$win.fr.bottom.hscroll config -background $color
    } else {
	$win.fr.vscroll config -foreground $color
	$win.fr.bottom.hscroll config -foreground $color
    }
    foreach ch [winfo children $win] {
	set cl [winfo class $ch]
	if {$cl == "Button" || $cl == "Menubutton"} {
	    $ch config -background $color
	}
    }
}

proc set-dag-window-buttonforeground {win color} {
    foreach ch [winfo children $win] {
	set cl [winfo class $ch]
	if {$cl == "Button" || $cl == "Menubutton"} {
	    $ch config -foreground $color
	}
    }
}

proc set-dag-window-activebuttonbackground {win color} {
    global tk_version
    if {$tk_version >= 4.0} {
	$win.fr.vscroll config -activebackground $color
	$win.fr.bottom.hscroll config -activebackground $color
    }
    foreach ch [winfo children $win] {
	set cl [winfo class $ch]
	if {$cl == "Button" || $cl == "Menubutton"} {
	    $ch config -activebackground $color
	}
    }
}

proc set-dag-window-activebuttonforeground {win color} {
    foreach ch [winfo children $win] {
	set cl [winfo class $ch]
	if {$cl == "Button" || $cl == "Menubutton"} {
	    $ch config -activeforeground $color
	}
    }
}

proc set-dag-window-troughcolor {win color} {
    global tk_version
    if {$tk_version >= 4.0} {
	$win.fr.vscroll config -troughcolor $color
	$win.fr.bottom.hscroll config -troughcolor $color
	foreach ch [winfo children $win] {
	    set cl [winfo class $ch]
	    if {$cl == "Button" || $cl == "Menubutton"} {
		$ch config -highlightbackground $color
	    }
	}
    } else {
	$win.fr.vscroll config -background $color
	$win.fr.bottom.hscroll config -background $color
    }
}

proc set-dag-window-buttonfont {win font} {
    foreach ch [winfo children $win] {
	set cl [winfo class $ch]
	if {$cl == "Button" || $cl == "Menubutton"} {
	    $ch config -font $font
	}
    }    
}

proc parse-bool {str} {
    set val 0
    catch {
	if "{$str}" {
	    set val 1
	} 
    }

    return $val
}

proc help-Proof {} {
    set win .proverhelp
    catch {destroy $win}
    toplevel $win -relief raised -bd 2
    message $win.text -aspect 390 -text "This is a proof display.  The turnstiles represent sequents, and are connected by proof commands.

The mouse has the following bindings:

Left on turnstile - display the corresponding sequent
Left on rule      - expand the rule if not already expanded
C-Left on turnstile or rule - moves the subtree rooted at that pair
C-Middle on turnstile or rule - moves the turnstile/rule pair only"
    button $win.dismiss -text Dismiss -command "destroy $win"
    pack $win.text -side top
    pack $win.dismiss -side left -padx 2 -pady 2
    wm iconname $win {PVS help prooftree}
    wm title $win "Prooftree Help"
}

proc help-TheoryHierarchy {} {
    set win .hierarchyhelp
    catch {destroy $win}
    toplevel $win -relief raised -bd 2
    message $win.text -aspect 390 -text "This is a theory hierarchy display.  Each name represents a theory, and the arcs represent IMPORTINGs between theories, where the imported theory is below the importing theory.

The mouse has the following bindings:

Left on theory name - display that theory in an Emacs buffer
C-Left on theory name - moves the name"
    button $win.dismiss -text Dismiss -command "destroy $win"
    pack $win.text -side top
    pack $win.dismiss -side left -padx 2 -pady 2
    wm iconname $win {PVS help hierarchy}
    wm title $win "Theory Hierarchy Help"
}

proc help-sequent {} {
    set win .sequenthelp
    catch {destroy $win}
    toplevel $win -relief raised -bd 2
    message $win.text -aspect 390 -text "This is a sequent display.
The titlebar should give a sequent number along with the formula name.
The sequent number is associated with the corresponding number next to
one of the sequents of the proof tree; it may not be visible.

The Dismiss button removes the sequent window.  The window is also
removed when the proof tree is modified so that the associated sequent
no longer exists.  The Stick button causes the sequent to remain even in
this case.  When the stick button is depressed, it disappears."
    button $win.dismiss -text Dismiss -command "destroy $win"
    pack $win.text -side top
    pack $win.dismiss -side left -padx 2 -pady 2
    wm iconname $win {PVS help sequent}
    wm title $win "Sequent Help"
}


proc help-commands-window {} {
    set win .commands-help
    catch {destroy $win}
    toplevel $win -relief raised -bd 2
    message $win.text -aspect 390 -text \
	"This displays all of the prover commands, including user-defined ones.

The following mouse and key bindings are available:

Space, d  - page down
Delete, u - page up
Left, c   - sends selected command to the prover window
Middle, h - provides help for selected command
Right, s  - provides strategy description for selected command"
    button $win.dismiss -text Dismiss -command "destroy $win"
    pack $win.text -side top
    pack $win.dismiss -side left -padx 2 -pady 2
    wm iconname $win {PVS Command Help}
    wm title $win "Prover Command Help"
}


proc nop {} {
}

proc min {x y} {
    if {$x < $y} then {return $x} else {return $y}
}

proc max {x y} {
    if {$x < $y} then {return $y} else {return $x}
}

proc allinfo {win} {
    ppr $win
    foreach c [winfo children $win] {
	allinfo $c
    }
}

proc ppr {win} {
    puts "\n$win"
    foreach opt [$win config] {
	puts "  $opt"
    }
}

proc tkcatch {script {varname novar}} {
    global $varname
    catch {rename tkerror tkerror.orig}
    proc tkerror {err} {error $err}
    set value [eval catch {$script} $varname]
    rename tkerror {}
    catch {rename tkerror.orig tkerror}
    return $value
}
