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

    set dag(succs,$tag) $succs

    foreach item [$win find withtag $tag] {
	set dag(idtotag,$item) $tag
	$win addtag dag-item withtag $item
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
		 -fill [get-option otherColor] \
		 -width 0]
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
	return $path$suffix
    } else {
	regexp {^(.*)\.([^.]+)$} $path whole pre post
	return [concat $path$suffix [ancestors $pre $suffix]]
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

    my-foreground $proofwin $path [get-option doneColor]
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
		 -anchor n \
		 -foreground [get-option otherColor]]
	if [info exists ${path}(rule)] {
	    set bbox [$proofwin bbox $seq]
	    set seqbot [lindex $bbox 3]
	    set linebot [expr $seqbot+$proof_y_sep]
	    $proofwin create line 0 $seqbot 0 $linebot \
		-tags "$path.line $path $path.real [ancestors $path .desc]" \
		-fill [get-option otherColor]
	    $proofwin create text 0 $linebot -text [set ${path}(rule)] \
		-tags "$path.rule $path $path.real rule [ancestors $path .desc]" \
		-anchor n \
		-fill [get-option otherColor]
	}
    }


    dag-add-item $proofwin $path [kids $path [set ${path}(kids)]] [ancestors $path .desc]
    if [info exists ${path}(done)] {
	my-foreground $proofwin $path [get-option doneColor]
    } elseif [info exists ${path}(tcc)] {
	my-foreground $proofwin $path [get-option tccColor]
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
    frame $win.fr
    pack $win.fr -expand yes -fill both
    text $win.fr.text -bd 2 -relief raised -height 2 -width 80 -setgrid true
    set text [string range $text 1 [expr [string length $text]-2]]
    $win.fr.text insert end $text
    set height [lindex [split [$win.fr.text index end] .] 0]
    $win.fr.text config -state disabled
    if {$height>5} {
	scrollbar $win.fr.s -command "$win.fr.text yview"
	$win.fr.text config -yscrollcommand "$win.fr.s set"
	pack $win.fr.s -fill y -side right
	wm minsize $win 80 2
	wm maxsize $win 80 $height
	if {$height>[get-option maxHeight]} {
	    set height [get-option maxHeight]
	}
    } else {
	wm minsize $win 80 $height
	wm maxsize $win 80 $height
    }
    pack $win.fr.text -expand yes -fill both
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
    my-foregroundconfig $win $tag -foreground $color
    my-foregroundconfig $win $tag -fill $color
}

proc update-color {path tag} {
    global proofwin $path curpath

    if [info exists ${path}(done)] {
	my-foreground $proofwin $tag [get-option doneColor]
	return
    }

    if {$path==$curpath} {
	my-foreground $proofwin $tag [get-option currentColor]
	$proofwin addtag current-subgoal withtag $tag
	return
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
    
    my-foreground $proofwin $tag [get-option otherColor]
}


proc my-foregroundconfig {win tag opt val} {
    foreach id [$win find withtag $tag] {
	if {!([$win type $id]=={oval} && $opt=={-foreground})} {
	    catch {$win itemconfig $id $opt $val}
	}
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
	$proofwin delete current-circle
	$proofwin dtag current-subgoal
	set ancs [ancestors $curpath]
	set curpath {}
	foreach tag $ancs {
	    update-color $tag $tag
	}
    }
    foreach tag [ancestors $path] {
	$proofwin addtag current-subgoal withtag $tag
    }
    my-foreground $proofwin current-subgoal [get-option ancestorColor]
    my-foreground $proofwin $path [get-option currentColor]

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

    set pwid [expr [lindex $bbox 2]-[lindex $bbox 0]]
    set phit [expr [lindex $bbox 3]-[lindex $bbox 1]]
    
    if {[parse-bool [get-option circleCurrent]]} {
	$proofwin create oval \
	    [expr [lindex $bbox 0]-$pwid/2.8] \
	    [expr [lindex $bbox 1]-$phit/2.8] \
	    [expr [lindex $bbox 2]+$pwid/2.8] \
	    [expr [lindex $bbox 3]+$phit/2.8] \
	    -tags "$path $path.outline current-circle [ancestors $path .desc]" \
	    -outline [get-option currentColor] \
	    -width 2
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

proc setup-proof {name file directory counter} {
    global proof proofwin
    set proof(name) $name
    set proof(file) $file
    set proof(directory) $directory
    set proof(counter) $counter
    catch {destroy .show-proof}
    reset-options
    set top [toplevel .show-proof -geometry 400x400 -class Proof]
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
    dag-bind-move $c .desc Control 1 to
    dag-bind-move $c {} Control 2 both
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
    bind $top <Destroy> "pvs-send {(stop-displaying-proof $counter)}"
    bind $top <Destroy> {+
	foreach kid [winfo children .] {
	    if {[string match .sequent* $kid]} {
		destroy $kid
	    }
	}
    }
}    

proc reset-options {} {
    option clear
    option add Tk.tccColor green4 startupFile
    option add Tk.doneColor blue startupFile
    option add Tk.ancestorColor firebrick startupFile
    option add Tk.otherColor black startupFile

    if {[tk colormodel .]=={color}} {
	option add Tk.currentColor DarkOrchid startupFile
	option add Tk.circleCurrent no startupFile
    } else {
	option add Tk.currentColor black startupFile
	option add Tk.circleCurrent yes startupFile
    }

    option add Tk.maxHeight 30 startupFile
}

proc get-option {opt} {
    set upper [string toupper [string range $opt 0 0]][string range $opt 1 end]

    option get . $opt $upper
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
