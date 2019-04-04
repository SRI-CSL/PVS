function getLastState() { return ws.lastState(); }

function cb(err, res) {
	if (res && !err) {
		ws.lastState(res.data);
		onOutPutUpdated(err, res);
	}
}

document.getElementById("btnUp").onclick = 
function(){
	ws.sendGuiAction("gpca_click_up(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}
document.getElementById("btnDown").onclick = 
function(){
	ws.sendGuiAction("gpca_click_dn(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}
document.getElementById("btnLeft").onclick = 
function(){
	ws.sendGuiAction("gpca_click_lf(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}
document.getElementById("btnRight").onclick = 
function(){
	ws.sendGuiAction("gpca_click_rt(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}
document.getElementById("btnOk").onclick = 
function(){
	ws.sendGuiAction("gpca_click_ok(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}


document.getElementById("btnCancel").onclick = 
function(){
	ws.sendGuiAction("gpca_click_cancel(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}

document.getElementById("btnStop").onclick = 
function(){
	ws.sendGuiAction("gpca_click_stop(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}

document.getElementById("btnEdit").onclick = 
function(){
	ws.sendGuiAction("gpca_click_edit(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}
document.getElementById("btnBolus").onclick = 
function(){
	ws.sendGuiAction("gpca_click_bolus(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}
document.getElementById("btnStart").onclick = 
function(){
	ws.sendGuiAction("gpca_click_start(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}

document.getElementById("btnOn").onclick = 
function(){
	ws.sendGuiAction("gpca_click_on(" + (getLastState()) + ");", cb);
	GUI_ACTION = 1;
}
