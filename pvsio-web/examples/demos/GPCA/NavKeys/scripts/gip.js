function GIPlog(msg){
	var c = document.getElementById('GIP-Controller_console');
    label = "Controller state<br>";
    msg = msg.substring(msg.indexOf("controller:=") + 12, msg.indexOf(" tick_enabled:=")).trim();
	msg = msg.substring(0, msg.length - 1); // this removes the final comma, separating the controller state from the ui state
	c.innerHTML = "[ " + (new Date()).toTimeString() + " ]<br>" + label + msg + "<br><br>" + c.innerHTML;
}

function UIlog(msg){
	var c = document.getElementById('GIP-UI_console');
    label = "User interface state<br>";
    msg = msg.substring(msg.indexOf("ui:=") + 4, msg.lastIndexOf("#)")).trim();
	c.innerHTML = "[ " + (new Date()).toTimeString() + " ]<br>" + label + msg + "<br><br>" + c.innerHTML;
}

var ticking = 0;

function start_tick() {
	ctrl_clock = setInterval(ctrl_tick, ctrl_clock_interval);
}

function stop_tick() {
	clearInterval(ctrl_clock);
	ticking = 0;
}

function ctrl_tick() {
	if(tick_enabled == "TRUE") {
		ws.sendGuiAction("gpca_tick(" + (getLastState()) + ");", cb);
	}
	else {
		stop_tick();
	}
}

function run_controller() {
	if(ticking == 0) {
		ticking = 1;
		start_tick();
	}
}
