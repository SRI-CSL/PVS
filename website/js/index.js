/* global document */
function googlesearch(query) {
    "use strict";
    query.q.value = "site:pvs.csl.sri.com " + document.getElementById("search_input").value;
}
function googlescholarsearch(query) {
    "use strict";
    query.q.value = document.getElementById("search_pvs").value + "&" + document.getElementById("search_date").value;
}
function onClick(el) {
    "use strict";
    el.form.target = '_blank';
}
function usr_click(id) {
    "use strict";
    document.getElementById(id).click();
}
