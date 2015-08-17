function googlesearch(query) {
    query.q.value = "site:pvs.csl.sri.com " + document.getElementById("search_input").value;
}

function onClick(_this) {
    _this.form.target = '_blank';
}
