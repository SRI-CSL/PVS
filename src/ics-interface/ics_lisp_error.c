
/* Caml errors sent to Lisp. */

/* add a global variable which is set in init and depending on */
/* its value for ics_init dispatch. */

static void (*lisp_error_function_address)();

void ics_error(char* funname, char* msg) {
  (*lisp_error_function_address)(funname,msg);
}

void register_lisp_error_function(int index) {
  lisp_error_function_address = (void (*)()) lisp_call_address(index);
}
