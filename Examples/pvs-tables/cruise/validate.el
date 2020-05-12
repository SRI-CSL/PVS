(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Totals for cruise.pvs: 20 proofs, 20 attempted, 18 succeeded")
 (pvs-message "Validating %s" default-directory)
 (pvs-validate-typecheck-and-prove "cruise")
 (pvs-message "Done validating - exiting"))
