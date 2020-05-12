(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Totals for mizar.pvs: 24 proofs, 24 attempted, 24 succeeded")
 (pvs-message "Validating %s" default-directory)
 (pvs-validate-typecheck-and-prove "mizar")
 (pvs-message "Done validating - exiting"))
