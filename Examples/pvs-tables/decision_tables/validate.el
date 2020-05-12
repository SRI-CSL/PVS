(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Totals for tablewise.pvs: 5 proofs, 5 attempted, 2 succeeded")
 (pvs-message "Validating %s" default-directory)
 (pvs-validate-typecheck-and-prove "tablewise")
 (pvs-message "Done validating - exiting"))
