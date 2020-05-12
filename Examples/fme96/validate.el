(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Totals for half.pvs: 20 proofs, 20 attempted, 17 succeeded")
 (pvs-message "Validating %s" default-directory)
 (pvs-validate-typecheck-and-prove "half")
 (pvs-message "Done validating - exiting"))
