(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Grand Totals: 6 proofs, 6 attempted, 6 succeeded")
 (pvs-message "Validating %s" default-directory)
 (pvs-validate-typecheck-and-prove "pipe" nil t)
 (pvs-message "Done validating - exiting"))
