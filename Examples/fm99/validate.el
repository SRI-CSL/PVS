(pvs-validate
 (validation-log-file)
 "."
 (pvs-message "Validating %s" default-directory)
 (setq pvs-expected-output
       "Grand Totals: 101 proofs, 101 attempted, 89 succeeded")
 (pvs-validate-typecheck-and-prove "top" nil t)
 (pvs-message "Done validating - exiting"))

