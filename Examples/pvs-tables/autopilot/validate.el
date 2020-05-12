(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Grand Totals: 10 proofs, 10 attempted, 10 succeeded")
 (pvs-message "Validating %s" default-directory)
 (pvs-validate-typecheck-and-prove "autopilot" nil t "linkedmodes")
 (pvs-message "Done validating - exiting"))
