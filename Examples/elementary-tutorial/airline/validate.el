(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Grand Totals: 19 proofs, 19 attempted, 19 succeeded")
 (pvs-message "Validating %s" default-directory)
 (pvs-validate-typecheck-and-prove "ops" nil t)
 (pvs-message "Done validating - EXPECT 19 out of 19 to prove"))
