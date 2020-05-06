(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Grand Totals: 23 proofs, 23 attempted, 20 succeeded")
 (pvs-message "Validating %s" default-directory)
 (pvs-validate-typecheck-and-prove "phones" nil t)
 (pvs-message "Done validating - EXPECT 20 out of 23 to prove"))
