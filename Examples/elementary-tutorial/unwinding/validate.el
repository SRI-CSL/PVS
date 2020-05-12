(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Totals for unwinding.pvs: 7 proofs, 7 attempted, 7 succeeded")
 (pvs-message "Validating %s" default-directory)
 (pvs-validate-typecheck-and-prove "unwinding")
 (pvs-message "Done validating - EXPECT all to prove"))
