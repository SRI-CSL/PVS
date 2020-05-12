(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Totals for simple_tables.pvs: 52 proofs, 52 attempted, 50 succeeded")
 (pvs-validate-typecheck-and-prove "simple_tables")
 (pvs-message "Done validating - exiting"))
