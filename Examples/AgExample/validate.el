(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Grand Totals: 277 proofs, 277 attempted, 277 succeeded")
 (pvs-message "Validating %s" default-directory)
 (typecheck "FODL_Language")
 (typecheck "FA_Element")
 (pvs-validate-typecheck-and-prove "SpecProperties" nil t)
 (pvs-message "Done validating - EXPECT 277 out of 277 to prove"))
