(pvs-validate
 (validation-log-file)
 "."
 (setq pvs-expected-output
       "Theory ripple_adder totals: 7 formulas, 7 attempted, 7 succeeded")
 (pvs-message "Validating %s" default-directory)
 (pvs-validate-typecheck-and-prove "adder")
 (pvs-message "Done validating - exiting"))
