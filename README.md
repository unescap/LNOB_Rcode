use_version and validationfile added as parameters in run_together
when use_version not 1, requires validation file
will check if overall mean calculated by the program is within error limit (1%) with the validated value. will stop if it is.
only when use_version is 1, one can add and create validation for new survey/inidcators.
