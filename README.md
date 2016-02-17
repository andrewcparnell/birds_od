# Negative binomial modelling of some over-dispersed bird data using stan

The key files are:
- `birds.rda` - The data file used stored as an R data frame
- `joint_model.R` - the R script file to fit a hierarichical model using stan on two different response variables
- `*_joint_convergence.txt` - A file containing the convergence diagnostics for each model run
- `*'_joint_pars.csv` - A file containing the parameter values from the output of each model run
- `process_output.R` - The R script file to take the output of the models and produce the papers
