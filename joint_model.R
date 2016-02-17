# A new joint model run incorporating all data

# The response variables are: Abundance (a), farmland indicator (i_a)
# The model should be Neg-bin with:
# mean: offset(duration)+fixed(day)+fixed(system)+random(region)+random(square in region)
# overdispersion: fixed(system)
# Fit to both winter and breeding season SIMULTANEOUSLY using Stan

# Key question: do dairy farms have more flocking (i.e. more dispersion) in winter?

# Clear the workspace just in case
rm(list=ls())

# Set the working directory
setwd("~/github/birds_od")

# Load in relevant packages
library(rstan)
library(parallel)
#library(reshape2)
#library(ggplot2)

## Load in data
load(file='birds.rda')

##########################

# Code for joint stan model
negbin_r_code = '
data {
  int<lower=1> N; // Number of obs
  int<lower=1> N_region; // Number of regions
  int<lower=1> N_square; // Number of squares per region - luckily this is balanced
  int y[N]; // Counts for response variable
  vector[N] log_dur; // Log duration - offset
  vector[N] day; // day - fixed effect
  vector[N] dairy; // dairy (1) or not (0) - main fixed effect in both mean and overdispersion
  int region[N]; // region - random effect
  int reg_square[N]; // square - random effect within region
  vector[N] winter; // Whether winter or not
}
parameters {
  real alpha; // mean intercept
  real beta_day; // effect of day
  real beta_dairy; // effect of dairy (on mean)
  real beta_winter; // effect of dairy (on mean)
  real beta_day_winter; // interaction between day and winter
  real beta_dairy_winter; // interaction between dairy and winter
  vector[N_region] b_region; // effect of region
  matrix[N_region,N_square] b_square; // effect of square within region
  real phi_alpha; // intercept on OD
  real phi_dairy; // effect of dairy on OD
  real phi_winter; // effect of winter on OD
  real phi_dairy_winter; // Interaction of dairy and winter on OD
  real<lower=0> sigma_region; // variability between regions
  real<lower=0> sigma_square; // variability between squares within regions
}
transformed parameters {
  vector[N] mu;
  vector<lower=0>[N] mu_phi;
  for (i in 1:N) {
    mu[i] <- log_dur[i] + beta_day*day[i] + beta_dairy*dairy[i] + beta_dairy*dairy[i] + beta_winter*winter[i] + beta_day_winter*winter[i]*day[i] + beta_dairy_winter*winter[i]*dairy[i] + b_region[region[i]] + b_square[region[i],reg_square[i]];
    mu_phi[i] <- exp(phi_alpha + phi_dairy*dairy[i] + phi_winter*winter[i] +phi_dairy_winter*winter[i]*dairy[i]);
  }
}
model {
  // Vague parameters on most of these
  alpha ~ normal(0,100);
  beta_day ~ normal(0,100); 
  beta_dairy ~ normal(0,100);
  beta_winter ~ normal(0,100);
  beta_dairy_winter ~ normal(0,100);
  beta_day_winter ~ normal(0,100);
  phi_alpha ~ normal(0,100);
  phi_dairy ~ normal(0,100);
  phi_winter ~ normal(0,100);
  phi_dairy_winter ~ normal(0,100);

  // Random effect priors
  for (j in 1:N_region) {
    b_region[j] ~ normal(alpha,sigma_region);
    for (k in 1:N_square) {
      b_square[j,k] ~ normal(0,sigma_square);
    }
  }
  sigma_region ~ cauchy(0,10);
  sigma_square ~ cauchy(0,10);

  // Likelihood
  y ~ neg_binomial_2_log(mu, mu_phi);

}
'

##########################
# Loop through and run on each response variable

for(i in 1:2) { # Loop through response variables
    # Set up  
    curr_data = birds
    stan_data = list(N=nrow(curr_data),N_region=length(unique(curr_data$region)),N_square=length(unique(curr_data$square2)),y=curr_data[,i],log_dur=log(curr_data$duration),day=curr_data$day,dairy=as.integer(curr_data$system=='Dairy'),region=as.integer(curr_data$region),reg_square=as.integer(curr_data$square2),winter=as.integer(curr_data$season=='winter'))
    stan_pars = c('alpha','beta_day','beta_dairy','beta_winter','beta_day_winter','beta_dairy_winter','phi_alpha','phi_dairy','phi_winter','phi_dairy_winter','sigma_region','sigma_square','b_region','b_square')
    
    # Test fit
    stop()
    # Only need to set up the model the first time
    if(i==1) fit1 = stan(model_code = negbin_r_code, data = stan_data, chains = 4, iter = 10)
    # Proper fit
    fit2 = stan(fit = fit1, data = stan_data, iter=10000, thin=10, chains = 4, pars=stan_pars)
    #plot(fit2,pars=stan_pars[1:12])
    #print(fit2)
    
    # Capture output
    capture.output(print(fit2),file=paste(colnames(birds)[i],'_joint_convergence.txt',sep=''))
    write.csv(extract(fit2,pars=stan_pars),file=paste(colnames(birds)[i],'_joint_pars.csv',sep=''))
    
}

