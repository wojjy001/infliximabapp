# global.R script for infliximabapp
# Objects that are not reactive are written here
# This also a safe place for functions that are then used in server.R
# ------------------------------------------------------------------------------
# Load package libraries
  library(shiny)
  library(shinydashboard)  # Package for making cooler user-interface for Shiny applications
  library(shinyjs)  # Package for perform common JavaScript operations
  library(ggplot2)  # Plotting
  library(grid)   #Plotting
  library(plyr)  # Split and rearrange data, ddply function
  library(dplyr)  # New plyr
  library(rmarkdown)  # Generate report to a Word, pdf or HTML document
  library(mrgsolve) # Metrum differential equation solver for pharmacometrics
# Define a custom ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 16))
# Source model code
  source("model.R")

# ------------------------------------------------------------------------------
# Set the number of individuals that make up the 95% confidence intervals
  n <- 1000
# 95% confidence interval functions
  CI95lo <- function(x) quantile(x,probs = 0.025)
  CI95hi <- function(x) quantile(x,probs = 0.975)
# Set seed for reproducible numbers
  set.seed(123456)

# ------------------------------------------------------------------------------
# Fit individual parameters given the observed concentrations, estimated doses and covariate values
  bayesian.function <- function(input.bayes.data) {
  # Initial parameter estimates
    initial.par <- c(exp(0),exp(0),exp(0),exp(0)) # Population values as initial estimates
    par <- initial.par
	# List of observations
		Yobs.times <- input.bayes.data$time[is.na(input.bayes.data$obs) == FALSE & input.bayes.data$obs != 0]
		Yobs.values <- input.bayes.data$obs[input.bayes.data$time %in% Yobs.times]

	# Function for estimating individual parameters by minimising the Bayesian objective function value
		bayesian.ofv <- function(par) {
			ETA1fit <- log(par[1])  # Bayesian estimated ETA for clearance
			ETA2fit <- log(par[2])  # Bayesian estimated ETA for volume of the central compartment
			ETA3fit <- log(par[3])  # Bayesian estimated ETA for intercompartmental clearance
			ETA4fit <- log(par[4])  # Bayesian estimated ETA for volume of the peripheral compartment
		# List of ETA values that will be optimised
		# These will updated and connected to ERR_X terms in the mrgsolve model code
			input.bayes.data$ETA1 <- ETA1fit
			input.bayes.data$ETA2 <- ETA2fit
			input.bayes.data$ETA3 <- ETA3fit
			input.bayes.data$ETA4 <- ETA4fit
		# Simulate concentration-time profile with nth iteration of ETA values
			bayes.data <- mod %>%
				mrgsim(data = input.bayes.data) %>%
				as.data.frame

		# Use only corresponding predicted concentrations for calculating the log-likelihood
			Yhat <- bayes.data$IPRE[bayes.data$time %in% Yobs.times]  # Make a Yhat vector based on IPRE in bayes.data
		# Posterior component (from the data)
		# Log densities of residuals
		# Residual error model, Y = IPRE*(1+ERR), Y = IPRE + IPRE*ERR
			error <- sqrt(as.matrix(smat(mod)))  # Pull out SIGMA from "mod" - original model code
			loglikpost <- dnorm(Yobs.values,mean = Yhat,sd = Yhat*error,log = T)
		# Prior component (from the model)
			ETA <- c(ETA1fit,ETA2fit,ETA3fit,ETA4fit) # List of Bayesian estimated ETAs
			ETABSV <- as.matrix(omat(mod)) # PPV for model parameters in "mod" - original model code
			ETABSV <- sqrt(c(ETABSV[1,1],ETABSV[2,2],ETABSV[3,3],ETABSV[4,4]))  # Pull out the OMEGAs and convert to SDs
			loglikprior <- dnorm(ETA,mean = 0,sd = ETABSV,log = T)
		# Calculate the combined likelihood
			OFVBayes <- -1*sum(loglikpost,loglikprior)
			OFVBayes
		}

  # Optimise the ETA parameters to minimise the OFVBayes
    resultfit <- optim(par,	# initial estimates
			bayesian.ofv,
			hessian = TRUE,
			method = "L-BFGS-B",
			lower = c(0.001,0.001,0.001,0.001), # lower boundaries for estimates
			upper = c(Inf,Inf,Inf,Inf),	# upper boundaries for estimates
			control = list(parscale = par,factr = 1e7)
		)
  }

# ------------------------------------------------------------------------------
# Extrapolate covariate values between measured time-points
# Linear extrapolation for continuous covariates
	lin.cov.extrap <- function(times,time,last.time,cov) {
		cov.times <- c(time[is.na(cov) == FALSE & cov != 0],last.time)
		cov.values <- c(cov[time %in% cov.times],tail(cov[time %in% cov.times],1))
		cov.func <- approxfun(cov.times,cov.values,method = "linear")
		cov.func(times)
	}
# Constant extrapolation for categorical covariate
	con.cov.extrap <- function(times,time,last.time,cov) {
		cov.times <- c(time,last.time)
		cov.values <- c(cov,tail(cov,1))
		cov.func <- approxfun(cov.times,cov.values,method = "constant")
		cov.func(times)
	}
