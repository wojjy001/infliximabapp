# Create demonstration plots for ANZTDM workshop presentation
# ------------------------------------------------------------------------------
# Remove objects in workspace
	rm(list = ls(all = TRUE))
# Load package libraries
  library(ggplot2)  # Plotting
  library(grid)   #Plotting
  library(plyr)  # Split and rearrange data, ddply function
  library(dplyr)  # New plyr
  library(mrgsolve) # Metrum differential equation solver for pharmacometrics
# Define a custom ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 16))
# Set the working directory
	dir <- "/Volumes/Prosecutor/PhD/2017_ANZTDM/infliximabapp/"
	setwd(dir)
# Source model code
  source("model.R")
	# Pull out the OMEGAs from "model.R" and convert to SDs
		ETABSV <- as.matrix(omat(mod)) # PPV for model parameters in "mod" - original model code
		ETABSV <- sqrt(c(ETABSV[1,1],ETABSV[2,2],ETABSV[3,3],ETABSV[4,4]))
	# Pull out SIGMA from "mod" - original model code
		error <- sqrt(as.matrix(smat(mod)))

# ------------------------------------------------------------------------------
# Set the number of individuals that make up the 95% confidence intervals
  nsim <- 100
# 95% confidence interval functions
  CI95lo <- function(x) quantile(x,probs = 0.025)
  CI95hi <- function(x) quantile(x,probs = 0.975)
# Set seed for reproducible numbers
  set.seed(123456)

# ------------------------------------------------------------------------------
# Set up simulation parameters
	times <- seq(from = 0,to = 154,by = 1)
	input.sim.data <- lapply(input.sim.data,rep.int,times = length(times)) %>%
		as.data.frame
	input.sim.data$time <- times

# Dosing information
	dose.times <- c(0,14,42,98)
	input.sim.data$evid[input.sim.data$time %in% dose.times] <- 1
	input.sim.data$rate[input.sim.data$time %in% dose.times] <- -2

# ------------------------------------------------------------------------------
# 1. Compare covariate effects, WT = 70 kg, ALB = 4 g/dL
	# ADA = 0
		input.sim.data.ada0 <- input.sim.data
		input.sim.data.ada0$ADA <- 0
		input.sim.data.ada0$amt[input.sim.data.ada0$time %in% dose.times] <- 350
		sim.data.ada0 <- mod %>%
			mrgsim(data = input.sim.data.ada0,carry.out = c("amt","cmt")) %>%
			as.data.frame
	# ADA turns on at day 98
		input.sim.data.ada1 <- input.sim.data.ada0
		input.sim.data.ada1$ADA[input.sim.data.ada1$time >= 98] <- 1
		sim.data.ada1 <- mod %>%
			mrgsim(data = input.sim.data.ada1,carry.out = c("amt","cmt")) %>%
			as.data.frame
	# Plot the two profiles
		plotobj1 <- NULL
		plotobj1 <- ggplot()
		plotobj1 <- plotobj1 + geom_line(aes(x = time,y = IPRE),
			data = sim.data.ada0,colour = "#3C8DBC",size = 2)
		plotobj1 <- plotobj1 + geom_line(aes(x = time,y = IPRE),
			data = sim.data.ada1,colour = "#EE3B3B",size = 2)
		plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 3),
			linetype = "dashed",size = 1)
		plotobj1 <- plotobj1 + scale_x_continuous("\nTime since first dose (days)",
			breaks = seq(from = 0,to = 154,by = 14),lim = c(98,154))
		plotobj1 <- plotobj1 + scale_y_log10("Infliximab Concentration (mg/L)\n",
			breaks = c(1,3,10,30,100))
		plotobj1

		ggsave(plot = plotobj1,filename = "plot1.png",units = "cm",
			width = 15,height = 10)

# ------------------------------------------------------------------------------
# 2. Compare different values for random effects, WT = 70 kg, ALB = 4 g/dL, ADA = 0
	# ETA1 = -0.5
		input.sim.data.etaneg <- input.sim.data
		input.sim.data.etaneg$ETA1 <- -0.2
		input.sim.data.etaneg$amt[input.sim.data.etaneg$time %in% dose.times] <- 350
		sim.data.etaneg <- mod %>%
			mrgsim(data = input.sim.data.etaneg,carry.out = c("amt","cmt")) %>%
			as.data.frame
	# ETA1 = 0.5
		input.sim.data.etapos <- input.sim.data.etaneg
		input.sim.data.etapos$ETA1 <- 0.2
		sim.data.etapos <- mod %>%
			mrgsim(data = input.sim.data.etapos,carry.out = c("amt","cmt")) %>%
			as.data.frame
	# Plot the two profiles
		plotobj2 <- NULL
		plotobj2 <- ggplot()
		plotobj2 <- plotobj2 + geom_line(aes(x = time,y = IPRE),
			data = sim.data.etaneg,colour = "#F39C12",size = 2)
		plotobj2 <- plotobj2 + geom_line(aes(x = time,y = IPRE),
			data = sim.data.etapos,colour = "#00A65A",size = 2)
		plotobj2 <- plotobj2 + geom_hline(aes(yintercept = 3),
			linetype = "dashed",size = 1)
		plotobj2 <- plotobj2 + scale_x_continuous("\nTime since first dose (days)",
			breaks = seq(from = 0,to = 154,by = 14))
		plotobj2 <- plotobj2 + scale_y_log10("Infliximab Concentration (mg/L)\n",
			breaks = c(1,3,10,30,100,300),lim = c(NA,300))
		plotobj2

		ggsave(plot = plotobj2,filename = "plot2.png",units = "cm",
			width = 15,height = 10)

# ------------------------------------------------------------------------------
# 3. Demonstrate intra-individual variability
	eps <- rnorm(length(times),mean = 0,sd = 0.419)
	input.sim.data.err <- input.sim.data
	input.sim.data.err$ERRPRO <- eps
	input.sim.data.err$amt[input.sim.data.err$time %in% dose.times] <- 350
	sim.data.err <- mod %>%
		mrgsim(data = input.sim.data.err,carry.out = c("amt","cmt")) %>%
		as.data.frame

	sample.times <- c(14,42,98,154)
	# Plot the IPRE and DV profiles
		plotobj3 <- NULL
		plotobj3 <- ggplot()
		plotobj3 <- plotobj3 + geom_line(aes(x = time,y = IPRE),
			data = sim.data.err,colour = "#605CA8",size = 2)
		plotobj3 <- plotobj3 + geom_point(aes(x = time,y = DV),
			data = sim.data.err[sim.data.err$time %in% sample.times,],
			size = 4)
		plotobj3 <- plotobj3 + geom_hline(aes(yintercept = 3),
			linetype = "dashed",size = 1)
		plotobj3 <- plotobj3 + scale_x_continuous("\nTime since first dose (days)",
			breaks = seq(from = 0,to = 154,by = 14))
		plotobj3 <- plotobj3 + scale_y_log10("Infliximab Concentration (mg/L)\n",
			breaks = c(1,3,10,30,100,300),lim = c(NA,300))
		plotobj3

		ggsave(plot = plotobj3,filename = "plot3.png",units = "cm",
			width = 15,height = 10)

# ------------------------------------------------------------------------------
# 4. Recreate dashboard review paper plot of dosing strategies
	# Rather than "exposure" make it trough concentration to be in context with IFX
	# Flat dose = 350 mg
	# Weight-based dosing = 5 mg/kg
	# Stratified dosing: 20 - 30 kg = 150 mg, 31 - 60 kg = 250 mg
	#										 61 - 90 kg = 250 mg, 91 - 120 kg = 450 mg
	# Individualised = based on pharmacokinetic model to achieve target conc

# Set up patient population based on weight
	# ID 1 = 20 kg, 2 = 30 kg, 3 = 40 kg, 4 = 50 kg, 5 = 60 kg, 6 = 70 kg,
	# 7 = 80 kg, 8 = 90 kg, 9 = 100 kg, 10 = 110 kg, 11 = 120 kg
		input.sim.data.wt <- lapply(input.sim.data,rep.int,times = 11) %>%
			as.data.frame
		ID.seq <- rep(1:11,times = length(times)) %>% sort
		WT.seq <- rep(seq(from = 20,to = 120,by = 10),times = length(times)) %>% sort
		input.sim.data.wt$ID <- ID.seq
		input.sim.data.wt$WT <- WT.seq

# Flat dosing scenario
	input.sim.data.wt.flat <- input.sim.data.wt
	input.sim.data.wt.flat$amt <- 350
	sim.data.wt.flat <- mod %>% mrgsim(data = input.sim.data.wt.flat,
		carry.out = c("amt","cmt")) %>%
	as.data.frame

# Weight-based dosing scenario
	input.sim.data.wt.based <- input.sim.data.wt
	input.sim.data.wt.based$amt <- 5*input.sim.data.wt.based$WT
	sim.data.wt.based <- mod %>% mrgsim(data = input.sim.data.wt.based,
		carry.out = c("amt","cmt")) %>%
	as.data.frame

# Stratified dosing scenario
	input.sim.data.wt.strat <- input.sim.data.wt
	input.sim.data.wt.strat$amt[input.sim.data.wt.strat$WT %in% c(20,30)] <- 150
	input.sim.data.wt.strat$amt[input.sim.data.wt.strat$WT %in% c(40,50,60)] <- 250
	input.sim.data.wt.strat$amt[input.sim.data.wt.strat$WT %in% c(70,80,90)] <- 350
	input.sim.data.wt.strat$amt[input.sim.data.wt.strat$WT %in% c(100,110,120)] <- 450
	sim.data.wt.strat <- mod %>% mrgsim(data = input.sim.data.wt.strat,
		carry.out = c("amt","cmt")) %>%
	as.data.frame

# Individualised dosing scenario
	input.sim.data.wt.ind <- input.sim.data.wt
	input.sim.data.wt.ind$amt <- 350*(input.sim.data.wt.ind$WT/70)^0.75
	sim.data.wt.ind <- mod %>% mrgsim(data = input.sim.data.wt.ind,
		carry.out = c("amt","cmt")) %>%
	as.data.frame

# Plot dose versus weight
	plotobj4 <- NULL
	plotobj4 <- ggplot()
	plotobj4 <- plotobj4 + geom_line(aes(x = WT,y = amt),
		data = sim.data.wt.flat[sim.data.wt.flat$time == 0,],
		size = 2,color = "#EE3B3B")	# Flat dosing = red
	plotobj4 <- plotobj4 + geom_line(aes(x = WT,y = amt),
		data = sim.data.wt.based[sim.data.wt.based$time == 0,],
		size = 2,color = "#F39C12")	# Weight-based dosing = orange
	plotobj4 <- plotobj4 + geom_line(aes(x = WT,y = amt),
		data = sim.data.wt.strat[sim.data.wt.strat$time == 0,],
		size = 2,color = "#3C8DBC")	# Stratified dosing = blue
	plotobj4 <- plotobj4 + geom_line(aes(x = WT,y = amt),
		data = sim.data.wt.ind[sim.data.wt.ind$time == 0,],
		size = 2,color = "#00A65A")	# Individualised dosing = green
	plotobj4 <- plotobj4 + scale_x_continuous("\nWeight (kg)",
		breaks = unique(WT.seq))
	plotobj4 <- plotobj4 + scale_y_continuous("Dose administered (mg)\n",
		breaks = seq(from = 0,to = 600,by = 100))
	plotobj4

	ggsave(plot = plotobj4,filename = "plot4.png",units = "cm",
		width = 15,height = 10)

# Plot trough concentration versus weight
	plotobj5 <- NULL
	plotobj5 <- ggplot()
	plotobj5 <- plotobj5 + geom_line(aes(x = WT,y = AUC),
		data = sim.data.wt.flat[sim.data.wt.flat$time == 21,],
		size = 2,color = "#EE3B3B")	# Flat dosing = red
	plotobj5 <- plotobj5 + geom_line(aes(x = WT,y = AUC),
		data = sim.data.wt.based[sim.data.wt.based$time == 21,],
		size = 2,color = "#F39C12")	# Weight-based dosing = orange
	plotobj5 <- plotobj5 + geom_line(aes(x = WT,y = AUC),
		data = sim.data.wt.strat[sim.data.wt.strat$time == 21,],
		size = 2,color = "#3C8DBC")	# Stratified dosing = blue
	plotobj5 <- plotobj5 + geom_line(aes(x = WT,y = AUC),
		data = sim.data.wt.ind[sim.data.wt.ind$time == 21,],
		size = 2,color = "#00A65A")	# Individualised dosing = green
	plotobj5 <- plotobj5 + scale_x_continuous("\nWeight (kg)",
		breaks = unique(WT.seq))
	plotobj5 <- plotobj5 + scale_y_continuous("Drug Exposure\n",lim = c(0,NA))
	plotobj5

	ggsave(plot = plotobj5,filename = "plot5.png",units = "cm",
		width = 15,height = 10)

# ------------------------------------------------------------------------------
# 5. Demonstrate a plot of many trials for a dose in context of dose optimisation
	input.optim.data <- input.sim.data
	input.optim.data$amt[input.optim.data$time %in% dose.times] <- 350
	input.optim.data$ALB <- 2
	input.optim.data$ADA <- 1

	# Initial dose and error estimates
		initial.dose <- 350 # 5 mg/kg - label recommendation
		par <- initial.dose

	# Optimise dose
		iter <- 0
		base.optim.data <- data.frame(
			ID = 1,time = 0,amt = 0,cmt = 0,CENT = 0,PERI = 0,AUC = 0,IPRE = 0,DV = 0,
			CL = 0,V1 = 0,Q = 0,V2 = 0,WT = 0,ALB = 0,ADA = 0,ETA1 = 0,ETA2 = 0,ETA3 = 0,
			ETA4 = 0,niter = 0
		)
		optimise <- function(par) {
		# Store iteration number
			iter <<- c(iter,0)
			niter <- length(iter)-1
		# Add fitted parameters to the input data frame
			input.optim.data$amt[input.optim.data$time == 98] <- par[1]
			err <- 0.0001
		# Simulate concentration-time profiles with fitted doses
			optim.data <- mod %>%
				mrgsim(data = input.optim.data,carry.out = c("amt","cmt")) %>%
				as.data.frame
		# Pull out the predicted trough concentrations with the fitted doses for the interval
		# Minimise the error between target trough (3 mg/L) and predicted trough concentrations
			optim.data$IPRE[is.finite(optim.data$IPRE) == F | optim.data$IPRE < .Machine$double.eps] <- .Machine$double.eps
			yhat <- optim.data$IPRE[optim.data$time == 154]
			res <- dnorm(3,yhat,yhat*err,log = T)
			optim.data$niter <- niter
			# browser()
			base.optim.data <<- rbind(base.optim.data,optim.data)
		# Objective function value and minimise the value
			objective <- -1*sum(res)
			objective
		}
		optimised.dose <- optim(par,
			optimise,
			hessian = FALSE,
			method = "L-BFGS-B",
			lower = 0.0001,
			upper = Inf,
			control = list(parscale = par,factr = 1e12)
		)

	# Remove the dummy data line
		base.optim.data <- base.optim.data[base.optim.data$niter > 0,]
		base.optim.data$IPRE[base.optim.data$IPRE < 0.1] <- 0.1
		maxiter <- tail(base.optim.data$niter,1)

	# Plot all interactions (highlighting the final one)
	  theme_bw2 <- theme_set(theme_bw(base_size = 14))
		plotobj6 <- NULL
		plotobj6 <- ggplot()
		plotobj6 <- plotobj6 + geom_line(aes(x = time,y = IPRE,group = niter),
			data = base.optim.data[base.optim.data$niter == maxiter &
			base.optim.data$time <= 98,],size = 2)	# black = previous dosing history
		plotobj6 <- plotobj6 + geom_line(aes(x = time,y = IPRE,group = niter),
			data = base.optim.data[base.optim.data$niter < maxiter &
			base.optim.data$time >= 98,],alpha = 0.3,colour = "#F39C12",size = 1)	# orange for other iterations
		plotobj6 <- plotobj6 + geom_line(aes(x = time,y = IPRE,group = niter),
			data = base.optim.data[base.optim.data$niter == 1 &
			base.optim.data$time >= 98,],colour = "#EE3B3B",size = 2)	# red for first iteration
		plotobj6 <- plotobj6 + geom_line(aes(x = time,y = IPRE,group = niter),
			data = base.optim.data[base.optim.data$niter == maxiter &
			base.optim.data$time >= 98,],size = 2,colour = "#00A65A")	# green for final iteration
		plotobj6 <- plotobj6 + geom_hline(aes(yintercept = 3),
			linetype = "dashed",size = 2)
		plotobj6 <- plotobj6 + scale_x_continuous("\nTime since first dose (days)",
			breaks = c(0,14,42,98,154))
		plotobj6 <- plotobj6 + scale_y_log10("Infliximab Concentration (mg/L)\n",
			breaks = c(0.1,0.3,1,3,10,30,100,300,1000,3000),
			labels = c(0.1,0.3,1,3,10,30,100,300,1000,3000))
		plotobj6

		ggsave(plot = plotobj6,filename = "plot6.png",units = "cm",
			width = 15,height = 10)

# ------------------------------------------------------------------------------
# 6. Demonstrate a plot of many trials for finding individual Bayes parameters
	input.bayes.data <- input.sim.data
	input.bayes.data$amt[input.bayes.data$time %in% dose.times] <- 350

	# Initial dose and error estimates
		initial.par <- c(exp(0),exp(0),exp(0),exp(0)) # Population values as initial estimates
		par <- initial.par
	# List of observations
		Yobs.times <- c(98,154)
		Yobs.values <- c(0.1,0.02)

	# Obtain Bayes parameters
		iter <- 0
		base.bayes.data <- data.frame(
			ID = 1,time = 0,amt = 0,cmt = 0,CENT = 0,PERI = 0,AUC = 0,IPRE = 0,DV = 0,
			CL = 0,V1 = 0,Q = 0,V2 = 0,WT = 0,ALB = 0,ADA = 0,ETA1 = 0,ETA2 = 0,ETA3 = 0,
			ETA4 = 0,niter = 0
		)
	# Function for estimating individual parameters by minimising the Bayesian objective function value
		bayesian.ofv <- function(par) {
		# Store iteration number
			iter <<- c(iter,0)
			niter <- length(iter)-1
		# List of ETA values that will be optimised
			ETA1fit <- log(par[1])  # Bayesian estimated ETA for clearance
			ETA2fit <- log(par[2])  # Bayesian estimated ETA for volume of the central compartment
			ETA3fit <- log(par[3])  # Bayesian estimated ETA for intercompartmental clearance
			ETA4fit <- log(par[4])  # Bayesian estimated ETA for volume of the peripheral compartment
		# These will updated and connected to ERR_X terms in the mrgsolve model code
			input.bayes.data$ETA1 <- ETA1fit
			input.bayes.data$ETA2 <- ETA2fit
			input.bayes.data$ETA3 <- ETA3fit
			input.bayes.data$ETA4 <- ETA4fit
		# Simulate concentration-time profile with nth iteration of ETA values
			bayes.data <- mod %>%
				mrgsim(data = input.bayes.data,carry.out = c("amt","cmt")) %>%
				as.data.frame
			# browser()
		# Use only corresponding predicted concentrations for calculating the log-likelihood
			Yhat <- bayes.data$IPRE[bayes.data$time %in% Yobs.times]  # Make a Yhat vector based on IPRE in bayes.data
		# Posterior component (from the data)
		# Log densities of residuals
		# Residual error model, Y = IPRE*(1+ERR), Y = IPRE + IPRE*ERR
			loglikpost <- dnorm(Yobs.values,mean = Yhat,sd = Yhat*error,log = T)
		# Prior component (from the model)
			ETA <- c(ETA1fit,ETA2fit,ETA3fit,ETA4fit) # List of Bayesian estimated ETAs
			loglikprior <- dnorm(ETA,mean = 0,sd = ETABSV,log = T)

			bayes.data$niter <- niter
			base.bayes.data <<- rbind(base.bayes.data,bayes.data)
		# Calculate the combined likelihood
			OFVBayes <- -1*sum(loglikpost,loglikprior)
			OFVBayes
		}
		resultfit <- optim(par,
			bayesian.ofv,
			hessian = FALSE,
			method = "L-BFGS-B",
			lower = c(0.001,0.001,0.001,0.001), # lower boundaries for estimates
			upper = c(Inf,Inf,Inf,Inf),	# upper boundaries for estimates
			control = list(parscale = par,factr = 1e7)
		)

	# Remove the dummy data line
		base.bayes.data <- base.bayes.data[!base.bayes.data$niter %in% c(0,19:27),]
		base.bayes.data$IPRE[base.bayes.data$IPRE < 0.011] <- 0.011
		maxiter7 <- tail(base.bayes.data$niter,1)

	# Plot all interactions (highlighting the final one)
	  theme_bw2 <- theme_set(theme_bw(base_size = 14))
		plotobj7 <- NULL
		plotobj7 <- ggplot()
		plotobj7 <- plotobj7 + geom_line(aes(x = time,y = IPRE,group = niter),
			data = base.bayes.data[base.bayes.data$niter < maxiter7,],
			alpha = 0.1,colour = "#F39C12",size = 1)	# orange for other iterations
		plotobj7 <- plotobj7 + geom_line(aes(x = time,y = IPRE,group = niter),
			data = base.bayes.data[base.bayes.data$niter == 1,],
			colour = "#EE3B3B",size = 2)	# red for first iteration
		plotobj7 <- plotobj7 + geom_line(aes(x = time,y = IPRE,group = niter),
			data = base.bayes.data[base.bayes.data$niter == maxiter7,],
			size = 2,colour = "#00A65A")	# green for final iteration
		plotobj7 <- plotobj7 + geom_hline(aes(yintercept = 3),
			linetype = "dashed",size = 2)
		plotobj7 <- plotobj7 + geom_point(aes(x = Yobs.times,y = Yobs.values),
			size = 4)
		plotobj7 <- plotobj7 + scale_x_continuous("\nTime since first dose (days)",
			breaks = c(0,14,42,98,154))
		plotobj7 <- plotobj7 + scale_y_log10("Infliximab Concentration (mg/L)\n",
			breaks = c(0.01,0.03,0.1,0.3,1,3,10,30,100,300,1000,3000),
			labels = c(0.01,0.03,0.1,0.3,1,3,10,30,100,300,1000,3000),
			lim = c(NA,300))
		plotobj7

		ggsave(plot = plotobj7,filename = "plot7.png",units = "cm",
			width = 15,height = 10)
