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
