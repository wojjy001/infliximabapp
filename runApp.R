# Script for running the Shiny app without having to use RStudio
	dir <- "/Volumes/Prosecutor/PhD/2017_ANZTDM/infliximabapp/"  # Application's directory
	setwd(dir)  # Set the working directory to be the application's directory
	library(shiny)  # Load the shiny package for the "runApp" function
	runApp()
