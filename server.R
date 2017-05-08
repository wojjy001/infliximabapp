# server.R script for infliximabapp
# Reactive objects (i.e., those dependent on widget input) are written here
# ------------------------------------------------------------------------------
# Define the "server" part of the Shiny application
shinyServer(function(input,output,session) {
	###########
	##_INPUT_##
	###########
	# Set up reactiveValues to keep track of the number of input boxes I have (n)
	# Also keep track of what is in my input.boxes (df)
	values <- reactiveValues(
		n = 4,
		df = data.frame(
			"time" = c(0,14,42,98),
			"wt" = c(70,70,70,70),
			"alb" = c(4,4,4,4),
			"ada" = c(0,0,0,0),
			"amt" = c(350,350,350,NA),
			"obs" = c(NA,NA,NA,5)
		)	# df
	)	# values

	button.values <- reactiveValues(
		save = 0,
		predict = 0,
		predict.once = 0
	)	# save.value

	observeEvent(input$save, {
		button.values$save <- button.values$save + 1
	})	# observeEvent

	observeEvent(input$next.label | input$next.numeric | input$next.slider | input$next.optim | input$add | input$remove, {
		button.values$save <- button.values$save - 1
		if (button.values$save < 0) button.values$save <- 0
		return(button.values)
	})	# observeEvent

	observeEvent(input$save, {
		button.values$predict <- button.values$predict + 1
	})	# observeEvent

	observeEvent(input$next.label | input$next.numeric | input$next.slider | input$next.optim | input$add | input$remove, {
		button.values$predict <- button.values$predict - 1
		if (button.values$predict < 0) button.values$predict <- 0
		return(button.values)
	})	# observeEvent

	observeEvent(input$predict, {
		button.values$predict.once <- button.values$predict.once + 1
	})

	# This reactive expression takes the value for n and df and creates UI elements accordingly
	input.boxes <- reactive({
		n <- values$n
		df <- values$df
		if (n > 0) {
			llply(seq_len(n), function(i) {
				wt.i <- values$df[i,2]
				amt.i <- values$df[i,5]
				mgkg.dose.i <- round(amt.i/wt.i,digits = 1)
				mgkg.dose.i.text <- paste0(mgkg.dose.i," mg/kg")
				if (mgkg.dose.i.text == "NA mg/kg") mgkg.dose.i.text <- " "
				fluidRow(
					column(1,
						style = "width:200px;",
						numericInput(paste0("one",i),
							label = NA,
							value = df[i,1]
						)	# input$one-i
					),	# column
					column(2,
						style = "width:200px;",
						numericInput(paste0("two",i),
							label = NA,
							value = df[i,2]
						)	# input$two-i
					),	# column
					column(2,
						style = "width:200px;",
						numericInput(paste0("three",i),
							label = NA,
							value = df[i,3]
						)	# input$three-i
					),	# column
					column(2,
						style = "width:200px;",
						selectInput(paste0("four",i),
							label = NA,
							choices = list("Not present" = 0,"Present" = 1),
							selected = df[i,4]
						)	# input$four-i
					),	# column
					column(2,
						style = "width:200px;",
						numericInput(paste0("five",i),
							label = NA,
							value = df[i,5]
						)	# input$five-i
					),	# column
					column(1,
						style = "width:100px;",
						h5(strong(mgkg.dose.i.text))
					),
					column(2,
						style = "width:200px;",
						numericInput(paste0("six",i),
							label = NA,
							value = df[i,6]
						)	# input$six-i
					)	# column
				)	# fluidRow
			})	# llply
		} # if
	})	# input.boxes

	observeEvent(input$next.label, {
		n <- values$n
		updateNumericInput(session,inputId = paste0("five",n),value = round(Rlabel.data()$amt[1]))
	})	# observeEvent

	observeEvent(input$next.numeric, {
		n <- values$n
		updateNumericInput(session,inputId = paste0("five",n),value = round(Rnumeric.data()$amt[1]))
	})	# observeEvent

	observeEvent(input$next.slider, {
		n <- values$n
		updateNumericInput(session,inputId = paste0("five",n),value = round(Rslider.data()$amt[1]))
	})	# observeEvent

	observeEvent(input$next.optim, {
		n <- values$n
		updateNumericInput(session,inputId = paste0("five",n),value = round(Roptim.data()$amt[1]))
	})	# observeEvent

	# Use observeEvent to determine if user wants to add or remove a row of boxes
	# Also use to save a current state of input.boxes for use
	# Each set either increases or decrease the number of boxes to render (n)
	# ldply creates a data.frame with n number of rows with values of data.frame being the input of the user
	observeEvent(input$add, {	# Add a new row
		values$n <- values$n + 1
		values$df <- ldply(seq_len(values$n), function(i) {
			data.frame(
				"time" = get("input")[[paste0("one",i)]],
				"wt" = get("input")[[paste0("two",i)]],
				"alb" = get("input")[[paste0("three",i)]],
				"ada" = get("input")[[paste0("four",i)]],
				"amt" = get("input")[[paste0("five",i)]],
				"obs" = get("input")[[paste0("six",i)]]
			)	# data.frame
		})	# ldply
	})	# observeEvent for add

	observeEvent(input$remove, {	# Remove a row
		if (values$n > 1) {
			values$n <- values$n - 1
		}	# if
		values$df <- ldply(seq_len(values$n), function(i) {
			data.frame(
				"time" = get("input")[[paste0("one",i)]],
				"wt" = get("input")[[paste0("two",i)]],
				"alb" = get("input")[[paste0("three",i)]],
				"ada" = get("input")[[paste0("four",i)]],
				"amt" = get("input")[[paste0("five",i)]],
				"obs" = get("input")[[paste0("six",i)]]
			)	# data.frame
		})	# ldply
	})	# observeEvent for remove

	observeEvent(input$save, {	# Save current state
		values$df <- ldply(seq_len(values$n), function(i) {
			data.frame(
				"time" = get("input")[[paste0("one",i)]],
				"wt" = get("input")[[paste0("two",i)]],
				"alb" = get("input")[[paste0("three",i)]],
				"ada" = get("input")[[paste0("four",i)]],
				"amt" = get("input")[[paste0("five",i)]],
				"obs" = get("input")[[paste0("six",i)]]
			)	# data.frame
		})	# ldply
		values$df <- values$df[with(values$df,order(values$df["time"])),]
		return(values$df)
	})	# observeEvent for save

	# Set up an input data frame for simulating predicted concentration time-profile
	# This may or may not be based on Bayes estimates
	# Uses the saved state of "values$df"
	Rinput.sim.data <- eventReactive(input$predict, {
		input.values <- values$df	# Assign reactive input values to a new object
		input.values$ada <- as.numeric(levels(input.values$ada))[input.values$ada]
		last.input.time <- max(input.values$time)	# last time in data frame
		if (is.na(input.values$amt[input.values$time == last.input.time]) == TRUE) {
			last.time <- last.input.time
		} else {
			last.time <- last.input.time + 56	# plus one dosing interval
		}
		dose.amts <- input.values$amt[is.na(input.values$amt) == FALSE & input.values$amt != 0]
		dose.times <- input.values$time[is.na(input.values$amt) == FALSE & input.values$amt != 0]
		times <- seq(from = 0,to = last.time,by = 1)	# time sequence for simulation

	# Repeat input.sim.data for the length of times
		input.sim.data <- lapply(input.sim.data,rep.int,times = length(times)) %>%
			as.data.frame

	# Add dosing information
		input.sim.data$time <- times	# paste in time sequence to repeated data frame
		input.sim.data$amt[input.sim.data$time %in% dose.times] <- dose.amts
		input.sim.data$evid[input.sim.data$time %in% dose.times] <- 1	# 1 = dosing event
		input.sim.data$rate[input.sim.data$time %in% dose.times] <- -2	# rate defined in mrgsolve code

	# Add covariate values
	# Weight
		input.sim.data$WT <- lin.cov.extrap(times,time = input.values$time,last.time,cov = input.values$wt)
	# Albumin
		input.sim.data$ALB <- lin.cov.extrap(times,time = input.values$time,last.time,cov = input.values$alb)
	# Anti-drug antibodies
		input.sim.data$ADA <- con.cov.extrap(times,time = input.values$time,last.time,cov = input.values$ada)

	# Create population typical data frame
		pred.sim.data <- input.sim.data
		pred.sim.data$ID <- 2
		input.sim.data <- rbind(pred.sim.data,input.sim.data)

	# Preset ETA values to zero
		input.sim.data$ETA1 <- 0
		input.sim.data$ETA2 <- 0
		input.sim.data$ETA3 <- 0
		input.sim.data$ETA4 <- 0

	# Create a Bayesian estimation flag - initially not performed
		input.sim.data$bayes <- 0

	# Obtain Bayesian parameter estimates if trough concentrations available
		obs.times <- input.values$time[is.na(input.values$obs) == FALSE & input.values$obs != 0]
		if (length(obs.times) > 0) {
			input.bayes.data <- input.sim.data[input.sim.data$ID == 1,]
			obs.values <- input.values$obs[input.values$time %in% obs.times]
			input.bayes.data$obs[input.bayes.data$time %in% obs.times] <- obs.values
			bayes.result <- bayesian.function(input.bayes.data)
		# Add Bayes estimated random effects to data frame
			input.sim.data$ETA1[input.sim.data$ID == 1] <- log(bayes.result$par[1])
			input.sim.data$ETA2[input.sim.data$ID == 1] <- log(bayes.result$par[2])
			input.sim.data$ETA3[input.sim.data$ID == 1] <- log(bayes.result$par[3])
			input.sim.data$ETA4[input.sim.data$ID == 1] <- log(bayes.result$par[4])
			input.sim.data$bayes <- 1
		}
		input.sim.data <- input.sim.data[with(input.sim.data,order(input.sim.data["ID"])),]
		return(input.sim.data)
	})	# observeEvent for Rinput.sim.data

	# Simulate predicted concentrations
	Rsim.data <- reactive({
		sim.data <- mod %>%
			mrgsim(data = Rinput.sim.data(),carry.out = c("amt","bayes","cmt")) %>%
			as.data.frame
		return(sim.data)
	})	# reactive for Rsim.data

	# Create generic input data frame that collects previous information
	# Will be used for simulating or calculating the "next dose"
	Rinput.next.dose <- reactive({
		input.sim.data <- Rinput.sim.data()
	# Last covariate values are carried forward for predictions
		last.time <- tail(input.sim.data$time,1)
		next.times <- seq(from = last.time,to = last.time + 56,by = 1)
		input.next.dose <- input.sim.data[input.sim.data$time == last.time,]
		input.next.dose <- lapply(input.next.dose,rep.int,times = length(next.times)) %>%
			as.data.frame
		input.next.dose <- input.next.dose[with(input.next.dose,order(input.next.dose["ID"])),]
		input.next.dose$time <- next.times
		return(input.next.dose)
	})	# reactive for Rinput.next.dose

	# Reactive model file that updates compartment initial conditions
	Rmod <- reactive({
		sim.data <- Rsim.data()
		last.time <- tail(sim.data$time,1)
	#	Collect previous amounts in compartments
	# For the individual and population typical
		prev.CENT <- c(0,0)
		prev.PERI <- c(0,0)
		mod2 <- llply(seq_len(2), function(i) {
			prev.CENT[i] <- sim.data$CENT[sim.data$time == last.time & sim.data$ID == i]
			prev.PERI[i] <- sim.data$PERI[sim.data$time == last.time & sim.data$ID == i]
			mod2 <- mod %>% init(CENT = prev.CENT[i],PERI = prev.PERI[i])
			mod2
		})	# llply
	})	# reactive for Rmod

	observeEvent(input$save | input$next.slider | input$next.optim | input$next.numeric, {
		updateCheckboxInput(session,inputId = "label_dose",value = FALSE)
	})	# observeEvent for resetting checkboxInput for label_dose

	observeEvent(input$save | input$next.label | input$next.optim | input$next.slider, {
		updateCheckboxInput(session,inputId = "numeric_dose",value = FALSE)
	})	# observeEvent for resetting checkboxInput for numeric_dose

	observeEvent(input$save | input$next.label | input$next.optim | input$next.numeric, {
		updateCheckboxInput(session,inputId = "slider_dose",value = FALSE)
	})	# observeEvent for resetting checkboxInput for slider_dose

	observeEvent(input$save | input$next.label | input$next.slider | input$next.numeric, {
		updateCheckboxInput(session,inputId = "optim_dose",value = FALSE)
	})	# observeEvent for resetting checkboxInput for optim_dose

	# Simulate next dose when given 5 mg/kg (56-day interval)
	Rlabel.data <- eventReactive(input$label_dose == TRUE, {
		input.label.dose <- Rinput.next.dose()
		mod2 <- Rmod()
	# Add in label dosing information to input data frame
		next.label.dose <- 5*input.label.dose$WT[1]	# next 5 mg/kg dose
		next.label.dose.time <- input.label.dose$time[1]
		input.label.dose$amt[input.label.dose$time == next.label.dose.time] <- next.label.dose
		input.label.dose$rate[input.label.dose$time == next.label.dose.time] <- -2
		input.label.dose$evid[input.label.dose$time == next.label.dose.time] <- 1
	# Simulate next 56 days given label dose
		label.data <- ldply(seq_len(2), function(i) {
			label.data <- mod2[[i]] %>%
				mrgsim(data = input.label.dose[input.label.dose$ID == i,],
				carry.out = c("amt","bayes","cmt")) %>%
				as.data.frame
		}) # llply
		return(label.data)
	})	# eventReactive for Rlabel.data

	# Simulate next dose when given a slider nominated dose (56 day-interval)
	Rslider.data <- eventReactive(input$slider_dose == TRUE | input$vari.dose, {
		input.slider.dose <- Rinput.next.dose()
		mod2 <- Rmod()
		if (is.null(input$vari.dose)) {
			NULL
		} else {
		# Add in slider dosing information to input data frame
			next.slider.dose <- input$vari.dose*input.slider.dose$WT[1]	# next 5 mg/kg dose
			next.slider.dose.time <- input.slider.dose$time[1]
			input.slider.dose$amt[input.slider.dose$time == next.slider.dose.time] <- next.slider.dose
			input.slider.dose$rate[input.slider.dose$time == next.slider.dose.time] <- -2
			input.slider.dose$evid[input.slider.dose$time == next.slider.dose.time] <- 1
		# Simulate next 56 days given slider dose
			slider.data <- ldply(seq_len(2), function(i) {
				slider.data <- mod2[[i]] %>%
					mrgsim(data = input.slider.dose[input.slider.dose$ID == i,],
					carry.out = c("amt","bayes","cmt")) %>%
					as.data.frame
			}) # llply
			return(slider.data)
		}
	})	# reactive for Rslider.data

	# Simulate next dose when given a numeric nominated dose (56 day-interval)
	Rnumeric.data <- eventReactive(input$numeric_dose == TRUE | input$vari.num.dose, {
		input.numeric.dose <- Rinput.next.dose()
		mod2 <- Rmod()
		if (is.null(input$vari.num.dose)) {
			NULL
		} else {
		# Add in numeric dosing information to input data frame
			next.numeric.dose <- input$vari.num.dose	# next 5 mg/kg dose
			next.numeric.dose.time <- input.numeric.dose$time[1]
			input.numeric.dose$amt[input.numeric.dose$time == next.numeric.dose.time] <- next.numeric.dose
			input.numeric.dose$rate[input.numeric.dose$time == next.numeric.dose.time] <- -2
			input.numeric.dose$evid[input.numeric.dose$time == next.numeric.dose.time] <- 1
		# Simulate next 56 days given numeric dose
			numeric.data <- ldply(seq_len(2), function(i) {
				numeric.data <- mod2[[i]] %>%
					mrgsim(data = input.numeric.dose[input.numeric.dose$ID == i,],
					carry.out = c("amt","bayes","cmt")) %>%
					as.data.frame
			}) # llply
			return(numeric.data)
		}
	})	# reactive for Rnumeric.data

	# Find the doses that maximise the likelihood of trough concentrations being the target
	# Simulate next dose when given the optimised dose (56-day interval)
	Roptim.data <- eventReactive(input$optim_dose == TRUE | input$target.trough, {
		input.optim.dose <- Rinput.next.dose()
		mod2 <- Rmod()
	# Add in slider dosing information to input data frame
		next.optim.dose.time <- input.optim.dose$time[1]
		input.optim.dose$rate[input.optim.dose$time == next.optim.dose.time] <- -2
		input.optim.dose$evid[input.optim.dose$time == next.optim.dose.time] <- 1

	# Initial dose and error estimates
		initial.dose <- 3*input.optim.dose$WT[1]	# 5 mg/kg - label recommendation
		initial.error <- 0.01
		par <- c(initial.dose,initial.error)

	# Optimise only for ID = 1
		input.optim.data <- input.optim.dose[input.optim.dose$ID == 1,]
		optimise <- function(par) {
		# Add fitted parameters to the input data frame
			input.optim.data$amt[input.optim.data$time == next.optim.dose.time] <- par[1]
			err <- par[2]
		# Simulate concentration-time profiles with fitted doses
			optim.data <- mod2[[1]] %>%
				mrgsim(data = input.optim.data) %>%
				as.data.frame
		# Pull out the predicted trough concentrations with the fitted doses for the interval
		# Minimise the error between target trough (3 mg/L) and predicted trough concentrations
			optim.data$IPRE[is.finite(optim.data$IPRE) == F | optim.data$IPRE < .Machine$double.eps] <- .Machine$double.eps
			yhat <- optim.data$IPRE[optim.data$time == next.optim.dose.time+56]
			res <- dnorm(input$target.trough,yhat,yhat*err,log = T)
		# Objective function value and minimise the value
			objective <- -1*sum(res)
		}
		optimised.dose <- optim(par,
			optimise,
			hessian = FALSE,
			method = "L-BFGS-B",
			lower = c(3*input.optim.dose$WT[1],0.0001),
			upper = c(20*input.optim.dose$WT[1],Inf),
			control = list(parscale = par,factr = 1e7)
		)
		next.optim.dose <- optimised.dose$par[1]

	# Simulate next 56 days given optimised dose
		input.optim.dose$amt[input.optim.dose$time == next.optim.dose.time] <- next.optim.dose
		optim.data <- ldply(seq_len(2), function(i) {
			optim.data <- mod2[[i]] %>%
				mrgsim(data = input.optim.dose[input.optim.dose$ID == i,],
				carry.out = c("amt","bayes","cmt")) %>%
				as.data.frame
		}) # llply
		return(optim.data)
	})	# reactive for Roptim.data

	############
	##_OUTPUT_##
	############
	# renderUI for the input.boxes
	output$input.boxes <- renderUI({
		input.boxes()
	})	# renderUI for input.boxes

	output$saved.patient <- renderText({
		if (button.values$save == 0) {
			saved.patient.text <- " "
		} else {
			saved.patient.text <- "Patient Data Saved!"
		}
		return(saved.patient.text)
	})	# renderText for saved.patient

	output$predict.button <- renderUI({
		if (button.values$predict == 0) {
			actionButton("fakepredict","Predict")
		} else {
			actionButton("predict","Predict",
			style = "border-color:#FFFFFF; background-color:#EE3B3B ; color:#FFFFFF")
		}
	})	# renderUI for predict.button

	output$checkbox.pop.typ <- renderUI({
		if (is.null(Rsim.data())) {
			NULL
		} else {
			checkboxInput("pop.typ",
			"Show population typical profile for this individual",
			value = FALSE)
		}
	})	# renderUI for checkbox.pop.typ

	output$slider.plot.times <- renderUI({
		max.time <- max(Rsim.data()$time)
		if(input$label_dose == FALSE & input$optim_dose == FALSE & input$slider_dose == FALSE & input$numeric_dose == FALSE) {
			max <- max.time
			value <- c(0,max.time)
		} else {
			max <- max.time + 56
			value <- c(0,max.time + 56)
		}
		sliderInput("plot.times",
			"Plot concentrations between times",
			min = 0,
			max = max,
			value = value,
			step = 7
		)	# sliderInput
	})	# renderUI for slider.plot.times

	output$sim.plot <- renderPlot({
		if (button.values$predict.once > 0) {
			if (is.null(input$plot.times)) {
				NULL
			} else {
			# Call in and rename reactive data frame
				sim.data <- Rsim.data()
			# Plot concentrations over time
				plotobj.pred <- NULL
				plotobj.pred <- ggplot()
				plotobj.pred <- plotobj.pred + geom_line(aes(x = time,y = IPRE),
					data = sim.data[sim.data$ID == 1,],size = 1,
					colour = "#3C8DBC")	# shinyblue
				if (input$pop.typ == TRUE) {
					plotobj.pred <- plotobj.pred + geom_line(aes(x = time,y = IPRE),
						data = sim.data[sim.data$ID == 2,],
						linetype = "dashed",size = 1,
						colour = "#3C8DBC")	# shinyblue
				}
				if (input$label_dose == TRUE) {
					label.data <- Rlabel.data()
					plotobj.pred <- plotobj.pred + geom_line(aes(x = time,y = IPRE),
						data = label.data[label.data$ID == 1,],size = 1,
						colour = "#EE3B3B")	# firebrick4
					if (input$pop.typ == TRUE) {
						plotobj.pred <- plotobj.pred + geom_line(aes(x = time,y = IPRE),
							data = label.data[label.data$ID == 2,],
							linetype = "dashed",size = 1,
							colour = "#EE3B3B")	# firebrick4
					}
				}
				if (input$numeric_dose == TRUE) {
					numeric.data <- Rnumeric.data()
					plotobj.pred <- plotobj.pred + geom_line(aes(x = time,y = IPRE),
						data = numeric.data[numeric.data$ID == 1,],size = 1,
						colour = "#F39C12")	# darkorange3
					if (input$pop.typ == TRUE) {
						plotobj.pred <- plotobj.pred + geom_line(aes(x = time,y = IPRE),
							data = numeric.data[numeric.data$ID == 2,],
							linetype = "dashed",size = 1,
							colour = "#F39C12")	# darkorange3
					}
				}
				if (input$slider_dose == TRUE) {
					slider.data <- Rslider.data()
					plotobj.pred <- plotobj.pred + geom_line(aes(x = time,y = IPRE),
						data = slider.data[slider.data$ID == 1,],size = 1,
						colour = "#00A65A")	# springgreen4
					if (input$pop.typ == TRUE) {
						plotobj.pred <- plotobj.pred + geom_line(aes(x = time,y = IPRE),
							data = slider.data[slider.data$ID == 2,],
							linetype = "dashed",size = 1,
							colour = "#00A65A")	# springgreen4
					}
				}
				if (input$optim_dose == TRUE) {
					optim.data <- Roptim.data()
					plotobj.pred <- plotobj.pred + geom_line(aes(x = time,y = IPRE),
						data = optim.data[optim.data$ID == 1,],size = 1,
						colour = "#605CA8")	# darkviolet
					if (input$pop.typ == TRUE) {
						plotobj.pred <- plotobj.pred + geom_line(aes(x = time,y = IPRE),
							data = optim.data[optim.data$ID == 2,],
							linetype = "dashed",size = 1,
							colour = "#605CA8")	# darkviolet
					}
				}
				plotobj.pred <- plotobj.pred + geom_point(aes(x = time,y = obs),
					data = values$df[is.na(values$df$obs) == FALSE,],size = 4)
				plotobj.pred <- plotobj.pred + geom_hline(aes(yintercept = input$target.trough),
					linetype = "dashed")
				plotobj.pred <- plotobj.pred + scale_y_log10("Infliximab Concentration (mg/L)\n")
				plotobj.pred <- plotobj.pred + scale_x_continuous("\nTime since first dose (days)",
					breaks = seq(from = 0,to = max(input$plot.times),by = 14),
					lim = input$plot.times)
				suppressWarnings(print(plotobj.pred))
			}	# if
		} # if
	})	# renderPlot

	output$label.amt <- renderText({
		amount <- round(Rlabel.data()$amt[1])
		amount.text <- paste0("Amount = ",amount," mg")
	})	# renderText for label.amt

	output$numeric.amt <- renderText({
		amount <- round(Rnumeric.data()$amt[1])
		amount.text <- paste0("Amount = ",amount," mg")
	})	# renderText for numeric.amt

	output$slider.amt <- renderText({
		amount <- round(Rslider.data()$amt[1])
		amount.text <- paste0("Amount = ",amount," mg")
	})	# renderText for slider.amt

	output$optimised.dose <- renderText({
		mgkg.dose <- round(Roptim.data()$amt[1]/Roptim.data()$WT[1],digits = 1)
		dose.text <- paste0("Dose = ",mgkg.dose," mg/kg")
	})	# renderText for optimised.dose

	output$optimised.amt <- renderText({
		amount <- round(Roptim.data()$amt[1])
		amount.text <- paste0("Amount = ",amount," mg")
	})	# renderText for optimised.amt

  #############
  ##_SESSION_##
  #############
  # Close the R session when Chrome closes
  session$onSessionEnded(function() {
    stopApp()
  })

	# Print session information
	output$session.info <- renderPrint({
		print(sessionInfo())
	})

	# # Open debug console for R session
	# observe(label = "console", {
	# 	if(input$console != 0) {
	# 		options(browserNLdisabled = TRUE)
	# 		# saved_console <- ".RDuetConsole"
	# 		# if (file.exists(saved_console)) load(saved_console)
	# 		isolate(browser())
	# 		# save(file = saved_console, list = ls(environment()))
	# 	}
	# })
})  # shinyServer"