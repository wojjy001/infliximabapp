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
		n = 1,
		df = data.frame(
			"time" = c(0),
			"wt" = c(70),
			"alb" = c(4),
			"ada" = c(0),
			"amt" = c(350),
			"obs" = c(NA)
		)	# df
	)	# values

	# This reactive expression takes the value for n and df and creates UI elements accordingly
	input.boxes <- reactive({
		n <- values$n
		df <- values$df
		if (n > 0) {
			llply(seq_len(n), function(i) {
				fluidRow(
					column(2,
						style = "max-width:200px;",
						numericInput(paste0("one",i),
							ifelse(i == 1,"Time (days)",NA),
							df[i,1]
						)	# input$one-i
					),	# column
					column(2,
						style = "max-width:200px;",
						numericInput(paste0("two",i),
							ifelse(i == 1,"Weight (kg)",NA),
							df[i,2]
						)	# input$two-i
					),	# column
					column(2,
						style = "max-width:200px;",
						numericInput(paste0("three",i),
							ifelse(i == 1,"Albumin(g/dL)",NA),
							df[i,3]
						)	# input$three-i
					),	# column
					column(2,
						style = "max-width:200px;",
						selectInput(paste0("four",i),
							ifelse(i == 1,"Anti-drug antibodies",NA),
							choices = list("Not present" = 0,"Present" = 1)
						)	# input$four-i
					),	# column
					column(2,
						style = "max-width:200px;",
						numericInput(paste0("five",i),
							ifelse(i == 1,"Infliximab dose (mg)",NA),
							df[i,5]
						)	# input$five-i
					),	# column
					column(2,
						style = "max-width:200px;",
						numericInput(paste0("six",i),
							ifelse(i == 1,"Concentration (mg/L)",NA),
							df[i,6]
						)	# input$six-i
					)	# column
				)	# fluidRow
			})	# llply
		} # if
	})	# input.boxes

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
	})	# observeEvent

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
	})	# observeEvent

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
		print(values$df)
	})	# observeEvent

	############
	##_OUTPUT_##
	############
	# renderUI for the input.boxes
	output$inputui <- renderUI({ input.boxes() })

  #############
  ##_SESSION_##
  #############
  # Close the R session when Chrome closes
  session$onSessionEnded(function() {
    stopApp()
  })

	# Print session information
	output$session.info <- renderPrint({
	 	session.info <- sessionInfo()
		print(session.info)
	})
})  # Brackets closing "shinyServer" function
