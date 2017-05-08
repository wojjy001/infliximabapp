# ui.R script for infliximabapp
# The user-interface and widget input for the Shiny application is defined here
# Sends user-defined input to server.R, calls created output from server.R
# Now using shinydashboard for the user-interface
# ------------------------------------------------------------------------------
# Application's header
header <-
  dashboardHeader(
		title = "Infliximab",
		titleWidth = 250
	)	# dashboardHeader
# Application's sidebar
sidebar <-
	dashboardSidebar(
		width = 250,	# Width of sidebar the same as width of header
		sidebarMenu(
      menuItem("Infliximab Dosing",
				tabName = "patient-dosing-info",
				icon = icon("medkit")
			),
			menuItem("Resources",
				tabName = "packages",
				icon = icon("question-circle")
			)
		)	# sidebarMenu
	) # dashboardSidebar
# Application's body
body <-
	dashboardBody(
		useShinyjs(),
    tabItems(
			tabItem(tabName = "patient-dosing-info",
				style = "overflow-y:scroll",
				box(
					title = fluidRow(
						div(
							style = "display:inline-block; padding-left:20px;",
							h4("Previous History")
						), # div
						div(
							div(
								style = "display:inline-block; width:200px; padding-left:20px",
								h5(strong("Time (days)"))
							),	# div
							div(
								style = "display:inline-block; width:200px; padding-left:15px",
								h5(strong("Weight (kg)"))
							), # div
							div(
								style = "display:inline-block; width:200px; padding-left:15px",
								h5(strong("Albumin (g/dL)"))
							), # div
							div(
								style = "display:inline-block; width:200px; padding-left:10px",
								h5(strong("Anti-drug antibodies"))
							), # div
							div(
								style = "display:inline-block; width:200px; padding-left:5px",
								h5(strong("Infliximab amount (mg)"))
							), # div
							div(
								style = "display:inline-block; width:250px; padding-left:100px",
								h5(strong("Concentration (mg/L)"))
							)	# div
						)	# div
					),	# fluidRow
					uiOutput("input.boxes"),
					footer = fluidRow(
						style = "padding-left:15px; display:inline-block",
						actionButton("add","Add",
						style = "border-color:#FFFFFF; background-color:#00A65A ; color:#FFFFFF"),
						actionButton("remove","Remove",
						style = "border-color:#FFFFFF; background-color:#00A65A ; color:#FFFFFF"),
						actionButton("save","Save",
						style = "border-color:#FFFFFF; background-color:#00A65A ; color:#FFFFFF"),
						div(
							style = "padding-left:15px; display:inline-block",
							textOutput("saved.patient")
						)
					),	# fluidRow
					style = "overflow-y:scroll; max-height:210px",
					width = 12,
					status = "success",
					collapsible = TRUE,
					collapsed = FALSE,
					solidHeader = TRUE
				),	# box
				box(
					title = fluidRow(
						div(
							style = "display:inline-block; padding-left:20px",
							h4("Predicted Infliximab Concentrations")
						),	# div
						div(
							style = "display:inline-block; padding-left:20px",
							uiOutput("predict.button")
						)	# div
					),
					plotOutput("sim.plot"),
					fluidRow(
						column(11,offset = 1,
							uiOutput("slider.plot.times"),
							uiOutput("checkbox.pop.typ")
						)	# column
					),	# fluidRow
					width = 8,
					status = "primary",
					collapsible = TRUE,
					collapsed = FALSE,
					solidHeader = TRUE
				),	# box
				box(
					title = "Next Dose",
					h5(strong("Target trough concentration (mg/L):")),
					numericInput("target.trough",
						NA,
						min = 0.1,
						max = 10,
						value = 3,
						width = 200
					),	# numericInput
					h5(strong("Next dose recommendation:")),
					checkboxInput("label_dose",
						strong("5 mg/kg (label recommended)",style = "color:#EE3B3B"),
						value = FALSE
					),	# checkboxInput
					conditionalPanel(condition = "input.label_dose",
						fluidRow(
							column(5,offset = 1,
								style = "color:#EE3B3B; padding-top:6px",
								strong(textOutput("label.amt"))
							),	# column
							column(6,
								actionButton("next.label","Administer label dose",style = "color:#FFFFFF; border-color:#FFFFFF; background-color:#EE3B3B; width:170px; text-align:center")
							)	# column
						)	# fluidRow
					),	# conditionalPanel
					checkboxInput("numeric_dose",
						strong("Explore range of amounts",style = "color:#F39C12"),
						value = FALSE
					),	# checkboxInput
					conditionalPanel(condition = "input.numeric_dose",
						fluidRow(
							column(5,offset = 1,
								style = "color:#F39C12; padding-top:6px",
								numericInput("vari.num.dose",
									"Dose (mg):",
									min = 5,
									max = 20000,
									value = 350,
									step = 0.1
								),	# numericInput
								strong(textOutput("numeric.amt"))
							),	# column
							column(6,
								actionButton("next.numeric","Administer numeric dose",style = "color:#FFFFFF; border-color:#FFFFFF; background-color:#F39C12; width:170px; text-align:center")
							)	# column
						)	# fluidRow
					),	# conditionalPanel
					checkboxInput("slider_dose",
						strong("Explore range of mg/kg doses using slider",style = "color:#00A65A"),
						value = FALSE
					),	# checkboxInput
					conditionalPanel(condition = "input.slider_dose",
						div(
							tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar, .js-irs-0 .irs-bar-edge {background:#00A65A}")),
							tags$style(HTML(".js-irs-0 .irs-bar, .js-irs-0 .irs-bar-edge  {border:#00A65A}")),
							sliderInput("vari.dose",
								"Dose (mg/kg):",
								min = 3,
								max = 20,
								value = 5,
								step = 0.1
							)	# sliderInput
						),	# div
						fluidRow(
							column(5,offset = 1,
								style = "color:#00A65A; padding-top:6px",
								strong(textOutput("slider.amt"))
							),	# column
							column(6,
								actionButton("next.slider","Administer slider dose",style = "color:#FFFFFF; border-color:#FFFFFF; background-color:#00A65A; width:170px; text-align:center")
							)	# column
						) # fluidRow
					),	# conditionalPanel
					checkboxInput("optim_dose",
						strong("Calculate best dose",style = "color:#605CA8"),
						value = FALSE
					),	# checkboxInput
					conditionalPanel(condition = "input.optim_dose",
						fluidRow(
							column(5,offset = 1,
								style = "color:#605CA8",
								strong(textOutput("optimised.dose")),
								strong(textOutput("optimised.amt"))
							),	# column
							column(6,
								actionButton("next.optim","Administer best dose",style = "color:#FFFFFF; border-color:#FFFFFF; background-color:#605CA8; width:170px; text-align:center")
							)	# column
						)	# fluidRow
					),	# conditionalPanel
					width = 4,
					status = "warning",
					collapsible = TRUE,
					collapsed = FALSE,
					solidHeader = TRUE
				)	# box
			), # tabItem for patient-dosing-info
			tabItem(tabName = "packages",
				pre(htmlOutput("session.info"))
			) # tabItem for packages
		)  # tabItems
	) # dashboardBody
# ------------------------------------------------------------------------------
# User-interface Object
  dashboardPage(header,sidebar,body,skin = "purple")
