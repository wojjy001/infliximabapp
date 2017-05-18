# ui.R script for infliximabapp
# The user-interface and widget input for the Shiny application is defined here
# Sends user-defined input to server.R, calls created output from server.R
# Now using shinydashboard for the user-interface
# ------------------------------------------------------------------------------
# Application's header
header <-
  dashboardHeader(
		title = "Infliximab",
		titleWidth = 175
	)	# dashboardHeader
# Application's sidebar
sidebar <-
	dashboardSidebar(
		width = 175,	# Width of sidebar the same as width of header
		sidebarMenu(
      menuItem("Infliximab Dosing",
				tabName = "patient-dosing-info",
				icon = icon("medkit")
			),
			menuItem("Resources",
				tabName = "packages",
				icon = icon("question-circle")
			)#,
			# actionButton("console","Debug console")
		)	# sidebarMenu
	) # dashboardSidebar
# Application's body
body <-
	dashboardBody(
		useShinyjs(),
		tags$style(HTML(".form-control, .selectize-input, selectize-dropdown {color:#000000; font-size:16px}")),	# font colour of numeric- and selectInput
		tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar, .js-irs-0 .irs-bar-edge {background:#00A65A; border:#00A65A}")),	# green slider for slider_dose
		tags$style(HTML(".irs-grid-text {color:#000000; font-size:12px}")),	# black markings on sliders
    tabItems(
			tabItem(tabName = "patient-dosing-info",
				style = "overflow-y:scroll",
				box(
					title = fluidRow(
						div(
							style = "display:inline-block; padding-left:20px; font-size:20px",
							p(strong("Previous History"))
						), # div
						div(
							div(
								style = "display:inline-block; width:200px; padding-left:20px; font-size:18px",
								"Time (days)"
							),	# div
							div(
								style = "display:inline-block; width:200px; padding-left:15px; font-size:18px",
								"Weight (kg)"
							), # div
							div(
								style = "display:inline-block; width:200px; padding-left:15px; font-size:18px",
								"Albumin (g/dL)"
							), # div
							div(
								style = "display:inline-block; width:200px; padding-left:10px; font-size:18px",
								"Anti-drug antibodies"
							), # div
							div(
								style = "display:inline-block; width:200px; padding-left:5px; font-size:18px",
								"Infliximab amount (mg)"
							), # div
							div(
								style = "display:inline-block; width:270px; padding-left:100px; font-size:18px",
								"Concentration (mg/L)"
							)	# div
						)	# div
					),	# fluidRow
					uiOutput("input.boxes"),
					footer = fluidRow(
						style = "padding-left:15px; display:inline-block",
						actionButton("add",strong("Add"),
						style = "border-color:#FFFFFF; background-color:#00A65A; color:#FFFFFF; font-size:16px"),
						actionButton("remove",strong("Remove"),
						style = "border-color:#FFFFFF; background-color:#00A65A; color:#FFFFFF; font-size:16px"),
						actionButton("save",strong("Save"),
						style = "border-color:#FFFFFF; background-color:#00A65A; color:#FFFFFF; font-size:16px"),
						div(
							style = "padding-left:15px; display:inline-block; font-size:16px",
							uiOutput("saved.patient")
						)	# div
					),	# fluidRow
					style = "overflow-y:scroll; max-height:105px",
					width = 10,
					status = "success",
					collapsible = TRUE,
					collapsed = FALSE,
					solidHeader = TRUE
				),	# box
				box(
					title = strong("Predicted Infliximab Concentrations",style = "font-size:20px"),
					div(
					 	style = "font-size:16px; color:#000000",
						align = "center",
					 	fluidRow(
						 	column(4,
							 	p("Solid line = individual predicted",
							 	style = "padding-top:11px")
						 	),	# column
						 	column(3,
							 	p("Dashed line = population typical",
							 	style = "padding-top:11px")
						 	),	# column
						 	column(5,
							 	checkboxInput("pop_ci",
							 	"Show 95% confidence intervals as shaded ribbons (n = 100)",
							 	value = FALSE)
						 	)	# column
					 	)	# fluidRow
				 	),	# div
					plotOutput("sim.plot",height = 600),
					fluidRow(
						column(11,offset = 1,
							style = "font-size:16px; color:#000000",
							uiOutput("slider.plot.times")
						) # column
					),	# fluidRow
					width = 9,
					status = "primary",
					collapsible = TRUE,
					collapsed = TRUE,
					solidHeader = TRUE
				),	# box
				box(
					title = strong("Next Dose",style = "font-size:20px"),
					p(strong("Target trough concentration (mg/L):",style = "font-size:16px")),
					numericInput("target.trough",
						NA,
						min = 0.1,
						max = 10,
						value = 3,
						width = 200
					),	# numericInput
					hr(),
					p(strong("Next dose recommendation:",style = "font-size:16px")),
					checkboxInput("label_dose",
						strong("5 mg/kg (label recommended)",style = "color:#EE3B3B; font-size:16px"),
						value = FALSE
					),	# checkboxInput
					conditionalPanel(condition = "input.label_dose",
						fluidRow(
							column(5,
								style = "color:#EE3B3B; padding-top:6px; font-size:16px",
								strong(textOutput("label.amt"))
							),	# column
							column(7,
								actionButton("next.label",strong("Administer label dose"),style = "color:#FFFFFF; border-color:#FFFFFF; background-color:#EE3B3B; width:200px; text-align:center; font-size:16px")
							)	# column
						)	# fluidRow
					),	# conditionalPanel
					checkboxInput("numeric_dose",
						strong("Explore range of amounts",style = "color:#F39C12; font-size:16px"),
						value = FALSE
					),	# checkboxInput
					conditionalPanel(condition = "input.numeric_dose",
						fluidRow(
							column(5,
								style = "color:#F39C12; font-size:16px",
								numericInput("vari.num.dose",
									"Dose (mg):",
									min = 5,
									max = 20000,
									value = 350,
									step = 0.1
								),	# numericInput
								strong(textOutput("numeric.amt"))
							),	# column
							column(7,
								style = "padding-top:26px",
								actionButton("next.numeric",strong("Administer numeric dose"),style = "color:#FFFFFF; border-color:#FFFFFF; background-color:#F39C12; width:200px; text-align:center; font-size:16px")
							)	# column
						)	# fluidRow
					),	# conditionalPanel
					checkboxInput("slider_dose",
						strong("Explore range of mg/kg doses using slider",style = "color:#00A65A; font-size:16px"),
						value = FALSE
					),	# checkboxInput
					conditionalPanel(condition = "input.slider_dose",
						div(
							style = "font-size:16px",
							sliderInput("vari.dose",
								"Dose (mg/kg):",
								min = 3,
								max = 20,
								value = 5,
								step = 0.1
							)	# sliderInput
						),	# div
						fluidRow(
							column(5,
								style = "color:#00A65A; padding-top:6px; font-size:16px",
								strong(textOutput("slider.amt"))
							),	# column
							column(7,
								actionButton("next.slider",strong("Administer slider dose"),style = "color:#FFFFFF; border-color:#FFFFFF; background-color:#00A65A; width:200px; text-align:center; font-size:16px")
							)	# column
						) # fluidRow
					),	# conditionalPanel
					checkboxInput("optim_dose",
						strong("Calculate best dose",style = "color:#605CA8; font-size:16px"),
						value = FALSE
					),	# checkboxInput
					conditionalPanel(condition = "input.optim_dose",
						fluidRow(
							column(5,
								style = "color:#605CA8; font-size:16px",
								strong(textOutput("optimised.dose")),
								strong(textOutput("optimised.amt"))
							),	# column
							column(7,
								actionButton("next.optim",strong("Administer best dose"),style = "color:#FFFFFF; border-color:#FFFFFF; background-color:#605CA8; width:200px; text-align:center; font-size:16px")
							)	# column
						)	# fluidRow
					),	# conditionalPanel
					width = 3,
					status = "warning",
					collapsible = TRUE,
					collapsed = TRUE,
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
