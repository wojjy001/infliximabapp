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
			menuItem("About",
				tabName = "about",
				icon = icon("question-circle"),
        menuSubItem("Population Model",
					tabName = "model",
					icon = icon("angle-double-right")
				),
				menuSubItem("Resources",
					tabName = "packages",
					icon = icon("angle-double-right")
				)
      ),
      menuItem("Infliximab Dosing",
				tabName = "patient-dosing-info",
				icon = icon("medkit")
			)
		)	# sidebarMenu
	) # dashboardSidebar
# Application's body
body <-
	dashboardBody(
    tabItems(
			tabItem(tabName = "patient-dosing-info",
				box(
					title = "History",
					uiOutput("inputui"),
					fluidRow(
						column(3,
							style = "padding-left:15px;",
							actionButton("add","Add"),
							actionButton("remove","Remove"),
							actionButton("save","Save")
						)	# column
					),	# fluidRow
					width = 12,
					status = "primary",
					collapsible = TRUE,
					collapsed = FALSE,
					solidHeader = TRUE
				)	# box
			), # tabItem for patient-dosing-info
      tabItem(tabName = "model",
				includeMarkdown("model_description.Rmd"),
        pre(includeText("model.R"))
      ), # tabItem for model
			tabItem(tabName = "packages",
				pre(htmlOutput("session.info"))
			) # tabItem for packages
		)  # tabItems
	) # dashboardBody
# ------------------------------------------------------------------------------
# User-interface Object
  dashboardPage(header,sidebar,body,skin = "blue")
