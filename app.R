library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(bs4Dash)
library(DT)
library(plotly)
library(psych)
library(tidyverse)
library(corrplot)
library(colourpicker)

bodyFontSize <<- "12px"
colorScheme <<- c("#DE6666", "#5DBDC4", "#E8E561", "#7CC33D", "#548094", "#343434")
correlationMethods <- c("circle", "square", "ellipse", "number", "shade", "color", "pie")
correlationTypes <- c("full", "lower", "upper")
correlationOrders <- c("original", "AOE", "FPC", "hclust", "alphabet")

makeReactiveTrigger <<- function() {
	rv <- reactiveValues(a = 0)
	list(
		depend = function() {
			rv$a
			invisible()
		},
		trigger = function() {
			rv$a <- isolate(rv$a + 1)
		}
	)
}

data_input__trigger <- makeReactiveTrigger()

popUpWindow <- function (popUpText, title = NULL, footer = NULL, easyClose = TRUE,
	color = "#333", bg_color = "#f7f7f7") {
	tags$div(
		class = "showmodal",
		showModal(
			modalDialog(
				style = paste0('color: ', color, '; background-color: ', bg_color),
				title = title, tags$div(HTML(popUpText), align = "center"), footer = footer, easyClose = easyClose
			)
		)
	)
}

textPlot <- function(text = "No data avaliable", color = "#000000") {
	plot <- ggplot()+
		geom_text(aes(x = 0, y = 0, label = text), size = 6, color = color) +
		labs(x = '', y = '') +
		theme(panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
	return(plot)
}

data_source_ui <- bs4TabItem(
	tabName = "data_source_tab",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Data Source")
			),
			fluidRow(
				align = "center", style = "margin-top: 10vh",
				column(
					12,
					HTML(
						'Upload your own data to analyze it!'
					),
					downloadButton("sample_data_download", "download sample data")
				),
				column(
					12, style = "margin-top: 10vh",
					fileInput(
						"data_source_input_file",
						"Upload your file or drag and drop it here",
						accept = c(".csv")
					)
				),
				column(12, DTOutput("input_data_table"))
			)
		)
	)	
)

summary_ui <- bs4TabItem(
	tabName = "summary_tab",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Summary")
			),
			bs4Card(
				title = "Correlation Plot", width = 12,
				closable = TRUE, collapsible = TRUE,
				fluidRow(
					align = "center",
					column(2, selectInput("corr_method", "Method", correlationMethods)),
					column(2, selectInput("corr_type", "Type", correlationTypes)),
					column(2, selectInput("corr_order", "Order", correlationOrders)),
					column(2, sliderInput("corr_width", "Plot Height", value = 400, min = 200, max = 1000)),
					column(2, colourInput("corr_color1", "Negative color", "#AD2D2D")),
					column(2, colourInput("corr_color2", "Positive color", "#0B438C")),
					column(12, uiOutput("correlation_plot_ui"))
				)
			)
		)
	)	
)

ui = tags$div(
	tags$head(
		tags$link(rel = "shortcut icon", type = "image/png", href = "vedha_space.png")
	),
	useShinyjs(),
	useShinyalert(),
	conditionalPanel(
		condition = "$('html').hasClass('shiny-busy')",
		tags$div(
			style = "position: fixed;top: 250px; left: 0px; width: 100%;
			padding: 5px 0px 5px 0px; text-align: center; font-weight: bold;
			font-size: 300%; color: #ffffff; background-color:'transparent'; z-index: 105;",
			tags$img(src = "loading_icon.svg", height = "200px", width = "200px")
		)
	),
	tags$style(
		paste0(
			".form-control {font-size: ", bodyFontSize, " !important;}",
			".shiny-input-container {font-size: ", bodyFontSize, " !important;}",
			".btn, .btn:link, .btn:visited {text-transform: uppercase;text-decoration: none;
				padding: 8px 32px; display: inline-block;
				transition: all .2s; position: relative; font-size: 12px;
				border: none; cursor: pointer;}
			.btn:hover {transform: translateY(-3px); box-shadow: 0 7.5px 15px rgba(0, 0, 0, 0.2);}
			.btn:hover::after {transform: scaleX(1.4) scaleY(1.6); opacity: 0;}
			.btn:active, .btn:focus {outline: none; transform: translateY(-1px); box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);}
			.p-bigger {font-size: 20px}"
		)
	),
	tags$div(
		id = "main_page_ui",
		bs4DashPage(
			title = "Explore your Data | vedha.space",
			sidebar_collapsed = TRUE,
			navbar = bs4DashNavbar(
				status = "white"
			),
			sidebar = bs4DashSidebar(
				skin = "dark",
				status = "secondary",
				title = "vedha.space",
				brandColor = "secondary",
				url = "http://www.vedha.space/",
				src = "vedha_space.png",
				elevation = 3,
				opacity = 0.9,
				bs4SidebarMenu(
					bs4SidebarMenuItem(
						text = "Data Source",
						tabName = "data_source_tab",
						icon = "database"
					),
					bs4SidebarMenuItem(
						text = "Summary",
						tabName = "summary_tab",
						icon = "hourglass-half"
					),
					bs4SidebarMenuItem(
						text = "Plots",
						tabName = "plots_tab",
						icon = "chart-bar"
					)
				)
			),
			body = bs4DashBody(
				bs4TabItems(data_source_ui, summary_ui)
			)
		)
	)
)

server = function(input, output, session) {
	analysisData <- data.frame()
	output$sample_data_download <- downloadHandler(  # downloads data
		filename = function() {
			"sample_data.csv"
		},
		content = function(file) {
			write.csv(read.csv("mtcars.csv"), file, row.names = FALSE)
		}
	)
	observeEvent(input$data_source_input_file, {
		inFile <- input$data_source_input_file
		analysisData <<- read.csv(inFile$datapath,stringsAsFactors = FALSE, header = TRUE)
		output$input_data_table <- renderDT({
			datatable(analysisData, rownames = FALSE, options = list(dom = 't'))
		})
		data_input__trigger$trigger()
	})
	output$correlation_plot <- renderPlot({
		data_input__trigger$depend()
		if (nrow(analysisData) == 0) {
			# popUpWindow("Either the data you uploaded is empty or you forgot to upload the file in the 'Data Source' tab")
			return(textPlot())
		}
		numericAnalysisData <- analysisData %>% select_if(is.numeric)
		if (nrow(numericAnalysisData) == 0) {
			popUpWindow("Your data does not does not contain any numeric columns")
			return(textPlot())
		}
		color_palette <- colorRampPalette(c(input$corr_color1, "#ffffff", input$corr_color2))
		corrplot(
		    cor(numericAnalysisData), method = input$corr_method,
		    type = input$corr_type, order = input$corr_order,
		    col = color_palette(100), tl.col = "black"
		)
	})
	output$correlation_plot_ui <- renderUI({
		plotOutput("correlation_plot", height = paste0(input$corr_width, "px"))
	})
}

shinyApp(ui, server)
