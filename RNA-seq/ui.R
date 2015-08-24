shinyUI(bootstrapPage(
				# Add custom CSS & Javascript;
				tagList(
						tags$head(
								tags$link(rel="stylesheet", type="text/css",href="style.css"),
								tags$script(type="text/javascript", src = "md5.js"),
								tags$script(type="text/javascript", src = "passwdInputBinding.js")
						)
				),
				
				## Login module;
				  theme = shinytheme("flatly"),
				  fluidRow(column(12, tags$header(strong(HTML("<p align = 'center'>RNA-seq Viz: Bringing <span style='color: red;'>D3</span> visualization to RNA-seq!")), style = "font-size: 50px; 
                                  background-color: #F0FFFF;"))),
				tags$hr(),
				div(class = "login",
						uiOutput("uiLogin"),
						textOutput("pass")
				),
				uiOutput("Navbar")
		))
