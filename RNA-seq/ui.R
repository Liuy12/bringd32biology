shinyUI(bootstrapPage(
  # Add custom CSS & Javascript;
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$script(type="text/javascript", src = "md5.js"),
      tags$script(type="text/javascript", src = "passwdInputBinding.js")
      # 								tags$script(type="text/javascript", src = "PentagonHover.js"),
      # 								tags$script(type="text/javascript", charset="UTF-8", src="https://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js")
    )
  ),
  HTML("<style>
         .tooltip {
           opacity:1;
         }
       </style>"),
  ## Login module;
  theme = shinytheme("flatly"),
  uiOutput("uiLogin"),
  uiOutput("uiSignup"),
  conditionalPanel(condition = "output.LoginStatus",
                   dashboardPage(skin = 'green',
                     dashboardHeader(title = "RNA-seq Vis"),
                     dashboardSidebar(
                       sidebarMenu(id = 'sidebar', 
                                   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                   menuItem("Application", tabName = "application", icon = icon("th"),
                                            menuSubItem('Docomentation', tabName = "doc", icon = icon("file-text")),
                                            menuSubItem('Start analysis', tabName = 'Analysis', icon = icon("bolt"))
                                   ),
                                   menuItem("Account", tabName = "Account", icon = icon('user')),
                                   menuItem("About us", tabName = "aboutus", icon = icon("users"))
                       ),
                       conditionalPanel(condition = "input.sidebar == 'Analysis'",
                                        uiOutput("AnalysisPanel"))
                     ),
                     dashboardBody(
                       tabItems(
                         # First tab content
                         tabItem(tabName = "dashboard", uiOutput("dashboardpage")),
                         tabItem(tabName = "Analysis", 
                                 uiOutput("Navpage")),
                         tabItem(tabName = "Account",
                                 uiOutput("AccountInfo")),
                         tabItem(tabName = "aboutus", "Things to do")
                       )
                     )
                   )
    )
))
