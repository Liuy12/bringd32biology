shinyUI(fluidPage(
  # Add custom CSS & Javascript;
  tagList(
    tags$head(
      tags$script(type="text/javascript", src = "md5.js"),
      tags$script(type="text/javascript", src = "passwdInputBinding.js"),
      HTML("<style>
         .tooltip {
           opacity:1;
           }
           </style>")
    )
  ),
  ## Login module;
  theme = shinytheme("flatly"),
  uiOutput("uiLogin"),
  uiOutput("uiSignup"),
  conditionalPanel(condition = "output.LoginStatus",
                   dashboardPage(skin = 'green',
                     dashboardHeader(title = "RNA-seq Vis"),
                     dashboardSidebar(
                       sidebarMenu(id = 'sidebar', 
                                   sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                                     label = "Search..."),
                                   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                   menuItem("Start analysis", tabName = "Analysis", icon = icon("bolt"),
                                            menuSubItem("Set input options", tabName = 'SetInput', icon = icon("flag")),
                                            menuSubItem('Charts', tabName = 'Charts', icon = icon("bar-chart"))
                                   ),
                                   menuItem('Docomentation', tabName = "doc", icon = icon("file-text")),
                                   menuItem("Account", tabName = "Account", icon = icon('user')),
                                   menuItem("About us", tabName = "aboutus", icon = icon("users"))
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         # First tab content
                         tabItem(tabName = "dashboard", "To do"),
                         tabItem(tabName = "SetInput", 
                                 uiOutput("InputBox")),
                         tabItem(tabName = "Charts",
                                 uiOutput("Chartpage")),
                         tabItem(tabName = "Account",
                                 uiOutput("AccountInfo")),
                         tabItem(tabName = "aboutus", "Things to do")
                       )
                     )
                   )
    )
))
