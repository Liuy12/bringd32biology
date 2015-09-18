shinyUI(fluidPage(
  # Add custom CSS & Javascript;
  tagList(
    tags$head(
      tags$script(type="text/javascript", src = "JavaScript/md5.js"),
      tags$script(type="text/javascript", src = "JavaScript/passwdInputBinding.js"),
      tags$script(type="text/javascript", src = "JavaScript/GoogleAnalytics.js"),
      tags$link(type="text/css", rel="stylesheet", href="CSS/style.css"),
      tags$link(type="text/css", rel="stylesheet", href="https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"),
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
                     dashboardHeader(title = img(src = 'img/logo.png', style = "max-width:50%")),
                     dashboardSidebar(
                       sidebarMenu(id = 'sidebar', 
                                   sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                                     label = "Search..."),
                                   menuItem("Introduction", tabName = "introduction", icon = icon("circle-o")),
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
                         tabItem(tabName = "introduction", 
                                 includeHTML('www/HTML/carousel.html'),
                                 includeHTML('www/HTML/Introduction.html')),
                         tabItem(tabName = "SetInput", 
                                 uiOutput("InputBox")),
                         tabItem(tabName = "Charts",
                                 uiOutput("Chartpage")),
                         tabItem(tabName = 'doc',
                                 includeHTML('www/HTML/Documentation.html')),
                         tabItem(tabName = "Account",
                                 uiOutput("AccountInfo")),
                         tabItem(tabName = "aboutus", 
                                 fluidRow(
                                   valueBoxOutput('totalvisits'),
                                   valueBoxOutput('thismonthvisits'),
                                   valueBoxOutput('totalusers')),
                                 includeHTML('www/HTML/timeline.html')
                     )
                   )
    )
))
))
