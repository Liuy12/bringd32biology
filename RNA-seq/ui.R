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
  uiOutput("Navbar")
))
