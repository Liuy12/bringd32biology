#### Log in module ###
USER <- reactiveValues(Logged = Logged,Group=Group)

output$LoginStatus <- reactive({
  USER$Logged
})

outputOptions(output, 'LoginStatus', suspendWhenHidden=FALSE)
  
output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
	return(
	  fluidRow(column(3, tags$header(strong("RNA-seq Viz"), style = "font-size: 50px; 
                                  ")),
	           column(2, offset = 3, textInput("userName", "User Name:"), style = "margin-top: 25px"),
	           column(2, offset = 0.5, passwordInput("passwd", "Password:"), style = "margin-top: 25px"),
	           column(2, offset = 0.5, actionButton("Login", "Log in"), style = "margin-top: 45px"),
	           column(2, offset = 0.5, textOutput("pass")),
	           style = "background-color: #87CEFA;"
	           )
	  )
  }
})

output$uiSignup <- renderUI({
  if(USER$Logged == FALSE) {
    return(
      fluidRow(column(6, forceNetworkOutput("ForceNetdemo")),
               column(4, h3("Sign up"), offset = 1,
                      textInput("userName_signup", label = "Username"),
                      passwordInput("passwd_signup", label = "Password"),
                      passwordInput("passwd_signup2", label = "Re-enter password"),
                      actionButton("Signup", "Sign up"),
                      h4(textOutput("Signupresult"))
               ),
               style = "background-color: #F0F8FF"
      )
    )
    }
  })

output$ForceNetdemo <- renderForceNetwork({
  data(MisLinks, MisNodes)
  forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
               Target = "target", Value = "value", NodeID = "name",
               Group = "group", opacity = 0.8)
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
   if (input$Login > 0) {
      Username <- isolate(input$userName)
      Password <- isolate(input$passwd)
	usernameInd <- which(GLOBALDATA$userData$user == Username)
	  if (length(usernameInd) > 0) {
		  if (GLOBALDATA$userData$password[usernameInd[1]]==Password) {
			  USER$Logged <- TRUE
			  
			  USER$Group <- GLOBALDATA$userData$group[usernameInd[1]]
		  }
		  else  {
			  "Password failed!"
		  }
	  } else  {
		  "User name doesn't exist!"
	  }
    } 
    }
  }
})

