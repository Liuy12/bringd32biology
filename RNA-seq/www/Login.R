#### Log in module ###
USER <- reactiveValues(Logged = Logged,Group=Group)

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
	return(
			list(
			  br(),
			  br(),
			  br(),
			  br(),
					column(12, wellPanel(
					  textInput("userName", "User Name:"),
					  passwordInput("passwd", "Password:"),
					  br(),
					  actionButton("Login", "Log in")
					), offset = 1)
					)
			)
  }
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

