#### Log in module ###
USER <- reactiveValues(Logged = Logged,Group=Group)

output$LoginStatus <- reactive({
  USER$Logged
})

outputOptions(output, 'LoginStatus', suspendWhenHidden=FALSE)
  
output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
	return(
	  fluidRow(column(3, tags$header(strong("RNA-seq Vis"), style = "font-size: 50px;margin-top: 25px; 
                                  ")),
	           column(2, offset = 1, textInput("userName", "User Name:"), style = "margin-top: 25px"),
	           column(2, offset = 0.5, passwordInput("passwd", "Password:"), style = "margin-top: 25px"),
	           column(1, offset = 0.5, actionButton("Login", "Log in"), style = "margin-top: 50px"),
	           column(1, offset = 0.5, tags$p(strong('or')), style = "margin-top: 55px;font-size: 20px"),
	           column(2, loginOutput("loginButton"), style = "margin-top: 50px;"),
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
               style = "background-color: #F0F8FF; height: 1000px;"
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

# output$pass <- renderText({  
#   if (USER$Logged == FALSE) {
#       if (!is.null(input$Login)) {
#         if (input$Login > 0) {
#           Username <- isolate(input$userName)
#           Password <- isolate(input$passwd)
#           usernameInd <- which(GLOBALDATA$userData$user == Username)
#           if (length(usernameInd) > 0) {
#             if (GLOBALDATA$userData$password[usernameInd[1]]==Password) {
#               USER$Logged <- TRUE
#               USER$Group <- GLOBALDATA$userData$group[usernameInd[1]]
#             }
#             else  {
#               "Password failed!"
#             }
#           } else  {
#             "User name doesn't exist!"
#           }
#         }
#       }
#     }
# })

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if(!is.null(shiny::isolate(access_token()))){
      USER$Logged <- TRUE
    }
    else{
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
  }
})

options("googleAuthR.webapp.client_id" = "254033886814-u7g1elsitroqliemldh3bmoei906vsf0.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "AO04YcCbnWXp6d5DDj-SaUk3")
options("googleAuthR.scopes.selected" = "https://www.googleapis.com/auth/analytics")


## Get auth code from return URL
access_token  <- reactiveAccessToken(session)


## Make a loginButton to display using loginOutput
output$loginButton <- renderLogin(session, access_token())


# output$loginButton <- shiny::renderUI({
#   if(is.null(shiny::isolate(access_token())) && USER$Logged == FALSE) {
#     shiny::actionLink("signed_in",
#                       shiny::a('Login via google', 
#                                href = gar_shiny_getAuthUrl(gar_shiny_getUrl(session)), 
#                                class="btn btn-primary", 
#                                role="button"))
#   } else {
#     shiny::a('Logout', 
#              href = gar_shiny_getUrl(session), 
#              class="btn btn-default", 
#              role="button")
#   }
# })


# observe({if(is.null(shiny::isolate(access_token()))) {
#   output$loginButton <- shiny::renderUI({
#       shiny::actionButton("signed_in",
#                         shiny::a('Login via google', 
#                                  href = gar_shiny_getAuthUrl(gar_shiny_getUrl(session)), 
#                                  class="btn btn-primary", 
#                                  role="button"))
#   }) }
#   else {
#     output$loginButton <- shiny::renderUI({
#     shiny::a('Logout', 
#              href = gar_shiny_getUrl(session), 
#              class="btn btn-default", 
#              role="button")
#     })
#   }
# })  



SignUpCount <- 0

SignupFun <- reactive({
  if (!is.null(input$Signup) &&
      input$Signup == SignUpCount) {
    return(NULL)
  } else {
    userName <- isolate(input$userName_signup)
    userPass1 <- isolate(input$passwd_signup)
    userPass2 <- isolate(input$passwd_signup2)
    group <- 'user'
    
    if (userPass1 != userPass2) {
      SignUpCount <<- input$Signup
      return("The passwords should be same in two inputs.")
    }
    
    temp2 <-
      c(userName,userPass1, group)
    userData <<- rbind(userData,temp2)
    GLOBALDATA$userData <<- userData
    write.csv(GLOBALDATA$userData,userFile,row.names = F)
    SignUpCount <<- input$Signup
    return(paste0(
      "You have successfully signed up as '", userName , "'. Please log in using your username and password." 
    ))
  }
})

output$Signupresult <- renderText({
  SignupFun()
})


