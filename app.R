library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
# library(sodium)


# pages
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("SIGN IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("loggedIn", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     shinyjs::hidden(div(id = "no_existing",
                                         tags$p("Oops! This email is not existing!",
                                                style = "color: red; font-weight: 600; 
                                              padding-top: 5px;font-size:16px;", 
                                                class = "text-center")))
                   )
                 )
)

signuppage <- div(id = "signuppage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                  wellPanel(
                    tags$h2("SIGN UP", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                    textInput("email", label = "Email"),
                    textInput("phonNum", label = "Phone Number"),
                    textInput("firstName", label = "First Name"),
                    textInput("midName", label = "Middle Name"),
                    textInput("lastName", label = "Last Name"),
                    textInput("mailAdd", label = "Mailing Address"),
                    textInput("occupation", label = "Occupation"),
                    textInput("userName_su", label = "Username"),
                    textInput("passwd_su", label = "Password"),
                    # passwordInput("passwd", label = "Password"),
                    br(),
                    div(
                      style = "text-align: center;",
                      actionButton("submitted", "SUBMIT", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                      shinyjs::hidden(
                        div(id = "existing_username",
                            tags$p("Oops! This username has been used!",
                                   style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                   class = "text-center"))),
                      br(),
                      shinyjs::hidden(div(id = "existing_email",
                                          tags$p("Oops! This email address has been used!",
                                                 style = "color: red; font-weight: 600; 
                                              padding-top: 5px;font-size:16px;", 
                                                 class = "text-center")),
                                      br(),
                                      shinyjs::hidden(div(id = "incomplete_info",
                                                          tags$p("Oops! Please fill in all fields!",
                                                                 style = "color: red; font-weight: 600; 
                              padding-top: 5px;font-size:16px;", 
                                                                 class = "text-center")))               
                      )
                    )
                  )
)

emailpage <- div(id = "emailpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("Welcome! This is your email.", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;")
                 )
)

retrievepage <- div(id = "retrievepage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                    wellPanel(
                      tags$h2("RETRIEVE USERNAME AND/OR PASSWORD", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                      textInput("email_rtr", label = "What is your email address?"),
                      br(),
                      div(
                        style = "text-align: center;",
                        actionButton("retrieved", "RETRIEVE", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                        shinyjs::hidden(
                          div(id = "noexist",
                              tags$p("Oops! This email address does not exist!",
                                     style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                     class = "text-center")))
                      ))
)


infopage <- fluidPage(
  textOutput('userinfo'),
  textOutput('passwdinfo'),
  actionButton("login_from_info", "LOG IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 6px 10px; width: 130px; cursor: pointer;
                                 font-size: 12px; font-weight: 400;"),
)


# ui
header <- dashboardHeader( title = "Singularity Inc", uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"), 
                            actionButton(inputId = 'sign_in', label = 'Sign in'), 
                            actionButton(inputId = 'sign_up', label = 'Sign up'),
                            actionButton(inputId = 'retrieve', label = 'Forgot username/password')
                            #, actionButton(inputId = 'upload', label = 'Upload file')
                            ) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

# server
server <- function(input, output) {
  
  login <- FALSE
  intension <- ""
  email <- ""
  
  fn <- "data/user_info.rds"
  if (file.exists(fn)) {
    credentials <- readRDS(fn)
  } else {
    credentials <- data.frame()
  }
  
  USER <- reactiveValues(login = login, intension = intension, email = email)
  
  observeEvent(input$sign_in, {
    USER$intension <- 'want_sign_in'
    USER$login <- F
  })
  
  observeEvent(input$sign_up, {
    USER$intension <- 'want_sign_up'
    USER$login <- F
  })
  
  observeEvent(input$retrieve, {
    USER$intension <- 'want_retrieve'
    USER$login <- F
  })
  
  # clicked log in botton, set login = T and intension as 'logged_in' if success, otherwise display alert
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$loggedIn)) {
        if (input$loggedIn > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          indx <-  which(tolower(credentials$username_id) == 
                        tolower(Username))
          if(length(indx) == 1) { 
            pasmatch  <- credentials["passod"][indx,]
            # pasverify <- password_verify(pasmatch, Password)
            pasverify <- pasmatch == Password
            if(pasverify) {
              USER$login <- TRUE
              USER$intension <- 'logged_in'
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "no_existing", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "no_existing", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  # click submit sign up botton, set login = T and save information if success
  observe({ 
    if (USER$login == FALSE) {
      if ( !is.null(input$submitted)) {
        if ( input$submitted > 0 ){
          # extract info
          Email <- isolate(input$email)
          Username <- isolate(input$userName_su)
          Password <- isolate(input$passwd_su)
          phonNum <- isolate(input$phonNum)
          firstName <- isolate(input$firstName)
          midName <- isolate(input$midName)
          lastName <- isolate(input$lastName)
          mailAdd <- isolate(input$mailAdd)
          occupation <- isolate(input$occupation)
          # check existing & incomplete infor
          existing <- length(which(tolower(credentials$username_id) == 
                                     tolower(Username))) == 1
          existing_email <- length(which(tolower(credentials$email_id) == 
                                           tolower(Email))) == 1
          incomplete <- Email == '' | phonNum == '' | firstName == '' | 
            midName == '' | lastName == '' | mailAdd == '' | 
            occupation == '' | Username == '' | Password == '' 
          if (existing_email) {
            shinyjs::toggle(id = "existing_email", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "existing_email", anim = TRUE, time = 1, animType = "fade"))
          } else if (existing) {
            shinyjs::toggle(id = "existing_username", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "existing_username", anim = TRUE, time = 1, animType = "fade"))
          } else if (incomplete) {
            shinyjs::toggle(id = "incomplete_info", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "incomplete_info", anim = TRUE, time = 1, animType = "fade"))
          } else {
            # add user info, if successed sign up
            newUser <- data.frame(
              email_id = Email,
              username_id = Username,
              passod = Password,
              stringsAsFactors = F)
            credentials <- rbind(credentials, newUser)
            saveRDS(credentials, file = fn)
            USER$login <- TRUE
            USER$intension <- 'logged_in'
          }
        }
      } 
    }
  })
  
  # click retrieve password botton
  observe({ 
    if ( !is.null(input$retrieved)) {
      if ( input$retrieved > 0 ){
        Email <- isolate(input$email_rtr)
        existing <- length(which(tolower(credentials$email_id) == 
                                   tolower(Email))) == 1
        if (existing) {
          USER$email <- Email
          USER$intension <- 'display_info'
        } else {
          shinyjs::toggle(id = "noexist", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(3000, shinyjs::toggle(id = "noexist", anim = TRUE, time = 1, animType = "fade"))
        }
      }
    }  
  })
  
  # back to log in from display information
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login_from_info)) {
        if (input$login_from_info > 0) {
          USER$intension <- 'want_sign_in'
        } 
      }
    }    
  })
  
  # if logged in
  # display log ou functionality
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$body <- renderUI({
    if (USER$intension == 'want_sign_in') {
      loginpage
    } else if (USER$intension == 'want_sign_up') {
      signuppage
    } else if (USER$intension == 'want_retrieve') {
      retrievepage
    } else if (USER$intension == 'display_info') {
      # find user & password info
      indx <- which(tolower(credentials$email_id) == tolower(USER$email))
      username <- credentials$username_id[indx]
      passwd <- credentials$passod[indx]
      output$userinfo <- renderText({paste0('Username: ', username)})
      output$passwdinfo <- renderText({paste0('Password: ', passwd)})
      # display
      infopage
    } else if (USER$intension == 'logged_in') {
      emailpage
    } 
  })
  
}

shinyApp(server = server, ui = ui)