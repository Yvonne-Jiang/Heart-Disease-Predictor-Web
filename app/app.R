library(shiny)
library(shinydashboard)
source('Logisitic.R')

# Header -------
header <-
  dashboardHeader(title = "Heart Failure Predictor", titleWidth = 320)

# Sidebar ------
sidebar <- dashboardSidebar(width = 320, sidebarMenu(
  menuItem(
    "Prediction",
    tabName = "Prediction",
    icon = icon("magnifying-glass")
  ),
  menuItem("Instruction",
           tabName = "Instruction",
           icon = icon("book"))
))

# Body ------
Body1 <- fluidRow(
  column(
    width = 12,
    title = h1("Enter Personal Details"),
    solidHeader = T,
    status = "primary"
  ),
  
  # age
  column(4,
         numericInput(
           "age", label = h4("Age"), value = 30
         )),
  
  # gender
  column(
    4,
    selectInput(
      "gender",
      label = h4("Gender"),
      selectize = TRUE,
      choices = list("Female" = 0, "Male" = 1)
    )
  ),
  
  # cp
  column(4,
         selectInput(
           "cp",
           label = h4("Chest Pain Type"),
           selectize = TRUE,
           choices = list(
             "Typical Angina" = 1,
             "Atypical Angina" = 2,
             "Non Anginal" = 3,
             "Asymptotic" = 4
           )
         ))
)

Body2 <- fluidRow(# trestbps
  column(4,
         numericInput(
           "trestbps",
           label = h4("Resting Blood Pressure"),
           value = 120
         )),
  
  # chol
  column(4,
         numericInput(
           "chol",
           label = h4("Serum Cholestoral"),
           value = 200
         )),
  
  # fbs type change
  column(4,
         selectInput(
           "fbs",
           label = h4("Fasting Blood Sugar"),
           choices = list("< 120 mg/dl" = 0, "> 120 mg/dl" = 1)
         )))

Body3 <- fluidRow(
  # restecg
  column(
    4,
    selectInput(
      "restecg",
      label = h4("Resting ECG Result"),
      selectize = TRUE,
      choices = list(
        "Normal" = 0,
        "Having ST-T Wave Abnormality" = 1,
        "Showing Left Ventricular Hypertrophy" = 2
      )
    )
  ),
  
  # thalach
  column(4,
         numericInput(
           "thalach",
           label = h4("Max Heart Rate Achieved"),
           value = 150
         )),
  
  # exang
  column(
    4,
    selectInput(
      "exang",
      label = h4("Exercise Induced Angina"),
      selectize = TRUE,
      choices = list("No" = 0,
                     "Yes" = 1)
    )
  ))



Body4 <- fluidRow(
  # oldpeak
  column(4,
         numericInput(
           "oldpeak",
           label = h4("ST Depression Induced by Exercise"),
           value = 1,
           step = 0.1
         )),
  
  # slope
  column(
    4,
    selectInput(
      "slope",
      label = h4("Slope of the ST Segment"),
      selectize = TRUE,
      choices = list(
        "Upsloping" = 1,
        "Flat" = 2,
        "Downsloping" = 3
      )
    )
  ),
  
  # ca
  column(4,
         selectInput(
           "ca",
           label = h4("Number of Major Vessels Colored"),
           choices = c(0, 1, 2, 3)
         )))

Body5 <- fluidRow(
  # thal
  column(
    4,
    selectInput(
      "thal",
      label = h4("Thalassemia"),
      selectize = TRUE,
      choices = list(
        "Normal" = 3,
        "Fixed defect" = 6,
        "Reversable defect" = 7
      ),
      br()
    )
  ))

Body6 <- fluidRow(column(
  10,
  br(),
  actionButton("pred", "Predict"),
  br(),
  br(),
  textOutput("Result"),
  tags$head(tags$style("#Result{color: black;
                                 font-size: 18px;
            font-style: bold;
            }"))
))

Instruction <- fluidRow(column(
  col = 2,
  width = 12,
  offset = 2,
  box(
    title = "Instructions For the Web App",
    solidHeader = T,
    strong("Model..."),
    p("after the doc...")
  )
))

body <-
  dashboardBody(tabItems(
    tabItem(tabName = "Prediction", Body1,
            Body2,
            Body3,
            Body4,
            Body5,
            Body6),
    tabItem(tabName = "Instruction", Instruction)
  ))

ui <-
  dashboardPage(title = 'Heart Failure Predictor',
                header,
                sidebar,
                body)

# ui <- fluidPage(titlePanel("Heart Failure Predictor"),
#                 Body1,
#                 Body2,
#                 Body3,
#                 Body4,
#                 Body5,
#                 Body6)

# Server -----

server <- function(input, output) {
  pred <- eventReactive(input$pred, {
    age <- as.numeric(as.character(input$age))
    sex <- as.factor(input$gender)
    cp <- as.factor(input$cp)
    trestbps <- as.numeric(as.character(input$trestbps))
    chol <- as.numeric(as.character(input$chol))
    fbs <- as.factor(input$fbs)
    restecg <- as.factor(input$restecg)
    thalach <-
      as.numeric(as.character(input$thalach))
    exang <- as.factor(input$exang)
    oldpeak <- as.numeric(as.character(input$oldpeak))
    slope <- as.factor(input$slope)
    ca <- as.factor(input$ca)
    thal <- as.factor(input$thal)
    
    newData <-
      data.frame(
        "age" = age,
        "sex" = sex,
        "cp" = cp,
        "trestbps" = trestbps,
        "chol" = chol,
        "fbs" = fbs,
        "restecg" = restecg,
        "thalach" = thalach,
        "exang" = exang,
        "oldpeak" = oldpeak,
        "slope" = slope,
        "ca" = ca,
        "thal" = thal
      )
    predict(m.logistic, newData, type = "response")[[1]]
  })
  
  # observeEvent(input$pred, {
  #   output$Result <- renderText({
  #     pred()
  #   })
  # })
  
  observeEvent(input$pred, {
    output$Result <- renderText({
      str1 <-
        paste(
          "Based on your current reports, you will have a",
          round(pred(), 4) * 100,
          "% probability of having a heart disease."
        )
      str1
    })
  })
}

shinyApp(ui, server)