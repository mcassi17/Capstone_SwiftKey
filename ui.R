library(shiny)
shinyUI(
  pageWithSidebar(
    headerPanel(span("Next Word Predictor", 
                     style = "color:blue ; font-family: 'courier'")
    ),
    
    sidebarPanel(
      numericInput('temp', 'Temperature in Fahrenheit', 32),
      submitButton('Predict Next Word'),
      p(),
      p(strong("Directions:")),
      p("In order to use this app, you will need to enter a
        phrase (muliple words) with the last word omitted."),
      p("Once you hit the Predict Next Word button, the app will predict
        the next word."),
      p("The app uses ngrams to predict the next word of the input 
        sentence")
      ),
    mainPanel(
      h3('Word Prediction', style = "color:green"),
      h4("You're Input Text", style = "color:red"),
      verbatimTextOutput("inputValue"),
      h4('Next Word Choices', style = "color:blue"),
      verbatimTextOutput("prediction")
    )
    )
  )