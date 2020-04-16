library(shiny)


shinyUI(
  
  pageWithSidebar(
    
    ###Application name
    headerPanel("Would I survive?"
                
    ),
    
    sidebarPanel(h4("COVID-19 Patients charecteristics"),
      sliderInput("age", "Patient Age (Years):",
                             min = 0, max = 100, value = 55),
                 sliderInput("symptom_to_hospital", "Dely to admit hospital (Days):",
                             min = 0, max = 20, value = 5),
                 selectInput('gender','Gender', c("Male" = "male",
                                                  "Female" = "female")),
                  radioButtons("fever", "Fever?",
                                                c("Yes" = "1",
                                                  "No" = "0")),
                  radioButtons("sore_thrt", "Sore throat?",
                               c("Yes" = "1",
                                 "No" = "0")),
                  radioButtons("diff_breath", "Difficulty in breathing?",
                               c("Yes" = "1",
                                 "No" = "0")),
                  radioButtons("pneumonia", "Pneumonia?",
                               c("Yes" = "1",
                                 "No" = "0")),
                  radioButtons("cough", "Cough?",
                               c("Yes" = "1",
                                 "No" = "0")),
                  radioButtons("chills", "Chills?",
                               c("Yes" = "1",
                                 "No" = "0")),
                  radioButtons("fatigue", "Fatigue?",
                               c("Yes" = "1",
                                 "No" = "0")),
                  radioButtons("loss_appetit", "Loss of appetit?",
                               c("Yes" = "1",
                                 "No" = "0"))),
                          
    mainPanel(
      h3('Results'),
      h4('Patients inputs'),
      verbatimTextOutput("inputValue"),
      h4('Probability of survival'),
      verbatimTextOutput("prediction")
    )
  )
  
)
