library(tidyverse)
library(shinydashboard)
library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
      dashboardHeader(title = "Metabolismo Calc"),
  dashboardSidebar(
    sliderInput("peso",label = "Selecione seu peso", min = 10, max = 250, value = 90, step = 1),
    sliderInput("altura",label = "Selecione sua altura", min = 100, max = 250, value = 180, step = 1),
    sliderInput("idade",label = "Selecione sua idade", min = 10, max = 115, value = 30, step = 1),
    selectInput("atividade",label = "Selecione seu nível de atividade",
                choices = c("Sedentário","Levemente Ativo","Moderadamente Ativo","Muito Ativo","Intensamente Ativo"), selected = "Sedentário")
    ),
    
    dashboardBody(
      fluidRow(column(12, markdown("  ### Instruções
      
                                      Ao lado Selecione seu peso, altura, idade e nível de atividade.
                                      
                                      ### Níveis de Atividade
                                      
                                      * Sedentário (pouco ou nenhum exercício): MB x 1,2
                                      * Levemente ativo (exercício leve ou esportes 1-3 dias/semana): MB x 1,375
                                      * Moderadamente ativo (exercício moderado 3-5 dias/semana): MB x 1,55
                                      * Muito ativo (exercício intenso 6-7 dias/semana): MB x 1,725
                                      * Extremamente ativo (exercício intenso diário ou exercício físico + trabalho físico): MB x 1,9
                                      
                                      ### Fórmula de Cálculo do Metabolismo Basal
                                      
                                      Metabolismo Basal (MB) = 66,5 + (13,75 x peso em kg) + (5,003 x altura em cm) - (6,75 x idade em anos)
                                      Metabolismo Basal (MB) * Fator de Atividade"))),
      fluidRow(column(12, h2(uiOutput(outputId = "resultado")))),
      fluidRow(column(12, actionButton("calcular", "Calcular",width = 150)))))

# Define server logic required to draw a histogram
server <- function(input, output) {

  metabolismo <- function(peso, altura, idade, atividade){
    
    atividade <- str_to_lower(atividade)
    
    mb <- 66.5 + (13.75 * peso) + (5.003 * altura) - (6.75 * idade)
    
    if(str_detect(atividade, "^sed")){
      res <- mb * 1.2
    }else if(str_detect(atividade, "leve")){
      res <- mb * 1.375
    }else if(str_detect(atividade, "modera")){
      res <- mb * 1.55
    }else if(str_detect(atividade, "muito")){
      res <- mb * 1.725
    }else{
      res <- mb * 1.9
    }
    
    return(res)
  
  }
  
  
  
    observeEvent(input$calcular,{
    
    func <- as.character(round(metabolismo(input$peso, input$altura, input$idade, input$atividade),0))
    
    resultado <- HTML(paste0("Para perda de peso consuma menos que " ,func, " kcal em seu dia.",br(),br(),
                        "Para ganho de peso consuma mais que ",func, " kcal em seu dia.",br(),br() ))
    
    output$resultado <- renderUI({ resultado })
    
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
