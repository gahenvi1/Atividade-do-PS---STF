library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)
library(DT)
library(plotly)
library(scales)

Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

# Load the dataset
banco <- read_excel("~/Documents/Atividade do PS - STF/banco/4797cb48-0ba9-4e66-832f-0ec6e43f44dd.xlsx") 

# UI Layout
ui <- dashboardPage(
  dashboardHeader(title = "STF Process Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Top 10 Processes", tabName = "top10", icon = icon("list")),
      menuItem("Old Processes by Minister", tabName = "old_processes", icon = icon("clock")),
      menuItem("Decision Status", tabName = "decision_status", icon = icon("gavel"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Tab 1: Top 10 Oldest Processes
      tabItem(tabName = "top10",
        fluidRow(
          box(title = "Filters", width = 12, status = "info", solidHeader = TRUE,
            selectInput("class_select", "Select Class:", choices = unique(banco$Classe), selected = "HC"),
            dateRangeInput("date_range", "Select Date Range:", start = min(banco$`Data autuação`), end = max(banco$`Data autuação`))
          )
        ),
        fluidRow(
          box(title = "Processes per Minister", width = 6, plotlyOutput("plot_top10_minister")),
          box(title = "Processes per Law Branch", width = 6, plotlyOutput("plot_top10_lawbranch"))
        )
      ),
      
      # Tab 2: Old Processes per Minister
      tabItem(tabName = "old_processes",
        fluidRow(
          box(title = "Filters", width = 12, status = "info", solidHeader = TRUE,
              sliderInput("days_old", "Days Since Last Update:", min = 30, max = 365, value = 180, step = 10),
              selectInput("minister_select", "Select Minister:", choices = unique(banco$Relator), multiple = TRUE)
          )
        ),
        fluidRow(
          box(title = "Old Processes by Minister", width = 12, plotlyOutput("plot_old_processes"))
        )
      ),
      
      # Tab 3: Decision Status
      tabItem(tabName = "decision_status",
        fluidRow(
          box(title = "Filters", width = 12, status = "info", solidHeader = TRUE,
              selectInput("decision_select", "Decision Status:", choices = unique(banco$`Situação da decisão final`), multiple = TRUE)
          )
        ),
        fluidRow(
          box(title = "Decisions per Minister", width = 12, plotlyOutput("plot_decision_status"))
        )
      )
      
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Reactive Data for Top 10 Processes
  filtered_top10 <- reactive({
    banco %>%
      filter(Classe == input$class_select,
             `Data autuação` >= input$date_range[1],
             `Data autuação` <= input$date_range[2]) %>%
      arrange(`Data autuação`) %>%
      head(10) %>%
      mutate(
        Processo = str_to_sentence(Processo) %>% str_remove("^Hc\\s+"),
        Relator = str_to_title(Relator),
        `Ramo do Direito` = str_to_sentence(`Ramo do Direito`),
        `Data autuação` = format(`Data autuação`, "%d de %B de %Y")
      )
  })
  
  # Reactive Data for Old Processes
  filtered_old <- reactive({
    banco %>%
      filter(`Data último andamento` < (Sys.time() - days(input$days_old)),
             Relator %in% input$minister_select) %>%
      group_by(Relator) %>%
      summarise(Quantidade = n()) %>%
      mutate(Frequencia = Quantidade / sum(Quantidade),
             Relator = str_to_title(Relator))
  })
  
  # Reactive Data for Decision Status
  filtered_decision <- reactive({
    banco %>%
      filter(`Situação da decisão final` %in% input$decision_select) %>%
      group_by(Relator, `Situação da decisão final`) %>%
      summarise(Quantidade = n()) %>%
      mutate(Frequencia = Quantidade / sum(Quantidade),
             Relator = str_to_title(Relator))
  })
  
  # Top 10 Processes - Minister
  output$plot_top10_minister <- renderPlotly({
    g1 <- filtered_top10() %>%
      count(Relator, name = "Quantidade") %>%
      ggplot(aes(x = fct_reorder(Relator, Quantidade), y = Quantidade, text = paste("Relator:", Relator, "<br>Quantidade:", Quantidade))) +
      geom_col(fill = "#3a445d") +
      coord_flip() +
      theme_bw() +
      labs(y = "Quantidade", x = "")

    ggplotly(g1, tooltip = "text")
  })
  
  # Top 10 Processes - Law Branch
  output$plot_top10_lawbranch <- renderPlotly({
    g2 <- filtered_top10() %>%
      count(`Ramo do Direito`, name = "Quantidade") %>%
      ggplot(aes(x = fct_reorder(`Ramo do Direito`, Quantidade), y = Quantidade, text = paste("Ramo:", `Ramo do Direito`, "<br>Quantidade:", Quantidade))) +
      geom_col(fill = "#3a445d") +
      coord_flip() +
      theme_bw() +
      labs(y = "Quantidade", x = "")

    ggplotly(g2, tooltip = "text")
  })
  
  # Old Processes by Minister
  output$plot_old_processes <- renderPlotly({
    g3 <- filtered_old() %>%
      ggplot(aes(x = fct_reorder(Relator, Quantidade), y = Quantidade, text = paste("Relator:", Relator, "<br>Quantidade:", Quantidade, "<br>Frequência:", percent(Frequencia)))) +
      geom_col(fill = "#3a445d") +
      coord_flip() +
      theme_bw() +
      labs(y = "Quantidade", x = "")

    ggplotly(g3, tooltip = "text")
  })
  
  # Decision Status per Minister
  output$plot_decision_status <- renderPlotly({
    g4 <- filtered_decision() %>%
      ggplot(aes(x = fct_reorder(Relator, Quantidade), y = Quantidade, fill = `Situação da decisão final`, text = paste("Relator:", Relator, "<br>Quantidade:", Quantidade, "<br>Frequência:", percent(Frequencia)))) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c("Com decisão final" = "#3a445d", "Sem decisão final" = "#DC758F")) +
      coord_flip() +
      theme_bw() +
      labs(y = "Quantidade", x = "")

    ggplotly(g4, tooltip = "text")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
