library(shiny)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(plotly)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)


ui = dashboardPage(
  
  shinydashboard::dashboardHeader(title = "Dashboard UCI"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItem(text = "Demográficos",tabName = "demograficos",icon = icon("user-friends")),
      menuItem(text = "Percepción",tabName = "percepcion",icon = icon("lightbulb")),
      menuItem(text = "Servicio",tabName = "servicio",icon = icon("concierge-bell")),
      menuItem(text = "Institución",tabName = "institucion",icon = icon("hospital-alt")),
      hr(),
      p("Seleccione los filtros deseados:"),
      shinyWidgets::selectizeGroupUI(
        id = "my-filters",
        params = list(
          Preg1 = list(inputId = "Preg1", title = "Rando de Edad:"),
          Preg2 = list(inputId = "Preg2", title = "Sexo:"),
          Preg3 = list(inputId = "Preg3", title = "Escolaridad:"),
          Preg4 = list(inputId = "Preg4", title = "Cargo:"),
          Preg5 = list(inputId = "Preg5", title = "Horas Semanales:"),
          Preg6 = list(inputId = "Preg6", title = "Antigüedad:"),
          Preg7 = list(inputId = "Preg7", title = "Empleo Adicional:"),
          Preg8 = list(inputId = "Preg8", title = "Turno:")
          ),inline = FALSE)
      )
    ),
  shinydashboard::dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),
    tabItems(
      tabItem(tabName = "demograficos",
              fluidRow(valueBoxOutput("value1"),
                       valueBoxOutput("value2"),
                       valueBoxOutput("value3")),
              fluidRow(
                column(4,shinydashboard::box(plotlyOutput("p1"),width = "400px")),
                column(4,shinydashboard::box(plotlyOutput("p2"),width = "400px")),
                column(4,shinydashboard::box(plotlyOutput("p4"),width = "400px"))
              ),
              fluidRow(
                column(6,shinydashboard::box(plotlyOutput("p3"),width = "600px")),
                column(6,shinydashboard::box(plotlyOutput("p5"),width = "600px"))
                )),
      tabItem(tabName = "percepcion",
              # uiOutput("filtros"),
              fluidRow(
                column(3,shinydashboard::box(plotlyOutput("5"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("6"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("7"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("8"),width = "300px"))
              ),
              fluidRow(
                column(3,shinydashboard::box(plotlyOutput("9"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("10"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("11"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("12"),width = "300px"))
              )),
      tabItem(tabName = "servicio",
              # uiOutput("filtros"),
              fluidRow(
                column(3,shinydashboard::box(plotlyOutput("13"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("14"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("15"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("16"),width = "300px"))
              ),
              fluidRow(
                column(3,shinydashboard::box(plotlyOutput("17"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("18"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("19"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("20"),width = "300px"))
              )),
      tabItem(tabName = "institucion",
              # uiOutput("filtros"),
              fluidRow(
                column(3,shinydashboard::box(plotlyOutput("21"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("22"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("23"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("24"),width = "300px"))
              ),
              fluidRow(
                column(3,shinydashboard::box(plotlyOutput("25"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("26"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("27"),width = "300px")),
                column(3,shinydashboard::box(plotlyOutput("28"),width = "300px"))
              ))
    )
  )
)


# Define server logic required to draw a histogram
server = (function(input, output, session) {
  
  df = readRDS("BD_clean.rds")
  
  df_filter <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = df,
    vars = names(df)
  )
  
  output$value1 <- renderValueBox({
    
    
    valueBox(value = nrow(df_filter()),subtitle = "Cantidad Personas",icon = icon("user-alt"),color = "green")
    
  })
  
  output$value2 <- renderValueBox({
    
    female = df_filter() %>% filter(Preg2=="Femenino")
    
    valueBox(value = nrow(female),subtitle = "Cantidad Mujeres",icon = icon("female"), color = "blue")
    
  })
  
  output$value3 <- renderValueBox({
    
    male = df_filter() %>% filter(Preg2=="Masculino")
    
    valueBox(value = nrow(male),subtitle = "Cantidad Hombres",icon = icon("male"), color = "orange")
    
  })
  
    output$p1 <- renderPlotly({
      
      data = df_filter() %>% mutate(Masculino = ifelse(Preg2=="Masculino",1,0)) %>% dplyr::group_by(Preg1) %>% dplyr::summarize(Conteo = n(),
                                                                                                                                Masculino = sum(Masculino),
                                                                                                                                Femenino = Conteo-Masculino) 

      
      fig <- plot_ly(data, x = ~Preg1, y = ~Masculino, type = 'bar', name = 'Masculino')
      fig <- fig %>% add_trace(y = ~Femenino, name = 'Femenino')
      fig <- fig %>%  layout(title = "Distribución por Edad y Sexo",
                           xaxis = list(title = "Rando de Edad",showgrid = FALSE, zeroline = FALSE),
                           yaxis = list(title = "Cantidad",showgrid = FALSE, zeroline = FALSE),
                           paper_bgcolor = "white",
                           barmode = 'stack')
    
    fig
  })
    
    output$p2 <- renderPlotly({
      
      data = df_filter() %>% mutate(Trabaja = ifelse(Preg7=="Si",1,0)) %>% dplyr::group_by(Preg3) %>% dplyr::summarize(Conteo = n(),
                                                                                                                       trabajo = sum(Trabaja),
                                                                                                                       Notrabaja= Conteo-trabajo) 
      
      
      fig <- plot_ly(data, x = ~Preg3, y = ~trabajo, type = 'bar', name = 'Empleo adicional')
      fig <- fig %>% add_trace(y = ~Notrabaja, name = 'Unico Empleo')
      fig <- fig %>%  layout(title = "Escolaridad y Empleo",
                             xaxis = list(title = "Escolaridad",showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(title = "Cantidad",showgrid = FALSE, zeroline = FALSE),
                             paper_bgcolor = "white",
                             barmode = 'stack')
      
      
      
      fig
    })
    
    output$p3 <- renderPlotly({
      
      data = df_filter() %>% mutate(manana = ifelse(Preg8=="Mañana",1,0),
                                    tarde = ifelse(Preg8=="Tarde",1,0),
                                    nochea = ifelse(Preg8=="Noche A",1,0)) %>% dplyr::group_by(Preg4) %>% dplyr::summarize(Conteo = n(),
                                                                                                                       man = sum(manana),
                                                                                                                       tar= sum(tarde),
                                                                                                                       na = sum(nochea),
                                                                                                                       nb = Conteo-(man+tar+na)) 
      
      
      fig <- plot_ly(data, x = ~Preg4, y = ~man, type = 'bar', name = 'Mañana')
      fig <- fig %>% add_trace(y = ~tar, name = 'Tarde')
      fig <- fig %>% add_trace(y = ~na, name = 'Noche A')
      fig <- fig %>% add_trace(y = ~nb, name = 'Noche B')
    fig <- fig %>%  layout(title = "Cargo y Turno",
                             xaxis = list(title = "Cargo",showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(title = "Cantidad",showgrid = FALSE, zeroline = FALSE),
                             paper_bgcolor = "white",
                             barmode = 'stack')
      
      
      
      fig
    })
    
    
    output$p4 <- renderPlotly({
      
      data = df_filter() %>% dplyr::group_by(Preg5) %>% dplyr::summarize(Conteo = n()) 
      fig <- plot_ly(data, x = ~Preg5, y = ~Conteo, type = 'bar')
      fig <- fig %>%  layout(title = "Horas Semanales",
                             xaxis = list(title = "Horas",showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(title = "Cantidad",showgrid = FALSE, zeroline = FALSE),
                             paper_bgcolor = "white",
                             barmode = 'stack')
      
      
      
      fig
    })
    
    output$p5 <- renderPlotly({
      
      data = df_filter() %>% dplyr::group_by(Preg6) %>% dplyr::summarize(Conteo = n()) 
      fig <- plot_ly(data, x = ~Preg6, y = ~Conteo, type = 'bar')
      fig <- fig %>%  layout(title = "Horas Semanales",
                             xaxis = list(title = "Horas",showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(title = "Cantidad",showgrid = FALSE, zeroline = FALSE),
                             paper_bgcolor = "white",
                             barmode = 'stack')
      
      
      
      fig
    })
    
})

shinyApp(ui, server)
