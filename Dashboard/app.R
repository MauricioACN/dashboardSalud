
source("functions.R")
source("global.R")

ui = dashboardPage(
  
  shinydashboard::dashboardHeader(title = "Dashboard UCI"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItem(text = "Caracterización",tabName = "demograficos",icon = icon("user-friends")),
      menuItem(text = "Percepción",tabName = "percepcion",icon = icon("lightbulb")),
      menuItem(text = "Nivel de Servicio",tabName = "servicio",icon = icon("concierge-bell")),
      menuItem(text = "Nivel de Institución",tabName = "seguridad",icon = icon("hospital-alt")),
      menuItem(text = "Tabulación",tabName = "tabla",icon = icon("table")),
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
              
              fluidRow(column(6,
                              tabBox(title = "Análisis de Preguntas Positivas",id = "secc_perc1",width = "600px",
                                     tabPanel(title = "Analisis 1",shinydashboard::box(plotlyOutput("p6"),width = "600px")),
                                     tabPanel(title = "Analisis 2",shinydashboard::box(plotlyOutput("p6_1"),width = "600px")))),
                       column(6,
                              tabBox(title = "Análisis de Preguntas Negativas",id = "secc_perc2",width = "600px",
                                     tabPanel(title = "Analisis 1",shinydashboard::box(plotlyOutput("p7"),width = "600px")),
                                     tabPanel(title = "Analisis 2",shinydashboard::box(plotlyOutput("p7_1"),width = "600px"))))
              ),
              fluidRow(column(2,fluidRow(column(6,box(h5(title=set_positivo$Pregunta[1],a("Pregunta 17"),style="color: #E8DED2"),width = "400px")),
                                         column(6,box(h5(title=set_positivo$Pregunta[2],a("Pregunta 22"),style="color: #E8DED2"),width = "400px")),
                                         column(6,box(h5(title=set_positivo$Pregunta[3],a("Pregunta 25"),style="color: #E8DED2"),width = "400px")),
                                         column(6,box(h5(title=set_positivo$Pregunta[4],a("Pregunta 41"),style="color: #E8DED2"),width = "400px")),
                                         column(6,box(h5(title=set_positivo$Pregunta[5],a("Pregunta 45"),style="color: #E8DED2"),width = "400px")))),
                       column(8,fluidRow(
                         column(12,tabBox(title = "Análisis de Otras Preguntas",id = "seccion2",width = "800px",
                                          tabPanel("Preg46",p(set$Pregunta[45]),hr(),plotlyOutput("p10")),
                                          tabPanel("Preg47",p(set$Pregunta[46]),hr(),wordcloud2::wordcloud2Output("CloudWord")),
                                          tabPanel("Preg60",p(set$Pregunta[59]),hr(),wordcloud2::wordcloud2Output("p14")),
                                          tabPanel("Preg61",p(set$Pregunta[60]),hr(),wordcloud2::wordcloud2Output("p15"))))
                       )),
                       column(2,fluidRow(column(6,box(h5(title=set_negativo$Pregunta[1],a("Pregunta 24"),style="color: #E8DED2"),width = "400px")),
                                         column(6,box(h5(title=set_negativo$Pregunta[2],a("Pregunta 39"),style="color: #E8DED2"),width = "400px")))
                       ))
                       ),
      tabItem(tabName = "servicio",
              
              fluidRow(
                column(6,
                       tabBox(title = "Análisis de Preguntas Positivas",id = "secc_serv1",width = "600px",
                              tabPanel(title = "Análisis 1",shinydashboard::box(plotlyOutput("p8"),width = "600px")),
                              tabPanel(title = "Análisis 2",shinydashboard::box(plotlyOutput("p8_1"),width = "600px")))),
                column(6,
                       tabBox(title = "Análisis de Preguntas Negativas",id = "secc_serv2",width = "600px",
                              tabPanel(title = "Análisis 1",shinydashboard::box(plotlyOutput("p9"),width = "600px")),
                              tabPanel(title = "Análisis 2",shinydashboard::box(plotlyOutput("p9_1"),width = "600px"))))
              ),
              
              fluidRow(column(2,
                              fluidRow(column(4,box(h6(title=set_positivo2$Pregunta[1],a("Pregunta 9"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[2],a("Pregunta 10"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[3],a("Pregunta 11"),style="color: #E8DED2"),width = "400px"))),
                              fluidRow(column(4,box(h6(title=set_positivo2$Pregunta[4],a("Pregunta 13"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[5],a("Pregunta 16"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[6],a("Pregunta 18"),style="color: #E8DED2"),width = "400px"))),
                              fluidRow(column(4,box(h6(title=set_positivo2$Pregunta[7],a("Pregunta 20"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[8],a("Pregunta 26"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[9],a("Pregunta 30"),style="color: #E8DED2"),width = "400px"))),
                              fluidRow(column(4,box(h6(title=set_positivo2$Pregunta[10],a("Pregunta 31"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[11],a("Pregunta 32"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[12],a("Pregunta 33"),style="color: #E8DED2"),width = "400px"))),
                              fluidRow(column(4,box(h6(title=set_positivo2$Pregunta[13],a("Pregunta 34"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[14],a("Pregunta 36"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[15],a("Pregunta 37"),style="color: #E8DED2"),width = "400px"))),
                              fluidRow(column(4,box(h6(title=set_positivo2$Pregunta[16],a("Pregunta 38"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[17],a("Pregunta 40"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_positivo2$Pregunta[18],a("Pregunta 42"),style="color: #E8DED2"),width = "400px"))),
                              fluidRow(column(12,box(h6(title=set_positivo2$Pregunta[19],a("Pregunta 43"),style="color: #E8DED2"),width = "1200px")),
                                       column(12,box(h6(title=set_positivo2$Pregunta[20],a("Pregunta 44"),style="color: #E8DED2"),width = "1200px")))),
                       column(8,fluidRow(
                         column(12,box(title = "Análisis de Otras Preguntas",id = "seccion2",width = "800px",
                                          tabPanel("Preg48",p(set$Pregunta[47]),hr(),plotlyOutput("p11"))))
                       )),
                       column(2,fluidRow(column(4,box(h6(title=set_negativo2$Pregunta[1],a("Pregunta 12"),style="color: #E8DED2"),width = "400px")),
                                         column(4,box(h6(title=set_negativo2$Pregunta[2],a("Pregunta 14"),style="color: #E8DED2"),width = "400px")),
                                         column(4,box(h6(title=set_negativo2$Pregunta[3],a("Pregunta 15"),style="color: #E8DED2"),width = "400px"))),
                              fluidRow(column(4,box(h6(title=set_negativo2$Pregunta[4],a("Pregunta 19"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_negativo2$Pregunta[5],a("Pregunta 21"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_negativo2$Pregunta[6],a("Pregunta 23"),style="color: #E8DED2"),width = "400px"))),
                              fluidRow(column(4,box(h6(title=set_negativo2$Pregunta[7],a("Pregunta 28"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_negativo2$Pregunta[8],a("Pregunta 29"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h6(title=set_negativo2$Pregunta[9],a("Pregunta 35"),style="color: #E8DED2"),width = "400px")))))
              ),
      tabItem(tabName = "seguridad",
              
              fluidRow(
                column(6,
                       tabBox(title = "Análisis de Preguntas Positivas",id = "secc_segu1",width = "600px",
                              tabPanel("Analisis 1",shinydashboard::box(plotlyOutput("p12"),width = "600px")))),
                column(6,
                       tabBox(title = "Análisis de Preguntas Negativas",id = "secc_segu2",width = "600px",
                              tabPanel("Analisis 1",shinydashboard::box(plotlyOutput("p13"),width = "600px"))))
              ),
              fluidRow(
                       column(6,
                              fluidRow(column(6,box(h4(title=set_positivo3$Pregunta[1],a("Pregunta 49"),style="color: #E8DED2"),width = "600px")),
                                       column(6,box(h4(title=set_positivo3$Pregunta[2],a("Pregunta 52"),style="color: #E8DED2"),width = "600px"))),
                              fluidRow(column(6,box(h4(title=set_positivo3$Pregunta[3],a("Pregunta 56"),style="color: #E8DED2"),width = "600px")),
                                       column(6,box(h4(title=set_positivo3$Pregunta[4],a("Pregunta 58"),style="color: #E8DED2"),width = "600px")))),
                       column(6,fluidRow(column(4,box(h4(title=set_negativo3$Pregunta[1],a("Pregunta 50"),style="color: #E8DED2"),width = "400px")),
                                         column(4,box(h4(title=set_negativo3$Pregunta[2],a("Pregunta 51"),style="color: #E8DED2"),width = "400px")),
                                         column(4,box(h4(title=set_negativo3$Pregunta[3],a("Pregunta 53"),style="color: #E8DED2"),width = "400px"))),
                              fluidRow(column(4,box(h4(title=set_negativo3$Pregunta[4],a("Pregunta 54"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h4(title=set_negativo3$Pregunta[5],a("Pregunta 55"),style="color: #E8DED2"),width = "400px")),
                                       column(4,box(h4(title=set_negativo3$Pregunta[6],a("Pregunta 57"),style="color: #E8DED2"),width = "400px"))),
                              fluidRow(column(12,box(h4(title=set_negativo3$Pregunta[7],a("Pregunta 59"),style="color: #E8DED2"),width = "1200px")))))
  ),
  tabItem(tabName = "tabla",
          tabBox(id = "tab_panel",width = "1200px",
                 tabPanel(title = "Tabulación",p("La nomenclatura Tipo I y Tipo II de la columna Tipo_respuesta corresponden a la siguiente estructura de pregunta: Tipo I (A veces, Siempre, etc.) y Tipo II (De acuerdo, En Desacuerdo, etc.)"),
                          hr(),DT::DTOutput("tabla_1"))))
  )
)
)

# Define server logic required to draw a histogram
server = (function(input, output, session) {
  
  color_oscuro = "#056676"
  color_oscuro_bajo = "#5EAAA8"
  color_claro_uno = "#A3D2CA"
  color_claro = "#E8DED2"
  
  df_filter <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = df,
    vars = names(df)
  )
  
  
  
  output$value1 <- renderValueBox({
    
    
    valueBox(value = nrow(df_filter()),subtitle = "Cantidad Personas",icon = icon("user-alt"),color = "teal")
    
  })
  
  output$value2 <- renderValueBox({
    
    female = df_filter() %>% filter(Preg2=="Femenino")
    
    valueBox(value = nrow(female),subtitle = "Cantidad Mujeres",icon = icon("female"), color = "aqua")
    
  })
  
  output$value3 <- renderValueBox({
    
    male = df_filter() %>% filter(Preg2=="Masculino")
    
    valueBox(value = nrow(male),subtitle = "Cantidad Hombres",icon = icon("male"), color = "navy")
    
  })
  
    output$p1 <- renderPlotly({
      
      data = df_filter() %>% mutate(Masculino = ifelse(Preg2=="Masculino",1,0)) %>% dplyr::group_by(Preg1) %>% dplyr::summarize(Conteo = n(),
                                                                                                                                Masculino = sum(Masculino),
                                                                                                                                Femenino = Conteo-Masculino) 

      
      fig <- plot_ly(data, x = ~Preg1, y = ~Masculino, type = 'bar', name = 'Masculino',marker=list(color = color_oscuro))
      fig <- fig %>% add_trace(y = ~Femenino, name = 'Femenino',marker=list(color = color_oscuro_bajo))
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
      
      
      fig <- plot_ly(data, x = ~Preg3, y = ~trabajo, type = 'bar', name = 'Empleo adicional',marker=list(color = color_oscuro))
      fig <- fig %>% add_trace(y = ~Notrabaja, name = 'Unico Empleo',marker=list(color = color_oscuro_bajo))
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
      
      
      fig <- plot_ly(data, x = ~Preg4, y = ~man, type = 'bar', name = 'Mañana',marker=list(color = color_oscuro))
      fig <- fig %>% add_trace(y = ~tar, name = 'Tarde',marker=list(color = color_oscuro_bajo))
      fig <- fig %>% add_trace(y = ~na, name = 'Noche A',marker=list(color = color_claro_uno))
      fig <- fig %>% add_trace(y = ~nb, name = 'Noche B',marker=list(color = color_claro))
    fig <- fig %>%  layout(title = "Cargo y Turno",
                             xaxis = list(title = "Cargo",showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(title = "Cantidad",showgrid = FALSE, zeroline = FALSE),
                             paper_bgcolor = "white",
                             barmode = 'stack')
      
      fig
    })
    
    
    output$p4 <- renderPlotly({
      
      data = df_filter() %>% dplyr::group_by(Preg5) %>% dplyr::summarize(Conteo = n()) 
      fig <- plot_ly(data, x = ~Preg5, y = ~Conteo, type = 'bar', marker = list(color = color_oscuro))
      fig <- fig %>%  layout(title = "Horas Semanales Trabajadas",
                             xaxis = list(title = "Horas",showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(title = "Cantidad",showgrid = FALSE, zeroline = FALSE),
                             paper_bgcolor = "white",
                             barmode = 'stack')
      
      
      
      fig
    })
    
    output$p5 <- renderPlotly({
      
      data = df_filter() %>% dplyr::group_by(Preg6) %>% dplyr::summarize(Conteo = n()) 
      fig <- plot_ly(data, x = ~Preg6, y = ~Conteo, type = 'bar', marker = list(color = color_oscuro_bajo))
      fig <- fig %>%  layout(title = "Antigüedad",
                             xaxis = list(title = "Horas",showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(title = "Cantidad",showgrid = FALSE, zeroline = FALSE),
                             paper_bgcolor = "white",
                             barmode = 'stack')
      fig
    })
    
    
    output$p6 <- renderPlotly({
      
      data = df_filter() %>% select(set_positivo$Cod)
      data = data %>% select(Preg17,Preg22,Preg25)
      data = one_hot(data.table::as.data.table(data))
      data = gather(data = data,key = "Var","valor",1:ncol(data)) 
      data = separate(data = data,col = Var,into = c("Preg","Categ"),sep = "_")
      data = data %>% group_by(Preg,Categ) %>% summarize(valor = sum(valor,na.rm = T))
      data = spread(data = data,key = Preg,value = valor,fill = 0)
      
      fig <- plot_ly(type = 'scatterpolar',fill = 'toself',data = data)
      fig <- fig %>% add_trace(r = ~Preg17, theta = ~Categ, name = set_positivo$Cod[1])
      fig <- fig %>% add_trace(r = ~Preg22, theta = ~Categ, name = set_positivo$Cod[2])
      fig <- fig %>% add_trace(r = ~Preg25, theta = ~Categ, name = set_positivo$Cod[3])

      fig <- fig %>%  layout(polar = list(radialaxis = list(visible = T,range = c(0,50))))
      fig
    })
    
    output$p6_1 <- renderPlotly({
      
      data = df_filter() %>% select(set_positivo$Cod)
      data = data %>% select(Preg41,Preg45)
      data = one_hot(data.table::as.data.table(data))
      data = gather(data = data,key = "Var","valor",1:ncol(data)) 
      data = separate(data = data,col = Var,into = c("Preg","Categ"),sep = "_")
      data = data %>% group_by(Preg,Categ) %>% summarize(valor = sum(valor,na.rm = T))
      data = spread(data = data,key = Preg,value = valor,fill = 0)
      
      fig <- plot_ly(type = 'scatterpolar',fill = 'toself',data = data)
      fig <- fig %>% add_trace(r = ~Preg41, theta = ~Categ, name = set_positivo$Cod[4])
      fig <- fig %>% add_trace(r = ~Preg45, theta = ~Categ, name = set_positivo$Cod[5])

      fig <- fig %>%  layout(polar = list(radialaxis = list(visible = T,range = c(0,50))))
      fig
    })
    
  
    
    output$p7 <- renderPlotly({
      
      data = df_filter() %>% select(set_negativo$Cod)
      data = data %>% select(Preg24)
      data = one_hot(data.table::as.data.table(data))
      data = gather(data = data,key = "Var","valor",1:ncol(data)) 
      data = separate(data = data,col = Var,into = c("Preg","Categ"),sep = "_")
      data = data %>% group_by(Preg,Categ) %>% summarize(valor = sum(valor,na.rm = T))
      data = spread(data = data,key = Preg,value = valor,fill = 0)
      
      fig <- plot_ly(type = 'scatterpolar',fill = 'toself',data = data)
      fig <- fig %>% add_trace(r = ~Preg24, theta = ~Categ, name = set_negativo$Cod[1])
      
      
      fig <- fig %>%  layout(polar = list(radialaxis = list(visible = T,range = c(0,50))))
      fig
    })
    
    output$p7_1 <- renderPlotly({
      
      data = df_filter() %>% select(set_negativo$Cod)
      data = data %>% select(Preg39)
      data = one_hot(data.table::as.data.table(data))
      data = gather(data = data,key = "Var","valor",1:ncol(data)) 
      data = separate(data = data,col = Var,into = c("Preg","Categ"),sep = "_")
      data = data %>% group_by(Preg,Categ) %>% summarize(valor = sum(valor,na.rm = T))
      data = spread(data = data,key = Preg,value = valor,fill = 0)
      
      fig <- plot_ly(type = 'scatterpolar',fill = 'toself',data = data)
      fig <- fig %>% add_trace(r = ~Preg39, theta = ~Categ, name = set_negativo$Cod[2])
      
      
      fig <- fig %>%  layout(polar = list(radialaxis = list(visible = T,range = c(0,50))))
      fig
    })
    
    
    output$p8 <- renderPlotly({
      
      data = df_filter() %>% select(set_positivo2$Cod)
      data = data %>% select(Preg9,Preg10,Preg11,Preg13,Preg16,Preg18,Preg20,Preg26)
      data = one_hot(data.table::as.data.table(data))
      data = gather(data = data,key = "Var","valor",1:ncol(data)) 
      data = separate(data = data,col = Var,into = c("Preg","Categ"),sep = "_")
      data = data %>% group_by(Preg,Categ) %>% summarize(valor = sum(valor,na.rm = T))
      data = spread(data = data,key = Preg,value = valor,fill = 0)
      
      fig <- plot_ly(type = 'scatterpolar',fill = 'toself',data = data)
      fig <- fig %>% add_trace(r = ~Preg9, theta = ~Categ, name = set_positivo2$Cod[1])
      fig <- fig %>% add_trace(r = ~Preg10, theta = ~Categ, name = set_positivo2$Cod[2])
      fig <- fig %>% add_trace(r = ~Preg11, theta = ~Categ, name = set_positivo2$Cod[3])
      fig <- fig %>% add_trace(r = ~Preg13, theta = ~Categ, name = set_positivo2$Cod[4])
      fig <- fig %>% add_trace(r = ~Preg16, theta = ~Categ, name = set_positivo2$Cod[5])
      fig <- fig %>% add_trace(r = ~Preg18, theta = ~Categ, name = set_positivo2$Cod[6])
      fig <- fig %>% add_trace(r = ~Preg20, theta = ~Categ, name = set_positivo2$Cod[7])
      fig <- fig %>% add_trace(r = ~Preg26, theta = ~Categ, name = set_positivo2$Cod[8])
      
      fig <- fig %>%  layout(polar = list(radialaxis = list(visible = T,range = c(0,50))))
      fig
    })
    
    output$p8_1 <- renderPlotly({
      
      data = df_filter() %>% select(set_positivo2$Cod)
      data = data %>% select(Preg30,Preg31,Preg32,Preg33,Preg34,Preg36,Preg37,Preg38,Preg40,Preg42,Preg43,Preg44)
      data = one_hot(data.table::as.data.table(data))
      data = gather(data = data,key = "Var","valor",1:ncol(data)) 
      data = separate(data = data,col = Var,into = c("Preg","Categ"),sep = "_")
      data = data %>% group_by(Preg,Categ) %>% summarize(valor = sum(valor,na.rm = T))
      data = spread(data = data,key = Preg,value = valor,fill = 0)
      
      fig <- plot_ly(type = 'scatterpolar',fill = 'toself',data = data)
      fig <- fig %>% add_trace(r = ~Preg30, theta = ~Categ, name = set_positivo2$Cod[9])
      fig <- fig %>% add_trace(r = ~Preg31, theta = ~Categ, name = set_positivo2$Cod[10])
      fig <- fig %>% add_trace(r = ~Preg32, theta = ~Categ, name = set_positivo2$Cod[11])
      fig <- fig %>% add_trace(r = ~Preg33, theta = ~Categ, name = set_positivo2$Cod[12])
      fig <- fig %>% add_trace(r = ~Preg34, theta = ~Categ, name = set_positivo2$Cod[13])
      fig <- fig %>% add_trace(r = ~Preg36, theta = ~Categ, name = set_positivo2$Cod[14])
      fig <- fig %>% add_trace(r = ~Preg37, theta = ~Categ, name = set_positivo2$Cod[15])
      fig <- fig %>% add_trace(r = ~Preg38, theta = ~Categ, name = set_positivo2$Cod[16])
      fig <- fig %>% add_trace(r = ~Preg40, theta = ~Categ, name = set_positivo2$Cod[17])
      fig <- fig %>% add_trace(r = ~Preg42, theta = ~Categ, name = set_positivo2$Cod[18])
      fig <- fig %>% add_trace(r = ~Preg43, theta = ~Categ, name = set_positivo2$Cod[19])
      fig <- fig %>% add_trace(r = ~Preg44, theta = ~Categ, name = set_positivo2$Cod[20])
      
      
      fig <- fig %>%  layout(polar = list(radialaxis = list(visible = T,range = c(0,50))))
      fig
    })
    
    output$p9 <- renderPlotly({
      
      data = df_filter() %>% select(set_negativo2$Cod)
      data = data %>% select(Preg12,Preg14,Preg15,Preg19,Preg21,Preg23,Preg28,Preg29)
      data = one_hot(data.table::as.data.table(data))
      data = gather(data = data,key = "Var","valor",1:ncol(data)) 
      data = separate(data = data,col = Var,into = c("Preg","Categ"),sep = "_")
      data = data %>% group_by(Preg,Categ) %>% summarize(valor = sum(valor,na.rm = T))
      data = spread(data = data,key = Preg,value = valor,fill = 0)
      
      fig <- plot_ly(type = 'scatterpolar',fill = 'toself',data = data)
      fig <- fig %>% add_trace(r = ~Preg12, theta = ~Categ, name = set_negativo2$Cod[1])
      fig <- fig %>% add_trace(r = ~Preg14, theta = ~Categ, name = set_negativo2$Cod[2])
      fig <- fig %>% add_trace(r = ~Preg15, theta = ~Categ, name = set_negativo2$Cod[3])
      fig <- fig %>% add_trace(r = ~Preg19, theta = ~Categ, name = set_negativo2$Cod[4])
      fig <- fig %>% add_trace(r = ~Preg21, theta = ~Categ, name = set_negativo2$Cod[5])
      fig <- fig %>% add_trace(r = ~Preg23, theta = ~Categ, name = set_negativo2$Cod[6])
      fig <- fig %>% add_trace(r = ~Preg28, theta = ~Categ, name = set_negativo2$Cod[7])
      fig <- fig %>% add_trace(r = ~Preg29, theta = ~Categ, name = set_negativo2$Cod[8])
      
      fig <- fig %>%  layout(polar = list(radialaxis = list(visible = T,range = c(0,50))))
      fig
    })
    
    output$p9_1 <- renderPlotly({
      
      data = df_filter() %>% select(set_negativo2$Cod)
      data = data %>% select(Preg35)
      data = one_hot(data.table::as.data.table(data))
      data = gather(data = data,key = "Var","valor",1:ncol(data)) 
      data = separate(data = data,col = Var,into = c("Preg","Categ"),sep = "_")
      data = data %>% group_by(Preg,Categ) %>% summarize(valor = sum(valor,na.rm = T))
      data = spread(data = data,key = Preg,value = valor,fill = 0)
      
      fig <- plot_ly(type = 'scatterpolar',fill = 'toself',data = data)
      fig <- fig %>% add_trace(r = ~Preg35, theta = ~Categ, name = set_negativo2$Cod[9])
      
      fig <- fig %>%  layout(polar = list(radialaxis = list(visible = T,range = c(0,50))))
      fig
    })
    
    
    output$CloudWord <- wordcloud2::renderWordcloud2({
      
      data = df_filter() %>% mutate(text = Preg47) %>% select(text) %>% preprocesing(.)
      data_f <- ngrams_func(data,n_gram = 2,n_row = 30)
      fig = wordcloud2::wordcloud2(data_f,minSize = 0.7,size = 1)
      fig
    })
    
    
    output$p10 <- renderPlotly({
      
      data = df_filter() %>% dplyr::group_by(Preg46) %>% dplyr::summarize(Conteo = n()) 
      fig <- plot_ly(data, x = ~Preg46, y = ~Conteo, type = 'bar', marker = list(color = color_oscuro_bajo))
      fig <- fig %>%  layout(title = "Grado de Seguridad del Paciente",
                             xaxis = list(title = "Seguridad",showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(title = "Cantidad",showgrid = FALSE, zeroline = FALSE),
                             paper_bgcolor = "white",
                             barmode = 'stack')
      
      
      
      fig
    })
    
    output$p11 <- renderPlotly({
      
      data = df_filter() %>% dplyr::group_by(Preg48) %>% dplyr::summarize(Conteo = n()) 
      fig <- plot_ly(data, x = ~Preg48, y = ~Conteo, type = 'bar', marker = list(color = color_claro_uno))
      fig <- fig %>%  layout(title = "Eventos Adversos",
                             xaxis = list(title = "Eventos",showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(title = "Cantidad",showgrid = FALSE, zeroline = FALSE),
                             paper_bgcolor = "white",
                             barmode = 'stack')
      
      fig
    })
    
    
    output$p12 <- renderPlotly({
      
      data = df_filter() %>% select(set_positivo3$Cod)
      data = one_hot(data.table::as.data.table(data))
      data = gather(data = data,key = "Var","valor",1:ncol(data)) 
      data = separate(data = data,col = Var,into = c("Preg","Categ"),sep = "_")
      data = data %>% group_by(Preg,Categ) %>% summarize(valor = sum(valor,na.rm = T))
      data = spread(data = data,key = Preg,value = valor,fill = 0)
      
      fig <- plot_ly(type = 'scatterpolar',fill = 'toself',data = data)
      fig <- fig %>% add_trace(r = ~Preg49, theta = ~Categ, name = set_positivo3$Cod[1])
      fig <- fig %>% add_trace(r = ~Preg52, theta = ~Categ, name = set_positivo3$Cod[2])
      fig <- fig %>% add_trace(r = ~Preg56, theta = ~Categ, name = set_positivo3$Cod[3])
      fig <- fig %>% add_trace(r = ~Preg58, theta = ~Categ, name = set_positivo3$Cod[4])
      
      
      fig <- fig %>%  layout(polar = list(radialaxis = list(visible = T,range = c(0,50))))
      fig
    })
    
    
    output$p13 <- renderPlotly({
      
      data = df_filter() %>% select(set_negativo3$Cod)
      data = one_hot(data.table::as.data.table(data))
      data = gather(data = data,key = "Var","valor",1:ncol(data)) 
      data = separate(data = data,col = Var,into = c("Preg","Categ"),sep = "_")
      data = data %>% group_by(Preg,Categ) %>% summarize(valor = sum(valor,na.rm = T))
      data = spread(data = data,key = Preg,value = valor,fill = 0)
      
      fig <- plot_ly(type = 'scatterpolar',fill = 'toself',data = data)
      fig <- fig %>% add_trace(r = ~Preg50, theta = ~Categ, name = set_negativo3$Cod[1])
      fig <- fig %>% add_trace(r = ~Preg51, theta = ~Categ, name = set_negativo3$Cod[2])
      fig <- fig %>% add_trace(r = ~Preg53, theta = ~Categ, name = set_negativo3$Cod[3])
      fig <- fig %>% add_trace(r = ~Preg54, theta = ~Categ, name = set_negativo3$Cod[4])
      fig <- fig %>% add_trace(r = ~Preg55, theta = ~Categ, name = set_negativo3$Cod[5])
      fig <- fig %>% add_trace(r = ~Preg57, theta = ~Categ, name = set_negativo3$Cod[6])
      fig <- fig %>% add_trace(r = ~Preg59, theta = ~Categ, name = set_negativo3$Cod[7])
      
      
      fig <- fig %>%  layout(polar = list(radialaxis = list(visible = T,range = c(0,50))))
      fig
    })
    
    output$p14 <- wordcloud2::renderWordcloud2({
      
      data = df_filter() %>% mutate(text = Preg60) %>% select(text) %>% preprocesing(.)
      data_f <- ngrams_func(data,n_gram = 2,n_row = 80)
      fig = wordcloud2::wordcloud2(data_f,minSize = 0.2,size = 0.5)
      fig
    })
    
    output$p15 <- wordcloud2::renderWordcloud2({
      
      data = df_filter() %>% mutate(text = Preg61) %>% select(text) %>% preprocesing(.)
      data_f <- ngrams_func(data,n_gram = 2,n_row = ifelse(nrow(data)<15,nrow(data),60))
      fig = wordcloud2::wordcloud2(data_f,minSize = 0.2,size = 0.5)
      fig
    })
    
    
    output$tabla_1 <- DT::renderDT(tab_function(df_filter(),nrow(df_filter())),
                                                filter = "top"
                                   )
    
    
})

shinyApp(ui, server)
