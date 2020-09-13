###library(shiny)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(plotly)
library(tidyverse)
library(shinydashboard)
library(mltools)
library(shinyWidgets)
library(wordcloud2)
library(data.table)
library(text2vec)

###Data
df = readRDS("BD_clean.rds")
set = readRDS("set_variables.rds")

vars = unique(set$Objetivo)

set_positivo =  set %>% filter(Tipo_pregunta=="POS",Objetivo==vars[3]) %>% select(Cod,Pregunta)
set_negativo = set %>% filter(Tipo_pregunta=="NEG",Objetivo==vars[3]) %>% select(Cod,Pregunta)
set_positivo2 =  set %>% filter(Tipo_pregunta=="POS",Objetivo==vars[2]) %>% select(Cod,Pregunta)
set_negativo2 = set %>% filter(Tipo_pregunta=="NEG",Objetivo==vars[2]) %>% select(Cod,Pregunta)
set_positivo3 =  set %>% filter(Tipo_pregunta=="POS",Objetivo==vars[4]) %>% select(Cod,Pregunta)
set_negativo3 = set %>% filter(Tipo_pregunta=="NEG",Objetivo==vars[4]) %>% select(Cod,Pregunta)

