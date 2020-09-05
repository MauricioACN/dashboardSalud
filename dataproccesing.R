library(tidyverse)
library(readxl)

## load data
base = read.csv("BaseDatos.csv", header = T, sep = ",", encoding = "UTF-8")
varaibles_list = read_excel(path = "asignacion_preguntas.xlsx",sheet = "Hoja2")
varaibles_list = varaibles_list %>% filter(Cod != "Preg27") 

### Proccesiong data
col_name = colnames(base)
caracter = "Puntuación|Comentarios"
variables = data.frame(col=col_name) %>% mutate(trash = str_detect(col,pattern = caracter))
variables$numero_preg = seq(1,191, by= 1)
colnames(base) = variables$numero_preg

### Filter data
otras_vars = c("X","Marca.temporal","X.Acepta.la.participación.en.el.estudio.")
set_vars = variables %>% filter(trash==F,!col %in% otras_vars) %>% filter(numero_preg != "87")
set_vars = cbind(set_vars,varaibles_list) %>% select(-trash)

### Select necesary columns
df = base %>% select(set_vars$numero_preg)

### Data cleaning
educacion = c("")
horas = c("")
horas1 = c("")
cinco_opcion = c("Muy de acuerdo","De acuerdo","Indiferente","En desacuerdo","Muy en desacuerdo")
cuatro_opcionA = c("Muy de acuerdo","De acuerdo","En desacuerdo","Muy en desacuerdo")
cuatro_opcion_pos = c("Muy de acuerdo","De acuerdo","Indiferente","En desacuerdo")
cuatro_opcion_neg = c("De acuerdo","Indiferente","En desacuerdo","Muy en desacuerdo")

df_clean = df %>% mutate(`189`= as.character(`189`),
                         `186`= as.character(`186`),
                         `147`= as.character(`147`),
                         `144`= as.factor(`144`),
                         `12`= factor(`12`,levels = educacion),
                         `18`= factor(`18`,levels = horas),
                         `21`= factor(`21`,levels = horas1),
                         `66`= ifelse(`66`=="","Indiferente",`66`),
                         `177`= ifelse(`177`=="De cauerdo","De acuerdo",`177`),
                         )
