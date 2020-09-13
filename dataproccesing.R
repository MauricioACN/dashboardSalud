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
educacion = c("Técnico","Pregrado","Diplomado","Maestría","Posgrado")
horas = c("Entre 20 y 39 horas","Entre 40 y 59 horas","Entre 60 y 79 horas","80 horas o más")
horas1 = c("Menos de 1 año","De 1-5 años","De 6-10 años","De 11-15 años","16 años o más")
cinco_opcion = c("Muy de acuerdo","De acuerdo","Indiferente","En desacuerdo","Muy en desacuerdo")
temp_a = c("Nunca","Casi nunca","A veces","Casi siempre","Siempre")

### Clean
df_clean = df %>% mutate(id = as.character(seq.int(from = 1,to = nrow(df),by = 1)),
                         `12`= factor(`12`,levels = educacion),
                         `18`= factor(`18`,levels = horas),
                         `30`= factor(`30`,levels = cinco_opcion),
                         `33`= factor(`33`,levels = cinco_opcion),
                         `36`= factor(`36`,levels = cinco_opcion),
                         `39`= factor(`39`,levels = cinco_opcion),
                         `42`= factor(`42`,levels = cinco_opcion),
                         `45`= factor(`45`,levels = cinco_opcion),
                         `48`= factor(`48`,levels = cinco_opcion),
                         `51`= factor(`51`,levels = cinco_opcion),
                         `54`= factor(`54`,levels = cinco_opcion),
                         `57`= factor(`57`,levels = cinco_opcion),
                         `63`= factor(`63`,levels = cinco_opcion),
                         `69`= factor(`69`,levels = cinco_opcion),
                         `72`= factor(`72`,levels = cinco_opcion),
                         `75`= factor(`75`,levels = cinco_opcion),
                         `78`= factor(`78`,levels = cinco_opcion),
                         `81`= factor(`81`,levels = cinco_opcion),
                         `90`= factor(`90`,levels = cinco_opcion),
                         `93`= factor(`93`,levels = cinco_opcion),
                         `96`= factor(`96`,levels = temp_a),
                         `99`= factor(`99`,levels = temp_a),
                         `102`= factor(`102`,levels = temp_a),
                         `105`= factor(`105`,levels = temp_a),
                         `108`= factor(`108`,levels = temp_a),
                         `111`= factor(`111`,levels = temp_a),
                         `114`= factor(`114`,levels = temp_a),
                         `117`= factor(`117`,levels = temp_a),
                         `120`= factor(`120`,levels = temp_a),
                         `123`= factor(`123`,levels = temp_a),
                         `126`= factor(`126`,levels = temp_a),
                         `132`= factor(`132`,levels = temp_a),
                         `135`= factor(`135`,levels = temp_a),
                         `138`= factor(`138`,levels = temp_a),
                         `141`= factor(`141`,levels = temp_a),
                         `144`= as.factor(`144`),
                         `147`= as.character(`147`),
                         `150`= as.character(`150`),
                         `153`= factor(`153`,levels = cinco_opcion),
                         `156`= factor(`156`,levels = cinco_opcion),
                         `159`= factor(`159`,levels = cinco_opcion),
                         `162`= factor(`162`,levels = cinco_opcion),
                         `165`= factor(`165`,levels = cinco_opcion),
                         `168`= factor(`168`,levels = cinco_opcion),
                         `171`= factor(`171`,levels = cinco_opcion),
                         `174`= factor(`174`,levels = cinco_opcion),
                         `180`= factor(`180`,levels = cinco_opcion),
                         `183`= factor(`183`,levels = cinco_opcion),
                         `186`= as.character(`186`),
                         `189`= as.character(`189`))
###
levels(df_clean$`21`)[levels(df_clean$`21`)==""] = "Menos de 1 año"
levels(df_clean$`66`)[levels(df_clean$`66`)==""] = "Indiferente"
levels(df_clean$`84`)[levels(df_clean$`84`)=="De cauerdo"] = "De acuerdo"
levels(df_clean$`177`)[levels(df_clean$`177`)=="De cauerdo"] = "De acuerdo"
levels(df_clean$`177`)[levels(df_clean$`177`)==""] = "Indiferente"
levels(df_clean$`129`)[levels(df_clean$`129`)=="Siemrpe"] = "Siempre"
##
texto1 = "personalmente 0, ya que la persona que se ha encargado de la notificación ha sido mi jefe inmediato."
texto2 = "durante este último año los reportes se han realizado por la aplicación institucional res (1evento por flebitis química)"

eventos = c("Ningún Evento","1 a 2 Eventos","3 a 4 Eventos","Más de 5 Eventos")

df_clean = df_clean %>% mutate(`21`= factor(`21`,levels = horas1),
                         `66`= factor(`66`,levels = cinco_opcion),
                         `84`= factor(`84`,levels = cinco_opcion),
                         `177`= factor(`177`,levels = cinco_opcion),
                         `129`= factor(`129`,levels = temp_a),
                         `150`= tolower(`150`),
                         `150`= gsub("ninguno",0,`150`),
                         `150`= gsub(texto1,0,`150`),
                         `150`= ifelse(`150`== texto2,1,`150`),
                         `150`= ifelse(`150`=="aproximadamente 14 ",14,`150`),
                         `150`= ifelse(`150`=="pocos ",1,`150`),
                         `150`= ifelse(`150`=="uno",1,`150`),
                         `150`= ifelse(`150`=="dos",2,`150`),
                         `150`= gsub("3 a 4",3,`150`),
                         `150`= ifelse(`150`=="no llevo la cuenta",1,`150`),
                         `150`= as.numeric(`150`),
                         `150` = as.factor(ifelse(`150`<=0,"Ningún Evento",
                                        ifelse(`150`<=2,"1 a 2 Eventos",
                                               ifelse(`150`<=4,"3 a 4 Eventos","Más de 5 Eventos")))),
                         `150`= factor(`150`,levels = eventos),
                         `147`= tolower(`147`),
                         `147`= ifelse(`147`=="",NA,`147`),
                         `186`= tolower(`186`),
                         `186`= ifelse(`186`=="no",NA,
                                       ifelse(`186`=="ninguna",NA,
                                              ifelse(`186`=="ninguno",NA,
                                                     ifelse(`186`=="ninguna ",NA,`186`)))),
                         `186`= ifelse(`186`=="",NA,`186`),
                         `186`= chartr("áéíóúñ","aeioun",`186`),
                         `186`= gsub('[[:punct:] ]+',' ',`186`),
                         `189`= ifelse(`189`=="",NA,`189`),
                         `189`= tolower(`189`),
                         `189`= chartr("áéíóúñ","aeioun",`189`),
                         `189`= gsub('[[:punct:] ]+',' ',`189`),
                         `147`= ifelse(`147`=="",NA,`147`),
                         `147`= tolower(`147`),
                         `147`= chartr("áéíóúñ","aeioun",`147`),
                         `147`= gsub('[[:punct:] ]+',' ',`147`))

##Rename
colnames(df_clean) = c(set_vars$Cod,"id")
df_clean = df_clean[,c(61,1:60)]

###Tipo de pregunta
tipo_respuesta = gather(df_clean[,c(10:46,49:58)],key = "Pregunta","Respuesta")
tipo_respuesta$Tipo_respuesta = ifelse(str_detect(tipo_respuesta$Respuesta,"veces"),"Tipo I",
                                       ifelse(str_detect(tipo_respuesta$Respuesta,"acuerdo"),"Tipo II","Otro"))
##
tipo_respuesta = tipo_respuesta %>% group_by(Pregunta,Tipo_respuesta) %>% summarize(no =n()) %>% select(-no)
tipo_respuesta = tipo_respuesta %>% filter(str_detect(Tipo_respuesta,"Tipo"))
###
set_vars = left_join(set_vars,tipo_respuesta, by = c("Cod"="Pregunta"))


###write RDS Data clean 
write_rds(df_clean,"Dashboard/BD_clean.rds")
write_rds(set_vars,"Dashboard/set_variables.rds")
