###
library(tm)


preprocesing = function(base){
    
    base$text = tm::removeWords(base$text, words = tm::stopwords("spanish"))
    base$text = tm::removeNumbers(base$text)
    base$text = tm::stripWhitespace(base$text)
    #base$text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *"," ",base$text) #Remove words with length of 1 or 2 characters 
    base$text = gsub("[ |\t]{2,}", " ", base$text)  # Remove tabs
    base$text = gsub("^ ", "", base$text)  # Leading blanks
    base$text = gsub(" $", "", base$text)  # Lagging blanks
    base$text = gsub(" +", " ", base$text) # General spaces
    base$text = gsub("[[:cntrl:]]", " ", base$text) # saltos de linea y tabulaciones
    base = base %>% filter(!text %in% c(""," "))
    base = base %>% mutate(char = nchar(text)) %>% filter(char>2)
    base = base %>% distinct(text,.keep_all = T)
    return(base)
    }


###Función de tabulación para la parte final de resultados 

tab_function = function(base,sub_total){
    vars_pos_neg = set %>% filter(Tipo_pregunta=="POS"|Tipo_pregunta=="NEG") %>% select(Cod)
    tabulacion = base %>% select(vars_pos_neg$Cod,-Preg46)
    data_tab = one_hot(data.table::as.data.table(tabulacion))
    data_tab = gather(data = data_tab,key = "Var","valor",1:ncol(data_tab)) 
    data_tab = separate(data = data_tab,col = Var,into = c("Preg","Categ"),sep = "_")
    
    positivos = c("De acuerdo","Muy de acuerdo","Casi siempre","Siempre")
    negativos = c("En desacuerdo","Muy en desacuerdo","Casi nunca","Nunca")
    neutros = c("Indiferente","A veces")
    
    data_tab$Categoria = ifelse(data_tab$Categ %in% positivos,"Respuestas Positivas",
                                ifelse(data_tab$Categ %in% negativos,"Respuestas Negativas","Respuestas Neutras"))
    
    data_tab = data_tab %>% group_by(Preg,Categoria) %>% summarize(valor = sum(valor,na.rm = T)) %>% mutate(valor = round(valor/sub_total*100,2))
    data_tab = spread(data = data_tab,key = Categoria,value = valor,fill = 0)
    data_tab = left_join(data_tab,set, by = c("Preg"="Cod")) %>% select(Preg,Pregunta,Tipo_pregunta,Tipo_respuesta,Variables,Objetivo,`Respuestas Positivas`,`Respuestas Neutras`,`Respuestas Negativas`) %>% ungroup() %>% arrange(Preg)
    data_tab$Preg = as.numeric(gsub("\\Preg","",data_tab$Preg))
    
    return(data_tab)  
}
