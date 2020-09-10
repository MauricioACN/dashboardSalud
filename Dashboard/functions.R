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
