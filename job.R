library(tabulizer)
library(tidyverse)

#this_dir <- function(directory)
#  setwd( file.path(getwd(), directory) )

pdf <- "https://www.ppac.gov.in/WriteReadData/userfiles/file/PP_9_a_DailyPriceMSHSD_Metro.pdf"

petrol_disel <- extract_tables(pdf,
                            output = "matrix",
                            pages = c(1,1),
                            area = list(
                              c(165.6,69.4,179.6,289.3),
                              c(165.6,310,179.6,544)),
                              guess = FALSE,
)

petrol_today <- as.data.frame(petrol_disel[[1]])

Pt <- petrol_today  %>% 
  mutate_all(type.convert) %>% 
  mutate_if(is.factor, as.character) %>%
  mutate(across(where(is.character), str_trim))%>%
  mutate(map_df(petrol_today, ~ gsub('\\s+', '', .x))) %>% 
  `colnames<-`(c("Date","Delhi","Mumbai","Chennai","Kolkata"))%>% 
  mutate(Date=as.Date(Date, format = '%d-%B-%y'))



diesel_today <- as.data.frame(petrol_disel[[2]])

dt <- diesel_today  %>% 
  mutate_all(type.convert) %>% 
  mutate_if(is.factor, as.character) %>%
  mutate(across(where(is.character), str_trim))%>%
  mutate(map_df(diesel_today, ~ gsub('\\s+', '', .x))) %>% 
  `colnames<-`(c("Date","Delhi","Mumbai","Chennai","Kolkata")) %>% 
  mutate(Date=as.Date(Date, format = '%d-%B-%y'))


# Append to csv

diesel <- read.csv("./data/Diesel.csv", sep = ",",
                    fileEncoding="utf-8") 
#%>% 
 # mutate(Date = as.Date(Date,format='%d/%m/%Y'))

diesel <- rbind(diesel, dt)  


petrol <- read.csv("./data/Petrol.csv", sep = ",",
                   fileEncoding="utf-8") 
#%>% 
  #mutate(Date = as.Date(Date,format='%d-%m-%Y'))

petrol <- rbind(petrol, Pt) %>% 
  distinct()


# Export to csv

write.csv(petrol,"./data/Petrol.csv", quote=F,row.names=FALSE)


write.csv(diesel,"./data/Diesel.csv", quote=F,row.names=FALSE)
