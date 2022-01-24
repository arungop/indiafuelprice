# Packages needed

library(tabulizer)
library(tidyverse)


# Source file
pdf <- "https://www.ppac.gov.in/WriteReadData/userfiles/file/PP_9_a_DailyPriceMSHSD_Metro.pdf"

# Extract first few rows of table
petrol_diesel <- extract_tables(pdf,
                            output = "matrix",
                            pages = c(1,1),
                            area = list(
                              c(164.246,72.158,244.977,292.080),
                              c(164.246,310.175,244.977,537.058)),
                              guess = FALSE,
)

# Petrol price
petrol_today <- as.data.frame(petrol_diesel[[1]]) # As data.frame


Pt <- petrol_today  %>%
  mutate_all(type.convert) %>%

  mutate_if(is.factor, as.character) %>% # Converted to character

  mutate(across(where(is.character), str_trim))%>%

  mutate(map_df(petrol_today, ~ gsub('\\s+', '', .x))) %>% # Remove unwanted characters

  `colnames<-`(c("Date","Delhi","Mumbai","Chennai","Kolkata"))%>% # City headings

  mutate(Date= lubridate::dmy(Date))%>% # Date in dmy format

  mutate_if(is.character, ~as.numeric(as.character(.))) # Convert other coloumns into numeric

Pt$Date <- as.Date(Pt$Date) # Changed date into ymd format


# Diesel price (Procedures same as above)

diesel_today <- as.data.frame(petrol_diesel[[2]])

dt <- diesel_today  %>%

  mutate_all(type.convert) %>%

  mutate_if(is.factor, as.character) %>%

  mutate(across(where(is.character), str_trim))%>%

  mutate(map_df(diesel_today, ~ gsub('\\s+', '', .x))) %>%

  `colnames<-`(c("Date","Delhi","Mumbai","Chennai","Kolkata"))%>%

  mutate(Date= lubridate::dmy(Date))%>%

  mutate_if(is.character, ~as.numeric(as.character(.)))

print(dt)
dt$Date <- as.Date(dt$Date)


# Append data to csv (two files)

diesel <- read.csv("./data/Diesel.csv", sep = ",",
                   fileEncoding="utf-8")%>%

  mutate_if(is.factor, ~as.numeric(as.character(.)))%>% # All other coloumns except date is converted to numeric

  mutate(Date = as.Date(Date,format='%Y-%m-%d')) # Date coloumn

diesele <- rbind(diesel, dt) %>% # Rbind to append scrapped data
  group_by(Date)  %>%
  distinct() # Unique rows


# Process repeated as above

petrol <- read.csv("./data/Petrol.csv", sep = ",",
                   fileEncoding="utf-8")%>%

  mutate_if(is.factor, ~as.numeric(as.character(.)))%>%

  mutate(Date = as.Date(Date,format='%Y-%m-%d'))

petrole <- rbind(petrol,Pt) %>%
  distinct()%>%
  group_by(Date)



# Export to csv

write.csv(petrole,"./data/Petrol.csv", quote=F,row.names=FALSE)


write.csv(diesele,"./data/Diesel.csv", quote=F,row.names=FALSE)

