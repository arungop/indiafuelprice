# Packages needed

library(tabulizer)
library(tidyverse)
library(lubridate)
<<<<<<< HEAD
library(jsonlite)
=======

>>>>>>> main

# Source file
pdf <- "https://www.ppac.gov.in/WriteReadData/userfiles/file/PP_9_a_DailyPriceMSHSD_Metro.pdf"

# Read csv files

petrol_file = "./data/Petrol.csv"
diesel_file = "./data/Diesel.csv"

read_csv <- function(csv_file) {
    
    # Process repeated as above
    
    oil <- read.csv(csv_file, sep = ",",
                    fileEncoding="utf-8")
    oil$Date <- parse_date_time(oil$Date, orders = c('dmy', 'ydm','ymd'))
    
    return (oil)
    
}

modify_df <- function(fuel_today) {
    PD <- fuel_today %>% 
        mutate_all(type.convert) %>% 
        mutate_if(is.factor, as.character) %>% # Converted to character
        
        mutate(across(where(is.character), str_trim))%>%
        
        mutate(map_df(fuel_today, ~ gsub('\\s+', '', .x))) %>% # Remove unwanted characters
        
        `colnames<-`(c("Date","Delhi","Mumbai","Chennai","Kolkata")) %>% 
        mutate_at(vars("Delhi","Mumbai","Chennai","Kolkata"), as.numeric)
    
    PD$Date <- parse_date_time(PD$Date, orders = c('dmy', 'ydm','ymd'))
    
    return (PD)
    
}

fuel_update <- function(item, pdf) {
    # Extract first few rows of table
    petrol_diesel <- extract_tables(pdf,
                                    output = "matrix",
                                    pages = c(1,1),
                                    area = list(
                                        c(164.246,72.158,244.977,292.080),
                                        c(164.246,310.175,244.977,537.058)),
                                    guess = FALSE,
    )
    if (item == 'petrol') {
        # Petrol price
        fuel_td <- as.data.frame(petrol_diesel[[1]]) # As data.frame
        Pt <- modify_df(fuel_td)
        petrol <- read_csv(petrol_file)
        petrole <- rbind(petrol,Pt) %>%
            distinct()
        
        petrole <-petrole[rev(order(as.Date(petrole$Date, format = "%d-%B-%y"))),]
        
        return (petrole)
    }
    else if (item == 'diesel') {
        fuel_td <- as.data.frame(petrol_diesel[[2]])
        dt <- modify_df(fuel_td)
        diesel <- read_csv(diesel_file)
        diesele <- rbind(diesel, dt) %>% # Rbind to append scrapped data
            distinct()
        
        diesele <-diesele[rev(order(as.Date(diesele$Date, format = "%d-%B-%y"))),]
        
        return (diesele)
    }
    else {
        print("Invalid Item. Please use 'petrol' or 'diesel'.")
    }
    
}

write_output <- function(item, format, param) {
    
    if (item == 'petrol' || item == 'diesel') {
        message(item)
        if (format == 'csv') {
            fname <- paste("./data/", str_to_title(item),".csv", sep="")
            write.csv(param,fname, quote=F,row.names=FALSE)
        }

<<<<<<< HEAD
        else if (format == 'json') {
            fname <- paste("./data/", str_to_title(item),".json", sep="")
            djson <- write_json(param, path=fname)
        }
        
        
        else {
            print("Invalid format. Use 'csv' or 'json' for output.")
        }
    }
    else {
        stop("Invalid item. Please use 'petrol' or 'diesel'.")
    }
}
=======
  mutate(map_df(petrol_today, ~ gsub('\\s+', '', .x))) %>% # Remove unwanted characters

  `colnames<-`(c("Date","Delhi","Mumbai","Chennai","Kolkata"))%>% # City headings

  mutate(Date= lubridate::dmy(Date))%>% # Date in dmy format

  mutate_if(is.character, ~as.numeric(as.character(.))) # Convert other coloumns into numeric

Pt$Date <- ymd(Pt$Date) # Changed date into ymd format


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

dt$Date <- ymd(dt$Date)


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
>>>>>>> main

pet <- fuel_update('petrol', pdf)
des <- fuel_update('diesel', pdf)
write_output('petrol', 'csv', pet)
write_output('petrol', 'json', pet)
write_output('diesel', 'csv', des)
write_output('diesel', 'json', des)
