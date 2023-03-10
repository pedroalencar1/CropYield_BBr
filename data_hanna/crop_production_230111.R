#*******************************************************************************
#crop production - Brandenburg 2007-2021
#*******************************************************************************
library (readxl)
library(tidyverse)
library(tidyr)
library(naniar)

#root <- "C:/Users/jarosch/Documents/crop_production/Archive/" #root directory Hanna
root <- "C:/Users/hanna/OneDrive/Dokumente/Arbeit TU Ökohydrologie/crop_production/Archive/" #homeoffice Hanna

year <- c("2001", "2006", "2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")
path <- paste0(root,"SB_C02-02-00_")
part <- paste0(year,"j01_BB.xls")
sheet <- c("4.1","4.2","4.3","4.4","4.5","4.6","4.7","4.8","4.9","4.10","4.11","4.12","4.13","4.14","4.15", "4.16", "4.17", "4.18") 
sheet1 <- c("Tabelle 4.1","Tabelle 4.2","Tabelle 4.3","Tabelle 4.4","Tabelle 4.5","Tabelle 4.6","Tabelle 4.7","Tabelle 4.8","Tabelle 4.9","Tabelle 4.10","Tabelle 4.11","Tabelle 4.12","Tabelle 4.13","Tabelle 4.14","Tabelle 4.15","Tabelle 4.16") #2007, 2008, 2009
sheet2 <- c("4.1","4.2","4.3","4.4","4.5","4.6","4.7","4.8","4.9","4.10","4.11","4.12","4.13","4.14","4.15") #2010, 2011, 2012
sheet3 <- c("4.1.1","4.1.2","4.1.3","4.1.4","4.2.1","4.2.2","4.2.3","4.2.4","4.2.5","4.2.6","4.2.7","4.2.8", "4.2.9", "4.2.10","4.2.11","4.2.12","4.2.13","4.2.14") #2013

names <- paste0("df_",sheet)
names_data <- paste0("data_",year)
names_df <- paste0("df_",year)

Landkreis <- rep(c("Kreisfreie Staedte","Brandenburg an der Havel (KfS)", "Cottbus (KfS)", "Frankfurt (Oder) (KfS)", "Potsdam (KfS)", "Barnim", "Dahme-Spreewald", "Elbe-Elster", "Havelland", "Maerkisch-Oderland", "Oberhavel", "Oberspreewald-Lausitz", "Oder-Spree", "Ostprignitz-Ruppin", "Potsdam-Mittelmark", "Prignitz", "Spree-Neiße", "Teltow-Flaeming", "Uckermark"), each=31)

Variety <- rep(c("Winterweizen einschl. Dinkel", "Weizen zusammen", "Roggen", "Roggen und Wintermenggetreide", "Brotgetreide zusammen", "Wintergerste", "Sommergerste", "Gerste zusammen","Hafer", "Triticale","Futter- und Industriegetreide","Getreide zur Ganzpflanzenernte","Getreide zusammen (ohne Koernermais und Corn-Cob-Mix)","Koernermais (einschl. Corn-Cob-Mix)", "Getreide insgesamt",
                 "mittelfruehe und spaete Kartoffeln","Kartoffeln zusammen", "Zuckerrueben",
                 "Erbsen", "Lupinen",
                 "Winterraps","Raps und Ruebsen zusammen","Sonnenblumen",
                 "Klee, Kleegras und Klee-Luzerne-Gemisch", "Leguminosen", "Luzerne, Luzernegras", "Feldgras/Grasanbau", "Silomais", "Wiesen", "Weiden", "Wiesen und Maehweiden zusammen"), times=19)

Crop <- rep(c("Getreide", "Getreide", "Getreide", "Getreide", "Getreide", "Getreide", "Getreide","Getreide", "Getreide", "Getreide", "Getreide", "Getreide", "Getreide", "Getreide", "Getreide","Hackfruechte", "Hackfruechte", "Hackfruechte","Huelsenfruechte","Huelsenfruechte","Oelfruechte","Oelfruechte","Oelfruechte", "Futterpflanzen, Dauergruenland, Gruenernte", "Futterpflanzen, Dauergruenland, Gruenernte", "Futterpflanzen, Dauergruenland, Gruenernte", "Futterpflanzen, Dauergruenland, Gruenernte", "Futterpflanzen, Dauergruenland, Gruenernte", "Futterpflanzen, Dauergruenland, Gruenernte", "Futterpflanzen, Dauergruenland, Gruenernte", "Futterpflanzen, Dauergruenland, Gruenernte"),times=19)

df <- data.frame(Landkreis,Crop,Variety)

#*******************************************************************************
#Functions
#*******************************************************************************
#### to read data
#for 2001-2006(average) and 2006
read_data <- function(path, part, sheet, year){
  file <- paste0(path,part)
  #read file
  data <- read_excel(file, sheet=sheet, na=c("•", "–"), col_names=F)
  
  #save which Landkreis
  Landkreis <- data[1,1]
  
  data <- data %>% slice(-c(1:6))
  
  if(year=="2006"){
  data <- data %>% select(-c(1,3,4,5,6,8,9,10,11,12))  # select columns
  } else {
    data <- data %>% select(-c(1,3,4,5,7,8,9,10,11,12)) # select columns
  }
    
  #add column A_ha, P_per_A, which are not given in data
  data <- data %>% add_column(A_ha = NA,
                              P_t = NA)

  #add Landkreis column
  data$Landkreis <- rep(Landkreis,nrow(data))
  
  #reorder columns
  data <- data[c(6,3,1,4,2,5)]
  
  #delete rows with only NAs
  data <- data[rowSums(is.na(data)) <= 4, ]
  
  #header
  names(data) <- c("Landkreis","Crop", "Variety", "A_ha", "P_per_A", "P_t")
  
  #convert amount from character to numeric
  data$A_ha <- as.numeric(data$A_ha)
  data$P_per_A <- as.numeric(data$P_per_A)
  data$P_t <- as.numeric(data$P_t)
  
  #delete rows where crop is NA
  data <- data[!is.na(data$Crop),]
  
  return(data)
  
}

#for 2007, 2008, 2009, 2010, 2011, 2012
read_data_1 <- function(path, part, sheet){
  file <- paste0(path,part)
  #read file
  data <- read_excel(file, sheet=sheet, na=c("•", "–"), col_names=F)
  
  #save which Landkreis
  Landkreis <- data[1,1]
  
  data <- data %>% slice(-c(1:6))
  
  if(ncol(data)==13) {
    data <- data %>% select(-c(1,3,4,5,6,7,11,12))  # select columns
    
    #add Landkreis column
    data$Landkreis <- rep(Landkreis,nrow(data))
    
    #reorder columns
    data <- data[c(6,5,1,2,3,4)]
    
    } 
  else if(ncol(data!=13)) 
    {
    if(ncol(data) == 11){
      data <- data %>% select(-c(1,3,4,5,6,7,9,10)) # select columns
    }
    else{
      #select columns
      data <- data %>% select(-c(2,3,5,6))
    }
      
    #add column A_ha, P_per_A, which are not given in data
    data <- data %>% add_column(A_ha = NA,
                                  P_t= NA)
      
    #add Landkreis column
    data$Landkreis <- rep(Landkreis,nrow(data))
      
    #reorder columns
    data <- data[c(6,3,1,4,2,5)]
    
    }

  #delete rows with only NAs
  data <- data[rowSums(is.na(data)) <= 4, ]
  
  
  #header
  names(data) <- c("Landkreis","Crop", "Variety", "A_ha", "P_per_A", "P_t")
  
  #convert amount from character to numeric
  data$A_ha <- as.numeric(data$A_ha)
  data$P_per_A <- as.numeric(data$P_per_A)
  data$P_t <- as.numeric(data$P_t)
  
  #delete rows where crop is NA
  data <- data[!is.na(data$Crop),]
  
  return(data)
  
}

#for 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
read_data_2 <- function(path, part, sheet){
  file <- paste0(path,part)
  #read file
  data <- read_excel(file, sheet=sheet, col_names=F)
  
  #save which Landkreis
  Landkreis <- data[1,1]
  
  #delete row 1 to 6
  data <- data %>% slice(-c(1:6))

  
  if(ncol(data)==9) {
    data <- data %>% select(-c(2,3,7,8))  # select columns
    } else {
    data <- data %>% select(-c(2,3,5,8,9)) # select columns
    }
  
  data[data == c("•", "•", "•","•","•","–","–","–","–")] <- NA
  
  #delete rows with only NAs
  data <- data[rowSums(is.na(data)) <= 4, ]

  #add Landkreis column
  data$Landkreis <- rep(Landkreis,nrow(data))

  #reorder columns
  data <- data[c(6,5,1,2,3,4)]
  
  #header
  names(data) <- c("Landkreis","Crop", "Variety", "A_ha", "P_per_A", "P_t")
  
  #convert amount from character to numeric
  data$A_ha <- as.numeric(data$A_ha)
  data$P_per_A <- as.numeric(data$P_per_A)
  data$P_t <- as.numeric(data$P_t)
  
  if(max(data$A_ha, na.rm=T) < 1000) {
    #Area in ha
    data$A_ha <- data$A_ha*1000
  }
  if(max(data$P_t, na.rm=T) < 1000) {
    #and Production in t
    data$P_t <- data$P_t *1000
  }

  #delete rows where crop is NA
  data <- data[!is.na(data$Crop),]
  
  return(data)
    
}


#### to rename variety
rename_variety <- function(data) {
  data <- data %>%
    mutate(Variety = case_when(
      str_detect(Variety, "Winterweizen") ~ "Winterweizen einschl. Dinkel",
      str_detect(Variety, "Roggen und Wintermenggetreide") ~ "Roggen und Wintermenggetreide",
      str_detect(Variety, "Roggen")  ~ "Roggen",
      str_detect(Variety, "Brotgetreide") ~ "Brotgetreide zusammen",
      str_detect(Variety, "Wintergerste") ~ "Wintergerste",
      str_detect(Variety, "Sommergerste") ~ "Sommergerste",
      str_detect(Variety, "Gerste") ~ "Gerste zusammen",
      str_detect(Variety, "Hafer") ~ "Hafer",
      str_detect(Variety, "Triticale") ~ "Triticale",
      str_detect(Variety, "Futter- und Industriegetreide") ~ "Futter- und Industriegetreide",
      str_detect(Variety, "Getreide zusammen") ~ "Getreide zusammen (ohne Koernermais und Corn-Cob-Mix)",
      str_detect(Variety, "(einschl. Corn-Cob-Mix)") ~ "Koernermais (einschl. Corn-Cob-Mix)",
      str_detect(Variety, "Getreide insgesamt") ~ "Getreide insgesamt",
      str_detect(Variety, "mittelfrühe und späte Kartoffeln") ~ "mittelfruehe und spaete Kartoffeln",
      str_detect(Variety, "Kartoffeln") ~ "Kartoffeln zusammen",
      str_detect(Variety, "Zuckerrüben") ~ "Zuckerrueben",
      str_detect(Variety, "Futtererbsen") ~ "Erbsen",
      str_detect(Variety, "Erbsen") ~ "Erbsen",
      str_detect(Variety, "Raps und Rübsen") ~ "Raps und Ruepsen zusammen",
      str_detect(Variety, "upinen") ~ "Lupinen",
      str_detect(Variety, "Winterraps") ~ "Winterraps",
      str_detect(Variety, "onnenblumen") ~ "Sonnenblumen",
      str_detect(Variety, "Klee") ~ "Klee, Kleegras und Klee-Luzerne-Gemisch",
      str_detect(Variety, "Luzerne") ~ "Luzerne, Luzernegras",
      str_detect(Variety, "Feldgras/Grasanbau") ~ "Feldgras/Grasanbau",
      str_detect(Variety, "Silomais") ~ "Silomais",
      str_detect(Variety, "Wiesen und Mähweiden") ~ "Wiesen und Maehweiden zusammen",
      str_detect(Variety, "iesen") ~ "Wiesen",
      str_detect(Variety, "eiden") ~ "Weiden",
      str_detect(Variety, "Getreide zur Ganzpflanzenernte") ~ "Getreide zur Ganzpflanzenernte",
      str_detect(Variety, "Leguminosen zur Ganzpflanzenernte") ~ "Leguminosen",
      TRUE ~ Variety
    )
    )
  return(data)
}

#### to rename crop
rename_crop <- function(data) {
  data <- data %>%
    mutate(Crop = case_when(
      str_detect(Crop, "Getreide") ~ "Getreide",
      str_detect(Crop, "Hackfrüchte")  ~ "Hackfruechte",
      str_detect(Crop, "Hülsenfrüchte") ~ "Huelsenfruechte",
      str_detect(Crop, "Ölfrüchte") ~ "Oelfruechte",
      str_detect(Crop, "Futterpflanzen") ~ "Futterpflanzen, Dauergruenland, Gruenernte",
      str_detect(Crop, "Dauergrünland") ~ "Futterpflanzen, Dauergruenland, Gruenernte",
      str_detect(Crop, "Pflanzen zur Grünernte") ~ "Futterpflanzen, Dauergruenland, Gruenernte",
      TRUE ~ Crop
    )
    )
  return(data)
}

#### to rename Landkreise
rename_LK <- function(data) {
  data <- data %>%
    mutate(Landkreis = case_when(
      str_detect(Landkreis, "Brandenburg") ~ "Brandenburg an der Havel (KfS)",
      str_detect(Landkreis, "Cottbus")  ~ "Cottbus (KfS)",
      str_detect(Landkreis, "Frankfurt") ~ "Frankfurt (Oder) (KfS)",
      str_detect(Landkreis, "Potsdam-Mittelmark") ~ "Potsdam-Mittelmark",
      str_detect(Landkreis, "Potsdam") ~ "Potsdam (KfS)",
      str_detect(Landkreis, "Barnim") ~ "Barnim",
      str_detect(Landkreis, "Dahme-Spreewald") ~ "Dahme-Spreewald",
      str_detect(Landkreis, "Elbe-Elster") ~ "Elbe-Elster",
      str_detect(Landkreis, "Havelland") ~ "Havelland",
      str_detect(Landkreis, "Märkisch-Oderland") ~ "Maerkisch-Oderland",
      str_detect(Landkreis, "Oberhavel") ~ "Oberhavel",
      str_detect(Landkreis, "Oberspreewald-Lausitz") ~ "Oberspreewald-Lausitz",
      str_detect(Landkreis, "Oder-Spree") ~ "Oder-Spree",
      str_detect(Landkreis, "Ostprignitz-Ruppin") ~ "Ostprignitz-Ruppin",
      str_detect(Landkreis, "Prignitz") ~ "Prignitz",
      str_detect(Landkreis, "Spree-Neiße") ~ "Spree-Neiße",
      str_detect(Landkreis, "Teltow-Fläming") ~ "Teltow-Flaeming",
      str_detect(Landkreis, "Uckermark") ~ "Uckermark",
      str_detect(Landkreis, "Kreisfreie Städte") ~ "Kreisfreie Staedte",
    )
    )
  return(data)
}


#### to merge data with a dataframe (df) containing all Landkreise, crops and varieties
merge_df <- function(df,data){
  data <- merge(x=df,y=data, 
                by=c("Landkreis", "Crop", "Variety"), all.x=TRUE)
  Year <- data[1,7]
  data$Year <- rep(Year,nrow(data))
  return(data)
}

#*******************************************************************************
# read data
#*******************************************************************************
#2001-2006 (average)
for (i in seq_along(sheet1)) {
  assign(names[i], read_data(path,part[3],sheet1[i], "2001-2006"))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16)
data_2001 <- bind_rows(data_list)

data_2001$Year <- rep("2001-2006",nrow(data_2001))

#2006
for (i in seq_along(sheet1)) {
  assign(names[i], read_data(path,part[3],sheet1[i], "2006"))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16)
data_2006 <- bind_rows(data_list)

data_2006$Year <- rep("2006",nrow(data_2006))

#2007
for (i in seq_along(sheet1)) {
  assign(names[i], read_data_1(path,part[3],sheet1[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16)
data_2007 <- bind_rows(data_list)

data_2007$Year <- rep("2007",nrow(data_2007))

#2008
for (i in seq_along(sheet1)) {
  assign(names[i], read_data_1(path,part[4],sheet1[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16)
data_2008 <- bind_rows(data_list)

data_2008$Year <- rep("2008",nrow(data_2008))


#2009
for (i in seq_along(sheet1)) {
  assign(names[i], read_data_1(path,part[5],sheet1[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16)
data_2009 <- bind_rows(data_list)

data_2009$Year <- rep("2009",nrow(data_2009))


#2010
for (i in seq_along(sheet2)) {
  assign(names[i], read_data_1(path,part[6],sheet2[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15)
data_2010 <- bind_rows(data_list)

data_2010$Year <- rep("2010",nrow(data_2010))

#2011
for (i in seq_along(sheet2)) {
  assign(names[i], read_data_1(path,part[7],sheet2[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15)
data_2011 <- bind_rows(data_list)

data_2011$Year <- rep("2011",nrow(data_2011))


#2012
for (i in seq_along(sheet2)) {
  assign(names[i], read_data_1(path,part[8],sheet2[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15)
data_2012 <- bind_rows(data_list)

data_2012$Year <- rep("2012",nrow(data_2012))


#2013
for (i in seq_along(sheet3)) {
  assign(names[i], read_data_2(path,part[9],sheet3[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16,df_4.17,df_4.18)
data_2013 <- bind_rows(data_list)

data_2013$Year <- rep("2013",nrow(data_2013))


#2014
for (i in seq_along(sheet3)) {
  assign(names[i], read_data_2(path,part[10],sheet3[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16,df_4.17,df_4.18)
data_2014 <- bind_rows(data_list)

data_2014$Year <- rep("2014",nrow(data_2014))


#2015
for (i in seq_along(sheet3)) {
  assign(names[i], read_data_2(path,part[11],sheet3[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16,df_4.17,df_4.18)
data_2015 <- bind_rows(data_list)

data_2015$Year <- rep("2015",nrow(data_2015))


#2016
for (i in seq_along(sheet3)) {
  assign(names[i], read_data_2(path,part[12],sheet3[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16,df_4.17,df_4.18)
data_2016 <- bind_rows(data_list)

data_2016$Year <- rep("2016",nrow(data_2016))


#2017
for (i in seq_along(sheet3)) {
  assign(names[i], read_data_2(path,part[13],sheet3[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16,df_4.17,df_4.18)
data_2017 <- bind_rows(data_list)

data_2017$Year <- rep("2017",nrow(data_2017))


#2018
for (i in seq_along(sheet3)) {
  assign(names[i], read_data_2(path,part[14],sheet3[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16,df_4.17,df_4.18)
data_2018 <- bind_rows(data_list)

data_2018$Year <- rep("2018",nrow(data_2018))


#2019
for (i in seq_along(sheet3)) {
  assign(names[i], read_data_2(path,part[15],sheet3[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16,df_4.17,df_4.18)
data_2019 <- bind_rows(data_list)

data_2019$Year <- rep("2019",nrow(data_2019))


#2020
for (i in seq_along(sheet3)) {
  assign(names[i], read_data_2(path,part[16],sheet3[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16,df_4.17,df_4.18)
data_2020 <- bind_rows(data_list)

data_2020$Year <- rep("2020",nrow(data_2020))

#2021
for (i in seq_along(sheet3)) {
  assign(names[i], read_data_2(path,part[17],sheet3[i]))
}

data_list <- list(df_4.1,df_4.2,df_4.3,df_4.4,df_4.5,df_4.6,df_4.7,df_4.8,df_4.9,df_4.10,df_4.11,df_4.12,df_4.13,df_4.14,df_4.15,df_4.16,df_4.17,df_4.18)
data_2021 <- bind_rows(data_list)

data_2021$Year <- rep("2021",nrow(data_2021))


#*******************************************************************************
#adjust names for variety, crop and Landkreis
#*******************************************************************************

data_list <- list(data_2001, data_2006, data_2007,data_2008, data_2009, data_2010, data_2011, data_2012, data_2013, data_2014, data_2015, data_2016, data_2017, data_2018, data_2019, data_2020, data_2021)

for (i in 1:length(data_list)) {
  assign(names_data[i], rename_variety(data_list[[i]]))
}

data_list <- list(data_2001, data_2006,data_2007,data_2008, data_2009, data_2010, data_2011, data_2012, data_2013, data_2014, data_2015, data_2016, data_2017, data_2018, data_2019, data_2020, data_2021)

for (i in 1:length(data_list)) {
  assign(names_data[i], rename_crop(data_list[[i]]))
}

data_list <- list(data_2001, data_2006,data_2007,data_2008, data_2009, data_2010, data_2011, data_2012, data_2013, data_2014, data_2015, data_2016, data_2017, data_2018, data_2019, data_2020, data_2021)

for (i in 1:length(data_list)) {
  assign(names_data[i], rename_LK(data_list[[i]]))
}

data_list <- list(data_2001, data_2006,data_2007,data_2008, data_2009, data_2010, data_2011, data_2012, data_2013, data_2014, data_2015, data_2016, data_2017, data_2018, data_2019, data_2020, data_2021)

#*******************************************************************************
#merge datasets with df to have the same length
#*******************************************************************************

for (i in 1:length(data_list)){
  assign(names_df[i],merge_df(df,data_list[[i]]))
}

#*******************************************************************************
#combine datasets of every year into one dataframe
#*******************************************************************************

df <- rbind(df_2001, df_2006,df_2007,df_2008, df_2009, df_2010, df_2011, df_2012, df_2013, df_2014, df_2015, df_2016, df_2017, df_2018, df_2019, df_2020, df_2021)

write.csv(df, paste0(root,"crop_Brandenburg.csv"), row.names=FALSE)


#P_per_A in dt/h


# ???
#2008,2009, 2011, 2012: dezitonnen --> only dt/h?!
#2013, 2014, 2015: in 1000 t --> 1000 h, dt/h, 1000 t
#2007, 2016-2021: in Tonnen --> h, dt/h, t
