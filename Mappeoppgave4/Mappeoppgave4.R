# laster ned pakker
library(rvest)
library(tidyverse)
library(rlist)

# Henter data og gjør den lesbar
sok_1005 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)
sok_1005 <- list.stack(sok_1005)

sok_1016 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&week=1-20&View=list") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)
sok_1016 <- list.stack(sok_1016)

sok_2030 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-2030-1&week=1-20&View=list") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)
sok_2030 <- list.stack(sok_2030)

sok_2001 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-2001-1&week=1-20&View=list") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)
sok_2001 <- list.stack(sok_2001)

# Endrer navn på en kolonne for å sette dem sammen
sok_2001 <- sok_2001 %>% rename("Uke 2" = `Uke 3`)  

# Setter sammen data settene
timeplan <- rbind(sok_1005, sok_1016, sok_2030, sok_2001)

# Setter øverste raden som kolonne navn
colnames(timeplan) <- timeplan[1,] 

# Fjerner unødvendig data og separerer dato og dag
timeplan <- timeplan %>% filter(!Dato == "Dato") %>% separate(Dato, 
                                                              into = c("Dag", "Dato"), 
                                                              sep = "(?<=[A-Za-z])(?=[0-9])")

# Fjerner blanke rekker
timeplan <- timeplan[-which(timeplan$Dag == ""), ]
timeplan <- timeplan[-which(timeplan$Rom == ""), ]

# Endrer format på dato
timeplan$Dato <- as.Date(timeplan$Dato, format="%d.%m.%Y")

# Genererer uke kolonne
timeplan$Uke <- strftime(timeplan$Dato, format = "%V")

# Velger data til tabellen
timeplan <- timeplan %>% select(Dag,Dato,Uke,Tid,Rom)

# DEL 2

library(rvest)
library(tidyverse)
library(rlist)


lister_timeplan <- list("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list","https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&week=1-20&View=list", "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-2030-1&week=1-20&View=list", "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-2001-1&week=1-20&View=list")


timeplan.f <- function(tp) {
  liste <- read_html(tp)
  df <- html_nodes(liste, "table")
  df <- html_table(df, fill = TRUE)
  df <- list.stack(df)
  colnames(df) <- df[1,] 
  df <- df %>% filter(!Dato == "Dato") %>% separate(Dato, 
                                                    into = c("Dag", "Dato"), 
                                                    sep = "(?<=[A-Za-z])(?=[0-9])")
  
  df$Dato <- as.Date(df$Dato, format="%d.%m.%Y")
  df$Uke <- strftime(df$Dato, format = "%V")
  df <- df %>% select(Dag,Dato,Uke,Tid,Rom)
  df <- na.omit(df)
  return(df)
}
map(lister_timeplan, timeplan.f)

