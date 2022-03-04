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

