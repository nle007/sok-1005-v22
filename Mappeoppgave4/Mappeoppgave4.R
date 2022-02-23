library(janitor)
library(rvest)
library(tidyverse)
library(rlist)



# Henter data og gjør den lesbar
sok_1005 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&View=list") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)
sok_1005 <- list.stack(sok_1005)

sok_1016 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&View=list") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)
sok_1016 <- list.stack(sok_1006)

sok_2030 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-2030-1&View=list") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)
sok_1030 <- list.stack(sok_1030)

sok_2001 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-2001-1&View=list") %>% 
  html_node("table") %>% 
  html_table(fill = TRUE)
sok_2001 <- list.stack(sok_2001)


