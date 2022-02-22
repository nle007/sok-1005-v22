library(tidyverse)
library(rvest)
library(dplyr)
library(ggplot2)

# Laster data som html 
tabell <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132") %>% 
  html_table(header = 1)

# Henter tabellen som vi skal bruke
tabell <- tabell[[1]] 

#fjerner unødvendig data
tabell <- tabell %>% 
  separate("WLTP-tall", c("wltp")) %>%  
  separate("STOPP", c("stopp")) %>% 
  slice(-c(19,26)) 

#gjør om til numric
tabell$wltp <- as.numeric(tabell$wltp)
tabell$stopp <- as.numeric(tabell$stopp)

# Oppgave 1
p <- ggplot(tabell, aes(x=wltp, y=stopp)) +
  geom_point()+
  scale_x_continuous(limits=c(200,600), name = "Oppgitt rekkevidde") +
  scale_y_continuous(limits=c(200,600), name = "Reel rekkevidde") +
  geom_abline(color="red") +
  ggtitle("Oppgitt rekkevide fra levrandører av elbiler\n
          og den faktiske rekkevidden") +
  theme_classic()

# Oppgave 2
lm(stopp ~ wltp, data = tabell)
p + geom_smooth(method = lm)
# Den røde linjen representerer den prfekte korerlasjon mellom stopp og wltp, som skal være lik 1 til 1 forhold.
# Vi ser at den faktiske korrelasjon mellom disse er litt mindre (den blåe linjen),
# men er fortsett ganske høy og er lik ca 0.86 i WLTP.
# Den oppgitte rekkevidden fra bilprodusenten gjelder kun/ oftest under de "perfekte" værforholdene, 
# noe som er veldig sjeldent i realiteten, det representeres med den røde linjen.
# Den reel rekkevidden representeres ved den blåe linjen.
# Det er ikke stort avvik fra oppgitt til reel rekkeviddet, noe som kan virke troverdig,
# da vær, temperatur og kjøremåten påvirker forbruket på en elbil. 





