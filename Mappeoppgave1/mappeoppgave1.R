library(ggplot2)
library(data.table)
library(lubridate)
library(zoo)
library(tidyverse)
library(scales)
library(dplyr)


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

# henter data settet
# Lower Troposphere :
data1 <- read_table("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

# Fjerner den unødvendig delen av tabellene,
# og andre kolonner som vi ikke blir å bruke. 
data_temp <-
  data1 %>% 
  select(Year, Mo, Globe) %>% 
  slice(1:(n() - 12))


# Endrer data fra character til numrik
data_temp$Year <- as.numeric(data_temp$Year)
data_temp$Mo <- as.numeric(data_temp$Mo)
data_temp$Globe <- as.numeric(data_temp$Globe)

# Legger til ny kolonne med rollmean,
# og setter sammen år og månder i egen kolonne samt
# fjerner oprinnelig kolonne for år å månde
data_temp = data_temp %>% 
  mutate(roll_mean = rollmean(Globe, k = 13, fill = NA, align = "right"),
         date = ymd(paste(data_temp$Year, data_temp$Mo, 1, sep="-")))
data_temp <- data_temp %>% 
  select(date, Globe, roll_mean)

# Lager plot
ggplot(data_temp, aes( x = date, y = Globe)) +
  geom_line(col = "dodgerblue3", size = 0.5) +
  geom_point(col = "dodgerblue3", shape = 1, size = 1.5) +
  geom_line(aes(y =  roll_mean), col = "firebrick1", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "twodash", col = "gray48") +
  scale_y_continuous(breaks = c(-0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0,
                                0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) +
  scale_x_date(breaks=date_breaks("1 years"), labels=date_format("%Y")) +
  guides(x = guide_axis(angle = 90)) +
  labs(title = "Siste globale temperaturene", 
       x = "År", y = "Endring i temperatur i celsius") +
  geom_text(x = as.Date("1988-01-01"), y=0.3, label="Global Lower Atmosphere",
            col = "dodgerblue3") +
  geom_text(x = as.Date("2017-01-01"), y = -0.6, label="Running, centered 13-month average",
            col = "firebrick1") +
theme_bw()



# Oppgave 2

# Henter inn resterende data.

# Mid-Troposphere :
data2 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")

# Tropopause :
data3 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")

# Lower Stratosphere :
data4 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

# Setter samme data i en ny egen data sett.
data_all <- bind_cols(data1, data2, data3, data4)

# samme prosedyre som i oppgave 1 

data_all <- data_all %>% 
  select( Year...1, Mo...2, NoPol...21, NoPol...50, NoPol...79, NoPol...108) %>% 
  slice(1:(n() - 12))

# endrer navn på kolonnene.
colnames(data_all) <-
  c("Year", "Mo", "NoPol1","NoPol2","NoPol3","NoPol4")

# Endrer data fra character til numrik
data_all$Year <- as.numeric(data_all$Year)
data_all$Mo <- as.numeric(data_all$Mo)
data_all$NoPol1 <- as.numeric(data_all$NoPol1)
data_all$NoPol2 <- as.numeric(data_all$NoPol2)
data_all$NoPol3 <- as.numeric(data_all$NoPol3)
data_all$NoPol4 <- as.numeric(data_all$NoPol4)

# Legger til ny kolonne med "date".
# og fjerner kolonner med månde og år.
data_all = data_all %>% 
  mutate(date = ymd(paste(data_all$Year, data_all$Mo, 1, sep="-")))
data_all <- data_all %>% 
  select(date, NoPol1, NoPol2, NoPol3, NoPol4)


# lager ny kolonne med "mean"
data_all <- data_all %>% 
  mutate(snitt = cummean(c(data_all$NoPol1 + data_all$NoPol2 +
                             data_all$NoPol3 + data_all$NoPol4)/4))


# Gjør den om til lang liste for å gjøre det enklere å lage plot
data_all <- data_all %>% 
  select(date, NoPol1, NoPol2, NoPol3, NoPol4, snitt) %>% 
  pivot_longer(-date, names_to = "Området", values_to = "deg.C")

# Lager plot.

ggplot(data_all, aes( x = date, y = deg.C, col = Området)) +
  geom_line() +
  scale_y_continuous(breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0,
                                1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  scale_x_date(breaks=date_breaks("1 years"), labels=date_format("%Y")) +
  guides(x = guide_axis(angle = 90)) +
  scale_color_discrete(name = "Nivå", labels = c("Lower Troposphere",
                                                 "Mid-Troposphere",
                                                 "Tropopause",
                                                 "Lower Stratosphere",
                                                 "Gjennomsnitt")) +
  labs(title = "Siste temperaturene fra 60-90 grader nord", 
       x = "År", y = "Endring i temperatur i celsius") +
  theme_bw()




