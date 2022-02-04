# Laster ned pakker

library(rjson)
library(ggplot2)
library(ggrepel)

# Henter json fila å gjør den lesbar
df <- "https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json"
df <- fromJSON(paste(readLines(df), collapse=""))
df <- do.call(rbind.data.frame, df)

# Oppgave 1
p1 <- ggplot(df, aes( x = fully_vaccinated_pct_of_pop, y = deaths_per_100k)) +
  geom_point(col = "darkcyan" , size = 4, alpha = 0.3) +
  geom_label_repel(aes(label = name), size = 3) +
  scale_x_continuous(labels=scales::percent,
                     breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(title = "Covid-19 deaths since universal adult vaccine eligilbility copmared with \n vaccination rates",
       x = "Share of total population fully vaccinated" ,
       y = "20 avg. monthly deaths per 100,000") +
  geom_text(aes(label="Lower vaccination rate, \n higher death rate", x=0.58,y=17))+
  geom_text(aes(label="Higher vaccination rate, \n lower death rate", x=0.71,y=10)) +
  geom_segment(aes(x = 0.56, y = 17, xend = 0.525 , yend = 18),
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = 0.725, y = 8, xend = 0.74 , yend = 6.5),
               arrow = arrow(length = unit(0.5, "cm"))) +
  theme_bw()

p1

# Oppgave 2 
lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = df)

# ser at y-verdien er 31.15 og x er fallende med -36.66
# altså en fallende linje som vi kan visualisere med funksjon geom_smooth.

p1 + geom_smooth(method = lm, se = FALSE)
