library(tidyverse)
library(readr)
library(data.table)
library(forecast)
library(xts)
library(strucchange)
library(changepoints)
library(zoo)
library(tseries)
library(gridExtra)
library(StratigrapheR)
library(readr)
library(data.table)
library(dplyr)

# Function to read + preprocess one city
load_city_data <- function(filename) {
  df <- read_csv(filename) %>% as.data.table()
  df <- rename(df, date = '...1')
  df$date <- as.Date(df$date)
  df$lagwiki <- lag(df$Ukrainian)
  df <- drop_na(df)
  return(df)
}

# Named vector of cities and their file paths
PolishCities <- c(
  "Białystok" = "DataPoland/Białystok-prop-daily-wikipedia.csv",
  "Bydgoszcz" = "DataPoland/Bydgoszcz-prop-daily-wikipedia.csv",
  "Częstochowa" = "DataPoland/Częstochowa-prop-daily-wikipedia.csv",
  "Gdańsk" = "DataPoland/Gdańsk-prop-daily-wikipedia.csv",
  "Gdynia" = "DataPoland/Gdynia-prop-daily-wikipedia.csv",
  "Gliwice" = "DataPoland/Gliwice-prop-daily-wikipedia.csv",
  "Katowice" = "DataPoland/Katowice-prop-daily-wikipedia.csv",
  "Kielce" = "DataPoland/Kielce-prop-daily-wikipedia.csv",
  "Kraków" = "DataPoland/Kraków-prop-daily-wikipedia.csv",
  "Łódź" = "DataPoland/Łódź-prop-daily-wikipedia.csv",
  "Lublin" = "DataPoland/Lublin-prop-daily-wikipedia.csv",
  "Poznań" = "DataPoland/Poznań-prop-daily-wikipedia.csv",
  "Radom" = "DataPoland/Radom-prop-daily-wikipedia.csv",
  "Rzeszów" = "DataPoland/Rzeszów-prop-daily-wikipedia.csv",
  "Sosnowiec" = "DataPoland/Sosnowiec-prop-daily-wikipedia.csv",
  "Szczecin" = "DataPoland/Szczecin-prop-daily-wikipedia.csv",
  "Toruń"  = "DataPoland/Toruń-prop-daily-wikipedia.csv",
  "Warsaw" = "DataPoland/Warsaw-prop-daily-wikipedia.csv",
  "Wrocław" = "DataPoland/Wrocław-prop-daily-wikipedia.csv"
)

GermanCities <- c(
  "Aachen" = "DataGermany/Aachen-prop-daily-wikipedia.csv",
  "Augsburg" = "DataGermany/Augsburg-prop-daily-wikipedia.csv",
  "Berlin" = "DataGermany/Berlin-prop-daily-wikipedia.csv",
  "Bielefeld" = "DataGermany/Bielefeld-prop-daily-wikipedia.csv",
  "Bochum" = "DataGermany/Bochum-prop-daily-wikipedia.csv",
  "Bonn" = "DataGermany/Bonn-prop-daily-wikipedia.csv",
  "Braunschweig" = "DataGermany/Braunschweig-prop-daily-wikipedia.csv",
  "Bremen" = "DataGermany/Bremen-prop-daily-wikipedia.csv",
  "Chemnitz" = "DataGermany/Chemnitz-prop-daily-wikipedia.csv",
  "Cologne" = "DataGermany/Cologne-prop-daily-wikipedia.csv",
  "Dortmund" = "DataGermany/Dortmund-prop-daily-wikipedia.csv",
  "Dresden" = "DataGermany/Dresden-prop-daily-wikipedia.csv",
  "Duisburg" = "DataGermany/Duisburg-prop-daily-wikipedia.csv",
  "Düsseldorf" = "DataGermany/Düsseldorf-prop-daily-wikipedia.csv",
  "Erfurt" = "DataGermany/Erfurt-prop-daily-wikipedia.csv",
  "Essen" = "DataGermany/Essen-prop-daily-wikipedia.csv",
  "Frankfurt" = "DataGermany/Frankfurt-prop-daily-wikipedia.csv",
  "Freiburg" = "DataGermany/Freiburg im Breisgau-prop-daily-wikipedia.csv",
  "Gelsenkirchen" = "DataGermany/Gelsenkirchen-prop-daily-wikipedia.csv",
  "Halle" = "DataGermany/Halle (Saale)-prop-daily-wikipedia.csv",
  "Hamburg" = "DataGermany/Hamburg-prop-daily-wikipedia.csv",
  "Hanover" = "DataGermany/Hanover-prop-daily-wikipedia.csv",
  "Karlsruhe" = "DataGermany/Karlsruhe-prop-daily-wikipedia.csv",
  "Kassel" = "DataGermany/Kassel-prop-daily-wikipedia.csv",
  "Kiel" = "DataGermany/Kiel-prop-daily-wikipedia.csv",
  "Krefeld" = "DataGermany/Krefeld-prop-daily-wikipedia.csv",
  "Leipzig" = "DataGermany/Leipzig-prop-daily-wikipedia.csv",
  "Lübeck" = "DataGermany/Lübeck-prop-daily-wikipedia.csv",
  "Magdeburg" = "DataGermany/Magdeburg-prop-daily-wikipedia.csv",
  "Mainz" = "DataGermany/Mainz-prop-daily-wikipedia.csv",
  "Mannheim" = "DataGermany/Mannheim-prop-daily-wikipedia.csv",
  "Mönchengladbach" = "DataGermany/Mönchengladbach-prop-daily-wikipedia.csv",
  "Munich" = "DataGermany/Munich-prop-daily-wikipedia.csv",
  "Münster" = "DataGermany/Münster-prop-daily-wikipedia.csv",
  "Nuremberg" = "DataGermany/Nuremberg-prop-daily-wikipedia.csv",
  "Oberhausen" = "DataGermany/Oberhausen-prop-daily-wikipedia.csv",
  "Rostock" = "DataGermany/Rostock-prop-daily-wikipedia.csv",
  "Stuttgart" = "DataGermany/Stuttgart-prop-daily-wikipedia.csv",
  "Wiesbaden" = "DataGermany/Wiesbaden-prop-daily-wikipedia.csv",
  "Wuppertal" = "DataGermany/Wuppertal-prop-daily-wikipedia.csv"
)

WorldCities <- c(
  "Beijing" = "DataWorld/Beijing-prop-daily-wikipedia.csv",
  "Jakarta" = "DataWorld/Jakarta-prop-daily-wikipedia.csv",
  "Kinshasa" = "DataWorld/Kinshasa-prop-daily-wikipedia.csv",
  "Lima" = "DataWorld/Lima-prop-daily-wikipedia.csv",
  "Tokyo" = "DataWorld/Tokyo-prop-daily-wikipedia.csv")


## Download city data
#Poland
for (i in names(PolishCities)) {
  df <- load_city_data(PolishCities[i])
  assign(i, df, envir = .GlobalEnv)   
}
rm(df)

#Germany
for (i in names(GermanCities)) {
  df <- load_city_data(GermanCities[i])
  assign(i, df, envir = .GlobalEnv)   
}
rm(df)

#World
for (i in names(WorldCities)) {
  df <- load_city_data(WorldCities[i])
  assign(i, df, envir = .GlobalEnv)   
}
rm(df)

#### Poland ####

### Idenfiying the structural breaks
set.seed(314)

### Białystok
autoreg <- lm(Ukrainian ~ lagwiki, data = Białystok)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Białystok, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Białystok$date[1]),
                end = as.Date(Białystok$date[1095]))


# Save confidence intervals
cisBialystok = confint(bps)
obs_Białystok <- as.data.table(cisBialystok$confint, keep.rownames = "Breakpoint")
setnames(obs_Białystok, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Białystok <- obs_Białystok[, .(
  Breakpoint,
  CI_2.5_date = Białystok$date[CI_2.5],
  Break_date = Białystok$date[Estimate],
  CI_97.5_date = Białystok$date[CI_97.5],
  City = "Białystok"
)]

as.Date(Białystok$date[bps[["breakpoints"]]]) # Check the corresponding dates of each break point as y%m%d
as.Date(Białystok$date[cisBialystok[["confint"]]]) # Check the corresponding dates for the confidence intervals of each break point as y%m%d

### Plot the results
ggplot(Białystok, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Białystok$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Białystok$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Białystok", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Białystok$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Białystok$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

BiałystokP <- recordPlot()

### Bydgoszcz
autoreg <- lm(Ukrainian ~ lagwiki, data = Bydgoszcz)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Bydgoszcz, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Bydgoszcz$date[1]),
                end = as.Date(Bydgoszcz$date[1095]))

# Save confidence intervals
cisBydgoszcz = confint(bps)
obs_Bydgoszcz <- as.data.table(cisBydgoszcz$confint, keep.rownames = "Breakpoint")
setnames(obs_Bydgoszcz, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Bydgoszcz <- obs_Bydgoszcz[, .(
  Breakpoint,
  CI_2.5_date = Bydgoszcz$date[CI_2.5],
  Break_date = Bydgoszcz$date[Estimate],
  CI_97.5_date = Bydgoszcz$date[CI_97.5],
  City = "Bydgoszcz"
)]

as.Date(Bydgoszcz$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Bydgoszcz$date[cisBydgoszcz[["confint"]]]) # Confidence intervals of the break points as y%m%d


### Plot the results
ggplot(Bydgoszcz, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Bydgoszcz$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Bydgoszcz$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Bydgoszcz", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Bydgoszcz$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Bydgoszcz$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

BydgoszczP <- recordPlot()

### Częstochowa
autoreg <- lm(Ukrainian ~ lagwiki, data = Częstochowa)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Częstochowa, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Częstochowa$date[1]),
                end = as.Date(Częstochowa$date[1095]))

# Save confidence intervals
cisCzęstochowa = confint(bps)
obs_Częstochowa <- as.data.table(cisCzęstochowa$confint, keep.rownames = "Breakpoint")
setnames(obs_Częstochowa, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Częstochowa <- obs_Częstochowa[, .(
  Breakpoint,
  CI_2.5_date = Częstochowa$date[CI_2.5],
  Break_date = Częstochowa$date[Estimate],
  CI_97.5_date = Częstochowa$date[CI_97.5],
  City = "Częstochowa"
)]

as.Date(Częstochowa$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Częstochowa$date[cisCzęstochowa[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Częstochowa, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Częstochowa$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Częstochowa$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Częstochowa", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Częstochowa$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Częstochowa$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

CzęstochowaP <- recordPlot()

### Lublin
autoreg <- lm(Ukrainian ~ lagwiki, data = Lublin)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Lublin, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Lublin$date[1]),
                end = as.Date(Lublin$date[1095]))

# Save confidence intervals
cisLublin = confint(bps)
obs_Lublin <- as.data.table(cisLublin$confint, keep.rownames = "Breakpoint")
setnames(obs_Lublin, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Lublin <- obs_Lublin[, .(
  Breakpoint,
  CI_2.5_date = Lublin$date[CI_2.5],
  Break_date = Lublin$date[Estimate],
  CI_97.5_date = Lublin$date[CI_97.5],
  City = "Lublin"
)]

as.Date(Lublin$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Lublin$date[cisLublin[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Lublin, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Lublin$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Lublin$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Lublin", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Lublin$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Lublin$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

LublinP <- recordPlot()

### Szczecin
autoreg <- lm(Ukrainian ~ lagwiki, data = Szczecin)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Szczecin, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Szczecin$date[1]),
                end = as.Date(Szczecin$date[1095]))

# Save confidence intervals
cisSzczecin = confint(bps)
obs_Szczecin <- as.data.table(cisSzczecin$confint, keep.rownames = "Breakpoint")
setnames(obs_Szczecin, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Szczecin <- obs_Szczecin[, .(
  Breakpoint,
  CI_2.5_date = Szczecin$date[CI_2.5],
  Break_date = Szczecin$date[Estimate],
  CI_97.5_date = Szczecin$date[CI_97.5],
  City = "Szczecin"
)]

as.Date(Szczecin$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Szczecin$date[cisSzczecin[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Szczecin, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Szczecin$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Szczecin$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Szczecin", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Szczecin$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Szczecin$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

SzczecinP <- recordPlot()

### Wrocław
autoreg <- lm(Ukrainian ~ lagwiki, data = Wrocław)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Wrocław, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Wrocław$date[1]),
                end = as.Date(Wrocław$date[1095]))

# Save confidence intervals
cisWrocław = confint(bps)
obs_Wrocław <- as.data.table(cisWrocław$confint, keep.rownames = "Breakpoint")
setnames(obs_Wrocław, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Wrocław <- obs_Wrocław[, .(
  Breakpoint,
  CI_2.5_date = Wrocław$date[CI_2.5],
  Break_date = Wrocław$date[Estimate],
  CI_97.5_date = Wrocław$date[CI_97.5],
  City = "Wrocław"
)]

as.Date(Wrocław$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Wrocław$date[cisWrocław[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Wrocław, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Wrocław$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Wrocław$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Wrocław", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Wrocław$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Wrocław$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

WrocławP <- recordPlot()

### Toruń
autoreg <- lm(Ukrainian ~ lagwiki, data = Toruń)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Toruń, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Toruń$date[1]),
                end = as.Date(Toruń$date[1095]))

# Save confidence intervals
cisToruń = confint(bps)
obs_Toruń <- as.data.table(cisToruń$confint, keep.rownames = "Breakpoint")
setnames(obs_Toruń, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Toruń <- obs_Toruń[, .(
  Breakpoint,
  CI_2.5_date = Toruń$date[CI_2.5],
  Break_date = Toruń$date[Estimate],
  CI_97.5_date = Toruń$date[CI_97.5],
  City = "Toruń"
)]

as.Date(Toruń$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Toruń$date[cisToruń[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Toruń, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Toruń$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Toruń$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Toruń", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Toruń$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Toruń$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

ToruńP <- recordPlot()

### Gdynia
autoreg <- lm(Ukrainian ~ lagwiki, data = Gdynia)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Gdynia, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Gdynia$date[1]),
                end = as.Date(Gdynia$date[1095]))

# Save confidence intervals
cisGdynia = confint(bps)
obs_Gdynia <- as.data.table(cisGdynia$confint, keep.rownames = "Breakpoint")
setnames(obs_Gdynia, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Gdynia <- obs_Gdynia[, .(
  Breakpoint,
  CI_2.5_date = Gdynia$date[CI_2.5],
  Break_date = Gdynia$date[Estimate],
  CI_97.5_date = Gdynia$date[CI_97.5],
  City = "Gdynia"
)]

as.Date(Gdynia$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Gdynia$date[cisGdynia[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Gdynia, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Gdynia$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Gdynia$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Gdynia", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Gdynia$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Gdynia$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

GdyniaP <- recordPlot()

### Łódź
autoreg <- lm(Ukrainian ~ lagwiki, data = Łódź)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Łódź, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Łódź$date[1]),
                end = as.Date(Łódź$date[1095]))

# Save confidence intervals
cisŁódź = confint(bps)
obs_Łódź <- as.data.table(cisŁódź$confint, keep.rownames = "Breakpoint")
setnames(obs_Łódź, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Łódź <- obs_Łódź[, .(
  Breakpoint,
  CI_2.5_date = Łódź$date[CI_2.5],
  Break_date = Łódź$date[Estimate],
  CI_97.5_date = Łódź$date[CI_97.5],
  City = "Łódź"
)]

as.Date(Łódź$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Łódź$date[cisŁódź[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Łódź, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Łódź$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Łódź$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Łódź", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Łódź$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Łódź$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

ŁódźP <- recordPlot()

### Katowice
autoreg <- lm(Ukrainian ~ lagwiki, data = Katowice)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Katowice, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Katowice$date[1]),
                end = as.Date(Katowice$date[1095]))

# Save confidence intervals
cisKatowice = confint(bps)
obs_Katowice <- as.data.table(cisKatowice$confint, keep.rownames = "Breakpoint")
setnames(obs_Katowice, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Katowice <- obs_Katowice[, .(
  Breakpoint,
  CI_2.5_date = Katowice$date[CI_2.5],
  Break_date = Katowice$date[Estimate],
  CI_97.5_date = Katowice$date[CI_97.5],
  City = "Katowice"
)]

as.Date(Katowice$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Katowice$date[cisKatowice[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Katowice, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Katowice$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Katowice$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Katowice", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Katowice$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Katowice$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

KatowiceP <- recordPlot()

### Gliwice
autoreg <- lm(Ukrainian ~ lagwiki, data = Gliwice)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Gliwice, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Gliwice$date[1]),
                end = as.Date(Gliwice$date[1095]))

# Save confidence intervals
cisGliwice = confint(bps)
obs_Gliwice <- as.data.table(cisGliwice$confint, keep.rownames = "Breakpoint")
setnames(obs_Gliwice, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Gliwice <- obs_Gliwice[, .(
  Breakpoint,
  CI_2.5_date = Gliwice$date[CI_2.5],
  Break_date = Gliwice$date[Estimate],
  CI_97.5_date = Gliwice$date[CI_97.5],
  City = "Gliwice"
)]

as.Date(Gliwice$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Gliwice$date[cisGliwice[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Gliwice, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Gliwice$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Gliwice$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Gliwice", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Gliwice$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Gliwice$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

GliwiceP <- recordPlot()

### Warsaw
autoreg <- lm(Ukrainian ~ lagwiki, data = Warsaw)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Warsaw, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Warsaw$date[1]),
                end = as.Date(Warsaw$date[1095]))

# Save confidence intervals
cisWarsaw = confint(bps)
obs_Warsaw <- as.data.table(cisWarsaw$confint, keep.rownames = "Breakpoint")
setnames(obs_Warsaw, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Warsaw <- obs_Warsaw[, .(
  Breakpoint,
  CI_2.5_date = Warsaw$date[CI_2.5],
  Break_date = Warsaw$date[Estimate],
  CI_97.5_date = Warsaw$date[CI_97.5],
  City = "Warsaw"
)]

as.Date(Warsaw$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Warsaw$date[cisWarsaw[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Warsaw, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Warsaw$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Warsaw$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Warsaw", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Warsaw$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Warsaw$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

WarsawP <- recordPlot()

### Poznań
autoreg <- lm(Ukrainian ~ lagwiki, data = Poznań)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Poznań, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Poznań$date[1]),
                end = as.Date(Poznań$date[1095]))

# Save confidence intervals
cisPoznań = confint(bps)
obs_Poznań <- as.data.table(cisPoznań$confint, keep.rownames = "Breakpoint")
setnames(obs_Poznań, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Poznań <- obs_Poznań[, .(
  Breakpoint,
  CI_2.5_date = Poznań$date[CI_2.5],
  Break_date = Poznań$date[Estimate],
  CI_97.5_date = Poznań$date[CI_97.5],
  City = "Poznań"
)]

as.Date(Poznań$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Poznań$date[cisPoznań[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Poznań, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Poznań$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Poznań$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Poznań", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Poznań$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Poznań$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

PoznańP <- recordPlot()

### Gdańsk
autoreg <- lm(Ukrainian ~ lagwiki, data = Gdańsk)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Gdańsk, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Gdańsk$date[1]),
                end = as.Date(Gdańsk$date[1095]))

# Save confidence intervals
cisGdańsk = confint(bps)
obs_Gdańsk <- as.data.table(cisGdańsk$confint, keep.rownames = "Breakpoint")
setnames(obs_Gdańsk, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Gdańsk <- obs_Gdańsk[, .(
  Breakpoint,
  CI_2.5_date = Gdańsk$date[CI_2.5],
  Break_date = Gdańsk$date[Estimate],
  CI_97.5_date = Gdańsk$date[CI_97.5],
  City = "Gdańsk"
)]

as.Date(Gdańsk$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Gdańsk$date[cisGdańsk[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Gdańsk, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Gdańsk$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Gdańsk$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Gdańsk", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Gdańsk$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Gdańsk$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

GdańskP <- recordPlot()

### Kielce
autoreg <- lm(Ukrainian ~ lagwiki, data = Kielce)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Kielce, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Kielce$date[1]),
                end = as.Date(Kielce$date[1095]))

# Save confidence intervals
cisKielce = confint(bps)
obs_Kielce <- as.data.table(cisKielce$confint, keep.rownames = "Breakpoint")
setnames(obs_Kielce, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Kielce <- obs_Kielce[, .(
  Breakpoint,
  CI_2.5_date = Kielce$date[CI_2.5],
  Break_date = Kielce$date[Estimate],
  CI_97.5_date = Kielce$date[CI_97.5],
  City = "Kielce"
)]

as.Date(Kielce$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Kielce$date[cisKielce[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Kielce, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Kielce$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Kielce$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Kielce", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Kielce$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Kielce$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

KielceP <- recordPlot()

### Sosnowiec
autoreg <- lm(Ukrainian ~ lagwiki, data = Sosnowiec)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Sosnowiec, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Sosnowiec$date[1]),
                end = as.Date(Sosnowiec$date[1095]))

# Save confidence intervals
cisSosnowiec = confint(bps)
obs_Sosnowiec <- as.data.table(cisSosnowiec$confint, keep.rownames = "Breakpoint")
setnames(obs_Sosnowiec, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Sosnowiec <- obs_Sosnowiec[, .(
  Breakpoint,
  CI_2.5_date = Sosnowiec$date[CI_2.5],
  Break_date = Sosnowiec$date[Estimate],
  CI_97.5_date = Sosnowiec$date[CI_97.5],
  City = "Sosnowiec"
)]

as.Date(Sosnowiec$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Sosnowiec$date[cisSosnowiec[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Sosnowiec, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Sosnowiec$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Sosnowiec$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Sosnowiec", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Sosnowiec$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Sosnowiec$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

SosnowiecP <- recordPlot()

### Rzeszów
autoreg <- lm(Ukrainian ~ lagwiki, data = Rzeszów)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Rzeszów, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Rzeszów$date[1]),
                end = as.Date(Rzeszów$date[1095]))

# Save confidence intervals
cisRzeszów = confint(bps)
obs_Rzeszów <- as.data.table(cisRzeszów$confint, keep.rownames = "Breakpoint")
setnames(obs_Rzeszów, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Rzeszów <- obs_Rzeszów[, .(
  Breakpoint,
  CI_2.5_date = Rzeszów$date[CI_2.5],
  Break_date = Rzeszów$date[Estimate],
  CI_97.5_date = Rzeszów$date[CI_97.5],
  City = "Rzeszów"
)]

as.Date(Rzeszów$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Rzeszów$date[cisRzeszów[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Rzeszów, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Rzeszów$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Rzeszów$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Rzeszów", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Rzeszów$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Rzeszów$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

RzeszówP <- recordPlot()

### Radom
autoreg <- lm(Ukrainian ~ lagwiki, data = Radom)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Radom, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Radom$date[1]),
                end = as.Date(Radom$date[1095]))

# Save confidence intervals
cisRadom = confint(bps)
obs_Radom <- as.data.table(cisRadom$confint, keep.rownames = "Breakpoint")
setnames(obs_Radom, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Radom <- obs_Radom[, .(
  Breakpoint,
  CI_2.5_date = Radom$date[CI_2.5],
  Break_date = Radom$date[Estimate],
  CI_97.5_date = Radom$date[CI_97.5],
  City = "Radom"
)]

as.Date(Radom$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Radom$date[cisRadom[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Radom, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Radom$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Radom$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Radom", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Radom$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Radom$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

RadomP <- recordPlot()

### Kraków
autoreg <- lm(Ukrainian ~ lagwiki, data = Kraków)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Kraków, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Kraków$date[1]),
                end = as.Date(Kraków$date[1095]))

# Save confidence intervals
cisKraków = confint(bps)
obs_Kraków <- as.data.table(cisKraków$confint, keep.rownames = "Breakpoint")
setnames(obs_Kraków, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Kraków <- obs_Kraków[, .(
  Breakpoint,
  CI_2.5_date = Kraków$date[CI_2.5],
  Break_date = Kraków$date[Estimate],
  CI_97.5_date = Kraków$date[CI_97.5],
  City = "Kraków"
)]

as.Date(Kraków$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Kraków$date[cisKraków[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Kraków, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Kraków$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Kraków$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Kraków", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Kraków$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Kraków$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

KrakówP <- recordPlot()

## 
PolandCI = rbind(date_Białystok, date_Bydgoszcz, date_Częstochowa, date_Gdańsk, date_Gdynia,
                 date_Gliwice, date_Katowice, date_Kielce, date_Kraków, date_Łódź, date_Lublin,
                 date_Poznań, date_Radom, date_Kraków, date_Rzeszów, date_Sosnowiec, date_Toruń,
                 date_Warsaw, date_Wrocław)

PolandPlots = plot_grid(BiałystokP, BydgoszczP, CzęstochowaP, GdańskP, GdyniaP,
                        GliwiceP, KatowiceP, KielceP, KrakówP, ŁódźP, LublinP,
                        PoznańP, RadomP, KrakówP, RzeszówP, SosnowiecP, ToruńP,
                        WarsawP, WrocławP, ncol = 2)

#### Germany ####

### Idenfiying the structural breaks
set.seed(314)

### Aachen
autoreg <- lm(Ukrainian ~ lagwiki, data = Aachen)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Aachen, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Aachen$date[1]),
                end = as.Date(Aachen$date[1095]))

# Save confidence intervals
cisAachen = confint(bps)
obs_Aachen <- as.data.table(cisAachen$confint, keep.rownames = "Breakpoint")
setnames(obs_Aachen, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Aachen <- obs_Aachen[, .(
  Breakpoint,
  CI_2.5_date = Aachen$date[CI_2.5],
  Break_date = Aachen$date[Estimate],
  CI_97.5_date = Aachen$date[CI_97.5],
  City = "Aachen"
)]

as.Date(Aachen$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Aachen$date[cisAachen[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Aachen, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Aachen$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Aachen$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Aachen", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Aachen$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Aachen$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

AachenP <- recordPlot()

### Augsburg
autoreg <- lm(Ukrainian ~ lagwiki, data = Augsburg)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Augsburg, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Augsburg$date[1]),
                end = as.Date(Augsburg$date[1095]))

# Save confidence intervals
cisAugsburg = confint(bps)
obs_Augsburg <- as.data.table(cisAugsburg$confint, keep.rownames = "Breakpoint")
setnames(obs_Augsburg, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Augsburg <- obs_Augsburg[, .(
  Breakpoint,
  CI_2.5_date = Augsburg$date[CI_2.5],
  Break_date = Augsburg$date[Estimate],
  CI_97.5_date = Augsburg$date[CI_97.5],
  City = "Augsburg"
)]

as.Date(Augsburg$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Augsburg$date[cisAugsburg[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Augsburg, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Augsburg$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Augsburg$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Augsburg", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Augsburg$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Augsburg$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

AugsburgP <- recordPlot()

### Berlin
autoreg <- lm(Ukrainian ~ lagwiki, data = Berlin)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Berlin, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Berlin$date[1]),
                end = as.Date(Berlin$date[1095]))

# Save confidence intervals
cisBerlin = confint(bps)
obs_Berlin <- as.data.table(cisBerlin$confint, keep.rownames = "Breakpoint")
setnames(obs_Berlin, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Berlin <- obs_Berlin[, .(
  Breakpoint,
  CI_2.5_date = Berlin$date[CI_2.5],
  Break_date = Berlin$date[Estimate],
  CI_97.5_date = Berlin$date[CI_97.5],
  City = "Berlin"
)]

as.Date(Berlin$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Berlin$date[cisBerlin[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Berlin, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Berlin$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Berlin$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Berlin", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Berlin$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Berlin$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

BerlinP <- recordPlot()

### Bielefeld
autoreg <- lm(Ukrainian ~ lagwiki, data = Bielefeld)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Bielefeld, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Bielefeld$date[1]),
                end = as.Date(Bielefeld$date[1095]))

# Save confidence intervals
cisBielefeld = confint(bps)
obs_Bielefeld <- as.data.table(cisBielefeld$confint, keep.rownames = "Breakpoint")
setnames(obs_Bielefeld, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Bielefeld <- obs_Bielefeld[, .(
  Breakpoint,
  CI_2.5_date = Bielefeld$date[CI_2.5],
  Break_date = Bielefeld$date[Estimate],
  CI_97.5_date = Bielefeld$date[CI_97.5],
  City = "Bielefeld"
)]

as.Date(Bielefeld$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Bielefeld$date[cisBielefeld[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Bielefeld, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Bielefeld$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Bielefeld$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Bielefeld", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Bielefeld$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Bielefeld$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

BielefeldP <- recordPlot()

### Bochum
autoreg <- lm(Ukrainian ~ lagwiki, data = Bochum)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Bochum, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Bochum$date[1]),
                end = as.Date(Bochum$date[1095]))

# Save confidence intervals
cisBochum = confint(bps)
obs_Bochum <- as.data.table(cisBochum$confint, keep.rownames = "Breakpoint")
setnames(obs_Bochum, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Bochum <- obs_Bochum[, .(
  Breakpoint,
  CI_2.5_date = Bochum$date[CI_2.5],
  Break_date = Bochum$date[Estimate],
  CI_97.5_date = Bochum$date[CI_97.5],
  City = "Bochum"
)]

as.Date(Bochum$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Bochum$date[cisBochum[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Bochum, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Bochum$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Bochum$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Bochum", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Bochum$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Bochum$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

BochumP <- recordPlot()

### Bonn
autoreg <- lm(Ukrainian ~ lagwiki, data = Bonn)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Bonn, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Bonn$date[1]),
                end = as.Date(Bonn$date[1095]))

# Save confidence intervals
cisBonn = confint(bps)
obs_Bonn <- as.data.table(cisBonn$confint, keep.rownames = "Breakpoint")
setnames(obs_Bonn, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Bonn <- obs_Bonn[, .(
  Breakpoint,
  CI_2.5_date = Bonn$date[CI_2.5],
  Break_date = Bonn$date[Estimate],
  CI_97.5_date = Bonn$date[CI_97.5],
  City = "Bonn"
)]

as.Date(Bonn$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Bonn$date[cisBonn[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Bonn, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Bonn$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Bonn$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Bonn", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Bonn$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Bonn$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

BonnP <- recordPlot()

### Braunschweig
autoreg <- lm(Ukrainian ~ lagwiki, data = Braunschweig)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Braunschweig, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Braunschweig$date[1]),
                end = as.Date(Braunschweig$date[1095]))

# Save confidence intervals
cisBraunschweig = confint(bps)
obs_Braunschweig <- as.data.table(cisBraunschweig$confint, keep.rownames = "Breakpoint")
setnames(obs_Braunschweig, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Braunschweig <- obs_Braunschweig[, .(
  Breakpoint,
  CI_2.5_date = Braunschweig$date[CI_2.5],
  Break_date = Braunschweig$date[Estimate],
  CI_97.5_date = Braunschweig$date[CI_97.5],
  City = "Braunschweig"
)]

as.Date(Braunschweig$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Braunschweig$date[cisBraunschweig[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Braunschweig, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Braunschweig$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Braunschweig$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Braunschweig", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Braunschweig$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Braunschweig$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

BraunschweigP <- recordPlot()

### Bremen
autoreg <- lm(Ukrainian ~ lagwiki, data = Bremen)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Bremen, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Bremen$date[1]),
                end = as.Date(Bremen$date[1095]))

# Save confidence intervals
cisBremen = confint(bps)
obs_Bremen <- as.data.table(cisBremen$confint, keep.rownames = "Breakpoint")
setnames(obs_Bremen, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Bremen <- obs_Bremen[, .(
  Breakpoint,
  CI_2.5_date = Bremen$date[CI_2.5],
  Break_date = Bremen$date[Estimate],
  CI_97.5_date = Bremen$date[CI_97.5],
  City = "Bremen"
)]

as.Date(Bremen$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Bremen$date[cisBremen[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Bremen, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Bremen$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Bremen$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Bremen", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Bremen$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Bremen$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

BremenP <- recordPlot()

### Chemnitz
autoreg <- lm(Ukrainian ~ lagwiki, data = Chemnitz)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Chemnitz, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Chemnitz$date[1]),
                end = as.Date(Chemnitz$date[1095]))

# Save confidence intervals
cisChemnitz = confint(bps)
obs_Chemnitz <- as.data.table(cisChemnitz$confint, keep.rownames = "Breakpoint")
setnames(obs_Chemnitz, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Chemnitz <- obs_Chemnitz[, .(
  Breakpoint,
  CI_2.5_date = Chemnitz$date[CI_2.5],
  Break_date = Chemnitz$date[Estimate],
  CI_97.5_date = Chemnitz$date[CI_97.5],
  City = "Chemnitz"
)]

as.Date(Chemnitz$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Chemnitz$date[cisChemnitz[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Chemnitz, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Chemnitz$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Chemnitz$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Chemnitz", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Chemnitz$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Chemnitz$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

ChemnitzP <- recordPlot()

### Cologne
autoreg <- lm(Ukrainian ~ lagwiki, data = Cologne)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Cologne, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Cologne$date[1]),
                end = as.Date(Cologne$date[1095]))

# Save confidence intervals
cisCologne = confint(bps)
obs_Cologne <- as.data.table(cisCologne$confint, keep.rownames = "Breakpoint")
setnames(obs_Cologne, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Cologne <- obs_Cologne[, .(
  Breakpoint,
  CI_2.5_date = Cologne$date[CI_2.5],
  Break_date = Cologne$date[Estimate],
  CI_97.5_date = Cologne$date[CI_97.5],
  City = "Cologne"
)]

as.Date(Cologne$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Cologne$date[cisCologne[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Cologne, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Cologne$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Cologne$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Cologne", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Cologne$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Cologne$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

CologneP <- recordPlot()

### Dortmund
autoreg <- lm(Ukrainian ~ lagwiki, data = Dortmund)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Dortmund, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Dortmund$date[1]),
                end = as.Date(Dortmund$date[1095]))

# Save confidence intervals
cisDortmund = confint(bps)
obs_Dortmund <- as.data.table(cisDortmund$confint, keep.rownames = "Breakpoint")
setnames(obs_Dortmund, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Dortmund <- obs_Dortmund[, .(
  Breakpoint,
  CI_2.5_date = Dortmund$date[CI_2.5],
  Break_date = Dortmund$date[Estimate],
  CI_97.5_date = Dortmund$date[CI_97.5],
  City = "Dortmund"
)]

as.Date(Dortmund$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Dortmund$date[cisDortmund[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Dortmund, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Dortmund$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Dortmund$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Dortmund", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Dortmund$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Dortmund$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

DortmundP <- recordPlot()

### Dresden
autoreg <- lm(Ukrainian ~ lagwiki, data = Dresden)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Dresden, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Dresden$date[1]),
                end = as.Date(Dresden$date[1095]))

# Save confidence intervals
cisDresden = confint(bps)
obs_Dresden <- as.data.table(cisDresden$confint, keep.rownames = "Breakpoint")
setnames(obs_Dresden, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Dresden <- obs_Dresden[, .(
  Breakpoint,
  CI_2.5_date = Dresden$date[CI_2.5],
  Break_date = Dresden$date[Estimate],
  CI_97.5_date = Dresden$date[CI_97.5],
  City = "Dresden"
)]

as.Date(Dresden$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Dresden$date[cisDresden[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Dresden, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Dresden$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Dresden$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Dresden", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Dresden$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Dresden$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

DresdenP <- recordPlot()

### Duisburg
autoreg <- lm(Ukrainian ~ lagwiki, data = Duisburg)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Duisburg, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Duisburg$date[1]),
                end = as.Date(Duisburg$date[1095]))

# Save confidence intervals
cisDuisburg = confint(bps)
obs_Duisburg <- as.data.table(cisDuisburg$confint, keep.rownames = "Breakpoint")
setnames(obs_Duisburg, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Duisburg <- obs_Duisburg[, .(
  Breakpoint,
  CI_2.5_date = Duisburg$date[CI_2.5],
  Break_date = Duisburg$date[Estimate],
  CI_97.5_date = Duisburg$date[CI_97.5],
  City = "Duisburg"
)]

as.Date(Duisburg$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Duisburg$date[cisDuisburg[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Duisburg, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Duisburg$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Duisburg$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Duisburg", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Duisburg$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Duisburg$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

DuisburgP <- recordPlot()

### Düsseldorf
autoreg <- lm(Ukrainian ~ lagwiki, data = Düsseldorf)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Düsseldorf, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Düsseldorf$date[1]),
                end = as.Date(Düsseldorf$date[1095]))

# Save confidence intervals
cisDüsseldorf = confint(bps)
obs_Düsseldorf <- as.data.table(cisDüsseldorf$confint, keep.rownames = "Breakpoint")
setnames(obs_Düsseldorf, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Düsseldorf <- obs_Düsseldorf[, .(
  Breakpoint,
  CI_2.5_date = Düsseldorf$date[CI_2.5],
  Break_date = Düsseldorf$date[Estimate],
  CI_97.5_date = Düsseldorf$date[CI_97.5],
  City = "Düsseldorf"
)]

as.Date(Düsseldorf$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Düsseldorf$date[cisDüsseldorf[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Düsseldorf, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Düsseldorf$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Düsseldorf$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Düsseldorf", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Düsseldorf$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Düsseldorf$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

DüsseldorfP <- recordPlot()

### Erfurt
autoreg <- lm(Ukrainian ~ lagwiki, data = Erfurt)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Erfurt, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Erfurt$date[1]),
                end = as.Date(Erfurt$date[1095]))

# Save confidence intervals
cisErfurt = confint(bps)
obs_Erfurt <- as.data.table(cisErfurt$confint, keep.rownames = "Breakpoint")
setnames(obs_Erfurt, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Erfurt <- obs_Erfurt[, .(
  Breakpoint,
  CI_2.5_date = Erfurt$date[CI_2.5],
  Break_date = Erfurt$date[Estimate],
  CI_97.5_date = Erfurt$date[CI_97.5],
  City = "Erfurt"
)]

as.Date(Erfurt$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Erfurt$date[cisErfurt[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Erfurt, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Erfurt$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Erfurt$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Erfurt", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Erfurt$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Erfurt$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

ErfurtP <- recordPlot()

### Essen
autoreg <- lm(Ukrainian ~ lagwiki, data = Essen)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Essen, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Essen$date[1]),
                end = as.Date(Essen$date[1095]))

# Save confidence intervals
cisEssen = confint(bps)
obs_Essen <- as.data.table(cisEssen$confint, keep.rownames = "Breakpoint")
setnames(obs_Essen, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Essen <- obs_Essen[, .(
  Breakpoint,
  CI_2.5_date = Essen$date[CI_2.5],
  Break_date = Essen$date[Estimate],
  CI_97.5_date = Essen$date[CI_97.5],
  City = "Essen"
)]

as.Date(Essen$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Essen$date[cisEssen[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Essen, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Essen$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Essen$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Essen", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Essen$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Essen$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

EssenP <- recordPlot()

### Frankfurt
autoreg <- lm(Ukrainian ~ lagwiki, data = Frankfurt)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Frankfurt, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Frankfurt$date[1]),
                end = as.Date(Frankfurt$date[1095]))

# Save confidence intervals
cisFrankfurt = confint(bps)
obs_Frankfurt <- as.data.table(cisFrankfurt$confint, keep.rownames = "Breakpoint")
setnames(obs_Frankfurt, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Frankfurt <- obs_Frankfurt[, .(
  Breakpoint,
  CI_2.5_date = Frankfurt$date[CI_2.5],
  Break_date = Frankfurt$date[Estimate],
  CI_97.5_date = Frankfurt$date[CI_97.5],
  City = "Frankfurt"
)]

as.Date(Frankfurt$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Frankfurt$date[cisFrankfurt[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results !!!
ggplot(Frankfurt, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Frankfurt$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Frankfurt$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Frankfurt", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Frankfurt$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Frankfurt$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

FrankfurtP <- recordPlot()

### Freiburg
autoreg <- lm(Ukrainian ~ lagwiki, data = Freiburg)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Freiburg, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Freiburg$date[1]),
                end = as.Date(Freiburg$date[1095]))

# Save confidence intervals
cisFreiburg = confint(bps)
obs_Freiburg <- as.data.table(cisFreiburg$confint, keep.rownames = "Breakpoint")
setnames(obs_Freiburg, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Freiburg <- obs_Freiburg[, .(
  Breakpoint,
  CI_2.5_date = Freiburg$date[CI_2.5],
  Break_date = Freiburg$date[Estimate],
  CI_97.5_date = Freiburg$date[CI_97.5],
  City = "Freiburg"
)]

as.Date(Freiburg$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Freiburg$date[cisFreiburg[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Freiburg, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Freiburg$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Freiburg$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Freiburg im Breisgau", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Freiburg$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Freiburg$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

FreiburgP <- recordPlot()

### Gelsenkirchen
autoreg <- lm(Ukrainian ~ lagwiki, data = Gelsenkirchen)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Gelsenkirchen, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Gelsenkirchen$date[1]),
                end = as.Date(Gelsenkirchen$date[1095]))

# Save confidence intervals
cisGelsenkirchen = confint(bps)
obs_Gelsenkirchen <- as.data.table(cisGelsenkirchen$confint, keep.rownames = "Breakpoint")
setnames(obs_Gelsenkirchen, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Gelsenkirchen <- obs_Gelsenkirchen[, .(
  Breakpoint,
  CI_2.5_date = Gelsenkirchen$date[CI_2.5],
  Break_date = Gelsenkirchen$date[Estimate],
  CI_97.5_date = Gelsenkirchen$date[CI_97.5],
  City = "Gelsenkirchen"
)]

as.Date(Gelsenkirchen$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Gelsenkirchen$date[cisGelsenkirchen[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Gelsenkirchen, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Gelsenkirchen$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Gelsenkirchen$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Gelsenkirchen", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Gelsenkirchen$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Gelsenkirchen$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

GelsenkirchenP <- recordPlot()

### Halle
autoreg <- lm(Ukrainian ~ lagwiki, data = Halle)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Halle, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Halle$date[1]),
                end = as.Date(Halle$date[1095]))

# Save confidence intervals
cisHalle = confint(bps)
obs_Halle <- as.data.table(cisHalle$confint, keep.rownames = "Breakpoint")
setnames(obs_Halle, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Halle <- obs_Halle[, .(
  Breakpoint,
  CI_2.5_date = Halle$date[CI_2.5],
  Break_date = Halle$date[Estimate],
  CI_97.5_date = Halle$date[CI_97.5],
  City = "Halle"
)]

as.Date(Halle$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Halle$date[cisHalle[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Halle, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Halle$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Halle$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Halle (Saale)", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Halle$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Halle$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

HalleP <- recordPlot()

### Hamburg
autoreg <- lm(Ukrainian ~ lagwiki, data = Hamburg)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Hamburg, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Hamburg$date[1]),
                end = as.Date(Hamburg$date[1095]))

# Save confidence intervals
cisHamburg = confint(bps)
obs_Hamburg <- as.data.table(cisHamburg$confint, keep.rownames = "Breakpoint")
setnames(obs_Hamburg, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Hamburg <- obs_Hamburg[, .(
  Breakpoint,
  CI_2.5_date = Hamburg$date[CI_2.5],
  Break_date = Hamburg$date[Estimate],
  CI_97.5_date = Hamburg$date[CI_97.5],
  City = "Hamburg"
)]

as.Date(Hamburg$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Hamburg$date[cisHamburg[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Hamburg, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Hamburg$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Hamburg$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Hamburg", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Hamburg$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Hamburg$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

HamburgP <- recordPlot()

### Hanover
autoreg <- lm(Ukrainian ~ lagwiki, data = Hanover)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Hanover, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Hanover$date[1]),
                end = as.Date(Hanover$date[1095]))

# Save confidence intervals
cisHanover = confint(bps)
obs_Hanover <- as.data.table(cisHanover$confint, keep.rownames = "Breakpoint")
setnames(obs_Hanover, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Hanover <- obs_Hanover[, .(
  Breakpoint,
  CI_2.5_date = Hanover$date[CI_2.5],
  Break_date = Hanover$date[Estimate],
  CI_97.5_date = Hanover$date[CI_97.5],
  City = "Hanover"
)]

as.Date(Hanover$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Hanover$date[cisHanover[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Hanover, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Hanover$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Hanover$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Hanover", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Hanover$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Hanover$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

HanoverP <- recordPlot()

### Karlsruhe
autoreg <- lm(Ukrainian ~ lagwiki, data = Karlsruhe)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Karlsruhe, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Karlsruhe$date[1]),
                end = as.Date(Karlsruhe$date[1095]))

# Save confidence intervals
cisKarlsruhe = confint(bps)
obs_Karlsruhe <- as.data.table(cisKarlsruhe$confint, keep.rownames = "Breakpoint")
setnames(obs_Karlsruhe, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Karlsruhe <- obs_Karlsruhe[, .(
  Breakpoint,
  CI_2.5_date = Karlsruhe$date[CI_2.5],
  Break_date = Karlsruhe$date[Estimate],
  CI_97.5_date = Karlsruhe$date[CI_97.5],
  City = "Karlsruhe"
)]

as.Date(Karlsruhe$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Karlsruhe$date[cisKarlsruhe[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Karlsruhe, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Karlsruhe$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Karlsruhe$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Karlsruhe", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Karlsruhe$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Karlsruhe$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

KarlsruheP <- recordPlot()

### Kassel
autoreg <- lm(Ukrainian ~ lagwiki, data = Kassel)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Kassel, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Kassel$date[1]),
                end = as.Date(Kassel$date[1095]))

# Save confidence intervals
cisKassel = confint(bps)
obs_Kassel <- as.data.table(cisKassel$confint, keep.rownames = "Breakpoint")
setnames(obs_Kassel, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Kassel <- obs_Kassel[, .(
  Breakpoint,
  CI_2.5_date = Kassel$date[CI_2.5],
  Break_date = Kassel$date[Estimate],
  CI_97.5_date = Kassel$date[CI_97.5],
  City = "Kassel"
)]

as.Date(Kassel$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Kassel$date[cisKassel[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Kassel, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Kassel$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Kassel$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Kassel", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Kassel$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Kassel$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

KasselP <- recordPlot()

### Kiel
autoreg <- lm(Ukrainian ~ lagwiki, data = Kiel)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Kiel, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Kiel$date[1]),
                end = as.Date(Kiel$date[1095]))

# Save confidence intervals
cisKiel = confint(bps)
obs_Kiel <- as.data.table(cisKiel$confint, keep.rownames = "Breakpoint")
setnames(obs_Kiel, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Kiel <- obs_Kiel[, .(
  Breakpoint,
  CI_2.5_date = Kiel$date[CI_2.5],
  Break_date = Kiel$date[Estimate],
  CI_97.5_date = Kiel$date[CI_97.5],
  City = "Kiel"
)]

as.Date(Kiel$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Kiel$date[cisKiel[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Kiel, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Kiel$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Kiel$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Kiel", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Kiel$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Kiel$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

KielP <- recordPlot()

### Krefeld
autoreg <- lm(Ukrainian ~ lagwiki, data = Krefeld)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Krefeld, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Krefeld$date[1]),
                end = as.Date(Krefeld$date[1095]))

# Save confidence intervals
cisKrefeld = confint(bps)
obs_Krefeld <- as.data.table(cisKrefeld$confint, keep.rownames = "Breakpoint")
setnames(obs_Krefeld, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Krefeld <- obs_Krefeld[, .(
  Breakpoint,
  CI_2.5_date = Krefeld$date[CI_2.5],
  Break_date = Krefeld$date[Estimate],
  CI_97.5_date = Krefeld$date[CI_97.5],
  City = "Krefeld"
)]

as.Date(Krefeld$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Krefeld$date[cisKrefeld[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Krefeld, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Krefeld$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Krefeld$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Krefeld", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Krefeld$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Krefeld$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

KrefeldP <- recordPlot()

### Leipzig
autoreg <- lm(Ukrainian ~ lagwiki, data = Leipzig)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Leipzig, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Leipzig$date[1]),
                end = as.Date(Leipzig$date[1095]))

# Save confidence intervals
cisLeipzig = confint(bps)
obs_Leipzig <- as.data.table(cisLeipzig$confint, keep.rownames = "Breakpoint")
setnames(obs_Leipzig, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Leipzig <- obs_Leipzig[, .(
  Breakpoint,
  CI_2.5_date = Leipzig$date[CI_2.5],
  Break_date = Leipzig$date[Estimate],
  CI_97.5_date = Leipzig$date[CI_97.5],
  City = "Leipzig"
)]

as.Date(Leipzig$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Leipzig$date[cisLeipzig[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Leipzig, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Leipzig$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Leipzig$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Leipzig", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Leipzig$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Leipzig$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

LeipzigP <- recordPlot()

### Lübeck
autoreg <- lm(Ukrainian ~ lagwiki, data = Lübeck)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Lübeck, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Lübeck$date[1]),
                end = as.Date(Lübeck$date[1095]))

# Save confidence intervals
cisLübeck = confint(bps)
obs_Lübeck <- as.data.table(cisLübeck$confint, keep.rownames = "Breakpoint")
setnames(obs_Lübeck, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Lübeck <- obs_Lübeck[, .(
  Breakpoint,
  CI_2.5_date = Lübeck$date[CI_2.5],
  Break_date = Lübeck$date[Estimate],
  CI_97.5_date = Lübeck$date[CI_97.5],
  City = "Lübeck"
)]

as.Date(Lübeck$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Lübeck$date[cisLübeck[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Lübeck, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Lübeck$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Lübeck$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Lübeck", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Lübeck$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Lübeck$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

LübeckP <- recordPlot()

### Magdeburg
autoreg <- lm(Ukrainian ~ lagwiki, data = Magdeburg)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Magdeburg, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Magdeburg$date[1]),
                end = as.Date(Magdeburg$date[1095]))

# Save confidence intervals
cisMagdeburg = confint(bps)
obs_Magdeburg <- as.data.table(cisMagdeburg$confint, keep.rownames = "Breakpoint")
setnames(obs_Magdeburg, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Magdeburg <- obs_Magdeburg[, .(
  Breakpoint,
  CI_2.5_date = Magdeburg$date[CI_2.5],
  Break_date = Magdeburg$date[Estimate],
  CI_97.5_date = Magdeburg$date[CI_97.5],
  City = "Magdeburg"
)]

as.Date(Magdeburg$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Magdeburg$date[cisMagdeburg[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Magdeburg, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Magdeburg$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Magdeburg$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Magdeburg", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Magdeburg$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Magdeburg$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

MagdeburgP <- recordPlot()

### Mainz
autoreg <- lm(Ukrainian ~ lagwiki, data = Mainz)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Mainz, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Mainz$date[1]),
                end = as.Date(Mainz$date[1095]))

# Save confidence intervals
cisMainz = confint(bps)
obs_Mainz <- as.data.table(cisMainz$confint, keep.rownames = "Breakpoint")
setnames(obs_Mainz, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Mainz <- obs_Mainz[, .(
  Breakpoint,
  CI_2.5_date = Mainz$date[CI_2.5],
  Break_date = Mainz$date[Estimate],
  CI_97.5_date = Mainz$date[CI_97.5],
  City = "Mainz"
)]

as.Date(Mainz$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Mainz$date[cisMainz[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Mainz, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Mainz$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Mainz$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Mainz", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Mainz$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Mainz$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

MainzP <- recordPlot()

### Mannheim
autoreg <- lm(Ukrainian ~ lagwiki, data = Mannheim)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Mannheim, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Mannheim$date[1]),
                end = as.Date(Mannheim$date[1095]))

# Save confidence intervals
cisMannheim = confint(bps)
obs_Mannheim <- as.data.table(cisMannheim$confint, keep.rownames = "Breakpoint")
setnames(obs_Mannheim, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Mannheim <- obs_Mannheim[, .(
  Breakpoint,
  CI_2.5_date = Mannheim$date[CI_2.5],
  Break_date = Mannheim$date[Estimate],
  CI_97.5_date = Mannheim$date[CI_97.5],
  City = "Mannheim"
)]

as.Date(Mannheim$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Mannheim$date[cisMannheim[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Mannheim, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Mannheim$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Mannheim$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Mannheim", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Mannheim$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Mannheim$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

MannheimP <- recordPlot()

### Mönchengladbach
autoreg <- lm(Ukrainian ~ lagwiki, data = Mönchengladbach)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Mönchengladbach, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Mönchengladbach$date[1]),
                end = as.Date(Mönchengladbach$date[1095]))

# Save confidence intervals
cisMönchengladbach = confint(bps)
obs_Mönchengladbach <- as.data.table(cisMönchengladbach$confint, keep.rownames = "Breakpoint")
setnames(obs_Mönchengladbach, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Mönchengladbach <- obs_Mönchengladbach[, .(
  Breakpoint,
  CI_2.5_date = Mönchengladbach$date[CI_2.5],
  Break_date = Mönchengladbach$date[Estimate],
  CI_97.5_date = Mönchengladbach$date[CI_97.5],
  City = "Mönchengladbach"
)]

as.Date(Mönchengladbach$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Mönchengladbach$date[cisMönchengladbach[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Mönchengladbach, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Mönchengladbach$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Mönchengladbach$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Mönchengladbach", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Mönchengladbach$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Mönchengladbach$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

MönchengladbachP <- recordPlot()

### Munich
autoreg <- lm(Ukrainian ~ lagwiki, data = Munich)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Munich, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Munich$date[1]),
                end = as.Date(Munich$date[1095]))

# Save confidence intervals
cisMunich = confint(bps)
obs_Munich <- as.data.table(cisMunich$confint, keep.rownames = "Breakpoint")
setnames(obs_Munich, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Munich <- obs_Munich[, .(
  Breakpoint,
  CI_2.5_date = Munich$date[CI_2.5],
  Break_date = Munich$date[Estimate],
  CI_97.5_date = Munich$date[CI_97.5],
  City = "Munich"
)]

as.Date(Munich$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Munich$date[cisMunich[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Munich, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Munich$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Munich$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Munich", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Munich$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Munich$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

MunichP <- recordPlot()

### Münster
autoreg <- lm(Ukrainian ~ lagwiki, data = Münster)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Münster, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Münster$date[1]),
                end = as.Date(Münster$date[1095]))

# Save confidence intervals
cisMünster = confint(bps)
obs_Münster <- as.data.table(cisMünster$confint, keep.rownames = "Breakpoint")
setnames(obs_Münster, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Münster <- obs_Münster[, .(
  Breakpoint,
  CI_2.5_date = Münster$date[CI_2.5],
  Break_date = Münster$date[Estimate],
  CI_97.5_date = Münster$date[CI_97.5],
  City = "Münster"
)]

as.Date(Münster$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Münster$date[cisMünster[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Münster, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Münster$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Münster$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Münster", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Münster$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Münster$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

MünsterP <- recordPlot()

### Nuremberg
autoreg <- lm(Ukrainian ~ lagwiki, data = Nuremberg)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Nuremberg, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Nuremberg$date[1]),
                end = as.Date(Nuremberg$date[1095]))

# Save confidence intervals
cisNuremberg = confint(bps)
obs_Nuremberg <- as.data.table(cisNuremberg$confint, keep.rownames = "Breakpoint")
setnames(obs_Nuremberg, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Nuremberg <- obs_Nuremberg[, .(
  Breakpoint,
  CI_2.5_date = Nuremberg$date[CI_2.5],
  Break_date = Nuremberg$date[Estimate],
  CI_97.5_date = Nuremberg$date[CI_97.5],
  City = "Nuremberg"
)]

as.Date(Nuremberg$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Nuremberg$date[cisNuremberg[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Nuremberg, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Nuremberg$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Nuremberg$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Nuremberg", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Nuremberg$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Nuremberg$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

NurembergP <- recordPlot()

### Oberhausen
autoreg <- lm(Ukrainian ~ lagwiki, data = Oberhausen)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Oberhausen, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Oberhausen$date[1]),
                end = as.Date(Oberhausen$date[1095]))

# Save confidence intervals
cisOberhausen = confint(bps)
obs_Oberhausen <- as.data.table(cisOberhausen$confint, keep.rownames = "Breakpoint")
setnames(obs_Oberhausen, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Oberhausen <- obs_Oberhausen[, .(
  Breakpoint,
  CI_2.5_date = Oberhausen$date[CI_2.5],
  Break_date = Oberhausen$date[Estimate],
  CI_97.5_date = Oberhausen$date[CI_97.5],
  City = "Oberhausen"
)]

as.Date(Oberhausen$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Oberhausen$date[cisOberhausen[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Oberhausen, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Oberhausen$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Oberhausen$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Oberhausen", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Oberhausen$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Oberhausen$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

OberhausenP <- recordPlot()

### Rostock
autoreg <- lm(Ukrainian ~ lagwiki, data = Rostock)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Rostock, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Rostock$date[1]),
                end = as.Date(Rostock$date[1095]))

# Save confidence intervals
cisRostock = confint(bps)
obs_Rostock <- as.data.table(cisRostock$confint, keep.rownames = "Breakpoint")
setnames(obs_Rostock, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Rostock <- obs_Rostock[, .(
  Breakpoint,
  CI_2.5_date = Rostock$date[CI_2.5],
  Break_date = Rostock$date[Estimate],
  CI_97.5_date = Rostock$date[CI_97.5],
  City = "Rostock"
)]

as.Date(Rostock$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Rostock$date[cisRostock[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Rostock, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Rostock$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Rostock$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Rostock", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Rostock$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Rostock$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

RostockP <- recordPlot()

### Stuttgart
autoreg <- lm(Ukrainian ~ lagwiki, data = Stuttgart)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Stuttgart, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Stuttgart$date[1]),
                end = as.Date(Stuttgart$date[1095]))

# Save confidence intervals
cisStuttgart = confint(bps)
obs_Stuttgart <- as.data.table(cisStuttgart$confint, keep.rownames = "Breakpoint")
setnames(obs_Stuttgart, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Stuttgart <- obs_Stuttgart[, .(
  Breakpoint,
  CI_2.5_date = Stuttgart$date[CI_2.5],
  Break_date = Stuttgart$date[Estimate],
  CI_97.5_date = Stuttgart$date[CI_97.5],
  City = "Stuttgart"
)]

as.Date(Stuttgart$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Stuttgart$date[cisStuttgart[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Stuttgart, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Stuttgart$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Stuttgart$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Stuttgart", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Stuttgart$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Stuttgart$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

StuttgartP <- recordPlot()

### Wiesbaden
autoreg <- lm(Ukrainian ~ lagwiki, data = Wiesbaden)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Wiesbaden, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Wiesbaden$date[1]),
                end = as.Date(Wiesbaden$date[1095]))

# Save confidence intervals
cisWiesbaden = confint(bps)
obs_Wiesbaden <- as.data.table(cisWiesbaden$confint, keep.rownames = "Breakpoint")
setnames(obs_Wiesbaden, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Wiesbaden <- obs_Wiesbaden[, .(
  Breakpoint,
  CI_2.5_date = Wiesbaden$date[CI_2.5],
  Break_date = Wiesbaden$date[Estimate],
  CI_97.5_date = Wiesbaden$date[CI_97.5],
  City = "Wiesbaden"
)]

as.Date(Wiesbaden$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Wiesbaden$date[cisWiesbaden[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Wiesbaden, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Wiesbaden$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Wiesbaden$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Wiesbaden", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Wiesbaden$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Wiesbaden$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

WiesbadenP <- recordPlot()

### Wuppertal
autoreg <- lm(Ukrainian ~ lagwiki, data = Wuppertal)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Wuppertal, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Wuppertal$date[1]),
                end = as.Date(Wuppertal$date[1095]))

# Save confidence intervals
cisWuppertal = confint(bps)
obs_Wuppertal <- as.data.table(cisWuppertal$confint, keep.rownames = "Breakpoint")
setnames(obs_Wuppertal, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Wuppertal <- obs_Wuppertal[, .(
  Breakpoint,
  CI_2.5_date = Wuppertal$date[CI_2.5],
  Break_date = Wuppertal$date[Estimate],
  CI_97.5_date = Wuppertal$date[CI_97.5],
  City = "Wuppertal"
)]

as.Date(Wuppertal$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Wuppertal$date[cisWuppertal[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Wuppertal, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Wuppertal$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Wuppertal$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Wuppertal", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Wuppertal$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Wuppertal$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

WuppertalP <- recordPlot()


GermanyCI = rbind(
  date_Aachen, date_Augsburg, date_Berlin, date_Bielefeld, date_Bochum, date_Bonn,
  date_Braunschweig, date_Bremen, date_Chemnitz, date_Cologne, date_Dortmund,
  date_Dresden, date_Duisburg, date_Düsseldorf, date_Erfurt, date_Essen, date_Frankfurt,
  date_Freiburg, date_Gelsenkirchen, date_Halle, date_Hamburg, date_Hanover, date_Karlsruhe, 
  date_Kassel, date_Kiel, date_Krefeld, date_Leipzig, date_Lübeck, date_Magdeburg, date_Mainz,
  date_Mannheim, date_Mönchengladbach, date_Munich, date_Münster, date_Nuremberg,
  date_Oberhausen, date_Rostock, date_Stuttgart, date_Wiesbaden, date_Wuppertal)

#### World ####

### Beijing
autoreg <- lm(Ukrainian ~ lagwiki, data = Beijing)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Beijing, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Beijing$date[1]),
                end = as.Date(Beijing$date[1095]))

# Save confidence intervals
cisBeijing = confint(bps)
obs_Beijing <- as.data.table(cisBeijing$confint, keep.rownames = "Breakpoint")
setnames(obs_Beijing, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Beijing <- obs_Beijing[, .(
  Breakpoint,
  CI_2.5_date = Beijing$date[CI_2.5],
  Break_date = Beijing$date[Estimate],
  CI_97.5_date = Beijing$date[CI_97.5],
  City = "Beijing"
)]

as.Date(Beijing$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Beijing$date[cisBeijing[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Beijing, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Beijing$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Beijing$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Beijing", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Beijing$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Beijing$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

BeijingP <- recordPlot()


### Jakarta
autoreg <- lm(Ukrainian ~ lagwiki, data = Jakarta)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Jakarta, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Jakarta$date[1]),
                end = as.Date(Jakarta$date[1095]))

# Save confidence intervals
cisJakarta = confint(bps)
obs_Jakarta <- as.data.table(cisJakarta$confint, keep.rownames = "Breakpoint")
setnames(obs_Jakarta, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Jakarta <- obs_Jakarta[, .(
  Breakpoint,
  CI_2.5_date = Jakarta$date[CI_2.5],
  Break_date = Jakarta$date[Estimate],
  CI_97.5_date = Jakarta$date[CI_97.5],
  City = "Jakarta"
)]

as.Date(Jakarta$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Jakarta$date[cisJakarta[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Jakarta, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Jakarta$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Jakarta$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Jakarta", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Jakarta$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Jakarta$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

JakartaP <- recordPlot()


### Kinshasa
autoreg <- lm(Ukrainian ~ lagwiki, data = Kinshasa)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Kinshasa, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Kinshasa$date[1]),
                end = as.Date(Kinshasa$date[1095]))

# Save confidence intervals
cisKinshasa = confint(bps)
obs_Kinshasa <- as.data.table(cisKinshasa$confint, keep.rownames = "Breakpoint")
setnames(obs_Kinshasa, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Kinshasa <- obs_Kinshasa[, .(
  Breakpoint,
  CI_2.5_date = Kinshasa$date[CI_2.5],
  Break_date = Kinshasa$date[Estimate],
  CI_97.5_date = Kinshasa$date[CI_97.5],
  City = "Kinshasa"
)]

as.Date(Kinshasa$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Kinshasa$date[cisKinshasa[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Kinshasa, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Kinshasa$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Kinshasa$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Kinshasa", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
    breaks = as.Date(Kinshasa$date[bps[["breakpoints"]]]),
    labels = as.character(as.Date(Kinshasa$date[bps[["breakpoints"]]])),  
    name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

KinshasaP <- recordPlot()

### Lima
autoreg <- lm(Ukrainian ~ lagwiki, data = Lima)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Lima, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Lima$date[1]),
                end = as.Date(Lima$date[1095]))

# Save confidence intervals
cisLima = confint(bps)
obs_Lima <- as.data.table(cisLima$confint, keep.rownames = "Breakpoint")
setnames(obs_Lima, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Lima <- obs_Lima[, .(
  Breakpoint,
  CI_2.5_date = Lima$date[CI_2.5],
  Break_date = Lima$date[Estimate],
  CI_97.5_date = Lima$date[CI_97.5],
  City = "Lima"
)]

as.Date(Lima$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
#as.Date(Lima$date[cisLima[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Lima, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Lima$date[549]), color = "red") +
  #geom_vline(xintercept = as.numeric(Lima$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Lima", y = "Wikipedia views in Ukrainian", x = NULL) +
  #scale_x_date(sec.axis = dup_axis(
  #  breaks = as.Date(Lima$date[bps[["breakpoints"]]]),
  #  labels = as.character(as.Date(Lima$date[bps[["breakpoints"]]])),  
  #  name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
        axis.title.x.top = element_blank())

LimaP <- recordPlot()

### Tokyo
autoreg <- lm(Ukrainian ~ lagwiki, data = Tokyo)
bps <- breakpoints(Ukrainian ~ lagwiki, data = Tokyo, h=0.15)
fitted.ts <- ts(fitted(autoreg), frequency = 1, 
                start = as.Date(Tokyo$date[1]),
                end = as.Date(Tokyo$date[1095]))

# Save confidence intervals
cisTokyo = confint(bps)
obs_Tokyo <- as.data.table(cisTokyo$confint, keep.rownames = "Breakpoint")
setnames(obs_Tokyo, c("Breakpoint", "CI_2.5", "Estimate", "CI_97.5"))

# Map row indices back to dates
date_Tokyo <- obs_Tokyo[, .(
  Breakpoint,
  CI_2.5_date = Tokyo$date[CI_2.5],
  Break_date = Tokyo$date[Estimate],
  CI_97.5_date = Tokyo$date[CI_97.5],
  City = "Tokyo"
)]

as.Date(Tokyo$date[bps[["breakpoints"]]]) # Dates of the break points as y%m%d
as.Date(Tokyo$date[cisTokyo[["confint"]]]) # Confidence intervals of the break points as y%m%d

### Plot the results
ggplot(Tokyo, aes(x = date, y = Ukrainian)) +
  geom_line() +
  geom_line(aes(y = fitted.ts), color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(Tokyo$date[549]), color = "red") +
  geom_vline(xintercept = as.numeric(Tokyo$date[bps[["breakpoints"]]]), linetype = "dashed") +
  labs(title = "Tokyo", y = "Wikipedia views in Ukrainian", x = NULL) +
  scale_x_date(sec.axis = dup_axis(
      breaks = as.Date(Tokyo$date[bps[["breakpoints"]]]),
      labels = as.character(as.Date(Tokyo$date[bps[["breakpoints"]]])),  
      name = NULL)) +
  theme_light() +
  theme(axis.text.x.top = element_text(size = 8, color = "black"), panel.grid = element_blank(), 
    axis.title.x.top = element_blank())

TokyoP <- recordPlot()

WorldCI = rbind(date_Beijing, date_Jakarta, date_Kinshasa, date_Tokyo)

write.csv(WorldCI, "WorldCI.csv")
write.csv(PolandCI, "PolandCI.csv")
write.csv(GermanyCI, "GermanyCI.csv")

###
library(knitr)
library(kableExtra)


colnames(WorldCI)
setcolorder(WorldCI, c("Breakpoint", "City", "Break_date", "CI_2.5_date", "CI_97.5_date" ))
kable(WorldCI[,2:5], "latex")

setcolorder(PolandCI, c("Breakpoint", "City", "Break_date", "CI_2.5_date", "CI_97.5_date" ))
kable(PolandCI[,2:5], "latex")

setcolorder(GermanyCI, c("Breakpoint", "City", "Break_date", "CI_2.5_date", "CI_97.5_date" ))
kable(GermanyCI[,2:5], "latex")




