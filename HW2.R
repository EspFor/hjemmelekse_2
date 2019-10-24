library(tidyverse)
library(lubridate)
library(purrr)
library(readxl)
library(dplyr)
library(tidyr)
library(ranger)
library(ggplot2)

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

x <- c(1, 2, 3, 10)
y <- c(4, 5, 6, 23)


#Oppgave 2.1
sum_two_num <- function(x = NA, y = NA) {
  
  if (is.na(x) | is.na(y)) {
    stop("Du mangler input-verdi", call. = FALSE)
  }
  
  x+y
}

sum_two_num(x,y)

#Oppgave 2.2
mul_two_num <- function(x = NA, y = NA) {
  
  if (is.na(x) | is.na(y)) {
    stop("Du mangler input-verdi", call. = FALSE)
  }
  
  x*y
}

mul_two_num(x,y)

#Oppgave 2.3
kvadrer_x <- function(x) {
  x^2
}
  
kvadrer_x() #Feilmelding. Mangler x-input.

#Fixer ved å sette default
kvadrer_x <- function(x=NA) {
  x^2
}

kvadrer_x()

#Oppgave 2.4
?cor

#Det er den første verdien i vektoren, "pearson", som er default-verdien til funksjonen. 
#Resten av vektoren viser de andre alternative verdiene for "method".

#Oppgave 2.5
contains_2 <- function(x = NA) {
  
  if (is.na(x)) {
    stop("Du mangler input-verdi", call. = FALSE)
  }
  
  2 %in% x
}

contains_2(x)
contains_2(y)

#Oppgave 2.6

#Det finnes allerede funksjoner med disse navnene i R-basic.
#Ved å bruke disse navnene på nye funksjoner vil disse overskrive standardfunksjonene.
#Dette er spesielt dårlig praksis om man jobber på prosjekt med flere personer som kanskje prøver å bruke standardfunksjonene.

#Oppgave 2.7
partall <- function(x = NA) {
  
  if (is.na(x)) {
    stop("Du mangler input-verdi", call. = FALSE)
    }
  
  if((x %% 2) == 0){
    return(TRUE)
    } 
  else{
    return(FALSE)
    }
}

partall(3)
partall(4)

#Bonus: partall(x) = FALSE impliserer at tallet er oddetall.

#Oppgave 2.8

#Ved en if setter vi verdien/gjøre en handling hvis if-condition er sann, mens ved ifelse kan vi også indikere verdi/handling ved false.
#Bruker ifelse når det har en verdi å returnere en verdi/gjøre en handling også ved if=False

#Oppgave 2.9
#Funksjonen vil ikke fungere (som ønsket) fordi R ikke forstår at "variabel" er en variabel, men tror det er et objekt.
#Kan fikserer ved å sette "variabel" i {{}}, da skjønner R at det er en variabel i tidyverse-funksjoner.

lag_histogram <-  function(.data, variabel) {
  
  .data %>% 
    ggplot(aes(x = {{variabel}})) + 
    geom_histogram()
  
}

#Oppgave 3.0

god_dag <-  function(time = lubridate::now()) {

  hour_int <- as.integer(strftime(time, format="%H"))
  
  case_when(
    (hour_int >= 6) & (hour_int < 9) ~ "God morgen",
    (hour_int >= 9) & (hour_int < 12) ~ "God formiddag",
    (hour_int >= 12) & (hour_int < 19) ~ "God ettermiddag",
    (hour_int >= 19) & (hour_int < 24) ~ "God kveld",
    TRUE ~ "God natt"
  )
  
}

ettermiddag <- as_datetime("20019-09-18 16:15:01")
god_dag(ettermiddag)

#Oppgave 4.0
celsius_to_fahrenheit <-  function(celsius = NA) {
  
  if (is.na(x)) {
    stop("Du mangler input-verdi", call. = FALSE)
  }
  
  round(celsius*(9/5)+32,0) 
}

fahrenheit_to_celsius <-  function(fahrenheit = NA) {

  if (is.na(x)) {
    stop("Du mangler input-verdi", call. = FALSE)
  }
  
  round((fahrenheit-32)*(5/9),0)
}

fahrenheit_to_celsius(32)
fahrenheit_to_celsius(64)
celsius_to_fahrenheit(0)
celsius_to_fahrenheit(100)
fahrenheit_to_celsius(celsius_to_fahrenheit(23))

#Oppgave 5.0

#Denne får jeg ikke helt til. Usikker på hvordan jeg best henviser til kol.
dingdong <-  function(x){
  
.df = as.data.frame(x)

.df %>%
  case_when( 
    {{x}} %% 3 == 0 & {{x}} %% 5 == 0 ~ "Dingdong",
    {{x}} %% 3 == 0 ~ "Ding",
    {{x}} %% 5 == 0 ~ "Dong",
    TRUE ~ as.character({{x}})
  )


return(.df)
}

dingdong(c(1:100))

#Oppgave 6.0
print_all <- function(.data) {
  .data %>% 
    print(nrow(.data))
}

iris %>% 
  as_tibble %>% 
  head(25) %>%  
  print_all()

#Oppgave 7.0
pw_generator <- function(n = 10) {
  pw <- sample(letters, n, TRUE)
  paste(pw,collapse = "")
}

pw_generator(6)



paste(sample(c(0:9, letters, LETTERS),n, replace=TRUE),
      collapse="")

pw_generator <- function(n = 10) {
  pw <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(pw, sprintf("%04d", sample(9, n, TRUE)), sample(LETTERS, n, TRUE))
  paste0(pw, sprintf("%04d", sample(9, n, TRUE)), sample(LETTERS, n, TRUE))
}

pw_generator(10)

#Oppgave 8.1
antall_na <- function(.data) {
  sum(is.na(.data))
}

antall_na(airquality)

prosent_na <- function(.data) {
  scales::percent(sum(is.na(.data))/sum(!is.na(.data)))
}

prosent_na(airquality)

#Oppgave 8.2
map_df(airquality, .f = antall_na)
map_df(airquality, .f = prosent_na)

#Oppgave 9
file_paths <- fs::dir_ls(path = "./Karakterer_per_fylke", regexp = "\\.csv$")

df <- map_df(file_paths, .f = read_csv(col_names = TRUE))

colnames(df)[1] = "Verdi"

df2 <- df  %>% 
  separate(Verdi,sep=";", into = c("Fylke","Fag", "Kjønn","Foreldrenes utdanningsnivå","Variabel", "Verdi"))  %>%
  na.omit() %>%
  mutate_all(~ifelse(. %in% c("N/A", "null", ""), NA, .)) %>% 
  na.omit() %>%
  separate(Variabel,sep="_", into=c("Variabel","year")) %>%
  filter(grepl("Gjennomsnittlig standpunktkarakter",Variabel))

finn_beste_fylke <- function(.data, year = 2019) {
  .data %>% 
    as_tibble %>% 
    filter({{year}} == year) %>%
    select(Fylke,Verdi) %>%
    group_by(Fylke) %>%
    summarise(Gjennomsnitlig_karakter = mean(as.integer(Verdi))) %>%
    arrange(desc(Gjennomsnitlig_karakter)) %>%
    slice(1:10)
}

df2 %>% 
  finn_beste_fylke(2016) #For lavt snitt fordi det leses inn karakter uten desimal, hvordan løser jeg dette enkelt?

#Oppgave 10
lag_plot <- function(.data, col1, col2, fill = "red", theme = theme_classic()) {
  .data %>% 
    ggplot(aes(x = {{col1}}, y = {{col2}}, color = {{fill}})) + 
    geom_point() + 
    {{theme}}
}

iris %>% 
  lag_plot(Sepal.Length, Sepal.Width, Species, theme_bw())


#Oppgave 11
mpg_to_kpl <- function(mpg = 0) {
  mpg*0.425144
}

lag_plot_mpg <- function(.data = mtcars,mpg = mpg, col2 = cyl, fill = "red", theme = theme_classic()) {
  .data %>% 
    ggplot(aes(x = mpg_to_kpl({{mpg}}), y = {{col2}}, color = {{fill}})) + 
    geom_point() + 
    {{theme}}
}

mtcars %>% 
  lag_plot_mpg(col2 = disp)

#Oppgave 12
if (!require(quantmod)) {
  install.packages("quantmod")
}

if (!require(reshape)) {
  install.packages("reshape")
}

# Last ned aksjedata
quantmod::getSymbols(c("GOOGL", "AAPL", "FB"), from = "2019-01-01")

# Lag liste av dataframes. Gjør radnavn til kolonne med navn "Dato"
list_of_df <- map(.x = list(FB = FB, GOOGL = GOOGL, AAPL = AAPL), 
                  .f = ~(rownames_to_column(.data = as.data.frame(.), 
                                            var = "Dato")))

# Join alle dfs på Dato
df_stocks <- reshape::merge_recurse(list_of_df)

# Plot
df_stocks %>% 
  select(Dato, contains("Close")) %>%
  arrange(Dato) %>% #Sorterer på dato slik at koden er mer robust i neste ledd
  mutate(AAPL.Close = (AAPL.Close/AAPL.Close[1])*100,
         FB.Close  = (FB.Close/FB.Close[1])*100,
         GOOGL.Close  = (GOOGL.Close/GOOGL.Close[1])*100) %>%
  pivot_longer(-Dato, names_to = "Stock", values_to = "Close_rescaled") %>% 
  ggplot(aes(x = as.Date(Dato), y = Close_rescaled, col = Stock, group = Stock)) + 
  geom_line() + 
  theme_bw()

rescale <- function(col1) {
    col1 = (col1/col1[1])*100
}

df_stocks %>% 
  select(Dato, contains("Close")) %>%
  arrange(Dato) %>% #Sorterer på dato slik at koden er mer robust i neste ledd
  mutate(AAPL.Close = rescale(AAPL.Close),
         FB.Close  = rescale(FB.Close),
         GOOGL.Close  = rescale(GOOGL.Close)) %>%
  pivot_longer(-Dato, names_to = "Stock", values_to = "Close_rescaled") %>% 
  ggplot(aes(x = as.Date(Dato), y = Close_rescaled, col = Stock, group = Stock)) + 
  geom_line() + 
  theme_bw()


#Oppgave 13
prefix_exists <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

remove_last_chr <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

change_length_vector <- function(x, y) {
  rep(y, length.out = length(x))
}

laveste_verdier_av_siste_vektor_og_storste_av_de_to_forste_vektorene <- function(x, y, z) {
  pmin(pmax(x, y), z)
}

#Oppgave 17.1
mtcars %>%
  map(~mean(., na.rm = TRUE))

#Oppgave 17.2
mtcars %>%
  map(~mean(.,, trim = 0.1, na.rm = TRUE))

#Oppgave 17.3
if (!require(reshape)) {
  install.packages("nycflights13")
}

library(nycflights13)

nycflights13::flights %>%
  map(class)

#Oppgave 17.4
iris %>%
  map(n_distinct)

#Oppgave 17.5
list <- c(seq(1, 4, by = 1))

powerup <- function(x) {
  x*x
}

list %>%
    map(.f = powerup)

vector <- c(as.integer(seq(1, 10, by = 1)))

vector %>%
  map_int(.f = powerup)



#Oppgave 19, ikke rukket å få til denne
#if (!require(ranger)) {
#  install.packages("ranger")
#}
#
#mod_ranger <- ranger(Species ~ ., 
#                     data = iris, 
#                     importance = "permutation")



#plot_importance <- function(.data) {
#  df_temp <- as.data.frame(ranger::importance(.data))
#  df_temp %>% 
#    ggplot(aes(x = index, y = importance(mod_ranger), color = {{fill}})) + 
#    geom_bar("identity")
#}

#plot_importance(mod_ranger)
