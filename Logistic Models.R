library(tidyverse)
library(ggplot2)
covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

library(FlexParamCurve)

# analyse US first

covid_us <- covid %>% filter(Country_code == "US")
covid_us <- covid_us %>% mutate(
    Date_num = as.numeric(difftime(Date_reported, Date_reported[1], units = "days")))

fk <- nls(Cumulative_cases ~ SSlogis(Date_num, phi1, phi2, phi3), data = covid_us)
alpha <- coef(fk)
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue")



modpar(covid_us$Date_num, covid_us$Cumulative_cases, verbose = T,
       force4par = T, pn.options = "myoptions", width.bounds = 100)
fj <- nls(Cumulative_cases ~ SSposnegRichards(Date_num, Asym = Asym, K = K, 
                                              Infl = Infl, M = M, 
                                              pn.options = "myoptions", modno = 12),
          data = covid_us)
beta <- coef(fj)

plot(covid_us$Date_num, covid_us$Cumulative_cases)
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue")
curve(posnegRichards.eqn(x, Asym = beta[1], K = beta[2], Infl = beta[3], 
                         M = beta[4], pn.options = "myoptions", modno = 12), add = T, col = "red")

f1 <- function(x) alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3]))
f2 <- function(x) posnegRichards.eqn(x, Asym = beta[1], K = beta[2], Infl = beta[3], 
                                     M = beta[4], pn.options = "myoptions", modno = 12)
                            

plot_us <- ggplot(covid_us, aes(x = Date_num, y = Cumulative_cases)) +
    geom_point() + geom_line() +
    geom_function(aes(colour = "Classic Logistic model"), size = 1.5, fun = f1) + 
    geom_function(aes(colour = "Generalized Richards model"), size = 1.5, fun = f2) + 
    labs(x = "Day number", y = "Cumulative cases", title = "COVID trend in US")


# Global

covid_global <- covid %>% group_by(Date_reported) %>%
    summarise(across(where(is.numeric), sum))
covid_global <- covid_global %>% mutate(
    Date_num = as.numeric(difftime(Date_reported, Date_reported[1], units = "days")))
covid_global <- head(covid_global, -1)

clm <- nls(Cumulative_cases ~ SSlogis(Date_num, phi1, phi2, phi3), data = covid_global)
alpha_g <- coef(clm)
modpar(covid_global$Date_num, covid_global$Cumulative_cases, verbose = T,
       force4par = T, pn.options = "myoptions", width.bounds = 100)
grm <- nls(Cumulative_cases ~ SSposnegRichards(Date_num, Asym = Asym, K = K, 
                                              Infl = Infl, M = M, 
                                              pn.options = "myoptions", modno = 12),
          data = covid_global)
beta_g <- coef(grm)

plot(covid_global$Date_num, covid_global$Cumulative_cases)
curve(alpha_g[1]/(1 + exp(-(x - alpha_g[2])/alpha_g[3])), add = T, col = "blue")
curve(posnegRichards.eqn(x, Asym = beta_g[1], K = beta_g[2], Infl = beta_g[3], 
                         M = beta_g[4], pn.options = "myoptions", modno = 12), add = T, col = "red")

# function for plot

trend_plot <- function(country) {
    clm <- nls(Cumulative_cases ~ SSlogis(Date_num, phi1, phi2, phi3), data = country)
    alpha <- coef(clm)
    modpar(country$Date_num, country$Cumulative_cases, verbose = T,
           force4par = T, pn.options = "myoptions", width.bounds = 100)
    grm <- nls(Cumulative_cases ~ SSposnegRichards(Date_num, Asym = Asym, K = K, 
                                                   Infl = Infl, M = M, 
                                                   pn.options = "myoptions", modno = 12),
               data = country)
    beta <- coef(grm)
    
    f1 <- function(x) alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3]))
    f2 <- function(x) posnegRichards.eqn(x, Asym = beta[1], K = beta[2], Infl = beta[3], 
                                         M = beta[4], pn.options = "myoptions", modno = 12)
    
    country_name <- country$Country[1]
    
    plot_country <- ggplot(country, aes(x = Date_num, y = Cumulative_cases)) +
        geom_point() + geom_line() +
        geom_function(aes(colour = "CLM"), size = 1.5, fun = f1) + 
        geom_function(aes(colour = "GRM"), size = 1.5, fun = f2) + 
        theme(legend.position = c(0.9, 0.1)) + 
        labs(x = "Day number", y = "Cumulative cases", 
             title = paste("COVID trend in", country_name))
    
    return(plot_country)
}


## GB



covid_gb <- covid %>% filter(Country_code == "GB")
covid_gb <- covid_gb %>% mutate(
    Date_num = as.numeric(difftime(Date_reported, Date_reported[1], units = "days")))
gb_plot <- trend_plot(covid_gb)

covid_in <- covid %>% filter(Country == "India")
covid_in <- covid_in %>% mutate(
    Date_num = as.numeric(difftime(Date_reported, Date_reported[1], units = "days")))
in_plot <- trend_plot(covid_in)

covid_et <- covid %>% filter(Country == "Ethiopia")
covid_et <- covid_et %>% mutate(
    Date_num = as.numeric(difftime(Date_reported, Date_reported[1], units = "days")))
et_plot <- trend_plot(covid_et)

covid_au <- covid %>% filter(Country == "Australia")
covid_au <- covid_au %>% mutate(
    Date_num = as.numeric(difftime(Date_reported, Date_reported[1], units = "days")))
au_plot <- trend_plot(covid_au)

us_plot <- trend_plot(covid_us)

global_plot <- trend_plot(covid_global)
global_plot$labels$title <- "COVID trend in the world"

grid.arrange(global_plot, us_plot, gb_plot, in_plot, et_plot, au_plot,
             nrow = 2)


