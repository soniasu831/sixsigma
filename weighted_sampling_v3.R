# %% load in libraries and source data #####
library(dplyr) # data wrangling
library(readr) # reading csv file
library(tidyr) # more data wrangling
library(janitor) # easy crosstabs
library(stringr)

# data source
source = "buses.csv" 
dat = source %>% read_csv()

source("tim_functions_process_control.R")

dat = dat %>% mutate(
  # convert from excel serial dates if date is available
  date_published_or_updated = date_published_or_updated %>% as.numeric() %>% as.Date(origin = "1899-12-30"),
  price_per_seat = round(base_price / seating_capacity, 2)
  )

# dat %>% write_csv("buses_with_price_per_seat_and_converted_dates.csv")

# remove NAs from the dataset
dat = dat[!is.na(dat$price_per_seat), ]

# %% getSamp function #####

#' a function to create a weighted sample of the data for one type of bus
#' @param dataset tibble, the bus dataset
#' @param type string, "Type A", "Type C" or "Type D"
#' @param manuf string, the manufacturer of interest, NA if just sampling from original dataset
#' @param weight numeric, the desired percentage of the manufacturer listed as a decimal (eg 0.1 for 10%)
#' @import dplyr
getSamp = function(dataset, type, manuf = NA, weight = NA){

  # test values
  # dataset = dat
  # type = "Type A"
  # manuf = "Blue Bird"
  # weight = 0.3

  if(is.na(weight)){
    # trim data to only include type of interest
    data = dataset[dataset$bus_type == type, ]
    sample_size = dim(data)[1]
    
    output = data %>% sample_n(size = sample_size, replace = TRUE)

  } else{
    # trim data to only include type of interest
    data = dataset[dataset$bus_type == type, ]
    sample_size = dim(data)[1]
    
    # create separate tibbles with and without the manufacturer of interest
    data_manuf = data[data$bus_manufacturer == manuf, ]
    data_manuf = data_manuf[!is.na(data_manuf$price_per_seat), ]
    data_no_manuf = data[data$bus_manufacturer != manuf, ]
    data_no_manuf = data_no_manuf[!is.na(data_no_manuf$price_per_seat), ]

    # create the sample of interest
    a = round(weight*sample_size)
    b = sample_size - a

    if (a > 0){
      outputa = data_manuf %>% sample_n(size = a, replace = TRUE)
      if (b > 0){
        outputb = data_no_manuf %>% sample_n(size = b, replace = TRUE)
        output = bind_rows(outputa, outputb)
      } else{
        output = outputa
      }
    } else if (b > 0){
      outputb = data_no_manuf %>% sample_n(size = b, replace = TRUE)
      output = outputb
    } 

  }

  return(output)

}

# %% getBoot function #####
#' a function to create bootstrapped samples
#' @param dat tibble, the bus dataset
#' @param boot_size int, the number of repetitions
#' @param type string, "Type A", "Type C" or "Type D"
#' @param manuf string, the manufacturer of interest
#' @param weights numeric vector, the desired percentages of the manufacturer listed as a decimal (eg 0.1 for 10%)
#'    weights = NA if just sampling from original dataset
getBoot = function(dat, boot_size, type, manuf, weights = NA){

  # test values
  # boot_size = 2
  # type = "Type A"
  # manuf = "Lightning eMotors/Collins Bus"
  # weights = c(0.2, 0.8)

  # create a tibble
  boot = tibble(
    w = weights %>% rep(times = boot_size),
    r = rep(1:boot_size, each = length(weights))
  ) %>% 
    group_by(w, r) %>% # for each type, weight, and rep
    reframe(
      sample = getSamp(dat, type, manuf, w) # get sample the original dataset
    ) %>% unnest(sample)

  # calculate overall statistics for each weight and rep
  boot_stat = boot %>% ungroup() %>% group_by(w, r) %>% 
    reframe(
      percentage = 100*length(which(bus_manufacturer == manuf)) / length(bus_manufacturer),
      average_pps = price_per_seat %>% mean()
    )

  return(boot_stat)
}

# %% scatter plot with trendline

# type = "Type A"
# manuf = "Lightning eMotors/Collins Bus"
# type = "Type D"
# manuf = "GreenPower"
type = "Type C"
manuf = "Lion Electric"

b1 = getBoot(dat, boot_size = 10, type, manuf, seq(from = .05, to = .95, by = 0.05))

b1 %>% ggplot(mapping = aes(x = percentage, y = average_pps))+
  geom_point() +
  labs(x = paste("Percentage of", manuf, "Buses"), 
       y = "Average Price per Seat ($/seat)",
       subtitle = str_wrap(paste("Average Price Per Seat vs Percentage Of", manuf, "for", type, "Buses" ), width = 50)
  )  +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE) # se = FALSE removes extra stuff

# b1 %>% lm(formula = average_pps ~ percentage) %>% summary()


# %% simulate data and create box plots

b2 = getBoot(dat, boot_size = 100, type, manuf, c(0.2, 0.8))

b2 %>% ggplot()+
  geom_boxplot(mapping = aes(x = w, y = average_pps, group = w)) +
  labs(x = paste("Percentage of", manuf, "Buses"), 
       y = "Average Bus Price per Seat ($/seat)",
       subtitle = str_wrap(paste("Average Price Per Seat vs Percentage Of", manuf, "for", type, "Buses" ), width = 50)
  )  +
  theme_classic()

# %% calculate confidence intervals 



# type = "Type C"
# manuf = "Lion Electric"
# would need to go up to 60

boot_size = 1000
weight = 0.90

type = "Type D"
manuf = "GreenPower"
# would need to go up to 90%

# type = "Type A"
# manuf = "Lightning eMotors/Collins Bus"
# only need to go up to 30% to have a statistically significant change (90% confidence)

# bootstrap original dataset
b_nom = getBoot(dat, boot_size = boot_size, type, manuf)
# bootstrap with change
b_x = getBoot(dat, boot_size = boot_size, type, manuf, weights = weight)

b_diff = b_x$average_pps - b_nom$average_pps

# the se of the sampling distribution is the SD
alpha = 0.05
delta = tibble(
  mean = mean(b_diff),
  lower = quantile(b_diff, probs = alpha),
  upper = quantile(b_diff, probs = 1-alpha)
)

delta

# %%

dat %>% tabyl(bus_manufacturer)
# no blue bird in ME, OH, SC, VA
# SC has NAs for seating


dat %>% tabyl(special_needs_bus)
