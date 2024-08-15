library(tidyverse)

file_prefix <- c("temp", "ph", "salinity")
file_suffix <- c(1, 2, 3, 4)

for (i in 1:length(file_prefix)){
  for (j in 1:length(file_suffix))
  print(paste0(file_prefix[i], "_", file_suffix[j]))
}
#seq_along

odds <- c(1, 3, 5)
evens <- c(2, 4, 6, 8)
for (i in seq_along((odds))) {
  for(j in seq_along(evens)){
    print(odds[i] * evens[j])
  }
}
#a function in r----
birddog_sum <- function(birds, dog){
  pets <- birds + dog
  return(pets)
}
#birddog_sum(birds = 2, dog = 5)

x <- birddog_sum(birds = 2, dog = 5)
x

double_it <- function(x) {
  print(2 * x)
}

double_it(4)
double_it(1:4)

exclaim_age <- function(age){
  print(paste("I am", age, "years old!"))
}
exclaim_age(age = 34)

#print vs return

find_max <- function(val1, val2){
  if(val1 > val2) {

  }return(val1) {
  } else if (val2 > val1) {
    return(val2)
  }
}

5 * find_max(7,3)
#whatever you want to use out of function, make sure you are returning it. dont see what happens in function bc it doesnt come out in global environment ----
#dont know about find_max
#only variables you see are the ones you put into return?
#print vs return ----
#print = show me ("print is for people")
#return = store it so it can be used later

quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)
#mile_splits
 #for (i in seq_along(quarter_splits)){ ----
   #print(quarter_splits[i] + (quar
#quarter_splits[1] + quarter_splits[2]

#splitz problem, but it actually works ----
#think how can I turn this into a general equation? add i?
#how can I express this using the one interator I have?
for (i in 1:length(quarter_splits)) {
  print (quarter_splits[i] + quarter_splits[i + 1])

}

 # function with (condition animal age) ----
animal_age <- function(animal, age) {
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  }
}
animal_age(animal = "dog", age = 8)
animal_age(animal = "bat", age = 2)
animal_age(animal = "dog", age = "yellow")
#what if we have more animals?
#could use columns for it? if you have a data set

#data frames and dog food choice, one arguement for function? ----
#it work but then it didnt and I dont know what I did

dog_choice <- data.frame(dog_names = c("khora",
                                       "teddy",
                                       "waffle",
                                       "banjo"),
                         food = c("everything", "salmon", "pancakes", "chicken"))
dog_menu <- function(name) {
  my_sub <- dog_choice |>
    dplyr::filter(dog_names == "name")

  print(paste("my name is", my_sub$dog_names, "and I like to eat", my_sub$food))

}

dog_menu("khora")

if(!animal %in% c("dog", "goat", )) {
  stop("Oops! its not a dog or goat, must be dog or goat")
}



if(is.numeric(age == false)){
  stop("the age must be a number")
}

if(age <= 0){
  stop("the age must be greater than 0")
}

animal_age <- function(animal, age) {
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  }
}

animal_age(animal = "cow", age = 2)

#if it doesnt print, check console for + sign, if there is a +, hit esc




#function for wind speed


calc_windpower <- function(rho, radius, windspeed){
  print (0.3 * rho * pi *(radius^2)*(radius^3))

  if (windspeed > 130) {
    warning("wow")
  }
  if (rho >1.225) {
    warning("sus")
  }
  if(radius <0) {
    stop("radius must be positive")
  }
}

calc_windpower
#afternoon p2 nest? a function with beaches? ----

gw_rate <- function(site) {
  if(!site %in% c("mountain", "praire", "desert", "beach") )
  {
  warning ("site no included")}
  gw_depths <- data.frame(sitename = c("mountain",
                                       "praire",
                                       "desert",
                                       "beach"),
                          depth = c(32, 41, 63, 2),
                          slope = c(11.2, 0.4, 0.8, 2.6))
  site_select <- filter(gw_depths, sitename == site)

  transport_rate <- 1.4 * site_select$slope + 3.6 * site_select$depth
  return(transport_rate)
}

gw_rate(site = "beach")

#logistic growth that works and makes a graph and morevie ----

logistic_growth <- function(N0, K, r, time){
  Nt <- K/(1 + ((K - N0) / N0) * exp(-r * time))
  return(Nt)
}

logistic_growth(N0 = 100, K = 6000, r = 0.27, time = 40)

time_vec <- seq(from = 0, to = 50, by = 0.1)

pop_1 <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec)

pop_1_vec <- vector( mode = "numeric", length = length(time_vec))

for (i in seq_along(time_vec)) {
  population <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec[i])
  pop_1_vec[i] <- population
}

pop_time_1 <- data.frame(time_vec, pop_1_vec)
ggplot(data = pop_time_1, aes(x = time_vec, y = pop_1_vec)) +
  geom_line()


r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)

out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

for(i in seq_along(r_seq)) {
  for (j in seq_along(time_vec)) {
    population <- logistic_growth(N0= 100, K = 6000, r = r_seq[i], time = time_vec[j])
    out_matrix[j, i] <- population
  }
}

out_df <- data.frame(out_matrix, time = time_vec)
colnames(out_df) <- c(paste0("growth_rate_", r_seq), "time")

out_df_long <- out_df %>%
  pivot_longer(cols = -time, names_to = "growth_rate", values_to = "population_size")

ggplot(data = out_df_long, aes(x = time, y = population_size,)) +
  geom_line(aes(color = growth_rate), show.legend = FALSE) +
  theme_minimal()


