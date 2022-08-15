library(shiny)
library(dplyr)
library(fmsb)
library(ggplot2)
library(reshape2)
library(maps)
library(mapproj)
library(stringr)
library(usmap)
# Summary information
pop_df <- read.csv("incarceration_trends.csv")
all_df <- read.csv("year-end-prison-2021.csv")
al_state_df <- filter(pop_df, state == "AL" )
bibb_county_df <- filter(al_state_df, county_name == "Bibb County")

num_row <- nrow(bibb_county_df)
num_col <- ncol(bibb_county_df)

summary_info <- list(median_black = black_median,
                     median_white = white_median,
                     bibby_female_average = b_female_average,
                     bibby_male_average = b_male_average,
                     total_female_average = t_female_average,
                     total_male_average = t_male_average,
                     all_total_pop = al_total_pop,
                     bibb_county_total_pop = bibb_total_pop,
                     black_jail_percentage = black_jail_percent,
                     black_max = black_biggest,
                     white_max = white_biggest,
                     aapi_max = aapi_biggest,
                     latinx_max = latinx_biggest,
                     native_max = native_biggest,
                     other_max = other_biggest
                     )


# 1. Calculate the medians of black and white poeple from 15 to 64 in bibb county
black_white <- select(bibb_county_df, black_jail_pop, white_jail_pop)
black_median <- median(black_white$black_jail_pop, na.rm = TRUE)
white_median <- median(black_white$white_jail_pop, na.rm = TRUE)

# 2. Calculate the average number of male and female prisoners in bibb county
# and compare that with average general in the U.S
# in general years
gender_df <- select(bibb_county_df, female_jail_pop, male_jail_pop)
general_gender_df <- select(pop_df,female_jail_pop, male_jail_pop)
b_female_average <- mean(bibb_county_df$female_jail_pop, na.rm = TRUE)
b_male_average <- mean(bibb_county_df$male_jail_pop, na.rm = TRUE)
t_female_average <- mean(pop_df$female_jail_pop, na.rm = TRUE)
t_male_average <- mean(pop_df$male_jail_pop, na.rm = TRUE)


# 3. Calculate the mean numbers of total average jail population in Alabama and total
# jail population of Bibb County
al_total_pop <- mean(al_state_df$total_jail_pop, na.rm = TRUE)
bibb_total_pop <- mean(bibb_county_df$total_jail_pop, na.rm = TRUE)


# 4. Calculate the mean percentage of black and white of 15 to 64 years old
# in prision for U.S
black_jail_mean <- mean(pop_df$black_jail_pop, na.rm = TRUE)
total_jail_mean <- mean(pop_df$total_jail_pop, na.rm = TRUE)
black_jail_percent <- black_jail_mean / total_jail_mean

# 5. Find the biggest number of each race in prison for US
black_biggest <- max(pop_df$black_jail_pop, na.rm = TRUE)
white_biggest <- max(pop_df$white_jail_pop, na.rm = TRUE)
aapi_biggest <- max(pop_df$aapi_jail_pop, na.rm = TRUE)
latinx_biggest <- max(pop_df$latinx_jail_pop, na.rm = TRUE)
native_biggest <- max(pop_df$native_jail_pop, na.rm = TRUE)
other_biggest <- max(pop_df$other_race_jail_pop, na.rm = TRUE)



## plot 1 Scatter plot
pop_df <- read.csv("incarceration_trends.csv")
all_df <- read.csv("year-end-prison-2021.csv")
al_state_df <- filter(pop_df, state == "AL" )
bibb_county_df <- filter(al_state_df, county_name == "Bibb County")
#View(bibb_county_df)
colors <- c("male jail pop" = "Blue","female jail pop" = "Orange")
ggplot()+
  geom_point(bibb_county_df, mapping = aes(x=male_jail_pop, y=year, color = "male jail pop"))+
  geom_point(bibb_county_df, mapping = aes(x=female_jail_pop, y=year, color = "female jail pop"))+
  labs(x="population", y = "year", color = "Legend",
       title = "Trend of Prisoner Number in Bibb County between Male and Female
       from 1970 to 2020") + scale_color_manual(values = colors)

## plot 2 bar chart
df <- bibb_county_df %>% select(year, male_juvenile_pop, male_adult_jail_pop)
df2 <- melt(df, id.vars="year")
ggplot(df2, aes(x=year, y=value, fill=variable)) + geom_bar(stat='identity',
                                                            position='dodge') + labs(title = "
                                                                                     Comparison
                                                                                     between male
                                                                                     adult pop
                                                                                     and male total
                                                                                     pop in prison")



# ## plot 3 map
plot_usmap(data = pop_df, values = 'total_pop', color = 'blue') +
  scale_fill_continuous(low = 'lightblue2', high = 'red', name = 'Number of prison', 
  label = scales::comma) + labs(title = 'Prisoners Number in U.S', 
  subtitle = 'This map shows how many prisoners are distributed location-wise in U.S') + 
  theme(legend.position = "right" )



