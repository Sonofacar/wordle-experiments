library(tidyverse)

# Import data
guess_data <- read_csv('data/woRdle/guess-data.csv')[c(2,3,5)]

# split into separate dataframes
standard   <- guess_data[guess_data$Sub.method == 'standard',]
blend      <- guess_data[guess_data$Sub.method == 'blend',]
orthogonal <- guess_data[guess_data$Sub.method == 'orthogonal',]

# then split by guess number
standard_one
standard_two
standard_three
standard_four
standard_five

blend_one
blend_two
blend_three
blend_four
blend_five

orthogonal_one
orthogonal_two
orthogonal_three
orthogonal_four
orthogonal_five
