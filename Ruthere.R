# Packages ----
library('matrixStats')
library('dplyr')
library('tidyr')
library('stringr')
library('writexl')
library('here')


# Load csv exports downloaded from facebook ----
page_data <- read.csv(here("Ruthere_Page_Data.csv"), header = TRUE)
post_data <- read.csv(here("Ruthere_Post_Data.csv"), header = TRUE)

# Extract different cities from column names ----
city_data <- page_data %>% select(starts_with('Lifetime.Likes.by.City')) %>%
                           gather(key = "City", value = "Lifetime.Likes") %>% 
                           group_by(City) %>%
                           filter(!is.na(Lifetime.Likes)) %>%
                           summarise(Likes = max(Lifetime.Likes))%>%
                           mutate(City = str_extract(City, '(?<=City...).*$')) %>%
                           separate(col = City, into = c("City", "State.Country"), sep = '\\.\\.+') %>%
                           mutate(City = gsub('\\.', ' ', City), State.Country = gsub('\\.', ' ', State.Country))

    # Rename Rio De Janero state to Brazil as country
    city_data$State.Country[city_data$State.Country == 'RJ'] <- 'Brazil'
                           
# Extract different genders / ages from column names ----
cohort_data <- page_data %>% select(Date, starts_with('Lifetime.Likes.by.Gender')) %>% 
                             filter(Date == "11/28/19") %>%
                             gather(key = "Gender.Age", value = "Lifetime.Likes", -Date) %>%
                             mutate(Gender.Age = str_extract(Gender.Age, '(?<=Age...).*$')) %>%
                             separate(col = Gender.Age, into = c("Gender", "Age.Min", "Age.Max"), sep = '\\.') %>% 
                             unite("Age.Range", Age.Min:Age.Max, sep = '-') %>%
                             mutate(Age.Range = gsub('[^a-zA-Z0-9\\s]+$', '+', Age.Range)) %>%
                             select(Age.Range, Gender, Lifetime.Likes) %>% arrange(Age.Range)

# Export dfs to Excel, would upload to personal google sheets if wasn't public ----
write_xlsx(list("City Data" = city_data, "Cohort Data" = cohort_data), here('Ruthere_Page_Sup.xlsx'))



                             



