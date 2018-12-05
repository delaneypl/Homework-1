install.packages("dplyr")
library("dplyr")
shrub_data <- read.csv("data/shrub-volume-data.csv")
# 1.
names(shrub_data)
# 2.
str(shrub_data)
# 3. 
head(shrub_data)
# 4.
select(shrub_data, length)
# 5. 
select(shrub_data, site, experiment)
# 6.
filter(shrub_data, height>5)
# 7. 
shrub_data_w_vols <- mutate(shrub_data,volume = length*width*height)


#Exercise 2
by_experiment <- group_by(shrub_data_w_vols, experiment)
avg_height <- summarize(by_experiment, avg_height = mean(height))
by_site <- group_by(shrub_data_w_vols, site)
max_height <- summarize(by_site, max_height = max(height))

# Exercise 3
shrub_experimental <- read.csv("data/shrub-volume-experiments-table.csv")
shrub_data_w_manipulation <- inner_join(shrub_data_w_vols, shrub_experimental)
View(shrub_data_w_manipulation)

# Exercise 4
surveys <- read.csv("data/surveys.csv")
# 1. 
surveys_ymds <- select(surveys, year, month, day, species_id)
# 2.
surveys_ysw <- select(surveys, year, species_id, weight)
mutate(surveys_ysw, weight_kg = weight/1000)
na.omit(surveys_ysw)
print(surveys_ysw)
# 3.
surveys_SH <- filter(surveys, species_id == "SH")
print(surveys_SH)
# 4.
surveys_species_id <- group_by(surveys, species_id)
summarize(surveys_species_id)
# 5. 
surveys_species_year <- group_by(surveys_species_id, year)
summarize(surveys_species_year)

# 6.
surveys_DO <- filter(surveys, species_id == "DO")
na.omit(surveys_DO, weight)
surveys_DO_meanw <- mutate(surveys_DO, weight_mean = mean(weight))
survey_DO_year <- group_by(surveys_DO_meanw, year, weight_mean, species_id)
summarize(survey_DO_year)
       


# Exercise 5
# line below downloads data set
read.csv("data/shrub-volume-data.csv")
# following aggregate the creation of the volume data column, grouping the data by site, and taking the mean volume of each site and summarizing them
shrub_data %>%
  mutate(volume = length * width * height) %>%
  group_by(site) %>%
  summarize(mean_volume = mean(volume))
# following aggregate the creation of the volume data column, grouping the data by experiment, and taking the mean volume of each experiment and summarizing them
shrub_data %>%
  mutate(volume = length * width * height)%>%
  group_by(experiment) %>%
  summarize(mean_volume = mean(volume))

#Exercise 6
surveys <- read.csv("data/surveys.csv")
species <- read.csv("data/species.csv")
plots <- read.csv("data/plots.csv")
# 1.
surveys_w_species <- inner_join(species, surveys)
# 2. 
surveys_w_species_plots <- inner_join(plots, inner_join(species, surveys))
View(surveys_w_species_plots)
# 3. 
surveys_plots <- inner_join(surveys, plots)
surveys_plots_control <- filter(surveys_plots, plot_type == "Control")
View(surveys_plots_control)
# 4. 
surveys_w_species_plots_Rodent <- filter(surveys_w_species_plots, taxa == "Rodent")
surveys_plots_cltke <- select(surveys_w_species_plots_Rodent, year, genus, species, weight, plot_type)
surveys_cltke_pt <- filter(surveys_plots_cltke, plot_type == "Control", plot_type == "Long-term Krat Exclosure")
surveys_cltke_pt_wna <- na.omit(surveys_cltke_pt, weight)
View(surveys_cltke_pt_wna)

