####################################################################################################
# Title: Geospatial Analysis of Air Pollution and Health Outcomes in India
# Author: Paul George
# Date: 11/12/2023
# Description:
#   This script is designed for a research project exploring the impact of air pollution on 
#   health outcomes in India. It involves loading, processing, and analyzing geospatial and 
#   health data from various sources.
#
#   Key operations include:
#   - Loading raw datasets including India health data, geospatial data, and geospatial covariates.
#   - Processing raster data for PM2.5 concentrations and converting it to a dataframe for analysis.
#   - Filtering PM2.5 data for Indian territorial boundaries and reducing data size for efficiency.
#   - Creating a list of dataframes to average pollution values by cluster, focusing on hospitals.
#   - Merging pollution data with geospatial coordinates and calculating mean PM2.5 values.
#   - Generating visualizations, including density plots and maps, to analyze the distribution of PM2.5.
#
#   The output includes a comprehensive geospatial dataset 'geospatial_PM25.rda', mapping PM2.5 
#   concentrations across India, and visual representations of the data for further research 
#   on the relationship between air pollution and health outcomes.
#
#   Please see George, PE et al Lancet Regional Health Southeast Asia (2023) for the corresponding publication
#
####################################################################################################



# Load necessary libraries
library(haven)
library(foreign)
library(mapview)
library(sp)
library(tidyverse)
library(raster)
library(geosphere)
library(data.table)

# Set working directory to an example project folder
setwd('<Your-Project-Folder-Path>')

# Clear current environment
rm(list = ls())

#### Data Loading ####

# Load health and geospatial data from India
df_India = read_dta('<Path-to-India-Health-Data>/ARI_Stata_Recoded.dta')
geospatial = read.dbf('<Path-to-Geospatial-Data>/IAGE7AFL.dbf')
gs_covariates = read.csv('<Path-to-Covariates-Data>/IAGC7AFL.csv')

#### Processing PM2.5 Raster Data ####

# Load and process global PM2.5 raster data
df1 = raster('<Path-to-PM25-Raster-Data>/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019.tif')

# Convert raster to dataframe and round coordinates for simplicity
df2 = as.data.frame(rasterToPoints(df1))
df2 = round(df2, 3)

# Filter for Indian territorial boundaries
PM_2019_India = df2 %>% filter(x > 66 & x < 99 & y > 3 & y < 36)
PM_2019_India_med = PM_2019_India

# Clean up memory
rm(df1, df2)

#### Averaging Pollution Values by Cluster ####

# Create list to store PM2.5 data by cluster
PM_list_of_dfs = list()
name_list = list()

# Populate list with geospatial data and placeholder columns
for(i in 1:nrow(geospatial)) {
  df_temp = data.frame(geospatial[i,])
  for(j in 1:40) {
    df_temp[, paste0('site_', j)] = NA
  }
  PM_list_of_dfs[[i]] = df_temp
  name_list[i] = paste0('cluster_', i)
}
names(PM_list_of_dfs) = name_list

# Add a unique SiteCode to the PM25 data
PM_2019_India_med = PM_2019_India_med %>% 
  mutate(SiteCode = row_number()) %>% 
  rename(Lon = x, Lat = y)

# Define distance thresholds for urban and rural areas
dist_10km = 0.15   # A bit larger for rural areas
dist_2km = 0.029   # A bit larger for urban areas

# Match PM2.5 data to clusters based on proximity
for(i in 1:nrow(geospatial)) {
  print(paste0("working on i = ", i))
  k = 1
  for(j in 1:nrow(PM_2019_India_med)) {
    dist = geosphere::distVincentySphere(
      matrix(c(PM_list_of_dfs[[i]]$LONGNUM[1], PM_list_of_dfs[[i]]$LATNUM[1]), ncol = 2),
      matrix(c(PM_2019_India_med$Lon[j], PM_2019_India_med$Lat[j]), ncol = 2)
    )
    threshold = ifelse(PM_list_of_dfs[[i]]$URBAN_RURA[1] == 'U', dist_2km, dist_10km)
    if (dist < threshold) {
      PM_list_of_dfs[[i]][1, 20 + k] = PM_2019_India_med$SiteCode[j]
      k = k + 1
    }
  }
}

# Save processed data
save(PM_2019_India_med, file = '<Path-to-Output>/PM_2019_India_med.rda')
saveRDS(PM_list_of_dfs, file = '<Path-to-Output>/PM_list_of_dfs_long_v2.RData')

# Load previously saved data
PM_2019_India_med = readRDS('<Path-to-Output>/PM_2019_India_med.rda')
PM_list_of_dfs = readRDS('<Path-to-Output>/PM_list_of_dfs_long_v2.RData')

#### Creating PM2.5 Data Specific to Each Cluster ####

# Create list to store PM2.5 data specific to each cluster
list_of_cluster_PM25 = list()

# Merge PM2.5 data with site codes for each cluster
for (i in 1:nrow(geospatial)){
  # Merge site codes with PM2.5 data
  merged_df = merge(PM_list_of_dfs[[i]][,21:ncol(PM_list_of_dfs[[i])], drop = FALSE], PM_2019_India_med, by = 'SiteCode')
  # Calculate the mean PM2.5 value for each cluster
  list_of_cluster_PM25[[i]] = data.frame(cluster = i, annual_PM25 = mean(merged_df$layer, na.rm = TRUE))
}

# Combine all clusters into a single dataframe
df_cluster_PM25 = do.call(rbind, list_of_cluster_PM25)

#### Visualization of PM2.5 Distribution ####

# Load geospatial data for mapping
geospatial_PM25 = geospatial
# Merge geospatial data with PM2.5 data
geospatial_PM25 = merge(geospatial_PM25, df_cluster_PM25, by = "cluster")

# Create a density plot for annual PM2.5 concentrations
ggplot(geospatial_PM25, aes(x = annual_PM25)) + 
  geom_density(fill = "blue", color = "black") + 
  labs(x = "Annual PM2.5 Concentration", y = "Density") + 
  ggtitle("Distribution of Annual PM2.5 Concentration")

# Save the merged geospatial and PM2.5 data
save(geospatial_PM25, file = '<Path-to-Output>/geospatial_PM25.rda')

#### Mapping PM2.5 Concentrations ####

# Load the saved geospatial PM2.5 data
geospatial_PM25 = readRDS('<Path-to-Output>/geospatial_PM25.rda')

# Select relevant columns for mapping
geospatial_mapview = geospatial_PM25 %>% 
  dplyr::select(LATNUM, LONGNUM, URBAN_RURA, annual_PM25)

# Project to SpatialPointsDataFrame
coordinates(geospatial_mapview) <- c('LONGNUM', 'LATNUM')
proj4string(geospatial_mapview) <- CRS("+proj=longlat +datum=WGS84")

# Create a map of annual PM2.5 values
mapview(geospatial_mapview, zcol = 'annual_PM25', 
        map.types = 'Esri.WorldShadedRelief', 
        at = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130), 
        pch = 21, size = 2)


####################################################################################################
# Title: Air Pollution and Child Health Outcomes in India
# Author: Paul George
# Date: 11/12/2023
# Description:
#   This script is part of a research project investigating the impact of air pollution on 
#   child health outcomes in India. It primarily involves data loading, cleaning, transformation, 
#   and integration from various sources including DHS (Demographic and Health Surveys) data, 
#   geospatial PM2.5 data (SEDAC), and additional covariates relevant to the study.
#
#   The final output is a consolidated dataset saved as 'India_child_environ_df.rda', ready for 
#   further analysis on the relationship between air pollution and child health outcomes.
#
# Prerequisites:
#   The following R packages are required: haven, foreign, tidyverse, dplyr, stringr.
####################################################################################################


# Load necessary libraries
library(haven)
library(foreign)
library(tidyverse)
library(dplyr)
library(stringr)

# Set the working directory to an example project folder
setwd('<Your-Project-Folder-Path>')

# Clear the current environment
rm(list = ls())

#### Data Loading ####

# Load the main dataset for India from an example path
India_df = read_dta('<Path-to-India-Dataset>/DHS Data -- India (2019-2021).dta')

# Load covariates dataset from an example path
gs_covariates = read.csv('<Path-to-Covariates-Dataset>/IAGC7AFL.csv')

# Load household member dataset from an example path
household_member_df = read_dta('<Path-to-Household-Member-Dataset>/Household Member Recode.DTA')

# Load geospatial PM2.5 dataset
load('<Path-to-Geospatial-PM25-Dataset>/geospatial_PM25.rda') 

######## Data Cleaning ########

# Convert ADM1NAME in geospatial_PM25 dataset to title case for consistency
geospatial_PM25$ADM1NAME <- str_to_title(tolower(geospatial_PM25$ADM1NAME))

######## Data Reduction ########

# Selecting specific columns from geospatial_PM25 to create a smaller dataset
geospatial_PM25_small = geospatial_PM25 %>% 
  dplyr::select(DHSYEAR, DHSCLUST, ADM1NAME, ADM1DHS, DHSREGNA, LATNUM, LONGNUM, ALT_GPS, annual_PM25, URBAN_RURA)

# Creating a smaller version of the gs_covariates dataset
gs_covariates_small = gs_covariates %>% 
  dplyr::select(DHSCLUST, DHSYEAR, All_Population_Count_2020, Aridity_2020, Day_Land_Surface_Temp_2020, Elevation, 
                Malaria_Prevalence_2020, Maximum_Temperature_2020, Minimum_Temperature_2020, Precipitation_2020, 
                starts_with('Temperature_'), UN_Population_Density_2020, Wet_Days_2020)

# Check the structure of a specific variable in household_member_df
str(household_member_df$hv215)

# Selecting distinct households from the household_member_df
household_member_distinct = distinct(household_member_df, hhid, .keep_all = TRUE)

# Further reducing the household_member_distinct dataset
household_member_distinct_small = household_member_distinct %>% 
  dplyr::select(hhid, hv001, hv005, hv025, hv040, hv201, hv205, hv213, hv214, hv215, hv239, hv241, hv242, hv252, sh28, sh52, sh55, sh71)

# Reducing the size of the main India dataset by selecting specific columns
India_df_small = India_df %>% 
  dplyr::select(caseid, hhid, v001, v005, v006, v007, v012, v024, v025, v040, 
                v101, v102, v113, v119, v130, v131, v133, v136, v137, v149, 
                v150, v155, v161, v190, v463a, v463b, v463f, v463j, v463k, v463l, v463x, v463z, v463aa, v464, 
                h11, h22, h31, h31b, h31c, ch_ari, wt, religion, region,  
                hw1, hw2, hw3, hw53, hw70, hw71, hw72, hw73, 
                b0, b1, b2, b3, bord, b4, b5, weight, m19)

###### Variable Addition and Transformation ######

# Creating a dataframe to map state numbers to names
state_df = geospatial_PM25_small %>% 
  dplyr::select(ADM1DHS, ADM1NAME) %>% 
  unique() %>% 
  arrange(ADM1DHS) %>% 
  rename(state_number = ADM1DHS, state_name = ADM1NAME)

# Renaming column in India_df_small for consistency
India_df_small = India_df_small %>% 
  rename(state_number = v024)

# Merging state_df with India_df_small
India_df_small = left_join(India_df_small, state_df)

# Function to determine the season based on the month
get_season <- function(month) {
  ifelse(month %in% c(12, 1, 2), "Winter",
         ifelse(month %in% c(3, 4, 5), "Summer",
                ifelse(month %in% c(6, 7, 8, 9), "Monsoon", "Post-Monsoon")))
}

# Adding a new variable for the season of the interview
India_df_small <- India_df_small %>%
  mutate(season_of_interview = case_when(
    state_name %in% c("Punjab", "Haryana", "Nct Of Delhi", "Uttar Pradesh", "Bihar") ~ get_season(v006),
    state_name %in% c("Rajasthan", "Gujarat", "Maharashtra", "Goa", "Dadra & Nagar Haveli And Daman & Diu") ~ get_season(v006 + 1),
    state_name %in% c("Madhya Pradesh", "Chhattisgarh", "Odisha", "Jharkhand") ~ get_season(v006 - 1),
    state_name %in% c("West Bengal", "Sikkim", "Assam", "Arunachal Pradesh", "Meghalaya", 
                      "Nagaland", "Manipur", "Mizoram", "Tripura", "Jammu & Kashmir", "Himachal Pradesh", 
                      "Chandigarh", "Uttarakhand", "Andhra Pradesh", "Karnataka", "Lakshadweep", "Kerala", 
                      "Tamil Nadu", "Puducherry", "Andaman & Nicobar Islands", "Telangana", "Ladakh") ~ get_season(v006),
    TRUE ~ "Unknown"
  ))

######### Data Integration #########

# Merging datasets to create a comprehensive dataset for child environmental analysis
India_child_environ_df = left_join(India_df_small, household_member_distinct_small)

# Renaming columns for consistency across datasets
geospatial_PM25_small = geospatial_PM25_small %>% 
  rename(cluster_number = DHSCLUST)

gs_covariates_small = gs_covariates_small %>% 
  rename(cluster_number = DHSCLUST)

# Further merging datasets
India_child_environ_df = left_join(India_child_environ_df, geospatial_PM25_small)
India_child_environ_df = left_join(India_child_environ_df, gs_covariates_small)

# Saving the final combined dataset
save(India_child_environ_df, file = '<Path-to-Output-Dataset>/India_child_environ_df.rda')




####################################################################################################
# Title: Environmental Factors and Child Health Outcomes in India
# Author: Paul George
# Date: 11/12/2023
# Description:
#   This R script is part of a research project analyzing the impact of environmental factors 
#   on child health outcomes in India. It utilizes data from the Demographic and Health Surveys 
#   (DHS) and geospatial PM2.5 data to explore the association between air pollution, living 
#   conditions, and health outcomes such as respiratory infections and anemia in children.
#
#   Key operations include:
#   - Data loading and preparation: Importing and preparing the DHS health data and geospatial PM2.5 data.
#   - Variable creation and transformation: Creating new variables for analysis, including health outcome 
#     indicators and environmental exposure measures.
#   - Logistic regression analyses: Assessing the relationship between environmental factors and child 
#     health outcomes.
#   - Correlation analysis: Exploring correlations between different environmental and household factors.
#   - Data visualization: Creating plots and geospatial maps to visualize the results and spatial distribution 
#     of health outcomes.
#
#   The outcomes of this script will aid in understanding the influence of environmental conditions on child 
#   health in India, contributing to public health research and policy-making.
#
#
# Prerequisites:
#   Required R packages include lme4, tidyverse, ggplot2, labelled, ggpubr, mapview, sf, mgcv, and corrplot.
####################################################################################






# Load necessary libraries
library(lme4)
library(tidyverse)
library(ggplot2)
library(labelled)
library(ggpubr)
library(mapview)
library(sf)
library(mgcv)
library(corrplot)

# Set working directory to an example project folder
setwd('<Your-Project-Folder-Path>')

# Clear current environment
rm(list = ls())

#### Data Loading and Preparation ####

# Load main India health dataset
India_df = read_dta('<Path-to-India-Health-Data>/DHS Data -- India (2019-2021).dta')

# Load processed environmental and child health datasets
load('<Path-to-Processed-Data>/India_child_environ_df.rda')
load('<Path-to-Processed-Data>/geospatial_PM25.rda')

#### Creating New Variables ####

# Convert and mutate variables for analysis
India_child_environ_df <- India_child_environ_df %>%
  mutate(hw53_num = as.numeric(hw53),
         severe_resp_infect_2wks = if_else(h22 == 1 & h31 == 2 & h31b == 1, 1, 0),
         cough_2_wks = if_else(h31 == 2, 1, 0),
         diarrhea_2_wks = if_else(h11 == 1 | h11 == 2, 1, 0),
         hemoglobin = if_else(hw53_num < 230, hw53_num, NA),
         diarrhea_2_wks = ifelse(h11 == 8, NA, diarrhea_2_wks),
         hemoglobin = ifelse(hemoglobin > 250, NA, hemoglobin),
         anemia = if_else(hemoglobin < 110, 1, 0), # WHO definition of anemia
         annual_PM25_quartile = cut(annual_PM25, breaks = quantile(annual_PM25, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Lowest Quartile", "2nd Quartile", "3rd Quartile", "Highest Quartile"), ordered = TRUE),
         v464 = replace_na(v464, 0),
         temp_month_of_interview = case_when(
           v006 == 1 ~ Temperature_January,
           # ... Add cases for other months
           TRUE ~ Temperature_December
         ))

#### Bivariate Analysis Preparations ####

# Convert several variables to factors
India_child_environ_df <- India_child_environ_df %>%
  mutate(across(c(v119, v161, hv242, v149, v155, v190, sh71, b4, hv252, hv213, hv214, hv215, hv241, region, ADM1NAME, hhid, hv201, hv205, weight), to_factor),
         annual_PM25_scale = (annual_PM25 - mean(annual_PM25, na.rm = TRUE)) / (2 * sd(annual_PM25, na.rm = TRUE)))

# Standardize PM2.5 variable
India_child_environ_df <- India_child_environ_df %>%
  mutate(annual_PM25_scale = (annual_PM25 - mean(annual_PM25, na.rm = TRUE)) / (2 * sd(annual_PM25, na.rm = TRUE)))

#### Correlation Analysis ####

# Prepare data for correlation analysis
India_child_environ_df <- India_child_environ_df %>%
  mutate(URBAN_RURA_num = as.numeric(URBAN_RURA == "U"),
         cooking_toxicity_group_num = as.numeric(cooking_toxicity_group == "Moderately-to-Highly Toxic"),
         # ... Add more mutations for other variables
  )

# Calculate and visualize correlation
cor_table <- cor(India_child_environ_df[<list-of-variables>], use = "pairwise.complete.obs")
correlation_plot <- corrplot(cor_table, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45, type = 'lower', order = 'hclust', diag = FALSE)

# Save the plot
ggsave("correlation_plot.png", plot = correlation_plot, width = 10, height = 7, dpi = 600)


# Logistic Regression Analyses for Health Outcomes
# This section performs logistic regression to assess the relationship between environmental factors and health outcomes

# Defining a function to run logistic regression and extract relevant metrics
run_logistic_regression <- function(data, outcome, predictors) {
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  model <- glm(formula, data = data, family = binomial)
  return(summary(model))
}

# Specify the outcome variables and predictors for the model
outcome_vars <- c("severe_resp_infect_2wks", "cough_2_wks", "diarrhea_2_wks", "anemia")
predictors <- c("annual_PM25_scale", "URBAN_RURA_num", "cooking_toxicity_group_num", "smoking_house_dichot_num")

# Running the models for each health outcome
model_results <- lapply(outcome_vars, function(outcome) run_logistic_regression(India_child_environ_df, outcome, predictors))

# Extracting and storing the results
logistic_regression_summary <- map(model_results, broom::tidy)

# Visualizing the Results of Logistic Regression
# Create plots for each health outcome
plot_list <- lapply(logistic_regression_summary, function(model_summary) {
  ggplot(model_summary, aes(x = term, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
    geom_pointrange() +
    coord_flip() +
    theme_minimal() +
    labs(title = "Logistic Regression Results", x = "Predictors", y = "Estimate")
})

# Display the plots
plot_grid(plotlist = plot_list, ncol = 2)

# Geospatial Visualization of Health Data
# Creating a map to visualize the spatial distribution of a health outcome

# For example, mapping anemia prevalence
anemia_map <- st_as_sf(India_child_environ_df, coords = c("LONGNUM", "LATNUM"), crs = 4326) %>%
  filter(!is.na(anemia)) %>%
  ggplot() +
  geom_sf(aes(color = anemia)) +
  scale_color_viridis_c(option = "A") +
  labs(title = "Spatial Distribution of Anemia Prevalence", color = "Anemia Prevalence") +
  theme_minimal()

# Save the map
ggsave("anemia_prevalence_map.png", plot = anemia_map, width = 10, height = 7, dpi = 300)

# Additional analyses and visualizations can be inserted here following similar patterns.




####################################################################################################
# Title: Non-linear Statistical Modeling of Environmental Impact on Respiratory Infections in India
# Author: Paul George
# Date: [Date of Last Update]
# Description:
#   This script is designed for statistical analysis in a research project examining the impact of 
#   environmental factors on severe respiratory infections in children in India. It utilizes the 
#   mgcv package in R for generalized additive modeling (GAM/BAM) to analyze complex relationships 
#   between environmental variables and health outcomes.
#
#   Key operations include:
#   - Data preparation: Loading and preprocessing the environmental and child health data.
#   - Subsetting and cleaning the dataset: Filtering the data to ensure quality and relevance for the analysis.
#   - BAM analysis: Performing advanced statistical modeling to understand 
#     the impact of variables like air pollution, urban/rural settings, seasonal changes, and household 
#     factors on child health, specifically severe respiratory infections.
#   - Model evaluation and summarization: Assessing the model's performance and extracting key findings.
#
#   The results from this script will contribute to understanding the nuanced environmental impacts on 
#   child health in India, especially in the context of severe respiratory infections.
#
#
####################################################################################################

# Load necessary libraries
library(mgcv)
library(dplyr)

# Set working directory to an example project folder
setwd('<Your-Project-Folder-Path>')

# Clear current environment
rm(list = ls())

#### Data Preparation and Cleaning ####

# Load the dataset for analysis
load(file = '<Path-to-Data>/India_child_environ_df_model.rda')

# Convert the 'cluster_number' variable to a factor
India_child_environ_df_model$cluster_number <- as.factor(India_child_environ_df_model$cluster_number)

# Sample a subset of clusters for analysis
all_clusters <- unique(India_child_environ_df_model$cluster_number)
small_df <- India_child_environ_df_model[India_child_environ_df_model$cluster_number %in% all_clusters, ]

# Clean up memory
rm(India_child_environ_df_model)

# Select relevant variables and remove rows with any missing values
small_df <- small_df %>%
  select(severe_resp_infect_2wks, diarrhea_2_wks, anemia, annual_PM25, URBAN_RURA, hv040, v190F_dich, wt,
         season_of_interview, temp_month_of_interview, Precipitation_2020, Malaria_Prevalence_2020,
         cooking_toxicity_group, smoking_house_dichot, hqualityF, piped_water, ph_sani_improve,
         hw1, b4F, current_weight, birth_weight, cluster_number) %>%
  filter(complete.cases(.))

# Remove unused levels in the 'cluster_number' factor
small_df$cluster_number <- droplevels(small_df$cluster_number)

# Remove clusters with less than one observation
cluster_counts <- table(small_df$cluster_number)
sparse_clusters <- names(cluster_counts[cluster_counts <= 1])
small_df <- subset(small_df, !(cluster_number %in% sparse_clusters))
small_df$cluster_number <- droplevels(small_df$cluster_number)

#### BAM Analysis ####

# Record start time
start_time <- Sys.time()

# Run the BAM model
model_severe_BAM <- bam(severe_resp_infect_2wks ~ s(annual_PM25, k = 3) + URBAN_RURA + scale(hv040) +
                          season_of_interview + s(temp_month_of_interview, k = 4) + scale(Precipitation_2020) +
                          scale(Malaria_Prevalence_2020) + cooking_toxicity_group + smoking_house_dichot +
                          hqualityF + piped_water + ph_sani_improve + v190F_dich + scale(hw1) +
                          b4F + scale(current_weight) + scale(birth_weight) + s(cluster_number, bs = 're'),
                        data = small_df, family = binomial, method = 'fREML', discrete = TRUE, weights = small_df$wt)

# Record end time and calculate duration for reference
end_time <- Sys.time()
duration <- end_time - start_time

# Save the model
saveRDS(model_severe_BAM, file = '<Path-to-Output>/model_severe_BAM.rds')

# Print model summary
summary(model_severe_BAM, re.test = FALSE)







####################################################################################################
# Title: Advanced Visualization of Health Data Using Bayesian Additive Models
# Author: Paul George
# Date: 11/12/2023
# Description:
#   This script is a continuation of non-linear statistical analyses from above. 
#   It focuses on the visualization of model estimates, specifically looking at the relationships 
#   between environmental factors (like PM2.5 levels and temperature) and various health outcomes 
#   (respiratory infections, anemia) across different regions in India.
#
#   Key operations include:
#   - Loading pre-trained BAM models for different health outcomes.
#   - Extracting and transforming smooth components from the models for visualization.
#   - Applying log transformations to model estimates to interpret multiplicative relationships.
#   - Creating and saving detailed plots to visually represent the model findings, 
#     including odds ratios for health outcomes against environmental exposures.
#   - Customizing visualizations for specific regions in India to understand regional variations.

#
# Usage:
#   The script assumes that the necessary BAM models are already fitted and saved. It primarily deals with 
#   the post-processing and visualization of these models. 
#
####################################################################################################




# Load necessary libraries
library(mgcv)
library(dplyr)
library(gratia)
library(ggplot2)

# Load the BAM model for severe respiratory infection
model_severe_BAM = readRDS(file = '<Path-to-Model>/model_severe_BAM_3_6_23.rds')

#### Visualization of Smooth Components from the Model ####

# Extract and transform smooth estimates for annual PM2.5
scomp_PM25 <- smooth_estimates(model_severe_BAM, "s(annual_PM25)") %>%
  transform(fit = exp(est),
            lower = exp(est - 1.96 * se),
            upper = exp(est + 1.96 * se))

# Plotting the relationship between annual PM2.5 and respiratory infection odds ratio
plot_PM25 <- ggplot(data = scomp_PM25, aes(x = annual_PM25, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_vline(xintercept = 40, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(20, 120, by = 20)) +
  labs(title = "Respiratory Infection Model - PM2.5",
       y = "Odds ratio for respiratory infection",
       x = "Annual PM2.5 (µg/m³)") +
  theme_minimal()

# Saving the PM2.5 plot
ggsave('Resp_infection_PM25_smooth.png', plot = plot_PM25, height = 3.5, width = 4.5, dpi = 600)

# Repeat the process for temperature
scomp_temp <- smooth_estimates(model_severe_BAM, "s(temp_month_of_interview)") %>%
  transform(fit = exp(est),
            lower = exp(est - 1.96 * se),
            upper = exp(est + 1.96 * se))

plot_temp <- ggplot(data = scomp_temp, aes(x = temp_month_of_interview, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_x_continuous(breaks = seq(-0, 30, by = 10)) +
  labs(title = "Respiratory Infection Model - Temperature",
       y = "Odds ratio for respiratory infection",
       x = "Temperature (Celsius)") +
  theme_minimal()

# Saving the temperature plot
ggsave('Resp_infection_temp_smooth.png', plot = plot_temp, height = 3.5, width = 4.5, dpi = 600)

# Load the BAM model for anemia
model_anemia_BAM = readRDS(file = '<Path-to-Model>/model_anemia_BAM_3_9_23.rds')

# Extract and transform smooth estimates for annual PM2.5 in anemia model
scomp_anemia <- smooth_estimates(model_anemia_BAM, "s(annual_PM25)") %>%
  transform(fit = exp(est),
            lower = exp(est - 1.96 * se),
            upper = exp(est + 1.96 * se))

# Plotting the relationship between annual PM2.5 and anemia odds ratio
plot_anemia <- ggplot(data = scomp_anemia, aes(x = annual_PM25, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_vline(xintercept = 40, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(20, 120, by = 20)) +
  labs(title = "Anemia Model - PM2.5",
       y = "Odds ratio for anemia",
       x = "Annual PM2.5 (µg/m³)") +
  theme_minimal()

# Saving the anemia PM2.5 plot
ggsave('Anaemia_smooth_PM25.png', plot = plot_anemia, height = 3.5, width = 4.5, dpi = 600)

# Repeat for models focusing on specific regions (North, Central, West, East, Northeast, South)
# Each model focuses on a particular region and the relationship between PM2.5 and health outcomes

# Example for the North region
model_North_BAM = readRDS(file = '<Path-to-Model>/model_North_BAM.rds')

# Extract and transform smooth estimates for the North region model
scomp_north <- smooth_estimates(model_North_BAM, "s(annual_PM25)") %>%
  transform(fit = exp(est),
            lower = exp(est - 1.96 * se),
            upper = exp(est + 1.96 * se))

# Plotting for the North region
plot_north <- ggplot(data = scomp_north, aes(x = annual_PM25, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_vline(xintercept = 40, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(20, 120, by = 20)) +
  labs(title = "North Region - PM2.5",
       y = "Odds ratio for health outcome",
       x = "Annual PM2.5 (µg/m³)") +
  theme_minimal()

# Saving the North region plot
ggsave('North_PM25.png', plot = plot_north, height = 3.5, width = 4.5, dpi = 600)

# Similar code repeatedd for the Central, West, East, Northeast, and South regions

# Concluding the script, additional models for specific variables or regions can be analyzed and visualized following the same structure.


# Extract smooth estimates for annual PM2.5 from the severe respiratory infection model
scomp_severe <- smooth_estimates(model_severe_BAM, "s(annual_PM25)")

# Applying log transformation to the estimates and calculating confidence intervals
# This transformation is often used when the relationship is multiplicative rather than additive
scomp_severe_log <- transform(scomp_severe,
                              fit_log = log(est),
                              lower_log = log(est - 1.96 * se),
                              upper_log = log(est + 1.96 * se))

# Plotting the log-transformed relationship
plot_severe_log <- ggplot(data = scomp_severe_log, aes(x = annual_PM25, y = fit_log)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_log, ymax = upper_log), alpha = 0.2) +
  scale_x_continuous(breaks = seq(20, 120, by = 20)) +
  labs(title = "Log-Transformed - Severe Respiratory Infection Model",
       y = "Log Odds ratio",
       x = "Annual PM2.5 (µg/m³)") +
  theme_minimal()

# Saving the log-transformed plot for severe respiratory infection
ggsave('Severe_Resp_Infection_PM25_log.png', plot = plot_severe_log, height = 3.5, width = 4.5, dpi = 600)





