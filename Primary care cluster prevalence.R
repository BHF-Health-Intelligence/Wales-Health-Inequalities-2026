#Analysis of PCC data for the Wales health inequalities report 2026
#Maya Trivedy
source("Packages and style.R")
source("Style Script Test 1 1.R")
#install.packages("ggtext")
library(ggtext)


#Import PCC data: chronic conditions, emergency admissions, mortality trends and cluster deprivation data
PCC_chronic_conditions <- read_excel("PCC chronic conditions data.xlsx")
PCC_emergency_admissions <- read_excel("PCC emergency admisions data.xlsx")
PCC_mortality <- read_excel("PCC mortality trend data.xlsx")
cluster_deprivation <- read_excel("deprivation-at-gp-cluster-level-v2-en-gb.xlsx") #https://stats.gov.wales/en-GB/9837e7d2-31bd-4813-90af-bdd8d9c20cc4/start



#Clean up datasets and merge clinical data with deprivation

#Chronic conditions data
PCC_chronic_conditions$LCL[!grepl("^[0-9.]+$", PCC_chronic_conditions$LCL)] <- ""
PCC_chronic_conditions$UCL[!grepl("^[0-9.]+$", PCC_chronic_conditions$UCL)] <- ""
PCC_chronic_conditions$Value <- as.numeric(PCC_chronic_conditions$Value)



cluster_deprivation <- cluster_deprivation %>%
  filter(Method == "Deprivation based on percentage of cluster population in most deprived 20% of WIMD") %>%
  filter(grepl("1|4", `Data description_reference`)) %>% #filter data for cluster population sizes and cluster deprivation quintile. We aren't interested in the rest
  filter(!is.na(`Data values`)) %>% 
  filter(Date == "10/04/2025") %>%
  select(`Data values`, `Data description`, Cluster) %>%
  pivot_wider(names_from = `Data description`, values_from = `Data values`)

#Check cluster population sizes
cluster_deprivation_population <- cluster_deprivation %>%
  group_by(`Cluster deprivation quintile`) %>%
  summarise(total_population = sum(`Cluster population`))
knitr::kable(cluster_deprivation_population)

# | Cluster deprivation quintile| total_population|
#   |----------------------------:|----------------:|
#   |                            1|           643181|
#   |                            2|           673748|
#   |                            3|           687046|
#   |                            4|           661323|
#   |                            5|           617994|

#Before joining deprivation data to prevalence data, some of the area names need to be edited so they match

cluster_deprivation$Cluster <- gsub("Amman Gwendraeth", "Amman/Gwendraeth", cluster_deprivation$Cluster)
cluster_deprivation$Cluster <- gsub("Central & South Denbighshire", "Central and South Denbighshire", cluster_deprivation$Cluster)
cluster_deprivation$Cluster <- gsub("City & Cardiff South", "City and Cardiff South", cluster_deprivation$Cluster)
cluster_deprivation$Cluster <- gsub("Dwyfor & North Meirionnydd", "Dwyfor and North Meirionnydd", cluster_deprivation$Cluster)
cluster_deprivation$Cluster <- gsub("North & West Wrexham", "North and West Wrexham", cluster_deprivation$Cluster)
cluster_deprivation$Cluster <- gsub("Tywi / Taf", "Taf / Tywi", cluster_deprivation$Cluster)

#Join chronic conditions data to deprivation quintile data
PCC_chronic_conditions_WIMDQ <- PCC_chronic_conditions %>%
  filter(Area_broad == "Cluster") %>%
  left_join(cluster_deprivation, by = c("Area" = "Cluster")) %>%
  select(Area, Condition, Sex, Measure, Value, LCL, UCL, `Cluster population`, `Cluster deprivation quintile`)

#caluclate weighted average rates for each deprivation quintile
weighted_avg_rates <- PCC_chronic_conditions_WIMDQ %>%
  left_join(cluster_deprivation_population) %>%
  mutate(weighting = `Cluster population`/total_population) %>%
  filter(Measure == "EASR per 100,000") %>%
  group_by(Condition, Sex, `Cluster deprivation quintile`) %>%
  summarise(w_mean = weighted.mean(Value, weighting, na.rm = TRUE)) %>% #calculates a weighted mean %>%
  select(Condition, Sex, `Cluster deprivation quintile`, w_mean)

names(weighted_avg_rates)[3] <- "Quintile"
names(PCC_chronic_conditions_WIMDQ)[9] <- "Quintile"

#Bind the weighted average rates to the chronic conditions data, so that you can plot them on the same graph
#reformulate columns for weighted averages data so that it binds
PCC_clean_data <- weighted_avg_rates %>%
  rename(Value = w_mean) %>%
  mutate(Area = "") %>%
  mutate(Measure = "weighted_mean") %>%
  mutate(LCL= "") %>%
  mutate(UCL = "") %>%
  mutate('Cluster population' = NA_real_) %>%
  rbind(PCC_chronic_conditions_WIMDQ) %>%
  filter(grepl("EASR|weighted_mean", Measure)) %>%
  mutate(Prevalence = Value/1000) %>%
  select(Measure, Area, Condition, Sex, Prevalence, LCL, UCL, `Cluster population`, Quintile)




diabetes_prev_plot<- ggplot() +
  geom_point(data = PCC_clean_data %>% 
               filter(Measure == "EASR per 100,000") %>%
               filter(Sex == "Persons")%>% 
               filter(Condition == "Diabetes Register"), 
             aes(x = Quintile, y = Prevalence, 
                 size = `Cluster population`),
             alpha = 0.8,
             colour = "#C7E1FB") +
  geom_point(data = PCC_clean_data %>% 
               filter(Measure == "weighted_mean") %>%
               filter(Sex == "Persons")%>% 
               filter(Condition == "Diabetes Register"), 
             aes(x = Quintile, y = Prevalence),
             colour = "#58339E",
             size = 4) +
  scale_size(range = c(1, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,12.2))+
  bhf_line_plot(textsize = 33) +
  labs(title = str_wrap("Figure 2: Age-standardised prevalence of diabetes among GP-registered paitents in Primary Care Clusters in Wales, 2024", 80),
       subtitle = paste0(
         "Size of <span style='color:#66ADF4;'>light blue</span> points corresponds to the number of patients registered in the cluster<br>",
         "<span style='color:#58339E;'>Purple</span> points represent the mean prevalence among clusters in each quintile, weighted by list size"
       ),
       y = "Prevalence (%)",
       x = "Deprivation Quintile (1 = most deprived, 5 = least deprived)",
       caption = "Source: Audit+, Digital Health and Care Wales") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none",
    plot.subtitle = ggtext::element_markdown(
      lineheight = 0.5,
      size = 31,
      margin = margin(b = 10)
    ),
    plot.title = element_text(
      lineheight = 0.5,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      margin = margin(t = 10)
    )
  )


ggsave("graphs/diabetes_prev_plot.png", diabetes_prev_plot, device = "png", width = 17,
       height = 12, units = "cm", dpi = 300)

hypertension_prev_plot<- ggplot() +
  geom_point(data = PCC_clean_data %>% 
               filter(Measure == "EASR per 100,000") %>%
               filter(Sex == "Persons")%>% 
               filter(Condition == "Hypertension Register"), 
             aes(x = Quintile, y = Prevalence, 
                 size = `Cluster population`),
             alpha = 0.8,
             colour = "#C7E1FB") +
  geom_point(data = PCC_clean_data %>% 
               filter(Measure == "weighted_mean") %>%
               filter(Sex == "Persons")%>% 
               filter(Condition == "Hypertension Register"), 
             aes(x = Quintile, y = Prevalence),
             colour = "#58339E",
             size = 4) +
  scale_size(range = c(1, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,20))+
  bhf_line_plot(textsize = 33) +
  labs(title = str_wrap("Figure 1: Age-standardised prevalence of diagnosed hypertension among GP-registered paitents in Primary Care Clusters in Wales, 2024", 90),
       subtitle = paste0(
         "Size of <span style='color:#66ADF4;'>light blue</span> points corresponds to the number of patients registered in the cluster<br>",
         "<span style='color:#58339E;'>Purple</span> points represent the mean prevalence among clusters in each quintile, weighted by list size"
       ),
       y = "Prevalence (%)",
       x = "Deprivation Quintile (1 = most deprived, 5 = least deprived)",
       caption = "Source: Audit+, Digital Health and Care Wales") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none",
    plot.subtitle = ggtext::element_markdown(
      lineheight = 0.5,
      size = 31,
      margin = margin(b = 10)),
    plot.title = element_text(
      lineheight = 0.5,
      margin = margin(b = 10)),
    plot.caption = element_text(
      margin = margin(t = 10))
  )


ggsave("graphs/hypertension_prev_plot.png", hypertension_prev_plot, device = "png", width = 17,
       height = 12, units = "cm", dpi = 300)

heartfailure_prev_plot<- ggplot() +
  geom_point(data = PCC_clean_data %>% 
               filter(Measure == "EASR per 100,000") %>%
               filter(Sex == "Persons")%>% 
               filter(Condition == "Heart Failure Register"), 
             aes(x = Quintile, y = Prevalence, 
                 size = `Cluster population`),
             alpha = 0.8,
             colour = "#C7E1FB") +
  geom_point(data = PCC_clean_data %>% 
               filter(Measure == "weighted_mean") %>%
               filter(Sex == "Persons")%>% 
               filter(Condition == "Heart Failure Register"), 
             aes(x = Quintile, y = Prevalence),
             colour = "#58339E",
             size = 4) +
  scale_size(range = c(1, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,2))+
  bhf_line_plot(textsize = 33) +
  labs(title = str_wrap("Figure 6: Age-standardised prevalence of heart failure among GP-registered paitents in Primary Care Clusters in Wales, 2024", 80),
       subtitle = paste0(
         "Size of <span style='color:#66ADF4;'>light blue</span> points corresponds to the number of patients registered in the cluster<br>",
         "<span style='color:#58339E;'>Purple</span> points represent the mean prevalence among clusters in each quintile, weighted by list size"
       ),
       y = "Prevalence (%)",
       x = "Deprivation Quintile (1 = most deprived, 5 = least deprived)",
       caption = "Source: Audit+, Digital Health and Care Wales") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none",
    plot.subtitle = ggtext::element_markdown(
      lineheight = 0.5,
      size = 31,
      margin = margin(b = 10)),
    plot.title = element_text(
      lineheight = 0.5,
      margin = margin(b = 10)),
    plot.caption = element_text(
      margin = margin(t = 10))
  )


ggsave("graphs/heartfailure_prev_plot.png", heartfailure_prev_plot, device = "png", width = 17,
       height = 12, units = "cm", dpi = 300)


chd_prev_plot<- ggplot() +
  geom_point(data = PCC_clean_data %>% 
               filter(Measure == "EASR per 100,000") %>%
               filter(Sex == "Persons")%>% 
               filter(Condition == "Secondary Prevention of Coronary Heart Disease Register"), 
             aes(x = Quintile, y = Prevalence, 
                 size = `Cluster population`),
             alpha = 0.8,
             colour = "#C7E1FB") +
  geom_point(data = PCC_clean_data %>% 
               filter(Measure == "weighted_mean") %>%
               filter(Sex == "Persons")%>% 
               filter(Condition == "Secondary Prevention of Coronary Heart Disease Register"), 
             aes(x = Quintile, y = Prevalence),
             colour = "#58339E",
             size = 4) +
  scale_size(range = c(1, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,4.1))+
  bhf_line_plot(textsize = 33) +
  labs(title = str_wrap("Figure 5: Age-standardised prevalence of coronary heart disease among GP-registered paitents in Primary Care Clusters in Wales, 2024", 80),
       subtitle = paste0(
         "Size of <span style='color:#66ADF4;'>light blue</span> points corresponds to the number of patients registered in the cluster<br>",
         "<span style='color:#58339E;'>Purple</span> points represent the mean prevalence among clusters in each quintile, weighted by list size"
       ),
       y = "Prevalence (%)",
       x = "Deprivation Quintile (1 = most deprived, 5 = least deprived)",
       caption = "Source: Audit+, Digital Health and Care Wales") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none",
    plot.subtitle = ggtext::element_markdown(
      lineheight = 0.5,
      size = 31,
      margin = margin(b = 10)),
    plot.title = element_text(
      lineheight = 0.5,
      margin = margin(b = 10)),
    plot.caption = element_text(
      margin = margin(t = 10))
  )


ggsave("graphs/chd_prev_plot.png", chd_prev_plot, device = "png", width = 17,
       height = 12, units = "cm", dpi = 300)


stroke_prev_plot<- ggplot() +
  geom_point(data = PCC_clean_data %>% 
               filter(Measure == "EASR per 100,000") %>%
               filter(Sex == "Persons")%>% 
               filter(Condition == "Stroke & TIA Register"), 
             aes(x = Quintile, y = Prevalence, 
                 size = `Cluster population`),
             alpha = 0.8,
             colour = "#C7E1FB") +
  geom_point(data = PCC_clean_data %>% 
               filter(Measure == "weighted_mean") %>%
               filter(Sex == "Persons")%>% 
               filter(Condition == "Stroke & TIA Register"), 
             aes(x = Quintile, y = Prevalence),
             colour = "#58339E",
             size = 4) +
  scale_size(range = c(1, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,3))+
  bhf_line_plot(textsize = 33) +
  labs(title = str_wrap("Figure 7: Age-standardised prevalence of stroke & TIA among GP-registered paitents in Primary Care Clusters in Wales, 2024", 80),
       subtitle = paste0(
         "Size of <span style='color:#66ADF4;'>light blue</span> points corresponds to the number of patients registered in the cluster<br>",
         "<span style='color:#58339E;'>Purple</span> points represent the mean prevalence among clusters in each quintile, weighted by list size"
       ),
       y = "Prevalence (%)",
       x = "Deprivation Quintile (1 = most deprived, 5 = least deprived)",
       caption = "Source: Audit+, Digital Health and Care Wales") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none",
    plot.subtitle = ggtext::element_markdown(
      lineheight = 0.5,
      size = 31,
      margin = margin(b = 10)),
    plot.title = element_text(
      lineheight = 0.5,
      margin = margin(b = 10)),
    plot.caption = element_text(
      margin = margin(t = 10))
  )


ggsave("graphs/stroke_prev_plot.png", stroke_prev_plot, device = "png", width = 17,
       height = 12, units = "cm", dpi = 300)


