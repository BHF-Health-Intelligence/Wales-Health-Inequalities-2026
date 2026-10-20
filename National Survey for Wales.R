#Analysis for the Wales health inequalities report 2026
#Risk factor data from the National Survey for Wales
#Maya Trivedy
source("Packages and style.R")
source("Style Script Test 1 1.R")

#import the subset of relevant indicators
nsw <- read_excel("national survey for wales data subset.xlsx")
table(nsw$Question)
#This subset includes 10 survey questions related to CVD risk, which had a breakdown by deprivation.
#The two measures of deprivation used were WIMD and material deprivation (yes/no)
#So many NAs

#Household material deprivation subset, filter out question about hearing
household_mat_dep <- nsw %>%
  filter(`Breakdown question`== "Household material deprivation (FG indicator)") %>%
  filter(Response == "Yes") %>%
  filter(!grepl("hearing", Question))

household_mat_dep$Question <- gsub("Eaten 5+ fruit or veg the previous day - binary", "Eaten 5+ fruit or veg the previous day", household_mat_dep$Question,
                                   fixed = TRUE)
household_mat_dep$Question <- gsub("Has a limiting long-standing illness, disability or infirmity", "Has a limiting long-standing illness or disability", household_mat_dep$Question)
household_mat_dep$Question <- gsub(
  "Participation in sporting activities three or more times a week (FG indicator)",
  "Participation in sporting activities 3+ times a week",
  household_mat_dep$Question,
  fixed = TRUE)
household_mat_dep$Question <- gsub("Latent demand for any activity", "Unmet demand for any physical activity", household_mat_dep$Question)



#plot responses to questions with data stratified by household material deprivation

household_mat_dep_plot <- ggplot(household_mat_dep %>%
                                   filter(grepl("fruit|illness|smoke|sporting", Question)), 
                                 aes(x = `Breakdown response`, y = Value, fill = `Breakdown response`)) +
  geom_col() +
  geom_errorbar(aes(ymin = `Lower Confidence Interval`, ymax = `Upper Confidence Interval`, width = 0.6)) +
  scale_fill_manual(
    values = c(
      "#58339e",  
      "#c7e1fb"   ) ) +
  facet_wrap(~Question, labeller = labeller(Question = function(x) str_wrap(x, width = 30))) +
  bhf_bar_plot(textsize = 33) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,67))+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.caption = element_text(lineheight = 0.3),
        strip.text = element_text(size = 18, lineheight = 0.3)) +
  labs(title = "Figure A1: Prevalence of lifestyle factors related to CVD risk by household \ndeprivation, Wales, 2024-25",
       subtitle = "Error bars represent the 95% confidence interval",
       y = "Proportion (%)",
       caption = "Source: National Survey for Wales",
       fill = "Deprivation status:",) +
  theme(plot.title = element_text(vjust = 2),
        title = element_text(lineheight = 0.5),
        strip.text = element_text(size = 30, lineheight = 0.5),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.position = "bottom"
  )

household_mat_dep_plot
ggsave("graphs/household_mat_dep_plot.png", household_mat_dep_plot, device = "png", width = 15,
       height = 15, units = "cm", dpi = 300)




nsw_2022 <- read_excel("nsw 2022.xlsx") #This contains data on smoking and obesity by welsh index of derivation quintile for 2022/23


my_palette <- c(
  "#58339E",
  "#6763BB",
  "#7C8FD4",
  "#9CB9E8",
  "#C7E1FB"
)

smoking_2022_plot <- ggplot(nsw_2022 %>% filter(Risk_factor == "smoking") %>% filter(Quintile != 0), 
                            aes(x = factor(Quintile), y = Prevalence, fill = factor(Quintile))) +
  geom_col() +
  geom_text(aes(label = round(Prevalence, 1)),
            vjust = -0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,25)) +
  scale_x_discrete() +
  scale_fill_manual(values = my_palette) +
  bhf_bar_plot(textsize = 33) +
  labs(title = "Figure 4: Prevalence of cigarette smoking in adults in Wales by deprivation quintile, 2022/23",
       subtitle = str_wrap("Quintile 1 = most deprived, quintile 5 = least deprived"),
       y = "Prevalence (%)",
       x = "Deprivation quintile",
       caption = "Source: National Survey for Wales") +
  theme(legend.position = "none",
        plot.title = element_text(vjust = 2),
        title = element_text(lineheight = 0.5),
  )

ggsave("graphs/smoking 2022.png", smoking_2022_plot, device = "png", width = 18,
       height = 12, units = "cm", dpi = 300)




obesity_2022_plot <- ggplot(nsw_2022 %>%
                              filter(Risk_factor == "bmi", Quintile != 0),
                            aes(x = factor(Quintile), y = Prevalence, fill = factor(Quintile))) +
  geom_col() +
  geom_text(aes(label = round(Prevalence, 1)),
            vjust = -0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,35)) +
  scale_x_discrete() +
  scale_fill_manual(values = my_palette) +
  bhf_bar_plot(textsize = 33) +
  labs(title = "Figure 3: Prevalence of obesity among adults in Wales by deprivation quintile, 2022/23",
       subtitle = str_wrap("Quintile 1 = most deprived, quintile 5 = least deprived"),
       y = "Prevalence (%)",
       x = "Deprivation quintile",
       caption = "Source: National Survey for Wales") +
  theme(legend.position = "none",
        plot.title = element_text(vjust = 2),
        title = element_text(lineheight = 0.5),
  )

ggsave("graphs/obesity 2022.png", obesity_2022_plot, device = "png", width = 17,
       height = 12, units = "cm", dpi = 300)