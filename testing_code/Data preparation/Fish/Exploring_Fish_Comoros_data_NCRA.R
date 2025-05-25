

#   Script Descriptions --------------------------------------------------

##  Name:         Exploring_Fish_Comoros_data_NCRA.R
##
#  Objective:    This template outlines the process of exploring Fish data for Comoros with a focus of NCRA analysis 
##                
##  Approach:     1. Import files, 
##                2. Visualise,
##                  2a. Boxplots National 
##                  2b. Boxplot Eco_region
##                  2c. Boxplot Year National
##                  2d. Boxplot Year Regional
##                  2e. Boxplot Management National
##                  2f. Boxplot Management Regional
##
##  Output files: 1. Object saved to *.rda. - Next script shall use this for further analysis
##                2. File saved to *.csv - Save clean replicate dataset
##
##  Authors:      Swaleh Aboud
##
##  Date:         18th April 2024
##

##  Notes:        



# 1. Setting working Directory and Import files ---------------------------------------------------------

# setting working directory
setwd("testing_code/")


load("../data_intermediate/Fish_NCRA_dataset_2025.RDA")

# Order values
Fish_NCRA_complete$`sub-national` <- factor(Fish_NCRA_complete$`sub-national`, 
                                            levels = c("Ngazidja", "Ndzuani", "Mwali","Reference"))


Fish_NCRA_complete$Management_level <- factor(Fish_NCRA_complete$Management_level, 
                      levels = c("National Park","Partially Restricted","Open access","Reference"))

Fish_NCRA_complete$family <- factor(Fish_NCRA_complete$family, 
                                              levels = c("Scarinae","Epinephelidae"))


# 2. Visualise ------------------------------------------------------------


library(ggplot2)


set.seed(123)  # For reproducibility
Fish_NCRA_complete$label_x <- jitter(rep(1, nrow(Fish_NCRA_complete)), amount = 0.2)

# Subset for labels
label_data <- subset(Fish_NCRA_complete,
                     (family == "Scarinae" & biomass.kg.ha. > 300) | 
                       (family == "Epinephelidae" & biomass.kg.ha. > 75))

ggplot(Fish_NCRA_complete, aes(x = Country, y = biomass.kg.ha.)) +
  geom_boxplot(fill = NA, aes(color = Country), outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.15, aes(color = Country), alpha = 0.5, size = 1.5) +
  geom_hline(data = data.frame(family = c("Scarinae", "Epinephelidae"),
                               threshold = c(300, 75)),
             aes(yintercept = threshold), linetype = "dashed", color = "red", inherit.aes = FALSE) +
  geom_text(data = label_data,
            aes(x = Country, y = biomass.kg.ha., label = paste(site_corrected, Year)),
            color = "red", size = 3, vjust = -0.7, inherit.aes = FALSE) +
  facet_wrap(~ family, scales = "free_y") +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12),
    axis.ticks.x = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    y = "Biomass (kg/ha)",
    x = ""
  )


ggsave("../figures/Fish/Explore_National_Biomass_Distribution_Families.png", width = 10, height = 6, dpi = 300)
 

# Initial vs current values
# Step 1: Create subsets with Group labels
current_values <- Fish_NCRA_complete %>%
  filter(Year >= 2016 & Year <= 2024) %>%
  mutate(Group = "Current \n (2016-2024)")

initial_complete <- Fish_NCRA_complete %>%
  filter(Eco_region == "Reference" | (Eco_region == "Mwali" & Year == 2018)) %>%
  mutate(Group = "Initial \n (Complete)")

initial_reference <- Fish_NCRA_complete %>%
  filter(Eco_region == "Reference") %>%
  mutate(Group = "Initial \n (Reference)")

mwali_2018 <- Fish_NCRA_complete %>%
  filter(Eco_region == "Mwali" & Year == 2018) %>%
  mutate(Group = "Moheli \n (2018)")

# Step 2: Combine all data
combined_data <- bind_rows(current_values, initial_complete, initial_reference, mwali_2018)

# Step 3: Order groups by mean biomass
group_means <- combined_data %>%
  group_by(Group) %>%
  summarise(mean_biomass = mean(biomass.kg.ha., na.rm = TRUE)) %>%
  arrange(mean_biomass)

# Reorder Group factor levels
combined_data$Group <- factor(combined_data$Group, levels = group_means$Group)

# Step 4: Plot
ggplot(combined_data, aes(x = Group, y = biomass.kg.ha.)) +
  geom_boxplot(aes(color = Group), fill = NA, outlier.shape = NA) +
  geom_jitter(aes(color = Group), width = 0.2, alpha = 0.6, size = 1.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3.5, color = "black") +
  facet_wrap(~ family, scales = "free_y") +
  theme_bw() +
  labs(y = "Biomass (kg/ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_blank())


ggsave("../figures/Fish/Explore_Initial_current_values.png", width = 10, height = 6, dpi = 300)

summar_comoros <- combined_data %>%
  group_by(Group) %>%
  summarise(average = round(mean(biomass.kg.ha.),2),
            sd = round(sd(biomass.kg.ha.),2),
            se = round(std.error(biomass.kg.ha.),2),
            minimum = min(biomass.kg.ha.),
            maximum = max(biomass.kg.ha.))

# 2b. Boxplot Eco_region --------------------------------------------------


# 2a. Boxplots National ---------------------------------------------------

ggplot(Fish_NCRA_complete, aes(x = `sub-national`, y = biomass.kg.ha.)) +  # Set x to 1 for one box per facet
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +          # Show all data points
  geom_hline(aes(yintercept = ifelse(family == "Scarinae", 300, 75)), 
             linetype = "dashed", color = "red") +                     # Different line for each family
  geom_text(
    data = subset(Fish_NCRA_complete, biomass.kg.ha. > ifelse(family == "Scarinae", 300, 75)), 
    aes(label = paste(site_corrected, Year), color = "red"),                  # Label with Station and Year
    size = 3, vjust = -0.5, hjust = 0.5  # Adjust position of labels
  ) +
  theme_bw() +
  theme(
    legend.position = "none",  # Remove the legend
    strip.text = element_text(size = 12),  # Ensure facet labels (family names) are visible
    axis.text.x = element_text(size = 12),  # Remove x-axis labels to avoid redundancy
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    panel.spacing = unit(1, "lines")  # Adjust spacing between facets
  ) +
  facet_wrap(~ family, scales = "free_y", labeller = label_value) +  # Facet by family, each facet gets its own y-axis range
  labs(
    y = "Biomass (kg/ha)",
    x = ""
  )

ggsave("../figures/Fish/Explore_Regions_Biomass_Distribution_Families.png", width = 10, height = 6, dpi = 300)



# 2c. Boxplot Year --------------------------------------------------------

Fish_NCRA_complete$Year <- as.factor(Fish_NCRA_complete$Year)

ggplot(Fish_NCRA_complete, aes(x = Year, y = biomass.kg.ha.)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  geom_hline(aes(yintercept = ifelse(family == "Scarinae", 300, 75)), 
             linetype = "dashed", color = "red") +
  geom_text(
    data = subset(Fish_NCRA_complete, biomass.kg.ha. > ifelse(family == "Scarinae", 300, 75)), 
    aes(label = paste(site_corrected, Year), color = "red"),
    size = 3, vjust = -0.5, hjust = 0.5
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.ticks.x = element_line(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(~ family, scales = "free_y", labeller = label_value) +
  labs(
    y = "Biomass (kg/ha)",
    x = "Year"
  )

ggsave("../figures/Fish/Explore_National_Year_Biomass_Distribution_Families.png", width = 10, height = 6, dpi = 300)


# 2d. Boxplot Year Regional -----------------------------------------------

ggplot(Fish_NCRA_complete, aes(x = Year, y = biomass.kg.ha.)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  geom_hline(aes(yintercept = ifelse(family == "Scarinae", 300, 75)), 
             linetype = "dashed", color = "red") +
  geom_text(
    data = subset(Fish_NCRA_complete, biomass.kg.ha. > ifelse(family == "Scarinae", 300, 75)), 
    aes(label = paste(site_corrected, Year), color = "red"),
    size = 3, vjust = -0.5, hjust = 0.5
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.ticks.x = element_line(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_grid(family ~ `sub-national`, scales = "free_y") +  # Allow each family its own y-axis scale
  labs(
    y = "Biomass (kg/ha)",
    x = "Year"
  )

ggsave("../figures/Fish/Explore_Regional_Year_Biomass_Distribution_Families.png", width = 10, height = 6, dpi = 300)



# 2e. Boxplot Management National --------------------------------------------


Fish_NCRA_complete$Management_level[which(Fish_NCRA_complete$Management_level=="Partially restricted")] <- "Partially Restricted"

ggplot(Fish_NCRA_complete, aes(x = Management_level, y = biomass.kg.ha.)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  geom_hline(aes(yintercept = ifelse(family == "Scarinae", 300, 75)), 
             linetype = "dashed", color = "red") +
  geom_text(
    data = subset(Fish_NCRA_complete, biomass.kg.ha. > ifelse(family == "Scarinae", 300, 75)), 
    aes(label = paste(site_corrected, Year), color = "red"),
    size = 3, vjust = -0.5, hjust = 0.5
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.ticks.x = element_line(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(~ family, scales = "free_y", labeller = label_value) +
  labs(
    y = "Biomass (kg/ha)",
    x = "Year"
  )

ggsave("../figures/Fish/Explore_National_Management_Biomass_Distribution_Families.png", width = 10, height = 6, dpi = 300)



# 2f. Boxplot Management Regional -----------------------------------------

ggplot(Fish_NCRA_complete, aes(x = Management_level, y = biomass.kg.ha.)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  geom_hline(aes(yintercept = ifelse(family == "Scarinae", 300, 75)), 
             linetype = "dashed", color = "red") +
  geom_text(
    data = subset(Fish_NCRA_complete, biomass.kg.ha. > ifelse(family == "Scarinae", 300, 75)), 
    aes(label = paste(site_corrected, Year), color = "red"),
    size = 3, vjust = -0.5, hjust = 0.5
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.ticks.x = element_line(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_grid(family ~ `sub-national`, scales = "free_y") +  # Allow each family its own y-axis scale
  labs(
    y = "Biomass (kg/ha)",
    x = "Year"
  )

ggsave("../figures/Fish/Explore_Regional_Management_Biomass_Distribution_Families.png", width = 10, height = 6, dpi = 300)


# Criteria D

load("data_intermediate/create_criterion_d_iterations_fish.rda")

create_criterion_iteration_summary_fish$status <- factor(
  create_criterion_iteration_summary_fish$status,
  levels = c("NT","LC","VU","EN","CR")
)

# Create the plot
ggplot(create_criterion_iteration_summary_fish, 
       aes(x = fish, y = percent, fill = status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c(
    "CR" = "red",
    "EN" = "orange",
    "VU" = "yellow",
    "LC" = "#1a9641",
    "NT" = "green"
  )) +
  facet_grid(Method ~ Ecoregion) +
  labs(y = "Percent") +
  theme_bw() +
  theme(axis.title = element_blank())


ggsave("figures/Fish/Explore_iterations_subnational_Families.png", width = 10, height = 6, dpi = 300)








 




