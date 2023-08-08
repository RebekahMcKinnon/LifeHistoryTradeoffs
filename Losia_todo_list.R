##### load packages -----
library(tidyverse)
library(dplyr)
library(metafor)
library(forestplot)
library(tidyr)
library(ggplot2)

##### load data -----
data <- read.csv("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/PDFs/Extractions 2023.06.26.csv")
life_hist <- read.csv("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/PDFs/life_hist_records.csv")
names(data)


# one value for each subjectID
new_data <- data %>%
  group_by(SubjectID) %>%
  slice(1) %>%
  ungroup()

names(new_data)

# Make plots of continuous variables 

ggplot(new_data, aes(x = ManipDay)) +
  geom_histogram(fill="lightsalmon2") +
  ggtitle("Histogram of Day Post-Hatch on Which Nestlings Were Transferred to Form Manipulated Broods")  +
  theme_minimal()

ggplot(new_data, aes(x = control_broodsize)) +
  geom_histogram(fill="lightsalmon2", binwidth = 1) +
  ggtitle("Histogram of (Average) Brood Size") +
  theme_minimal()

ggplot(new_data, aes(x = Treatment_CONT)) +
  geom_histogram(fill="lightsalmon2", binwidth = 1) +
  ggtitle("Histogram of Number of Eggs/Nestlings Added/Removed")  +
  theme_minimal()

ggplot(new_data, aes(x = Nnests_control)) +
  geom_histogram(fill="lightsalmon2", binwidth = 1) +
  ggtitle("Histogram of the Number of Nests in the Control Group") +
  theme_minimal()

ggplot(new_data, aes(x = Nnests_treat)) +
  geom_histogram(fill="lightsalmon2", binwidth = 1) +
  ggtitle("Histogram of the Number of Nests in the Treatment Groups") +
  theme_minimal() 


treat_ave <- cor(new_data$Treatment_CONT, new_data$control_broodsize)
treat_ave

names(life_hist)

ggplot(life_hist, aes(x =Age_at_first_repo )) +
  geom_histogram(fill="lightsalmon3", binwidth = 1) +
  ggtitle("Histogram of the Age at First Repoduction") +
  theme_minimal()

ggplot(life_hist, aes(x = fledging_period)) +
  geom_histogram(binwidth = 1, fill="lightsalmon3") +
  ggtitle("Histogram of Average Fledging Period") +
  theme_minimal()

ggplot(life_hist, aes(x = Max_lifespan)) +
  geom_histogram(fill="lightsalmon2", binwidth = 1) +
  ggtitle("Histogram of the (Max) Lifespan") +
  theme_minimal()

# correlations 

first_period <- cor(life_hist$Age_at_first_repo, life_hist$fledging_period)
first_period # 0.74

first_span <- cor(life_hist$Age_at_first_repo, life_hist$Max_lifespan)
first_span # 0.82

span_period <-cor(life_hist$fledging_period, life_hist$Max_lifespan)
span_period # 0.84





# plot of D values 
feeding_group <-subset(data, RespCat == "FeedingRates")

names(data)

ggplot(feeding_group, aes(x = Treatment, y = D, color = Treatment)) +
  geom_point() +
  theme_minimal()

# Calculate counts of estimates for each category level in 'RespCat' column
resp_cat_counts <- table(data$RespCat)
print(resp_cat_counts)
# Calculate counts of estimates for each category level in 'ResponseVar' column
response_var_counts <- table(data$ResponseVar)
print(response_var_counts)


# Create bar plot for 'RespCat' counts
data_filtered <- subset(data, RespCat != "NA")

ggplot(data_filtered, aes(x = RespCat)) +
  geom_bar(fill = "lightsalmon2") +
  ggtitle("Counts of Estimates by RespCat") +
  theme_minimal()
##


# Cross-tabulation of two categorical variables
cross_table <- table(new_data$FocalSpC, new_data$TreatDurCat)

# Chi-square test for independence
chisq_result <- chisq.test(cross_table)

# Print the cross-tabulation and chi-square test results
print(cross_table)
print(chisq_result)

# there is overlap between species and treatment duration
# both long and short duration conducted in blue tits (2 and 2 subjectID) and Great tits (10 and 3)
# all other species only long (mostly) or only short (2)


# Cross-tabulation of two categorical variables
cross_table <- table(new_data$FocalSpC, new_data$RespCat)

# Chi-square test for independence
chisq_result <- chisq.test(cross_table)

# Print the cross-tabulation and chi-square test results
print(cross_table)
print(chisq_result)

# also between species and response category 
# some response categories are only tested in 1 species e.g., feeding rates per nestling is only in Northern Flickers
# but overall feeding rates tested in most species 
# 26 unique species total 

# only 2-3 studies didnt look at both enlarged and reduced (only looked at 1) so no overlap between this and any other category 

## lots of studies probably underpowered (many small sample sizes)
# check if effect size is correlated with sample size
names(data)
consample_D <- cor(data$Nnests_control, data$D)
consample_D

# 0.065
# no correlation between sample size of control nests and D 

treatsample_D <- cor(data$Nnests_treat, data$D)
treatsample_D
#0.006
# again no correlation between sample size of treatment nests and D 


# Create box plots for each moderator
boxplot_effect <- function(moderator) {
  plot_data <- data.frame(D = data$D, Moderator = data[[moderator]])
  plot_title <- paste("Effect Size (D) by", moderator)
  
  # Create the box plot using ggplot2
  ggplot(plot_data, aes(x = Moderator, y = D)) +
    geom_boxplot() +
    labs(title = plot_title, x = moderator, y = "Effect Size (D)") +
    theme_bw()
}

# List of moderators
moderators <- c("FocalSpC", "Treatment", "Treatment_CONT", "Treatment_stage", "TreatDurCat", "RespCat")

# Create box plots for each moderator
for (moderator in moderators) {
  boxplot_effect(moderator)
}



# Create box plots for each moderator
boxplot_effect <- function(moderator) {
  plot_data <- data.frame(D = data$D, Moderator = data[[moderator]])
  plot_title <- paste("Effect Size (D) by", moderator)
  
  # Create the box plot using ggplot2
  plot_obj <- ggplot(plot_data, aes(x = Moderator, y = D)) +
    geom_boxplot() +
    labs(title = plot_title, x = moderator, y = "Effect Size (D)") +
    theme_bw()
  
  print(plot_obj)
}

# List of moderators
moderators <- c("FocalSpC", "Treatment", "Treatment_CONT", "Treatment_stage", "TreatDurCat", "RespCat")

# Create and display box plots for each moderator
for (moderator in moderators) {
  boxplot_effect(moderator)
}


##### Calculate Hedges G -----
# already have values for Cohen's D in the spreadsheet ('D')
# but calculating Hedges G to use as it is better at dealing with small study sample size 


effect_sizes <- escalc(m1i = data$Treatment_Mean, m2i = data$Control_Mean,# using escalc function to calculate hedges G
                       sd1i = data$Treatment_SD, sd2i = data$Control_SD,
                       n1i = data$Nnests_treat, n2i = data$Nnests_control,
                       measure = "SMD")

# add new columns back into dataframe 
data <- bind_cols(data, effect_sizes)

##### Meta-analysis -----
meta_result <- rma(yi = data$yi, vi = data$vi)
meta_result

data <- data %>%
  mutate(EffectID = row_number())

meta_result2 <- rma.mv(yi = yi, V=vi, random = list(~1 |RecNo, ~1 |EffectID), data = data)
meta_result2
# RecNo is an identifier for each STUDY i.e., their Endnote record number 
# EffectID is just row numbers 
# is this correct? 

meta_result3 <- rma.mv(yi = yi, V=vi, mods = ~Treatment, random = list(~1 |RecNo, ~1 |EffectID), data = data)
meta_result3

combined_data <- left_join(data, life_hist, by = c("FocalSpC"))

meta_result4 <- rma.mv(yi = yi, V=vi, mods = ~Treatment + Lifespan_ave, random = list(~1 |RecNo, ~1 |EffectID), data = combined_data)
meta_result4

#####
# Calculate the mean G value for the enlarged group
combined_data$enlarged_mean <- mean(combined_data$yi[combined_data$Treatment == "enlarged"], na.rm = TRUE)

# Calculate the mean G value for the reduced group
combined_data$reduced_mean <- mean(combined_data$yi[combined_data$Treatment == "reduced"], na.rm = TRUE)

# Calculate the lower and upper CIs for the enlarged group
combined_data$enlarged_lower_ci <- quantile(combined_data$yi[combined_data$Treatment == "enlarged"], 0.025, na.rm = TRUE)
combined_data$enlarged_upper_ci <- quantile(combined_data$yi[combined_data$Treatment == "enlarged"], 0.975, na.rm = TRUE)

# Calculate the lower and upper CIs for the reduced group
combined_data$reduced_lower_ci <- quantile(combined_data$yi[combined_data$Treatment == "reduced"], 0.025, na.rm = TRUE)
combined_data$reduced_upper_ci <- quantile(combined_data$yi[combined_data$Treatment == "reduced"], 0.975, na.rm = TRUE)

##### make figures for meta-analysis results

ggplot() +
  geom_point(data = subset(combined_data, Treatment == "enlarged"), aes(x = enlarged_mean, y = Treatment), size = 5, color="tomato2", shape="square") +
  geom_errorbarh(data = subset(combined_data, Treatment == "enlarged"), aes(xmin = enlarged_lower_ci, xmax = enlarged_upper_ci, y = Treatment), color="tomato2", height = 0.2) +
  geom_point(data = subset(combined_data, Treatment == "reduced"), aes(x = reduced_mean, y = Treatment), size = 5,color="cornflowerblue", shape="square") +
  geom_errorbarh(data = subset(combined_data, Treatment == "reduced"), aes(xmin = reduced_lower_ci, xmax = reduced_upper_ci, y = Treatment), color="cornflowerblue", height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray", size=1) +
  labs(x = "Effect Size (Hedges' g)", y = "", color="Max_lifespan") +
  guides(color = FALSE) +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 12, face="bold"),
        axis.title.x = element_text(size = 12, face = "bold"))
###


ggplot() +
  geom_jitter(data = combined_data, aes(y = Lifespan_ave, x = yi, color = Treatment), width = 0.1, height = 0, size = 3, alpha = 0.5) +
  labs(x = "Effect Size (Hedges' g)", y = "Longevity", color = "Treatment") +
  scale_color_manual(values = c("reduced" = "cornflowerblue", "enlarged" = "tomato2")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size=12, face="bold"))
###
ggplot() +
  geom_jitter(data = combined_data, aes(y = Lifespan_ave, x = yi, color = Treatment), width = 0.1, height = 0, size = 3, alpha = 0.5) +
  labs(x = "Effect Size (Hedges' g)", y = "Longevity", color = "Treatment") +
  scale_color_manual(values = c("reduced" = "cornflowerblue", "enlarged" = "tomato2")) +
  geom_vline(xintercept = -1.633603, linetype = "dashed", color = "cornflowerblue", size = 0.5) +
  geom_vline(xintercept = 1.014291, linetype = "dashed", color = "cornflowerblue", size = 0.5) +
  geom_vline(xintercept = -0.8457002, linetype = "dashed", color = "tomato2", size = 0.5) +
  geom_vline(xintercept = 1.56096, linetype = "dashed", color = "tomato2", size = 0.5) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))


#####


# Create box plots for each moderator
boxplot_effect <- function(moderator) {
  plot_data <- data.frame(G = combined_data$yi, Moderator = combined_data[[moderator]])
  plot_title <- paste("Effect Size (G) by", moderator)
  
  # Create two separate panels for enlarged and reduced Treatment with color mapping
  plot_obj <- ggplot(plot_data, aes(x = Moderator, y = G, fill = combined_data$Treatment)) +
    geom_boxplot(varwidth=TRUE) +
    scale_fill_manual(values = c("cornflowerblue", "tomato2")) +  # Define colors for each group
    facet_wrap(~ combined_data$Treatment, scales = "fixed") +  # Use the same scale for both panels
    labs(title = plot_title, x = NULL, y = "Effect Size (G)") +
    theme_light() +
    guides(fill = FALSE)  # Remove the legend
  
  print(plot_obj)
}

# List of moderators
moderators <- c("FocalSpC", "Treatment_stage", "TreatDurCat", "RespCat")

# Create and display box plots for each moderator
for (moderator in moderators) {
  boxplot_effect(moderator)
}

