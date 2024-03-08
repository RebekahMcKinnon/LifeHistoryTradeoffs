##### Load packages -----
library(tidyverse)
library(dplyr)
library(metafor)
library(forestplot)
library(tidyr)
library(ggplot2)
library(ape, curl)
library(rotl)
library(readxl)
library(here)
library(readr)
library(patchwork)
library(meta)
library(emmeans)


##### Making explanatory figures for intro -----

# figure 1
# Creating the dataframe (invented data)
df <- data.frame(
  individual = rep(c("a", "b", "c", "d", "e"), each = 3),
  trait_1 = c(1, 1.5, 0.5, 2.1, 2.58, 1.6, 2.85, 3.49, 2.0, 4, 4.6, 3.4, 4.9, 5.5, 4.1),
  trait_2 = c(1, 0.94, 1.1, 2, 1.92, 2.13, 3, 2.95, 3.12, 4, 3.91, 4.11, 5, 4.93, 5.12)
)

# simple figure 
figure1 <- ggplot(df, aes(x = trait_2, y = trait_1)) +
  geom_point(aes(color = individual), size = 4) +
  geom_line(data = data.frame(x = c(0.7, 5.1), y = c(0.7, 5.1)),
            aes(x = x, y = y), linetype = "dashed", color = "grey25", size = 1) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(breaks = seq(1, 5, 1), limits = c(0, 6)) +
  scale_x_continuous(breaks = seq(1, 5, 1), limits = c(0, 6)) +
  labs(x = "Trait 2", y = "Trait 1") +
  theme(legend.position = "none")

print(figure1)

# save this figure in high DPI for publication 
# Specify the file path and name
#file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/Figure_1_colour.tiff"

# Save the combined plot with high DPI
#ggsave(file_path, plot = figure1, width = 12, height = 8, dpi = 600)

# same figure in greyscale in case needed for publication 
figure1_greyscale <- ggplot(df, aes(x = trait_2, y = trait_1, color = individual)) +
  geom_point(size = 4) +
  geom_line(data = data.frame(x = c(0.7, 5.1), y = c(0.7, 5.1)),
            aes(x = x, y = y), linetype = "dashed", color = "grey25", size = 1) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(breaks = seq(1, 5, 1), limits = c(0, 6)) +
  scale_x_continuous(breaks = seq(1, 5, 1), limits = c(0, 6)) +
  labs(x = "Trait 2", y = "Trait 1") +
  theme(legend.position = "none") +
  scale_color_manual(values = grey.colors(n = length(unique(df$individual)), start = 0.2, end = 0.8))

print(figure1_greyscale)

# save this figure in high DPI for publication 
# Specify the file path and name
#file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/Figure_1_greyscale.tiff"

# Save the combined plot with high DPI
#ggsave(file_path, plot = figure1_greyscale, width = 12, height = 8, dpi = 600)

# figure 2
# made as individual figures then combined
# Create a dataframe
df <- data.frame(
  category = c("Reduced", "Control", "Enlarged", "Reduced", "Control", "Enlarged"),
  investment = c(0.95, 1, 1.06, 0.5, 1, 1.5),
  line = c("a", "a", "a", "b", "b", "b")
)

# figure 2a
figure2a <- ggplot(df, aes(x = factor(category, levels = c("Reduced", "Control", "Enlarged")), y = investment, color = line)) +
  geom_line(aes(group = line), size = 1.2, linetype = ifelse(df$line == "a", "dashed", "solid")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  labs(x = "", y = "Parental Investment") +
  scale_y_continuous(limits = c(0, 1.7)) +
  scale_color_manual(values = c("a" = "saddlebrown", "b" = "thistle3")) +
  theme(legend.position = "none") 

print(figure2a)

# figure 2b
df <- data.frame(
  category = c("Reduced", "Control", "Enlarged", "Reduced", "Control", "Enlarged", "Reduced", "Control", "Enlarged"),
  investment = c(0.95, 1, 1.06, 0.5, 1, 1.15, 0.7, 1, 1.3),
  line = c("a", "a", "a", "b", "b", "b", "c", "c", "c")
)

figure2b <- ggplot(df, aes(x = factor(category, levels = c("Reduced", "Control", "Enlarged")), y = investment, color = line)) +
  geom_line(aes(group = line, linetype = line), size = 1.2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  labs(x = "", y = "Parental Investment") +
  scale_y_continuous(limits = c(0, 1.7)) +
  scale_color_manual(values = c("a" = "saddlebrown", "b" = "thistle3", "c" = "darkslateblue"), guide = "none") +
  scale_linetype_manual(values = c("a" = "dashed", "b" = "solid", "c" = "dotted"), guide = "none") +
  labs(y = NULL, x = NULL) 

print(figure2b)

# combine into 2 panel figure 
figure2_complete <- (figure2a + figure2b) + plot_layout(ncol=2) + plot_annotation(tag_levels = "A")
figure2_complete

# save this figure in high DPI for publication 
# Specify the file path and name
#file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/Figure_2.tiff"

# Save the combined plot with high DPI
#ggsave(file_path, plot = figure2_complete, width = 18, height = 8, dpi = 600)

# figure 2 greyscale
df_a <- data.frame(
  category = c("Reduced", "Control", "Enlarged", "Reduced", "Control", "Enlarged"),
  investment = c(0.95, 1, 1.06, 0.5, 1, 1.5),
  line = c("a", "a", "a", "b", "b", "b")
)

# figure 2a
figure2a_greyscale <- ggplot(df_a, aes(x = factor(category, levels = c("Reduced", "Control", "Enlarged")), y = investment, color = line)) +
  geom_line(aes(group = line), size = 1.2, linetype = ifelse(df_a$line == "a", "dashed", "solid")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  labs(x = "", y = "Parental Investment") +
  scale_y_continuous(limits = c(0, 1.7)) +
  scale_color_manual(values = c("a" = "grey20", "b" = "grey30", "c" = "grey40"), guide = "none") +
  theme(legend.position = "none") 

print(figure2a_greyscale)

#figure 2b
df_b <- data.frame(
  category = c("Reduced", "Control", "Enlarged", "Reduced", "Control", "Enlarged", "Reduced", "Control", "Enlarged"),
  investment = c(0.95, 1, 1.06, 0.5, 1, 1.15, 0.7, 1, 1.3),
  line = c("a", "a", "a", "b", "b", "b", "c", "c", "c")
)

figure2b_greyscale <- ggplot(df_b, aes(x = factor(category, levels = c("Reduced", "Control", "Enlarged")), y = investment, color = line)) +
  geom_line(aes(group = line, linetype = line), size = 1.2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  labs(x = "", y = "Parental Investment") +
  scale_y_continuous(limits = c(0, 1.7)) +
  scale_linetype_manual(values = c("a" = "dashed", "b" = "solid", "c" = "dotted"), guide = "none") +
  scale_color_manual(values = c("a" = "grey20", "b" = "grey30", "c" = "grey40"), guide = "none") +
  labs(y = NULL, x = NULL) +
  theme(legend.position = "none") 


print(figure2b_greyscale)

# combine into 2 panel figure 
figure2_complete_greyscale <- (figure2a_greyscale + figure2b_greyscale) + plot_layout(ncol=2) + plot_annotation(tag_levels = "A")
figure2_complete_greyscale


# save this figure in high DPI for publication 
# Specify the file path and name
#file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/Figure_2_greyscale.tiff"

# Save the combined plot with high DPI
#ggsave(file_path, plot = figure2_complete_greyscale, width = 18, height = 8, dpi = 600)


##### Load data -----
data <- read.csv("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/PDFs/Extractions 2023.06.26.csv")
life_hist <- read.csv("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/PDFs/life_hist_records.csv")
names(data)
names(life_hist)
##### Initial data exploration -----

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

##### Meta-analysis initial exploration (perhaps delete later) -----
meta_result <- rma(yi = data$yi, vi = data$vi)
meta_result

data <- data %>%
  mutate(EffectID = as.factor(row_number()))

data$RecNo <- as.factor(data$RecNo)


meta_result2 <- rma.mv(yi = yi, V=vi, random = list(~1 |RecNo, ~1 |EffectID), data = data)
meta_result2
# RecNo is an identifier for each STUDY i.e., their Endnote record number 
# EffectID is just row numbers 

meta_result3 <- rma.mv(yi = yi, V=vi, mods = ~Treatment, random = list(~1 |RecNo, ~1 |EffectID), data = data)
meta_result3

combined_data <- left_join(data, life_hist, by = c("FocalSpC"))

meta_result4 <- rma.mv(yi = yi, V=vi, mods = ~Treatment + Treatment:Lifespan_ave, random = list(~1 |RecNo, ~1 |EffectID), data = combined_data)
meta_result4

meta_result5 <- rma.mv(yi = yi, V=vi, mods = ~Treatment + Treatment:Lifespan_ave, random = list(~1 |RecNo, ~1 |EffectID, ~1 |FocalSpC), data = combined_data)
meta_result5

names(data)

(combined_data$Lifespan_ave)

# preparing some things to make figures with 

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

ggplot() +
  geom_jitter(data = combined_data, aes(y = Lifespan_ave, x = yi, color = Treatment), width = 0.1, height = 0, size = 3, alpha = 0.5) +
  labs(x = "Effect Size (Hedges' g)", y = "Longevity", color = "Treatment") +
  scale_color_manual(values = c("reduced" = "cornflowerblue", "enlarged" = "tomato2")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size=12, face="bold"))

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


##### Create box plots for each moderator -----
boxplot_effect <- function(moderator) {plot_data <- data.frame(G = combined_data$yi, Moderator = combined_data[[moderator]])
  plot_title <- paste("Effect Size (G) by", moderator)
  
  # Create two separate panels for enlarged and reduced Treatment with color mapping
  plot_obj <- ggplot(plot_data, aes(x = Moderator, y = G, fill = combined_data$Treatment)) +
    geom_boxplot(varwidth=TRUE) +
    scale_fill_manual(values = c("cornflowerblue", "tomato2")) +  # Define colors for each group
    facet_wrap(~ combined_data$Treatment, scales = "fixed") +  # Use the same scale for both panels
    labs(title = plot_title, x = NULL, y = "Effect Size (G)") +
    theme_light() +
    guides(fill = FALSE)  # Remove the legend
  print(plot_obj)}

# List of moderators
moderators <- c("FocalSpC", "Treatment_stage", "TreatDurCat", "RespCat")

# Create and display box plots for each moderator
for (moderator in moderators) {boxplot_effect(moderator)}


##### Flipping ES -----
# flip ES
combined_data$G_flip <- combined_data$yi * combined_data$ES_flip
# we flipped the ES's where appropriate such that a positive value always indicated 
# change in the direction predicted by theory 
# for the meta-analyses etc. below I use this flipped ES 

##### Remaking box plots with flipped ES -----

# Create box plots for each moderator but for flipped data 
boxplot_effect <- function(moderator) {
  plot_data <- data.frame(G = combined_data$G_flip, Moderator = combined_data[[moderator]])
  plot_title <- paste("Effect Size (G) by", moderator)
  
  # Create two separate panels for enlarged and reduced Treatment with color mapping
  plot_obj <- ggplot(plot_data, aes(x = Moderator, y = G, fill = combined_data$Treatment)) +
    geom_boxplot(varwidth=TRUE) +
    scale_fill_manual(values = c("cornflowerblue", "tomato2")) +  # Define colors for each group
    facet_wrap(~ combined_data$Treatment, scales = "fixed") +  # Use the same scale for both panels
    labs(title = NULL, x = NULL, y = "Effect Size (G)") +
    theme_minimal() +
    guides(fill = FALSE)  # Remove the legend
  
  print(plot_obj)
}

# List of moderators
moderators <- c("FocalSpC", "Treatment_stage", "TreatDurCat", "RespCat", "FocalSpL_corrected")

# Create and display box plots for each moderator
for (moderator in moderators) {
  boxplot_effect(moderator)
}
# these are just remakes of the box plots from the last section but using the flipped G values, so makes more sense 

##### Checking outliers -----
# Calculate Control_SE as a percentage of Control_Mean
combined_data$Control_Percentage_SE <- (combined_data$Control_SE / combined_data$Control_Mean) * 100

# Flag rows where the percentage is less than 5 or greater than 50
combined_data$Flag_Anomalous <- ifelse(
  combined_data$Control_Percentage_SE < 5 | combined_data$Control_Percentage_SE > 50, 
  "Anomalous", 
  "Not Anomalous")

# Print the first few rows to check the results
head(combined_data)

# Print rows where Control_Percentage_SE is Anomalous
anomalous_rows <- combined_data[combined_data$Flag_Anomalous == "Anomalous", ]

# Print the anomalous rows
print(anomalous_rows)

# Find row numbers of flagged rows
anomalous_row_numbers <- which(combined_data$Flag_Anomalous == "Anomalous")

# Print row numbers
cat("Row numbers of flagged 'Anomalous' rows: ", paste(anomalous_row_numbers, collapse = ", "), "\n")

# Calculate the percentage of Treatment_SE relative to Treatment_Mean
combined_data$Treatment_Percentage_SE <- (combined_data$Treatment_SE / combined_data$Treatment_Mean) * 100

# Find row numbers of flagged rows
anomalous_treatment_row_numbers <- which(combined_data$Treatment_Percentage_SE < 5 | combined_data$Treatment_Percentage_SE > 50)

# Print row numbers
cat("Row numbers of flagged 'Anomalous' Treatment rows: ", paste(anomalous_treatment_row_numbers, collapse = ", "), "\n")

# we did this so that I could double check these ones to make sure they weren't errors in calculations/recording in the spreadsheet etc 
# but that they were actually real 
# all seemed good 

##### Modifying variables -----
combined_data$Lifespan_ave_scaled <- scale(combined_data$Lifespan_ave, scale = 2)
combined_data$Breeding_years_scaled <- scale(combined_data$Breeding_years, scale = 2)

##### Linking to Tree of Life -----

names(combined_data)

#get list of unique species (in latin) from FocalSpL column
myspecies <- as.character(unique(combined_data$FocalSpL)) 

length(myspecies) #27 species
length(unique(myspecies)) #27

# use rotl package to retreive synthetic species tree from open tree of life
taxa <- tnrs_match_names(names = myspecies, context_name = "Birds")
dim(taxa) #27 8

# check number of approximate matches 
table(taxa$approximate_match) #4 approximate matches

taxa[taxa$approximate_match==TRUE, ]
# unique name is name displayed on tree (tip.label)
# need to correct 4 names in my original table 
# 3 are capatilisation based 
# 1 remaining is different species name; should be Petrochelidon ariel instead of hirundo ariel 

names(combined_data)

# get list of unique species from FocalSpL column 
combined_data$FocalSpL_corrected <- combined_data$FocalSpL # create column in which to put the corrected names 

length(unique(combined_data$FocalSpL_corrected)) # 27 species 

# fix species names to match the tree of life 
combined_data$FocalSpL_corrected <- gsub("certhia familiaris", "Certhia familiaris", combined_data$FocalSpL_corrected)
combined_data$FocalSpL_corrected <- gsub("Certhia familiaris ", "Certhia familiaris", combined_data$FocalSpL_corrected)
combined_data$FocalSpL_corrected <- gsub("Hirundo ariel", "Petrochelidon ariel", combined_data$FocalSpL_corrected)
combined_data$FocalSpL_corrected <- gsub("Petrochelidon ariel ", "Petrochelidon ariel", combined_data$FocalSpL_corrected)
combined_data$FocalSpL_corrected <- gsub("strix aluco", "Strix aluco", combined_data$FocalSpL_corrected)
combined_data$FocalSpL_corrected <- gsub("Strix aluco ", "Strix aluco", combined_data$FocalSpL_corrected)
combined_data$FocalSpL_corrected <- gsub("phaethon rubricauda", "Phaethon rubricauda", combined_data$FocalSpL_corrected)
combined_data$FocalSpL_corrected <- gsub("Phaethon rubricauda ", "Phaethon rubricauda", combined_data$FocalSpL_corrected)

# check on changes 
print(unique(combined_data$FocalSpL_corrected))

# get list of unique species from FocalSpL_corrected column 
myspecies2 <- as.character(unique(combined_data$FocalSpL_corrected))
length(myspecies2) # confirming still the same (27), it is 

print(myspecies2)

# rerun matching to tree of life 
taxa2 <- tnrs_match_names(names = myspecies2, context_name = "Birds")
dim(taxa2) # 27 8 still

# check to make sure all approximate matches now corrected 
table(taxa2$approximate_match)
taxa[taxa2$approximate_match==TRUE, ] # 0 approximate matches - good 

# now link to tree:
# get tree 
tree <- tol_induced_subtree(ott_ids = taxa[["ott_id"]], label_format = "name")  
plot(tree, cex=.6, label.offset =.1, no.margin = TRUE)

tree$tip.label <- gsub(" \\(.*", "", tree$tip.label) #remove comments
tree$tip.label <- gsub("_"," ", tree$tip.label) #get rid of the underscores
length(tree$tip.label) #26 i.e., 1 missing 

# check which one is missing 
print(myspecies2)
# parus montanus is missing, should be with Parus major 

# check if species names are matching 
#check overlap and differences with taxa list
intersect(unique(combined_data$FocalSpL_corrected), tree$tip.label) # 22 overlap
setdiff(unique(combined_data$FocalSpL_corrected), tree$tip.label) # 5 in data, not in tree 
setdiff(tree$tip.label, unique(combined_data$FocalSpL_corrected)) # 4 in tree, not in data

# parus montanus in my dataset is poecile montanus in the tree (tip.label) 
# so replace this in the corrected column 

combined_data$FocalSpL_corrected <- gsub("Parus montanus", "Poecile montanus", combined_data$FocalSpL_corrected)
combined_data$FocalSpL_corrected <- gsub("Catharacta skua", "Stercorarius skua", combined_data$FocalSpL_corrected)
combined_data$FocalSpL_corrected <- gsub("Parus palustris", "Poecile palustris", combined_data$FocalSpL_corrected)
combined_data$FocalSpL_corrected <- gsub("Tarsiger cyanurus", "Luscinia cyanura", combined_data$FocalSpL_corrected)

#check overlap and differences with taxa list
intersect(unique(combined_data$FocalSpL_corrected), tree$tip.label) 
setdiff(unique(combined_data$FocalSpL_corrected), tree$tip.label) 
setdiff(tree$tip.label, unique(combined_data$FocalSpL_corrected)) 
# theres still one thats present in the data but not in the tree...
# not sure how to fix this as theres no tree name to change it to 
# trying a synonym for it?
combined_data$FocalSpL_corrected <- gsub("Parus caeruleus", "Cyanistes caeruleus", combined_data$FocalSpL_corrected)
# both are blue tits 
# so this reduces the number needed to match to 26
# confirmed both are blue tits in the original dataframe 
# check overlap and differences with taxa list
intersect(unique(combined_data$FocalSpL_corrected), tree$tip.label)
setdiff(unique(combined_data$FocalSpL_corrected), tree$tip.label)  
setdiff(tree$tip.label, unique(combined_data$FocalSpL_corrected)) 

print(unique(combined_data$FocalSpL))

# redo tree again 
myspecies3 <- as.character(unique(combined_data$FocalSpL_corrected)) #get list of unique species from FocalSpL_corrected column
taxa3 <- tnrs_match_names(names = myspecies3, context_name = "Birds")
dim(taxa3)
table(taxa3$approximate_match)

tree2 <- tol_induced_subtree(ott_ids = taxa3[["ott_id"]], label_format = "name")  
plot(tree2, cex=.6, label.offset =.1, no.margin = TRUE)

tree2$tip.label <- gsub(" \\(.*", "", tree2$tip.label) #remove comments
tree2$tip.label <- gsub("_"," ", tree2$tip.label) #get rid of the underscores
length(tree2$tip.label) #26

#check overlap and differences with taxa list
intersect(unique(combined_data$FocalSpL_corrected), tree2$tip.label) # all 26 overlap
setdiff(unique(combined_data$FocalSpL_corrected), tree2$tip.label) # 0 in data, not in tree 
setdiff(tree2$tip.label, unique(combined_data$FocalSpL_corrected)) # 0 in tree, not in data
# everything seems good 

#check if the tree is really binary 
is.binary(tree) #TRUE
# also good 

# plot final tree
plot(tree2, cex=.6, label.offset =.1, no.margin = TRUE)

# no branch lengths are included in tree 
# need to be created via simulations

# Simulate branch lengths based on a molecular clock model
tree_with_branch_lengths <- compute.brlen(tree2, method = "molecular_clock", rate = 1) # ask losia what the (clock) rate should be set to? 

# Plot the tree with branch lengths
plot(tree_with_branch_lengths, cex = 0.6, label.offset = 0.1, no.margin = TRUE)

# Check the tree with branch lengths
summary(tree_with_branch_lengths)

#create matrix to use in meta-analysis models 
cor_tree <- vcv(tree_with_branch_lengths, corr=T)

# tested using the model which most closely answers my research questions below
# but run models of increasing complexity in later section 

### should be running with flipped G 
meta_result_with_phylo <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Lifespan_ave,
                                 random = list(~1 | RecNo, ~1 | EffectID, ~1 | FocalSpL_corrected, ~1 | FocalSpC),
                                 R = list(FocalSpL_corrected=cor_tree),
                                 test = "t", 
                                 method = "REML", 
                                 sparse = TRUE, 
                                 data = combined_data)

# Print the results
summary(meta_result_with_phylo)

str(cor_tree)

##### Linking to Jetz Tree -----

# load tree
# (note: called tree_retry because I originally ran using a different tree file. This note is for personal reference - please ignore)
tree_retry <- read.tree("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/R stuff/tree.tre")
plot(tree_retry)

# remove unnecessary stuff from tree to allow for easier matching 
tree_retry$tip.label <- gsub(" \\(.*", "", tree_retry$tip.label) #remove comments
tree_retry$tip.label <- gsub("_"," ", tree_retry$tip.label) #get rid of the underscores
length(tree_retry$tip.label) # check length - 9993 species in full tree 

# check overlap and differences with taxa list 
combined_data$FocalSpL_jet <- combined_data$FocalSpL
length(unique(combined_data$FocalSpL_jet))

intersect(unique(combined_data$FocalSpL_jet), tree_retry$tip.label)# overlapping between data and tree 
setdiff(unique(combined_data$FocalSpL_jet), tree_retry$tip.label) # in data but not in tree
setdiff(tree_retry$tip.label, unique(combined_data$FocalSpL_jet)) # in tree but not in data 

# correct those that are in my data but aren't in the tree 
combined_data$FocalSpL_jet <- gsub("Certhia familiaris ", "Certhia familiaris", combined_data$FocalSpL_jet)# unnecessary space
combined_data$FocalSpL_jet <- gsub("Hirundo ariel ", "Hirundo ariel", combined_data$FocalSpL_jet)# unnecessary space 
combined_data$FocalSpL_jet <- gsub("Strix aluco ", "Strix aluco", combined_data$FocalSpL_jet)# unnecessary space 
combined_data$FocalSpL_jet <- gsub("Phaethon rubricauda ", "Phaethon rubricauda", combined_data$FocalSpL_jet)# unnecessary space 
combined_data$FocalSpL_jet <- gsub("Cyanistes caeruleus", "Parus caeruleus", combined_data$FocalSpL_jet)# synonym

intersect(unique(combined_data$FocalSpL_jet), tree_retry$tip.label)# now all overlapping between data and tree 
setdiff(unique(combined_data$FocalSpL_jet), tree_retry$tip.label) # none in data but not in tree
setdiff(tree_retry$tip.label, unique(combined_data$FocalSpL_jet)) # still many in tree but not in data 

# need to prune the tree to only include those in the data 
pruned_tree <- keep.tip(tree_retry, combined_data$FocalSpL_jet)
length(pruned_tree$tip.label)
plot(pruned_tree)

# turn into correlation matrix
corr_jet_tree <- vcv(pruned_tree, corr=T)

##### Running meta-analyses -----

# notes for readers: 
# RecNo is StudyID: the RecNo I use is the record number for that paper in my Endnote library 
# FocalSpL_corrected is Phylo

# ran models of increasing complexity 

## Step 1: no moderators, only random effects (increasing complexity)

# Model 1: Random effect 'RecNo', no moderators
model_1 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo),
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_1

# Model 2: Random effect 'EffectID', no moderators
model_2 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | EffectID),
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_2

# Model 3: Random effect 'FocalSpC', no moderators
model_3 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | FocalSpC),
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_3

# Model 4: Random effect 'FocalSpL_corrected', no moderators
model_4 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | FocalSpL_corrected),
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_4

# notes on findings from the above 4 models:
# recno and focalSpC have similar estimates variance components (0.1325 and 0.1254, respectively) 
# and similar t-values, indicating that they might have a similar level of impact.

# focalSpC also (obviously) has the same estimated variance and t-value as FocalSpL_corrected 
# meaning probably we should only keep FocalSpL_corrected in the models and not focalSpC

# EffectID has the highest estimated variance component (0.1393) and the highest t-value (13.4745)
# meaning that EffectID should definitely be included in all models going forward 

# conclusion:
# I will check using an ANOVA whether or not I should retain FocalSpL_corrected in the models

# model with all random effects, no moderators 
model_4a <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID),
                  test = "t", 
                  method = "REML", 
                  sparse = FALSE, 
                  data = combined_data)
model_4a

#model with 2 random effects (not FocalSpL_corrected), no moderators 
model_4b <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                   test = "t", 
                   method = "REML", 
                   sparse = FALSE, 
                   data = combined_data)
model_4b

# these are effectively the same 
# model_4b is slightly better indicating that FocalSpL_corrected should not be retained in future models

# checking if I should retain Phylogeny in future models 

model_4c <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID),
                   R = list(FocalSpL_corrected=cor_tree),
                   test = "t", 
                   method = "REML", 
                   sparse = FALSE, 
                   data = combined_data)
model_4c


# final check of random effects to include: 
model_4d <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID, ~1 | FocalSpC),
                   R = list(FocalSpL_corrected=cor_tree),
                   test = "t", 
                   method = "REML", 
                   sparse = FALSE, 
                   data = combined_data)
model_4d

#conclusion from model_4d: 
# neither latin, commmon name nor phylo matter so dont include in future models (0.00)

# comparing model 4s using AIC to validate support for above conclusions 

# Calculate AIC for each model
AIC_4a <- AIC(model_4a)
AIC_4b <- AIC(model_4b)
AIC_4c <- AIC(model_4c)
AIC_4d <- AIC(model_4d)

# Compare AIC values for model_4a with model_4b
if (AIC_4a < AIC_4b) {
  cat("Model 4a is preferred over Model 4b based on AIC.\n")
} else {
  cat("Model 4b is preferred over Model 4a based on AIC.\n")
}
# model 4b is better than 4a i.e., supports dropping FocalSpL_corrected from models 

# Compare AIC values for model_4a with model_4c
if (AIC_4a < AIC_4c) {
  cat("Model 4a is preferred over Model 4c based on AIC.\n")
} else {
  cat("Model 4c is preferred over Model 4a based on AIC.\n")
}
# model 4c is slightly better i.e., supports inclusion of phylo BUT estimate is 0 so stay with idea of not including it 

# Compare AIC values for model_4a with model_4d
if (AIC_4a < AIC_4d) {
  cat("Model 4a is preferred over Model 4d based on AIC.\n")
} else {
  cat("Model 4d is preferred over Model 4a based on AIC.\n")
}
# 4a is better so again supports not including species in model 

## Step 2: Introducing moderators 
names(combined_data)

# Model with 2 random effects and 'Treatment' as moderator
model_5 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ Treatment-1,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)

model_5
# both intercepts sig different from 0
# more of a change with reduction than enlargement 
# however, very small (not sig) difference between effect of reduction and effect of enlargement 
# test of moderators has p-val of 0.2018 (when running without -1) 
#indicating that reduced and enlarged effects not sig different from eachother


# Model with 2 random effects and 'Lifespan_ave' as moderator
model_6 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ Breeding_years_scaled-1,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_6

# Model with 2 random effects and 'Treatment_stage' as moderator
model_7 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ Treatment_stage,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_7

# Model with 2 random effects and 'TreatDurCat' as moderator
model_8 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ TreatDurCat,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_8

# Model with 2 random effects and 'Effort_level' as moderator
model_9 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ Effort_level,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
# note on model 9: im using Effort_level here to indicate if the response category is a measure of 
# total (called parental) effort or effort per nestling 
# this is in place of using response category itself as a moderator 
model_9

# notes on findings from above models (5-9) i.e., single moderator models:
# somewhat annoyingly none of the moderators in the models (i.e., Treatment, Lifespan_ave, Treatment_stage, TreatDurCat, Effort_level)
# appear to be significant in explaining the variation in the results...

# variation in the models also remains relatively constant across moderators 
# seems that none of the included moderators have a strong impact on observed variation 

# conclusion:
# I will include (have included) the moderators most pertinent to my research qu.in further models 

# Model with all random effects and 2 moderators: Treatment and Lifespan
model_10 <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Lifespan_ave,
                   random = list(~1 | RecNo, ~1 | EffectID),
                   test = "t", 
                   method = "REML", 
                   sparse = TRUE, 
                   data = combined_data)
model_10

# Model with all random effects and 3 moderators: Treatment, LIfespan and Effort level 
model_11 <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Lifespan_ave + Effort_level,
                   random = list(~1 | RecNo, ~1 | EffectID),
                   test = "t", 
                   method = "REML", 
                   sparse = TRUE, 
                   data = combined_data)
# effort level is arguably not directly related to my research qu 
# but is predicted to be important in theory...and logic 
# decided not to include it in the final model below as, again, it seemingly had no real impact in explaining variation 
model_11

# Model with all random effects and moderators for Treatment and the interaction between treatment and lifespan 
final_model <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Lifespan_ave,
                                 random = list(~1 | RecNo, ~1 | EffectID),
                                 test = "t", 
                                 method = "REML", 
                                 sparse = TRUE, 
                                 data = combined_data)
# this is the model which most directly addresses my research question 
# i.e., does species longevity mediate response to BSM (Treatment)?
final_model


# checking if this is different if i use breeding years instead of average lifespan 
# breeding years is the average lifespan - age at first reproduction 
final_model_BA <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Breeding_years,
                      random = list(~1 | RecNo, ~1 | EffectID),
                      test = "t", 
                      method = "REML", 
                      sparse = TRUE, 
                      data = combined_data)
final_model_BA

AIC_final <- AIC(final_model)
AIC_finalBA<- AIC(final_model_BA)
AIC_final
AIC_finalBA

# Compare AIC values 
if (AIC_final < AIC_finalBA) {
  cat("Final_model is preferred over Model Final_model_BA based on AIC.\n")
} else {
  cat("Model Final_model_BA is preferred over Final_model based on AIC.\n")
}

# Final model BA is preferred - use breeding years instead of average lifespan 


# Model with all random effects and moderators for Treatment and the interaction between treatment and lifespan but using scaled lifespan average  
final_model_b <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Lifespan_ave_scaled, # if add -1 means it shows separate intercepts 
                      random = list(~1 | RecNo, ~1 | EffectID),
                      test = "t", 
                      method = "REML", 
                      sparse = TRUE, 
                      data = combined_data)
final_model_b
# now effect of an increase in 1 SD not 1 year 

final_model_b_BA <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment-1 + Treatment:Breeding_years_scaled, # if add -1 means it shows separate intercepts 
                        random = list(~1 | RecNo, ~1 | EffectID),
                        test = "t", 
                        method = "REML", 
                        sparse = TRUE, 
                        data = combined_data)
final_model_b_BA

AIC_final_b <- AIC(final_model_b)
AIC_final_b_BA<- AIC(final_model_b_BA)
AIC_final_b
AIC_final_b_BA

# Compare AIC values 
if (AIC_final_b < AIC_final_b_BA) {
  cat("Final_model_b is preferred over Model Final_model_b_BA based on AIC.\n")
} else {
  cat("Model Final_model_b_BA is preferred over Final_model_b based on AIC.\n")
}


# again final model b BA is best model 
# use this one as final model - breeding years & scaled 


# notes from the above 3 models:
# based on significance values, final model b appears to be best 
# because it has the lowest p-value for the intercept 
# indicating that it explains the most variation
# it is also the model which most directly addresses my research question 


# running the final model but with species and phylo using the jet tree instead just to confirm it isn't different 
final_model_jet <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Lifespan_ave,
                      random = list(~1 | RecNo, ~1 | EffectID, ~1 | FocalSpL_jet),
                      R = list(FocalSpL_jet=corr_jet_tree),
                      test = "t", 
                      method = "REML", 
                      sparse = TRUE, 
                      data = combined_data)
final_model_jet

# they are effectively the same 
# there is slightly higher unexplained variation related to phylo in the jet model 
# but nothing particularly notable i don't think 

# re-running final model but with species and phylo using scaled average lifespan data 
final_model_jet_b <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Lifespan_ave_scaled,
                      random = list(~1 | RecNo, ~1 | EffectID, ~1 | FocalSpL_corrected),
                      R = list(FocalSpL_corrected=cor_tree),
                      test = "t", 
                      method = "REML", 
                      sparse = TRUE, 
                      data = combined_data)
final_model_jet_b

# conclusion:
# can comfortably just use the tree of life model. same thing. but in either case, not including species and phylo in final models 

##### Plotting posteriors for important models -----
forest(final_model_b, slab = combined_data$RecNo, col="steelblue", refline=0)

forest(final_model_b_BA, slab = combined_data$RecNo, col="steelblue", refline=0)

##### OrchaRd plots -----

#install and load necessary packages 

# only run once:
#install.packages("pacman")
#rm(list = ls())
#devtools::install_github("daniel1noble/orchaRd", ref = "main", force = TRUE)
#pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, orchaRd, emmeans, ape, phytools, flextable)


# make plots of models - orchaRd models (Shinichi package): https://daniel1noble.github.io/orchaRd/
# cite this paper in publication: #Shinichi Nakagawa, Malgorzata Lagisz, Rose E. O’Dea, Patrice Pottier, Joanna Rutkowska, Alistair M. Senior, Yefeng Yang, Daniel W.A. Noble. 2023. orchaRd 2.0: An R package for visualizing meta-analyses with orchard plots. Methods in Ecology and Evolution, https://doi.org/10.1111/2041-210X.14152 (preprint = EcoEvoRxiv, https://doi.org/10.32942/X2QC7).

orchaRd::orchard_plot(final_model_b_BA, mod="1", group = "RecNo", xlab = "Standardised mean difference", 
                      transfm = "none") 

# use i2_sn function to obtain the total I^2

I2 <- orchaRd::i2_ml(final_model_b_BA)

model_results <- orchaRd::mod_results(final_model_b_BA, mod = "1", at = NULL,  group = "RecNo")
model_results

orchaRd::orchard_plot(model_results, mod="1", xlab = "Standardised mean difference") + 
  annotate(geom="text", x= 0.80, y= 1, label= paste0("italic(I)^{2} == ", round(I2[1],4)), 
           color="black", parse = TRUE, size = 5) +
  scale_fill_manual(values="cornflowerblue") +
  scale_colour_manual(values="cornflowerblue")

# figures show that overall estimate from a random-effects meta-analysis of 301 effect sizes from 52 studies is close to 0
# 95% CIs span the line of no effect 

# doing the same thing but for the other random effect i.e., effectID
orchaRd::orchard_plot(final_model_b_BA, mod="1", group = "EffectID", xlab = "Standardised mean difference", 
                      transfm = "none") 

# use i2_sn function to obtain the total I^2

I2 <- orchaRd::i2_ml(final_model_b_BA)

model_results2 <- orchaRd::mod_results(final_model_b_BA, mod = "1", at = NULL,  group = "EffectID")
model_results2

orchaRd::orchard_plot(model_results2, mod="1", xlab = "Standardised mean difference") + 
  annotate(geom="text", x= 0.80, y= 1, label= paste0("italic(I)^{2} == ", round(I2[1],4)), 
           color="black", parse = TRUE, size = 5) +
  scale_fill_manual(values="cornflowerblue") +
  scale_colour_manual(values="cornflowerblue")

# also close to 0 with CIS that span 0 (no effect)

# a caterpillar plot 

# 1. for RecNo
orchaRd::caterpillars(model_results, mod="1", xlab = "Standardised mean difference") 

# 2. for EffectID
# a caterpillar plot (not a caterpillars plot)
orchaRd::caterpillars(model_results2, mod="1", xlab = "Standardised mean difference") 

# adding moderators 
# Again, we can create a table of results
res2 <- orchaRd::mod_results(final_model_b_BA, mod = "Treatment", group = "RecNo")
res2

# creating this also for my interaction 
res2b <- orchaRd::mod_results(final_model_b_BA, mod = "Breeding_years_scaled", by = "Treatment", group = "RecNo")
res2b


# making cool figure to compare reduced and enlarged broods 
p1 <- orchaRd::orchard_plot(res2, 
                            mod = "Treatment", group = "RecNo", xlab = "Standardised mean difference")

p1 # can likely use this in publication 
# save this figure in high DPI for publication 
# Specify the file path and name
file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/red_enl_fig.tiff"

# Save the combined plot with high DPI
ggsave(file_path, plot = p1, width = 12, height = 8, dpi = 600)

# caterpillar of same thing 
p1b <- orchaRd::caterpillars(res2, 
                             mod = "Treatment", group = "RecNo",  xlab = "Standardised mean difference")
p1b


# bubble plot showing effect of treatment across average lifespan values 

orchaRd::bubble_plot(res2b, group = "RecNo",  mod = "Breeding_years_scaled", xlab = "Average Breeding Years (Scaled)", legend.pos = "top.left")
# awesome plot of results, could use in puplication 

# remaking this using unscaled average lifespan data as it improves reader understanding 

# creating this also for my interaction 
res2b_unscaled <- orchaRd::mod_results(final_model_BA, mod = "Breeding_years", by = "Treatment", group = "RecNo")
res2b_unscaled

figure4 <- orchaRd::bubble_plot(res2b_unscaled, group = "RecNo",  mod = "Breeding_years", xlab = "Average Breeding Years", legend.pos = "none")
figure4
# save this figure in high DPI for publication 
# Specify the file path and name
#file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/Figure_4.tiff"

# Save the combined plot with high DPI
#ggsave(file_path, plot = figure4, width = 12, height = 8, dpi = 600)

##### model sensitivity analysis -----

# there are several datapoints which are markedly different from the others 
# these come from the same paper
# we ran a sensitivity analysis to see if removing that paper changes results or not '

# create a table with only the 10 highest G_flip values 
sensitivity_check <- combined_data %>%
  select(RecNo, Author, G_flip, vi) %>%
  arrange(desc(G_flip)) %>%
  head(10)

# only top 3 really stand out 
# all from paper by Ardia i.e., RecNo 60
# num 6 is also from the Ardia paper 
# removing this paper for sensitivity check 

sensitivity_dataset <- combined_data %>%
  filter(Author != 'Ardia')

# now running the final model with this dataset instead 
final_model_scaled_sensitivity <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Breeding_years_scaled, # if add -1 means it shows separate intercepts 
                           random = list(~1 | RecNo, ~1 | EffectID),
                           test = "t", 
                           method = "REML", 
                           sparse = TRUE, 
                           data = sensitivity_dataset)
final_model_b_BA
final_model_scaled_sensitivity
# does change values, especially for the effect of the interaction between breeding years and the reduced treatment
# but doesnt change significance of anything 

# checking again with unscaled data 
final_model_unscaled_sensitivity <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Breeding_years,
                         random = list(~1 | RecNo, ~1 | EffectID),
                         test = "t", 
                         method = "REML", 
                         sparse = TRUE, 
                         data = sensitivity_dataset)
final_model_BA
final_model_unscaled_sensitivity
# again shows an increase in the effect of the interaction between breeding years and the reduced treatment 
# but doesnt change significance 

# comparing figures for these 

# first without the paper removed 
figure4

# with the paper removed 
res2b_unscaled_sensitivity <- orchaRd::mod_results(final_model_unscaled_sensitivity, mod = "Breeding_years", by = "Treatment", group = "RecNo")
res2b_unscaled_sensitivity

figure4_sensitivity <- orchaRd::bubble_plot(res2b_unscaled_sensitivity, group = "RecNo",  mod = "Breeding_years", xlab = "Average Breeding Years", legend.pos = "none")
figure4_sensitivity
# definitely improves the figure - data less crushed etc. 

# save this figure in high DPI for ESM of publication 
# Specify the file path and name
#file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/sensitivity_Figure_4.tiff"

# Save the combined plot with high DPI
#ggsave(file_path, plot = figure4_sensitivity, width = 12, height = 8, dpi = 600)

##### Publication bias checks -----

# 1. funnel plots 
funnel(final_model_b_BA, 
       yaxis="seinv",
       type = "rstudent", 
       ylab = "Precision (inverse of SE)",
       xlab = "Standardized residuals")

# nicer funnel plot 
funnel_plot <- funnel(
  final_model_b_BA,
  yaxis = "seinv",
  type = "rstudent",
  ylab = "Precision (inverse of SE)",
  xlab = "Standardized residuals",
  pch = 16,
  col = "cornflowerblue",
  ylim = c(0.5, 3)  # Adjust y-axis limits
)

print(funnel_plot)

# Specify the file path and name
file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/funnel_plot.tiff"

# Save the combined plot with high DPI
ggsave(file_path, plot = funnel_plot, width = 12, height = 8, dpi = 600)


# 2. egger tests 

# prepare needed variables 
combined_data$effectN <- (combined_data$Nnests_control * combined_data$Nnests_treat) / (combined_data$Nnests_control + combined_data$Nnests_treat)
combined_data$sqeffectN <- sqrt(combined_data$effectN)

# running model same as final model but with sqeffectN as moderator 
final_model_egger <- rma.mv(yi = G_flip, V = vi, mods = ~ sqeffectN + Treatment + Treatment:Breeding_years_scaled,
                            random = list(~1 | RecNo, ~1 | EffectID),
                           test = "t", 
                           method = "REML", 
                           sparse = TRUE, 
                           data = combined_data)
summary(final_model_egger)

# make orchaRd plot of outcome 
orchaRd::bubble_plot(final_model_egger,
            mod = "sqeffectN",
            group = "RecNo",
            xlab = "Effective N",
            g = TRUE)

# checking for decline effect (pub year)
final_model_egger_year <- rma.mv(yi = G_flip, V = vi, mods = ~ Year + Treatment + Treatment:Breeding_years_scaled,
                                 random = list(~1 | RecNo, ~1 | EffectID),
                            test = "t", 
                            method = "REML", 
                            sparse = TRUE, 
                            data = combined_data)
summary(final_model_egger_year)

# make orchaRd plot of outcome 
orchaRd::bubble_plot(final_model_egger_year,
                     mod = "Year",
                     group = "RecNo",
                     xlab = "Publication Year",
                     g = TRUE)

# include both 
final_model_both <- rma.mv(yi = G_flip, V = vi, mods = ~ Year + sqeffectN + Treatment + Treatment:Breeding_years_scaled,
                           random = list(~1 | RecNo, ~1 | EffectID),
                           test = "t", 
                           method = "REML", 
                           sparse = TRUE, 
                           data = combined_data)
final_model_both

# obtain predicted values and SEs from model 
dat_fulle <- qdrg(object=final_model_both, 
                  data=combined_data) # as dataframe

# use created dataframe to calculate marginal means for sqeffect and year
# weights proportional to levels of treatment (enlarged, reduced)
res_fulle1 <- emmeans(dat_fulle, 
                      specs = ~ sqeffectN + Year,
                      df = final_model_egger$ddf, 
                      weights = "prop")
res_fulle1 # estimated marginal means and SEs 
# in 2007 estimated marginal mean is 2.52 with large SE (14.7) and wide CIs 
# uncertainty in estimate 


# 3. Trim and fill to correct for plot asymmetry 

# found asymmetry in funnel plot so need to correct for this 
# need to rerun model as rma or uni.rma so using effectN instead of G_flip

resid_test <- rma(yi=effectN, vi=vi, data=combined_data) # works when i use effectN instead of sqEffectN...
resid_test # print model results to compare to trimfill adjustment below 

# run trimfill analysis 
trim_results <- trimfill(resid_test)
trim_results
# results comparable to resid_test model 
# i.e. even though seems to be publication bias  even after adjustment no significant changes to results 
# suggests generally robust results despite potential bias 



###
# Calculate the most and least frequently reported response variables
most_frequent_responses <- names(sort(table(combined_data$RespCat), decreasing = TRUE))[1:2]
least_frequent_responses <- names(sort(table(combined_data$RespCat))[1:2])

# Calculate the range of average lifespan and breeding years
min_lifespan <- min(combined_data$Lifespan_ave, na.rm = TRUE)
max_lifespan <- max(combined_data$Lifespan_ave, na.rm = TRUE)
min_breeding_years <- min(combined_data$Breeding_years, na.rm = TRUE)
max_breeding_years <- max(combined_data$Breeding_years, na.rm = TRUE)

# Fill in the blanks in the sentences
most_freq_sentence <- paste("The most frequently reported response variables were", 
                            most_frequent_responses[1], 
                            "(", length(combined_data$G_flip[combined_data$RespCat == most_frequent_responses[1]]), "estimates) and",
                            most_frequent_responses[2], 
                            "(", length(combined_data$G_flip[combined_data$RespCat == most_frequent_responses[2]]), "estimates).", sep = " ")

least_freq_sentence <- paste("The least commonly reported response variables were", 
                             least_frequent_responses[1], "and", least_frequent_responses[2], 
                             "(", sum(combined_data$G_flip[combined_data$RespCat == least_frequent_responses[1] | combined_data$RespCat == least_frequent_responses[2]] != "_"), "estimates).", sep = " ")

lifespan_sentence <- paste("The average lifespan of included species ranged from", 
                           min_lifespan, "for", combined_data$FocalSpL_corrected[which.min(combined_data$Lifespan_ave)], 
                           "to", max_lifespan, "for", combined_data$FocalSpL_corrected[which.max(combined_data$Lifespan_ave)], ".")

breeding_years_sentence <- paste("The resulting average breeding years ranged from", 
                                 min_breeding_years, "for", combined_data$FocalSpL_corrected[which.min(combined_data$Breeding_years)], 
                                 "to", max_breeding_years, "for", combined_data$FocalSpL_corrected[which.max(combined_data$Breeding_years)], ".")

# Print the sentences
cat(most_freq_sentence, "\n")
cat(least_freq_sentence, "\n")
cat(lifespan_sentence, "\n")
cat(breeding_years_sentence, "\n")



# Count the number of estimates for VisitRatesPerNestling
count_visit_rates <- sum(combined_data$G_flip[combined_data$RespCat == "VisitRatesPerNestling"] != "_")

# Print the result
cat("The number of estimates for VisitRatesPerNestling is:", count_visit_rates, "\n")


# Count the number of estimates for LoadSize
count_load_size <- sum(combined_data$G_flip[combined_data$RespCat == "LoadSize"] != "_")

# Print the result
cat("The number of estimates for LoadSize is:", count_load_size, "\n")

## alluvial plots
names(combined_data)

library(ggalluvial)
library(alluvial)

# Create the alluvial plot
alluvial_plot <- alluvial(
  combined_data[, c("Treatment", "Treatment_stage", "TreatDurCat", "Effort_level")],
  freq = 1, col = ifelse(combined_data$Treatment == "reduced", "red", "blue")
)


# Assuming 'combined_data' is your dataframe
breeding_years_gt_5 <- sum(combined_data$Breeding_years > 5)

# Print the result
cat("Number of estimates from species with breeding years greater than 5:", breeding_years_gt_5, "\n")


##### To do items: -----

# make nice looking plots for the tree eith colours and the images etc. 
# - use Shinichi code in Github repo multimodality / R / test.qmd at the end 
# below is shinichis code:

slist <- read_excel(here("data", "Species list.xlsx"))

slist %>% mutate_if(is.character, as.factor) ->slist


slist <- as.data.frame(slist)
slist$Order <- factor(slist$Order, levels = c("Passeriformes", "Falconiformes","Strigiformes",
                                              "Coraciiformes", "Piciformes", "Accipitriformes",
                                              "Procellariiformes", "Charadriformes", "Gruiformes",
                                              "Columbiformes", "Anseriformes", "Galliformes"),
                      labels =c("Passeriformes", "Falconiformes","Strigiformes",
                                "Coraciiformes", "Piciformes", "Accipitriformes",
                                "Procellariiformes", "Charadriformes", "Gruiformes",
                                "Columbiformes", "Anseriformes", "Galliformes"))

dat2 <- dat %>% group_by(Spp) %>% 
  summarise(N_obs = n())

dat2$Spp <- gsub("_", " ",dat2$Spp)

slist$Species_L <- gsub("_", " ", slist$Species_L)

# change tip label
tree$tip.label <-  gsub("_", " ", tree$tip.label)

match(slist$Species_L, tree$tip.label)

phy_fig1 <- ggtree(tree, branch.length = "branch.length") 

#phy_fig1

phy_fig2 <- phy_fig1 %<+% slist + geom_tiplab(size=2,fontface = "italic") + 
  geom_tippoint(aes(color= Order)) + 
  xlim_expand(c(0,120), panel = "Tree")
#phy_fig2

phy_fig3 <- facet_plot(phy_fig2, panel = 'k (effect sizes)', data = dat2, 
                       geom = geom_barh, 
                       mapping = aes(x = N_obs), alpha = 0.7, stat='identity') + 
  
  # geom_facet(panel = "Estimates of mean ratio (lnRR)", data = re.spp.lnRR2,
  #          geom = geom_barh, 
  #          mapping =  aes(x = N_obs, fill=Family, color=Family), alpha = 0.4, stat='identity') + 
  
  guides(fill="none") + 
  theme_tree2() + theme(strip.background = element_rect(fill = "white")) +
  theme(legend.position = c(0.07, 0.75)) + 
  scale_colour_discrete(breaks = c("Passeriformes", "Falconiformes","Strigiformes",
                                   "Coraciiformes", "Piciformes", "Accipitriformes",
                                   "Procellariiformes", "Charadriformes", "Gruiformes",
                                   "Columbiformes", "Anseriformes", "Galliformes")) 


phy_fig4 <-  facet_widths(phy_fig3, widths = c(0.7, 0.3))

phy_fig4

# adding incons
filenames <- list.files("images", pattern=".png", full.names=TRUE)
ldf <- purrr::map(filenames,~readPNG(.x))
names(ldf) <- substr(filenames, 8, 60)
ldf <- purrr::map(ldf, ~image_negate(image_read(.x)))
#ldf <- map(ldf, ~ image_blank(nrow(.x), ncol(.x), color = "black"))
#ldf <- lapply(ldf, raster)

# this works but does not knit
phy_fig5 <- ggdraw(phy_fig4) +
  draw_image(ldf$Passeriformes.png, y = 0.40, x = -0.33, scale = 0.04) +
  draw_image(ldf$Falconiformes.png, y = 0.36, x = -0.29, scale = 0.04) +
  draw_image(ldf$Strigiformes.png, y = 0.33, x = -0.33, scale = 0.04) +
  draw_image(ldf$Coraciiformes.png, y = 0.29, x = -0.29, scale = 0.04) +
  draw_image(ldf$Piciformes.png, y = 0.25, x = -0.33, scale = 0.04) +
  draw_image(ldf$Accipitriformes.png, y = 0.21, x = -0.29, scale = 0.04) +
  draw_image(ldf$Procellariiformes.png, y = 0.18, x = -0.325, scale = 0.04) +
  draw_image(ldf$Charadriiformes.png, y = 0.14, x = -0.29, scale = 0.04) +
  draw_image(ldf$Gruiformes.png, y = 0.11, x = -0.33, scale = 0.04) +
  draw_image(ldf$Columbiformes.png, y = 0.08, x = -0.29,scale = 0.04) +
  draw_image(ldf$Anseriformes.png, y = 0.05, x = -0.33, scale = 0.04) +
  draw_image(ldf$Galliformes.png, y = 0.01, x = -0.29,scale = 0.04) 

#phy_fig5

