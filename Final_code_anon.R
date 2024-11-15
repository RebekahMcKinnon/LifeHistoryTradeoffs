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
library(cowplot)
library(png)
library(imager)
library(purrr)
library(rphylopic)
library(magick)
library(gridExtra)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

library(ggtree)

##### Making explanatory figures for intro-----

# figure 1
# Creating the dataframe (invented data)
df <- data.frame(
  individual = rep(c("a", "b", "c", "d", "e"), each = 3),
  trait_1 = c(1, 1.5, 0.5, 2.1, 2.74, 1.6, 2.85, 3.49, 2.0, 3.87, 4.6, 3.1, 4.9, 5.5, 4.1),
  trait_2 = c(1, 0.6, 1.4, 2, 1.4, 2.4, 3, 2.7, 3.52, 4, 3.61, 4.59, 5, 4.48, 5.42)
)

# Define colors for each individual
individual_colors <- c("a" = "palevioletred4", "b" = "cornflowerblue", "c" = "darkolivegreen4", "d" = "goldenrod2", "e" = "plum3")

# simple figure 
figure1 <- ggplot(df, aes(x = trait_2, y = trait_1)) +
  geom_line(data = data.frame(x = c(0.7, 5.1), y = c(0.7, 5.1)),
            aes(x = x, y = y), linetype = "dashed", color = "grey35", size = 1) +
  scale_color_manual(values = individual_colors) +
  geom_point(aes(color = individual), size = 3.5) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title.x = element_text(size = 28, face = "bold"),
    axis.title.y = element_text(size = 28, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(breaks = seq(1, 5, 1), limits = c(0, 6)) +
  scale_x_continuous(breaks = seq(1, 5, 1), limits = c(0, 6)) +
  labs(x = "Investment in Current Brood", y = "Investment in Future Brood") +
  labs(color="Individual") +
  theme(legend.text = element_blank(),
        legend.title= element_text(size=18, face="bold"),
                                 legend.position=c(0.9,0.3),
                                 legend.key.size = unit(2.5, "lines"), 
        legend.key.width=unit(6, "lines"), 
        legend.box="horizontal", 
        legend.box.margin=margin(6), 
        legend.box.background=element_rect(color="grey49", fill=NA, size=0.2))

print(figure1)


falco_1 <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/falco s.png") %>% 
  image_colorize(100, "plum3")
falco_2 <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/falco s.png") %>% 
  image_colorize(100, "goldenrod2")
falco_3 <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/falco s.png") %>% 
  image_colorize(100, "darkolivegreen4")
falco_4 <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/falco s.png") %>% 
  image_colorize(100, "cornflowerblue")
falco_5 <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/falco s.png") %>% 
  image_colorize(100, "palevioletred4")


figure1_pics <- ggdraw(figure1) +
  draw_image(falco_1, y=-0.338, x=0.393, scale=0.067) +
  draw_image(falco_2, y=-0.252, x=0.393, scale=0.067) +
  draw_image(falco_3, y=-0.165, x=0.393, scale=0.067) +
  draw_image(falco_4, y=-0.08, x=0.393, scale=0.067) +
  draw_image(falco_5, y=0.007, x=0.393, scale=0.067)

figure1_pics


# save this figure in high DPI for publication 
# Specify the file path and name
file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/Figure_1_colour.tiff"

# Save the combined plot with high DPI
ggsave(file_path, plot = figure1_pics, width = 8.94, height = 5.77, dpi = 600)


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
  geom_line(aes(group = line), linewidth = 3.2, linetype = ifelse(df$line == "a", "dashed", "solid")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 28, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 28, face = "bold"),
    axis.title.y = element_text(size = 28, face = "bold"),
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
  geom_line(aes(group = line, linetype = line), size = 3.2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 28, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 28, face = "bold"),
    axis.title.y = element_text(size = 28, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  labs(x = "", y = "Parental Investment") +
  scale_y_continuous(limits = c(0, 1.7)) +
  scale_color_manual(values = c("a" = "saddlebrown", "b" = "thistle3", "c" = "darkslateblue"), guide = "none") +
  scale_linetype_manual(values = c("a" = "dashed", "b" = "solid", "c" = "dotted"), guide = "none") +
  theme(legend.position = "none") 

print(figure2b)




# Plot each figure separately
plot1 <- figure2a + annotate("text", x = 0.6, y = 1.7, label = "A", size = 8, fontface = "bold")
plot2 <- figure2b + annotate("text", x = 0.6, y = 1.7, label = "B", size = 8, fontface = "bold")

# Create a layout for the combined plots
layout <- rbind(c(1, 2))

# Combine the plots into a single grid
combined_plot <- grid.arrange(plot1, plot2, layout_matrix = layout)



chick <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/for fig 2/chick.png")


# test 
test_plot <- ggdraw(combined_plot) +
  draw_image(chick, y=-0.3, x=-0.38, scale=0.09) +
  draw_image(chick, y=-0.3, x=-0.26, scale=0.09) +
  draw_image(chick, y=-0.3, x=-0.23, scale=0.09) +
  draw_image(chick, y=-0.3, x=-0.20, scale=0.09) +
  draw_image(chick, y=-0.2, x=-0.105, scale=0.09) + #e
  draw_image(chick, y=-0.3, x=-0.12, scale=0.09) +
  draw_image(chick, y=-0.3, x=-0.09, scale=0.09) +
  draw_image(chick, y=-0.3, x=-0.06, scale=0.09) +
  draw_image(chick, y=-0.2, x=-0.075, scale=0.09) + #e
  
  draw_image(chick, y=-0.3, x=0.38, scale=0.09) + # enlarged 
  draw_image(chick, y=-0.2, x=0.395, scale=0.09) + # enlarged
  draw_image(chick, y=-0.3, x=0.41, scale=0.09) + # enlarged 
  draw_image(chick, y=-0.3, x=0.44, scale=0.09) + # enlarged 
  draw_image(chick, y=-0.2, x=0.425, scale=0.09) + # enlarged 
  draw_image(chick, y=-0.3, x=0.235, scale=0.09) + # control
  draw_image(chick, y=-0.3, x=0.265, scale=0.09) + # control
  draw_image(chick, y=-0.3, x=0.295, scale=0.09) + # control
  draw_image(chick, y=-0.3, x=0.12, scale=0.09) 



test_plot 

test_2 <- test_plot + 
  annotate("text", x = 0.125, y = 0.25, label = "-2", size = 3, fontface = "bold") + 
  annotate("text", x = 0.625, y = 0.25, label = "-2", size = 3, fontface = "bold") + 
  annotate("text", x = 0.275, y = 0.25, label = "0", size = 3, fontface = "bold") +
  annotate("text", x = 0.770, y = 0.25, label = "0", size = 3, fontface = "bold") +
  annotate("text", x = 0.915, y = 0.25, label = "+2", size = 3, fontface = "bold") +
  annotate("text", x = 0.415, y = 0.25, label = "+2", size = 3, fontface = "bold") 
test_2


# save this figure in high DPI for publication 
# Specify the file path and name
file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/Figure_2_chicks.tiff"

# Save the combined plot with high DPI
ggsave(file_path, plot = test_2, width = 18, height = 8, dpi = 600)










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

##### Combining dataframes -----
data <- data %>%
  mutate(EffectID = as.factor(row_number()))

data$RecNo <- as.factor(data$RecNo)

combined_data <- left_join(data, life_hist, by = c("FocalSpC"))

##### Meta-analysis initial exploration  -----
meta_result <- rma(yi = data$yi, vi = data$vi)
meta_result

meta_result2 <- rma.mv(yi = yi, V=vi, random = list(~1 |RecNo, ~1 |EffectID), data = data)
meta_result2
# RecNo is an identifier for each STUDY i.e., their Endnote record number 
# EffectID is just row numbers 

meta_result3 <- rma.mv(yi = yi, V=vi, mods = ~Treatment, random = list(~1 |RecNo, ~1 |EffectID), data = data)
meta_result3



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
plot(tree_with_branch_lengths, cex = 0.6, label.offset = 0.02, no.margin = TRUE)

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
plot(pruned_tree, cex = 1)

# turn into correlation matrix
corr_jet_tree <- vcv(pruned_tree, corr=T)

##### Make nice tree for inclusion in manuscript -----
# Read the images
hirundo_ariel <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/hinurdo a.png")
hirundo_rustica <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/hirundo r.png")
tachycineta_b <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/tachycineta b.png")
tachycineta_l <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/tachycineta l.png")
parus_p <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/parus palustris.png")
parus_major <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/parus major.png")
parus_mont <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/parus montanus.png")
parus_c <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/parus caeruleus.png")
ficedula_a <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/ficedula a.png")
ficedula_h <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/ficedula h.png")
tarsiger_c <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/tarsiger c.png")
sturnus_u <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/sturnus u.png")
certhia_f <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/certhia f.png")
agelaius_p <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/agelaius p.png")
sayornis_p <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/sayornis p.png")
tyrannus <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/tyrannus.png")
elaenia <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/elaenia.png")
hylophylax <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/hylophylax.png")
falco_s <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/falco s.png")
falco_t <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/falco t.png")
colaptes_a <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/colaptes.png")
strix <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/strix aluco.png")
skua <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/skua.png")
phaethon_r <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/phaethon.png")
bubulcus <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/bubulcus.png")
pelecanus <- image_read("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/phylopics/pelecanus.png")

##
plot(pruned_tree)

exper <- ggtree(pruned_tree, branch.length="branch.length", cex=1, layout=c("rectangular"))+
  geom_tiplab(aes(label = label), size = 5, fontface="italic") +
  xlim_expand(c(0.7,230), panel="Tree")
plot(exper)


dat_long <- combined_data %>%
  group_by(FocalSpL_jet) %>%
  summarise(
    n_estimates = n(),
    ave_years = mean(Breeding_years), 
    n_studies= n_distinct(RecNo))


long_plot <- facet_plot(exper, panel="Average Breeding Years", data=dat_long, 
                        geom=geom_segment, 
                        mapping=aes(x=0, xend=ave_years, y=y, yend=y, color=ave_years), size=6) +
  scale_color_gradient(name = "Years", low = "sienna2", high = "slateblue", n.breaks=10) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position=c(0.9,0.955),
        legend.direction="horizontal",
        legend.key.size = unit(1.5, "lines"),
        strip.text = element_text(size = 18), 
        strip.background = element_rect(fill="snow2", linewidth=1))

long_plot



#adding panel for k (number of estimates for each species)


# long_plot2 <- facet_plot(long_plot, panel="Number of Estimates", data=dat_long, 
#                          geom=geom_segment, 
#                          mapping=aes(x=0, xend=n_estimates, y=y, yend=y), size=6, color="grey") +
#   theme(legend.title = element_blank(),  
#         legend.text = element_text(size = 10),
#         legend.position=c(0.58,0.935),
#         legend.direction="horizontal",
#         legend.key.size = unit(1.5, "lines"),
#         strip.text = element_text(size = 18), 
#         strip.background = element_rect(fill="snow2")) 
# long_plot2


test <- facet_plot(long_plot, panel = "Average Breeding Years", 
                   data=dat_long, geom=geom_text, 
                   mapping=aes(x=ave_years+0.5, label=n_estimates, fontface=c("italic")), color="grey32")

test <- facet_plot(test, panel="Average Breeding Years", 
                   data=dat_long, geom=geom_text, 
                   mapping=aes(x=ave_years+1.4, label=(paste("(", n_studies, ")", sep="")), fontface=c("italic")), color="grey32")
test



# add phylopic images to plot
long_plot3 <- ggdraw(test) +
  draw_image(hirundo_ariel, y=0.43, x=-0.05, scale=0.04) +
  draw_image(hirundo_rustica, y=0.39, x=-0.05, scale=0.04) +
  draw_image(tachycineta_b, y=0.355, x=-0.05, scale=0.03) +
  draw_image(tachycineta_l, y=0.315, x=-0.05, scale=0.03) +
  draw_image(parus_p, y=0.277, x=-0.05, scale=0.04) +
  draw_image(parus_major, y=0.246, x=-0.05, scale=0.03) +
  draw_image(parus_mont, y=0.21, x=-0.05, scale=0.03) +
  draw_image(parus_c, y=0.18, x=-0.05, scale=0.04) +
  draw_image(ficedula_a, y=0.14, x=-0.05, scale=0.03) +
  draw_image(ficedula_h, y=0.105, x=-0.05, scale=0.03) +
  draw_image(tarsiger_c, y=0.07, x=-0.05, scale=0.03) +
  draw_image(sturnus_u, y=0.035, x=-0.05, scale=0.03) +
  draw_image(certhia_f, y=-0.0004, x=-0.05, scale=0.04) +
  draw_image(agelaius_p, y=-0.035, x=-0.05, scale=0.03) +
  draw_image(sayornis_p, y=-0.07, x=-0.05, scale=0.04)+
  draw_image(tyrannus, y=-0.11, x=-0.05, scale=0.04) +
  draw_image(elaenia, y=-0.145, x=-0.05, scale=0.03) +
  draw_image(hylophylax, y=-0.18, x=-0.05, scale=0.03) +
  draw_image(falco_s, y=-0.21, x=-0.05, scale=0.03) +
  draw_image(falco_t, y=-0.245, x=-0.05, scale=0.03) +
  draw_image(colaptes_a, y=-0.285, x=-0.05, scale=0.03) +
  draw_image(strix, y=-0.32, x=-0.05, scale=0.03) +
  draw_image(skua, y=-0.355, x=-0.05, scale=0.04) +
  draw_image(phaethon_r, y=-0.395, x=-0.05, scale=0.04) +
  draw_image(bubulcus, y=-0.43, x=-0.05, scale=0.03) +
  draw_image(pelecanus, y=-0.466, x=-0.05, scale=0.03) 



long_plot3 <- long_plot3 +  annotate(geom="text", x= 0.6549, y= 0.94, label= paste0("Estimates(Studies)"), 
                       color="grey32", size = 2.4, fontface="italic")

long_plot3

# Specify the file path and name
file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/phylo_plot_test.tiff"

# Save the combined plot with high DPI
ggsave(file_path, plot = long_plot3, width = 10, height = 8, dpi = 600)


































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

I2_4d <- orchaRd::i2_ml(model_4d)
I2_4d

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
                  mods = ~ Treatment,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)

model_5
round(orchaRd::r2_ml(model_5)*100, 2)

anova(model_4b, model_5, refit=TRUE)


model_5_ML <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ Treatment,
                  test = "t", 
                  method = "ML", 
                  sparse = TRUE, 
                  data = combined_data)

model_5_ML



# both intercepts sig different from 0
# more of a change with reduction than enlargement 
# however, very small (not sig) difference between effect of reduction and effect of enlargement 
# test of moderators has p-val of 0.2018 (when running without -1) 
#indicating that reduced and enlarged effects not sig different from eachother


# Model with 2 random effects and 'Lifespan_ave' as moderator
model_6 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ Lifespan_ave_scaled,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_6
round(orchaRd::r2_ml(model_6)*100, 2)

# Model with 2 random effects and 'Treatment_stage' as moderator
model_7 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ Treatment_stage,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_7
round(orchaRd::r2_ml(model_7)*100, 2)

# Model with 2 random effects and 'TreatDurCat' as moderator
model_8 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ TreatDurCat,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_8
round(orchaRd::r2_ml(model_8)*100, 2)

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
round(orchaRd::r2_ml(model_9)*100, 2)

# notes on findings from above models (5-9) i.e., single moderator models:
# somewhat annoyingly none of the moderators in the models (i.e., Treatment, Lifespan_ave, Treatment_stage, TreatDurCat, Effort_level)
# appear to be significant in explaining the variation in the results...

# variation in the models also remains relatively constant across moderators 
# seems that none of the included moderators have a strong impact on observed variation 

model_8.5 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ Breeding_years_scaled,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_8.5
round(orchaRd::r2_ml(model_8.5)*100, 2)


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
round(orchaRd::r2_ml(model_10)*100, 2)

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

combined_data$Treatment <- factor(combined_data$Treatment, levels = c("reduced", "enlarged"))

final_model_BA <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Breeding_years,
                      random = list(~1 | RecNo, ~1 | EffectID),
                      test = "t", 
                      method = "REML", 
                      sparse = TRUE, 
                      data = combined_data)
final_model_BA

final_model_BA2 <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment:Breeding_years,
                         random = list(~1 | RecNo, ~1 | EffectID),
                         test = "t", 
                         method = "REML", 
                         sparse = TRUE, 
                         data = combined_data)
final_model_BA2

round(orchaRd::r2_ml(final_model_BA2)*100, 2)

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

final_model_b_BA <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Breeding_years_scaled, # if add -1 means it shows separate intercepts 
                        random = list(~1 | RecNo, ~1 | EffectID),
                        test = "t", 
                        method = "REML", 
                        sparse = TRUE, 
                        data = combined_data)
final_model_b_BA


round(orchaRd::r2_ml(final_model_b_BA)*100, 2)


final_model_b_BA_ML <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Breeding_years_scaled, # if add -1 means it shows separate intercepts 
                           random = list(~1 | RecNo, ~1 | EffectID),
                           test = "t", 
                           method = "ML", 
                           sparse = TRUE, 
                           data = combined_data)
final_model_b_BA_ML


anova(model_4b, final_model_b_BA, refit=TRUE)




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


##

final_model_meeting <- rma.mv(yi = G_flip, V = vi, 
                           random = list(~1 | RecNo, ~1 | EffectID, ~1 | FocalSpC, ~1 | FocalSpL_corrected),
                           test = "t", 
                           method = "REML", 
                           sparse = TRUE, 
                           data = combined_data)
final_model_meeting

I2 <- orchaRd::i2_ml(final_model_meeting)
I2
# i2 for model with no fixed effects but all random effects 


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


# make plots of models - orchaRd models: https://daniel1noble.github.io/orchaRd/

orchaRd::orchard_plot(final_model_b_BA, mod="1", group = "RecNo", xlab = "Standardised mean difference", 
                      transfm = "none") 

# use i2_sn function to obtain the total I^2

I2 <- orchaRd::i2_ml(final_model_b_BA)
I2

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

I2 <- orchaRd::i2_ml(final_model_b_BA)

p1 <- orchaRd::orchard_plot(res2, 
                            mod = "Treatment", group = "RecNo", xlab = "Standardised Mean Difference (SMD)") + 
  theme(legend.direction="horizontal", axis.title= element_text(size = 18), axis.text.y=element_text(size=18), axis.text.x=element_text(size=12),
        legend.text = element_text(size=12), legend.title=element_text(size=14), legend.text.align = 1) +
  annotate(geom = "text", x = 1.45, y = -1, label = paste0("italic(I)^{2} == ",
                                                           round(I2[1], 4)), color = "black", parse = TRUE, size = 3.5)

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

combined_data$Treatment <- trimws(combined_data$Treatment)
unique(combined_data$Treatment)

combined_data$Treatment <- factor(combined_data$Treatment, levels = c("reduced", "enlarged"), ordered=TRUE)
str(combined_data$Treatment)



final_model_BA <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Breeding_years,
                         random = list(~1 | RecNo, ~1 | EffectID),
                         test = "t", 
                         method = "REML", 
                         sparse = TRUE, 
                         data = combined_data)
final_model_BA

figure4 <- orchaRd::bubble_plot(final_model_BA, group = "RecNo", by= "Treatment", mod = "Breeding_years", xlab = "Average Breeding Years", ylab = "Standardised Mean Difference (SMD)", 
                                legend.pos="top.out", k.pos="top.right")+
  scale_fill_manual(values=c("tomato2", "skyblue2")) +
  theme(legend.direction="horizontal", axis.title= element_text(size = 18), axis.text.y=element_text(size=12), axis.text.x=element_text(size=12),
        legend.text = element_text(size=12), legend.title=element_text(size=14), legend.text.align = 1,
        strip.text = element_text(size = 18), 
        strip.background = element_rect(fill="snow2"))
figure4



# save this figure in high DPI for publication 
# Specify the file path and name
file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/Figure_4.tiff"

# Save the combined plot with high DPI
ggsave(file_path, plot = figure4, width = 12, height = 8, dpi = 600)

##### Model sensitivity analysis -----

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
# Reset plot margins 
# (bottom, left, top, right) + adds to all
par(mar = c(5, 6, 4, 2) +0.1)  

# Modify axis title and label text size
par(cex.lab = 2, cex.axis = 1.5)

funnel(
  final_model_b_BA,
  xlab = "Standardized Residuals",
  ylab = "Precision (Inverse of SE)",
  back = NULL,
  bg = "black", 
  level = c(90, 95, 99),
  shade = c("gray90", "gray52", "gray34"),
  refline = 0, 
  yaxis = "seinv",
  type = "rstudent",
  axes = TRUE, 
  col = "darkslategray",
  ylim = c(0.01, 6), 
  xlim = c(-6, 6))




# Plot the funnel plot with suppressed axes
funnel(
  final_model_b_BA,
  xlab = "Standardized Residuals",
  ylab = "Precision (Inverse of SE)",
  back = NULL,
  bg = "black", 
  level = c(90, 95, 99),
  shade = c("gray90", "gray52", "gray34"),
  refline = 0, 
  yaxis = "seinv",
  type = "rstudent",
  yaxt = "n", # Suppress the default y-axis
  xaxt = "n", # Suppress the default x-axis
  col = "darkslategray",
  ylim = c(0.01, 6), 
  xlim = c(-6, 6)
)

# Add custom x-axis and y-axis
axis(1) # x-axis
axis(2, at = seq(2, 6, by = 2)) # y-axis with break points in increments of 2

# Add box around the plot
box()



# Plot the funnel plot with suppressed axes
funnel(
  final_model_b_BA,
  xlab = "Standardized Residuals",
  ylab = "Precision (Inverse of SE)",
  back = NULL,
  bg = "black", 
  level = c(90, 95, 99),
  shade = c("gray90", "gray52", "gray34"),
  refline = 0, 
  yaxis = "seinv",
  type = "rstudent",
  yaxt = "n", # Suppress the default y-axis
  xaxt = "n", # Suppress the default x-axis
  col = "darkslategray",
  ylim = c(0.01, 4), 
  xlim = c(-6, 6)
)

# Add custom x-axis and y-axis
axis(1) # x-axis
axis(2, at = seq(0, 4, by = 2)) # y-axis with break points in increments of 2

# Add box around the plot
box()













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
egger1 <- orchaRd::bubble_plot(final_model_egger,
            mod = "sqeffectN",
            group = "RecNo",
            xlab = "Effective N",
            g = TRUE, 
            ylab = "Mean   Difference   (SMD)           ",legend.pos="top.out", k.pos="top.right")+
  theme(legend.direction="horizontal", axis.title= element_text(size = 18), axis.text.y=element_text(size=12), axis.text.x=element_text(size=12),
        legend.text = element_text(size=12), legend.title=element_text(size=14), legend.text.align = 1) +
  annotate(geom = "text", x = 1, y = 6.1, label = "A", color = "black", parse = TRUE, size = 6)
egger1

# checking for decline effect (pub year)
final_model_egger_year <- rma.mv(yi = G_flip, V = vi, mods = ~ Year + Treatment + Treatment:Breeding_years_scaled,
                                 random = list(~1 | RecNo, ~1 | EffectID),
                            test = "t", 
                            method = "REML", 
                            sparse = TRUE, 
                            data = combined_data)
summary(final_model_egger_year)

# make orchaRd plot of outcome 
egger2 <- orchaRd::bubble_plot(final_model_egger_year,
                     mod = "Year",
                     group = "RecNo",
                     xlab = "Publication Year",
                     g = TRUE, 
                     ylab = "          Standardised",legend.pos="none", k.pos="top.right")+
  theme(legend.direction="horizontal", axis.title= element_text(size = 18), axis.text.y=element_text(size=12), axis.text.x=element_text(size=12),
        legend.text = element_text(size=12), legend.title=element_text(size=14), legend.text.align = 1) +
  annotate(geom = "text", x = 1989, y = 6.1, label = "B", color = "black", parse = TRUE, size = 6)
egger2

egger_comb <- egger1/egger2

# Specify the file path and name
file_path <- "G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/Drafts/Figures to include in manuscript/egger_comb.tiff"

# Save the combined plot with high DPI
ggsave(file_path, plot = egger_comb, width = 12, height = 7, dpi = 600)

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

#further examine potential publiation bias through models 
# need to rerun model as rma or uni.rma so using effectN instead of G_flip

resid_test <- rma(yi=effectN, vi=vi, data=combined_data) # works when i use effectN instead of sqEffectN...
resid_test # print model results to compare to trimfill adjustment below 

# run trimfill analysis 
trim_results <- trimfill(resid_test)
trim_results
# results comparable to resid_test model 
# i.e. even though seems to be publication bias  even after adjustment no significant changes to results 
# suggests generally robust results despite potential bias 

##### General data checks -----
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


cat("The number of estimates for LoadSize is:", count_load_size, "\n")

breeding_years_gt_5 <- sum(combined_data$Breeding_years > 5) 

cat("Number of estimates from species with breeding years greater than 5:", breeding_years_gt_5, "\n")

##### alluvial plots -----
names(combined_data)

library(ggalluvial)
library(alluvial)


# Create the alluvial plot
alluvial_plot <- alluvial(
  combined_data[, c("Treatment", "Treatment_stage", "TreatDurCat", "Effort_level")],
  freq = 1, col = ifelse(combined_data$Treatment == "reduced", "red", "skyblue2"), cw=0.15, cex=1.6, cex.axis=1.7, alpha=1, 
  axis_labels= c("Treatment", "Manipulation Stage", "Study Duration", "Effort Level")) +
  theme(plot.margin = margin(5, 6, 4 + 0.5, 2))


# Create a copy of the data frame to avoid modifying the original data
modified_data <- combined_data

# Rename categories in the Treatment variable
modified_data$Effort_level <- factor(modified_data$Effort_level,
                                  levels = c("parental", "nestlinglev"),
                                  labels = c("nest", "nestling"))

alluvial_plot <- alluvial(
  modified_data[, c("Treatment", "Treatment_stage", "TreatDurCat", "Effort_level")],
  freq = 1,
  col = ifelse(modified_data$Treatment == "reduced", "tomato2", "skyblue2"),
  border = ifelse(modified_data$Treatment == "reduced", "tomato2", "skyblue2"),
  cw = 0.15,
  cex = 1.6,
  cex.axis = 1.5,
  alpha = 0.9,
  axis_labels = c("Treatment", "Manipulation Stage", "Study Duration", "Effort Level")) 

##### Post-hoc analyses -----
# create new datasheet to use that only has male and female in sex
# Subset the data frame to include only rows where Sex is male or female
# Check unique values in the Sex column
unique(combined_data$Sex)
# Clean up Sex column and remove leading/trailing spaces
combined_data$Sex <- trimws(combined_data$Sex)
unique(combined_data$Sex)

new_data <- combined_data[combined_data$Sex != "both", ]
unique(new_data$Treatment)
unique(new_data$Sex)
unique(new_data$FocalSpL_corrected)


post_hoc <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Breeding_years_scaled + Sex, # if add -1 means it shows separate intercepts 
                           random = list(~1 | RecNo, ~1 | EffectID),
                           test = "t", 
                           method = "REML", 
                           sparse = TRUE, 
                           data = combined_data)
post_hoc

post_hoc2 <- rma.mv(yi = G_flip, V = vi, mods = ~ Sex, # if add -1 means it shows separate intercepts 
                   random = list(~1 | RecNo, ~1 | EffectID),
                   test = "t", 
                   method = "REML", 
                   sparse = TRUE, 
                   data = new_data)
post_hoc2
round(orchaRd::r2_ml(post_hoc2)*100, 2)

ph_sex <- orchaRd::mod_results(post_hoc2, mod = "Sex", by = "Treatment", group = "RecNo")
ph_sex

ph_fig <- orchaRd::bubble_plot(ph_sex, group = "RecNo",  mod = "Sex", by = "Treatment", xlab = "Parental Sex", legend.pos = "none")
ph_fig

# run the model with a dataset that only has male and female, not both 
# then figure out how to make a figure like the other one where its divided like male - famale and enlarged - reduced 


new_data$Sex <- factor(new_data$Sex, levels = c("female", "male"), ordered=TRUE)
new_data$Treatment <- factor(new_data$Treatment, levels = c("reduced", "enlarged"), ordered=TRUE)

post_hoc3 <- metafor::rma.mv(yi = G_flip, V = vi, mods = ~ Treatment:Sex, # if add -1 means it shows separate intercepts 
                    random = list(~1 | RecNo, ~1 | EffectID),
                    test = "t", 
                    method = "REML", 
                    sparse = TRUE, 
                    data = new_data)
post_hoc3

ph_sex <- orchaRd::mod_results(post_hoc3, mod = "Sex", by = "Treatment", group = "RecNo")
ph_sex

ph_fig <- orchaRd::bubble_plot(ph_sex, group = "RecNo",  mod = "Sex", by = "Treatment", xlab = "Parental Sex", legend.pos = "none")
ph_fig


new_data$Sex <- factor(new_data$Sex, levels = c("male", "female"))

post_hoc4 <- metafor::rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Sex, 
                             random = list(~1 | RecNo, ~1 | EffectID),
                             test = "t", 
                             method = "REML", 
                             sparse = TRUE, 
                             data = new_data)
post_hoc4

##this one works 
orchaRd::orchard_plot(post_hoc4, group = "RecNo", mod = "Treatment", by = "Sex", xlab = "Standardised Mean Difference (SMD)",
                      legend.pos= "top.right", condition.lab="Parental Sex", k.pos=c("left"),
                      alpha=0.5, trunk.size=10, branch.size=2, fill=FALSE) +
  theme(legend.direction="horizontal", axis.title= element_text(size = 18), axis.text.y=element_text(size=18), axis.text.x=element_text(size=12),
        legend.text = element_text(size=12), legend.title=element_text(size=14), legend.text.align = 1) +
  scale_fill_manual(values=c("grey30", "grey30"))




#this one is for testing stuff
orchaRd::orchard_plot(post_hoc4, group = "RecNo", mod = "Treatment", by = "Sex", xlab = "Standardised mean difference (SMD)", 
                      legend.pos= "top.right", condition.lab="Parental Sex", k.pos=c("left"), 
                      alpha=0.5, trunk.size=10, branch.size=2, colour=FALSE, cb=FALSE, fill=FALSE) + 
  theme(legend.direction="horizontal", axis.title= element_text(size = 18), axis.text.y=element_text(size=18), axis.text.x=element_text(size=12), 
        legend.text = element_text(size=12), legend.title=element_text(size=14), legend.text.align = 1) +
  scale_color_manual(values=c("green", "orange")) 




# Create a new datasheet with rows where G_flip is negative and only columns for Author and G_flip
new_datasheet <- subset(combined_data, G_flip < 0, select = c(Author, G_flip, ResponseVar, Treatment, Sex))









##### Running additional models & checks (integrate into above later) -----

#run sex models using full dataset instead of reduced dataset
sex_only <- rma.mv(yi = G_flip, V = vi, mods = ~ Sex, # if add -1 means it shows separate intercepts 
                    random = list(~1 | RecNo, ~1 | EffectID),
                    test = "t", 
                    method = "REML", 
                    sparse = TRUE, 
                    data = combined_data)
sex_only
round(orchaRd::r2_ml(sex_only)*100, 2)

#sex and treatment 
sex_treatment <- metafor::rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Sex, 
                             random = list(~1 | RecNo, ~1 | EffectID),
                             test = "t", 
                             method = "REML", 
                             sparse = TRUE, 
                             data = combined_data)
sex_treatment
round(orchaRd::r2_ml(sex_treatment)*100, 2)

#sex, sex treatment interaction 
sex_treatment_int <- metafor::rma.mv(yi = G_flip, V = vi, mods = ~ Sex + Treatment:Sex, 
                             random = list(~1 | RecNo, ~1 | EffectID),
                             test = "t", 
                             method = "REML", 
                             sparse = TRUE, 
                             data = combined_data)
sex_treatment_int
round(orchaRd::r2_ml(sex_treatment_int)*100, 2)

###

# run models excluding breeding years >5 
# to check how much they drive relationships 

#create reduced database 
BY_under5 <- combined_data[combined_data$Breeding_years <5, ]

# repeating model for 

# longevity only moderator 
model_6_red <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ Lifespan_ave_scaled,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = BY_under5)
model_6_red
round(orchaRd::r2_ml(model_6_red)*100, 2)

# breeding years only moderator 
model_8.5_red <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                    mods = ~ Breeding_years_scaled,
                    test = "t", 
                    method = "REML", 
                    sparse = TRUE, 
                    data = BY_under5)
model_8.5_red
round(orchaRd::r2_ml(model_8.5_red)*100, 2)

# treatment + treatment:breeding years 
final_model_b_BA_red <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Breeding_years_scaled,  
                           random = list(~1 | RecNo, ~1 | EffectID),
                           test = "t", 
                           method = "REML", 
                           sparse = TRUE, 
                           data = BY_under5)
final_model_b_BA_red
round(orchaRd::r2_ml(final_model_b_BA_red)*100, 2)


##### extra moderators -----

# relative change (%) only 
model_relchange <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                                mods = ~ Relchange,
                                test = "t", 
                                method = "REML", 
                                sparse = TRUE, 
                                data = combined_data)

model_relchange
round(orchaRd::r2_ml(model_relchange)*100, 2)


relchange_fig2 <- orchaRd::bubble_plot(model_relchange, group = "RecNo", mod = "Relchange", xlab = "Relative Change in Brood Size (%)", ylab = "Standardised Mean Difference (SMD)", 
                                      legend.pos="top.out", k.pos="top.right") +
  theme(legend.direction="horizontal", axis.title= element_text(size = 18), axis.text.y=element_text(size=12), axis.text.x=element_text(size=12),
        legend.text = element_text(size=12), legend.title=element_text(size=14), legend.text.align = 1,
        strip.text = element_text(size = 18), 
        strip.background = element_rect(fill="snow2"))
relchange_fig2


#relative change interaction with treatment 
model_relchange_treat <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                  mods = ~ Relchange:Treatment,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)

model_relchange_treat
round(orchaRd::r2_ml(model_relchange_treat)*100, 2)


relchange_fig <- orchaRd::bubble_plot(model_relchange_treat, group = "RecNo", by= "Treatment", mod = "Relchange", xlab = "Relative Change in Brood Size (%)", ylab = "Standardised Mean Difference (SMD)", 
                                legend.pos="top.out", k.pos="top.right")+
  scale_fill_manual(values=c("tomato2", "skyblue2")) +
  theme(legend.direction="horizontal", axis.title= element_text(size = 18), axis.text.y=element_text(size=12), axis.text.x=element_text(size=12),
        legend.text = element_text(size=12), legend.title=element_text(size=14), legend.text.align = 1,
        strip.text = element_text(size = 18), 
        strip.background = element_rect(fill="snow2"))
relchange_fig



# relative change interaction with breeding years 
model_relchange_BY <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                                mods = ~ Relchange:Breeding_years_scaled,
                                test = "t", 
                                method = "REML", 
                                sparse = TRUE, 
                                data = combined_data)

model_relchange_BY
round(orchaRd::r2_ml(model_relchange_BY)*100, 2)


summary(combined_data$Relchange)

cov(combined_data$Breeding_years_scaled, combined_data$Relchange)
cor(combined_data$Breeding_years_scaled, combined_data$Relchange)
plot(combined_data$Breeding_years_scaled, combined_data$Relchange)


cor(combined_data$Lifespan_ave, combined_data$Relchange)
cor(combined_data$Breeding_years, combined_data$Relchange)

plot(combined_data$Breeding_years, combined_data$Relchange)








anova(model_relchange_treat, model_relchange_BY, refit=TRUE)

# treat is better performing than BY
# means that irrespective of RRV proxy (breeding years) 
# a larger relative brood reduction results in a greater change in behaviour
# than a larger relative brood enlargement 
# the more a brood is reduced, the more they reduce their investment 
# the more a brood is enlarged, the more they increase their investment 


combined_data$relchange_scaled <- scale(combined_data$Relchange, scale = 2)
# relative change (%) only 
model_relchange_s <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                          mods = ~ relchange_scaled,
                          test = "t", 
                          method = "REML", 
                          sparse = TRUE, 
                          data = combined_data)

model_relchange_s
round(orchaRd::r2_ml(model_relchange_s)*100, 2)

#relative change interaction with treatment 
model_relchange_treat_s <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                                mods = ~ Treatment +  relchange_scaled:Treatment-1,
                                test = "t", 
                                method = "REML", 
                                sparse = TRUE, 
                                data = combined_data)

model_relchange_treat_s
round(orchaRd::r2_ml(model_relchange_treat_s)*100, 2)



# relative change interaction with breeding years 
model_relchange_BY_s <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                             mods = ~ relchange_scaled:Breeding_years_scaled,
                             test = "t", 
                             method = "REML", 
                             sparse = TRUE, 
                             data = combined_data)

model_relchange_BY_s
round(orchaRd::r2_ml(model_relchange_BY_s)*100, 2)

