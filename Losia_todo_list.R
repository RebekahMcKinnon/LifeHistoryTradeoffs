##### Load packages -----
library(tidyverse)
library(dplyr)
library(metafor)
library(forestplot)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(ape, curl)
library(rotl)
library(readxl)
library(here)
library(readr)

##### Load data -----
data <- read.csv("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/PDFs/Extractions 2023.06.26.csv")
life_hist <- read.csv("G:/.shortcut-targets-by-id/15aIOTzK-SdA0QZzPxWaQk_8cNO0OoEUl/Rebekah thesis/META-ANALYSIS/2021-2023/PDFs/life_hist_records.csv")
names(data)

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


##### Remaking box plots with flipped ES -----
# flip ES
combined_data$G_flip <- combined_data$yi * combined_data$ES_flip
# we flipped the ES's where appropriate such that a positive value always indicated 
# change in the direction predicted by theory 
# for the meta-analyses etc. below I use this flipped ES 

# Create box plots for each moderator but for flipped data 
boxplot_effect <- function(moderator) {
  plot_data <- data.frame(G = combined_data$G_flip, Moderator = combined_data[[moderator]])
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

#turn into correlation matrix
corr_jet_tree <- vcv(pruned_tree, corr=T)

##### Running meta-analyses -----

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
# I will keep RecNo, FocalSpL_corrected and EffectID as random effects in all the more complex models 

# model with all random effects, no moderators 
model_4a <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID),
                  test = "t", 
                  method = "REML", 
                  sparse = FALSE, 
                  data = combined_data)
model_4a
#
model_4b <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | EffectID),
                   test = "t", 
                   method = "REML", 
                   sparse = FALSE, 
                   data = combined_data)
model_4b

anova(model_4a, model_4b)

model_4c <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID),
                   R = list(FocalSpL_corrected=cor_tree),
                   test = "t", 
                   method = "REML", 
                   sparse = FALSE, 
                   data = combined_data)
model_4c
# this tells us we shouldnt have phylo in model either

model_4d <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID, ~1 | FocalSpC),
                   R = list(FocalSpL_corrected=cor_tree),
                   test = "t", 
                   method = "REML", 
                   sparse = FALSE, 
                   data = combined_data)
model_4d
# neither latin, commmon name nor phylo matter so dont include in future model 

## Step 2: Introducing moderators 
names(combined_data)
# can justify removing focalSpL_correlected from future models as AIC not different with or without 

# Model with 3 random effects and 'Treatment' as moderator
model_5 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID),
                  mods = ~ Treatment-1,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)

model_5
# overlaps 0 so not 'sig'
# but in expected direction - more of a change with reduction than enlargement 
# can calculate bayesian p value - proportion overlap 

# Model with 3 random effects and 'Lifespan_ave' as moderator
model_6 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID),
                  mods = ~ Lifespan_ave,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_6

# Model with 3 random effects and 'Treatment_stage' as moderator
model_7 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID),
                  mods = ~ Treatment_stage,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_7

# Model with 3 random effects and 'TreatDurCat' as moderator
model_8 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID),
                  mods = ~ TreatDurCat,
                  test = "t", 
                  method = "REML", 
                  sparse = TRUE, 
                  data = combined_data)
model_8

# Model with 3 random effects and 'Effort_level' as moderator
model_9 <- rma.mv(yi = G_flip, V = vi, random = list(~1 | RecNo, ~1 | FocalSpL_corrected, ~1 | EffectID),
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
                   random = list(~1 | RecNo, ~1 | EffectID, ~1 | FocalSpL_corrected, ~1 | FocalSpC),
                   R = list(FocalSpL_corrected=cor_tree),
                   test = "t", 
                   method = "REML", 
                   sparse = TRUE, 
                   data = combined_data)
model_10

# Model with all random effects and 3 moderators: Treatment, LIfespan and Effort level 
model_11 <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Lifespan_ave + Effort_level,
                   random = list(~1 | RecNo, ~1 | EffectID, ~1 | FocalSpL_corrected, ~1 | FocalSpC),
                   R = list(FocalSpL_corrected=cor_tree),
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
                                 random = list(~1 | RecNo, ~1 | EffectID, ~1 | FocalSpL_corrected),
                                 R = list(FocalSpL_corrected=cor_tree),# how closely related the sp are
                                 test = "t", 
                                 method = "REML", 
                                 sparse = TRUE, 
                                 data = combined_data)
# this is the model which most directly addresses my research question 
# i.e., does species longevity mediate response to BSM (Treatment)?
final_model


# Model with all random effects and moderators for Treatment and the interaction between treatment and lifespan 
final_model_redo <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Lifespan_ave_scaled -1,
                      random = list(~1 | RecNo, ~1 | EffectID),
                      test = "t", 
                      method = "REML", 
                      sparse = TRUE, 
                      data = combined_data)
final_model_redo
# now effect of an increase in 1 SD not 1 year 
# notes from the above 3 models:
# based on significance values, final model appears to be best 
# because it has the lowest p-value for the intercept 
# indicating that it explains the most variation
# it is also the model which most directly addresses my research question 

# some things of note from this model - 

#(1)
# QE (df = 297) = 613.5334, p-value < 0.0001.
#This indicates the presence of high residual heterogeneity, 
#i.e., a significant amount of unexplained variation in the data.

#(2) connected to (1)
# none of the moderators have a significant impact on outcomes 

#(3) 
#The intercept (intrcpt) is estimated at 0.3299 with a standard error of 0.1158. 
#It is statistically significant (p-val = 0.0047), 
#this suggests a baseline effect that exists regardless of the moderators.

#(4)
#The largest sources of residual variance
#are associated with the random effects "RecNo," "EffectID," and "FocalSpL_corrected."
#this suggests that the species (and the study itself) have a greater impact
#on response to BSM than longevity 

# running the final model using the jet tree instead just to confirm it isn't different 
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

# re-running final model using scaled average lifespan data 
final_model2 <- rma.mv(yi = G_flip, V = vi, mods = ~ Treatment + Treatment:Lifespan_ave_scaled,
                      random = list(~1 | RecNo, ~1 | EffectID, ~1 | FocalSpL_corrected),
                      R = list(FocalSpL_corrected=cor_tree),
                      test = "t", 
                      method = "REML", 
                      sparse = TRUE, 
                      data = combined_data)
final_model2
# slightly changes intercept - now more significant value (than in unscaled model) but everything else the same 
# p value of 0.0047 in final_model versus <0.0001 in scaled model 

##### To do items: -----
# subtract the year of first breeding from average lifespan to create new column (breeding years)
# use breeding years in models and compare to check if different results 

# take out the paper with the very different results and check if it changes the results of the meta-analysis 

# rename RecNo as StudyID 
# run AIC of random effects model (4a) with and without FocalSpL_corrected (name 4c)
# MuMIn - consider using (multi model inference): https://cran.r-project.org/web/packages/MuMIn/index.html

# plot posteriors for important models 

# make plots of models - orchaRd models (Shinichi package): https://daniel1noble.github.io/orchaRd/
#Shinichi Nakagawa, Malgorzata Lagisz, Rose E. O’Dea, Patrice Pottier, Joanna Rutkowska, Alistair M. Senior, Yefeng Yang, Daniel W.A. Noble. 2023. orchaRd 2.0: An R package for visualizing meta-analyses with orchard plots. Methods in Ecology and Evolution, https://doi.org/10.1111/2041-210X.14152 (preprint = EcoEvoRxiv, https://doi.org/10.32942/X2QC7).

# plan results structure while writing the methods section; plan which figures to include and where 
# dataset, overall, moderator analysis... 

# to do: 
# step 1: write the methods and results, including making and choosing figures to include 
# step 2: send to Kim
# step 3: Kim review 
# step 4: make edits
# repeat steps 3-4
# step 5: send to Losia 