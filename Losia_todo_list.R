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
  mutate(EffectID = as.factor(row_number()))

data$RecNo <- as.factor(data$RecNo)


meta_result2 <- rma.mv(yi = yi, V=vi, random = list(~1 |RecNo, ~1 |EffectID), data = data)
meta_result2
# RecNo is an identifier for each STUDY i.e., their Endnote record number 
# EffectID is just row numbers 
# is this correct? 

meta_result3 <- rma.mv(yi = yi, V=vi, mods = ~Treatment, random = list(~1 |RecNo, ~1 |EffectID), data = data)
meta_result3

combined_data <- left_join(data, life_hist, by = c("FocalSpC"))

meta_result4 <- rma.mv(yi = yi, V=vi, mods = ~Treatment + Treatment:Lifespan_ave, random = list(~1 |RecNo, ~1 |EffectID), data = combined_data)
meta_result4

meta_result5 <- rma.mv(yi = yi, V=vi, mods = ~Treatment + Treatment:Lifespan_ave, random = list(~1 |RecNo, ~1 |EffectID, ~1 |FocalSpC), data = combined_data)
meta_result5

names(data)

(combined_data$Lifespan_ave)

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


##### remaking figures with flipped ES
# flip ES
combined_data$G_flip <- combined_data$yi * combined_data$ES_flip
combined_data$V_flip <- combined_data$vi * combined_data$ES_flip


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



##### checking outlier stuff -----
# Calculate Control_SE as a percentage of Control_Mean
combined_data$Control_Percentage_SE <- (combined_data$Control_SE / combined_data$Control_Mean) * 100

# Flag rows where the percentage is an order of magnitude more or less than 10%
combined_data$Flag_Anomalous <- ifelse(
  combined_data$Control_Percentage_SE < 1 | combined_data$Control_Percentage_SE > 1000, 
  "Anomalous", 
  "Not Anomalous"
)

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
anomalous_treatment_row_numbers <- which(combined_data$Treatment_Percentage_SE < 1 | combined_data$Treatment_Percentage_SE > 1000)

# Print row numbers
cat("Row numbers of flagged 'Anomalous' Treatment rows: ", paste(anomalous_treatment_row_numbers, collapse = ", "), "\n")


##### Tree of Life -----

# load packages needed here 
library(tidyverse)
library(ape, curl)
library(rotl)
library(readxl)

names(combined_data)

#get list of unique species (in latin) from FocalSpL column
myspecies <- as.character(unique(combined_data$FocalSpL)) 

length(myspecies) #27 species
length(unique(myspecies)) #27

# use rotl package to retreive synthetic species tree from open tree of life
taxa <- tnrs_match_names(names = myspecies, context_name = "Birds")
dim(taxa) #27 8 
# ask losia what these numbers mean 

table(taxa$approximate_match) #4 approximate matches

taxa[taxa$approximate_match==TRUE, ]
# unique name is name displayed on tree (tip.label)
# need to correct 4 names in my original table 
# 3 are capatilisation based 
# 1 remaining is different species name; should be Petrochelidon ariel instead of hirundo ariel 

names(combined_data)

# get list of unique species from FocalSpL column 
combined_data$FocalSpL_corrected <- combined_data$FocalSpL

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

### get tree 
tree <- tol_induced_subtree(ott_ids = taxa[["ott_id"]], label_format = "name")  
plot(tree, cex=.6, label.offset =.1, no.margin = TRUE)

tree$tip.label <- gsub(" \\(.*", "", tree$tip.label) #remove comments
tree$tip.label <- gsub("_"," ", tree$tip.label) #get rid of the underscores
length(tree$tip.label) #26 i.e., 1 missing 

# check which one is missing 
print(myspecies2)
# parus montanus is missing, assuming it should be with Parus major 

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

# theres still one thats present in the data but not in the tree...
# not sure how to fix this as theres no tree name to change it to 
# trying a synonym for it?
combined_data$FocalSpL_corrected <- gsub("Parus caeruleus", "Cyanistes caeruleus", combined_data$FocalSpL_corrected)
# both are blue tits 
# so this reduces the number needed to match to 26
# confirmed both are blue tits in the original dataframe 

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

# no branch lengths are included in tree from Losia code, 
# need to be created later via simulations
# trying to do this below...

# Simulate branch lengths based on a molecular clock model
tree_with_branch_lengths <- compute.brlen(tree2, method = "molecular_clock", rate = 1) # ask losia what the (clock) rate should be set to? 

# Plot the tree with branch lengths
plot(tree_with_branch_lengths, cex = 0.6, label.offset = 0.1, no.margin = TRUE)

# Check the tree with branch lengths
summary(tree_with_branch_lengths)

### now attempting to incorporate this info into a meta-analysis 
# not sure if this is correct 
# check with Losia during meeting

# Extract branch lengths from the tree_with_branch_lengths
branch_lengths <- tree_with_branch_lengths$edge.length

# Create a named vector with species names as names and branch lengths as values
branch_lengths_named <- setNames(branch_lengths, tree_with_branch_lengths$tip.label)

# Match branch lengths to species in combined_data based on species names
combined_data$BranchLength <- branch_lengths_named[combined_data$FocalSpL_corrected]

# Fit a meta-analysis model with BranchLength as a moderator
meta_result_with_phylo <- rma.mv(yi = yi, V = vi, mods = ~ Treatment + Treatment:Lifespan_ave + BranchLength,
                                 random = list(~1 | RecNo, ~1 | EffectID, ~1 | FocalSpL_corrected),
                                 data = combined_data)

# Print the results
summary(meta_result_with_phylo)


