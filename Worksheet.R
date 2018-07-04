##-------------------------- Outline -----------------------------------------------------------------

# Purpose: 
# What I should deal with NAs in future projects?
# What kind of pattern & clues of NAs I should be looking at?

# One question:
# % of NAs; distribution of NAs (MAR or MCAR or __ )
# Impact of dropping; Impact of imputation


# Step 1 
# Assume MCAR:
# Drop __% (5, 15, 30) of the values randomly
# NOTE: % = #NAs/#obs not #entries
# TODO: how to simulate systematically missing values

# Assume MAR: 
# TODO

# Step 1.5 
# Describe the NAs:
# Freq NA distribution across rows
# NAs with outcome variable

# Step 2
# NA imputation
## multip;e imputation:
## correlation matrix to select best predictors then impute predicted values

##(EVAL:First, because the replaced values were predicted from other variables
#they tend to fit together “too well”and so standard error is deflated. 
# One must also assume that there is a linear relationship between the variables 
# used in the regression equation when there may not be one.)

# Categorical:
## Mode

# Continuous:
## Amelia
## Hmisc
## mice
## Mean
## VIM (kNN(sleep, variable = c("NonD","Gest")))

## 1: knnImputation()
# 2: > cor(algae[,4:18],use = "complete.obs")
# 
# 参数use = "complete.obs"可以忽略含有NA的记录，另外函数symnum()可以输出用符号表示相关值的形式。测试数据集中，相关值大于0.9的两个变量可以通过相关性填补这两个变量的缺失值。以P04和oP04为例，需要先找到这两个变量之间的线性相关关系：
# 
# > lm(PO4~oPO4,data = algae)

# Step 3
# Compare imputed values vs real values
# Measure discrepency rate
## Use the "best" imputation method to create "imputed dataset"

# Step 4
# Compare different model approaches on original dataset, imputed dataset, ommitd dataset (5, 15, 30%)
# Parametric (regression) vs Non-parametric (KNN, decision trees)
# See if some models are more tolerative to NAs, and to which levels.


## Citation:
#https://gking.harvard.edu/amelia
#When multiple imputation works properly, it fills in data in such a way as to not change any relationships in the data but which enables the inclusion of all the observed data in the partially missing rows
##-------------------------- Implementation -----------------------------------------------------------------

## Step 0.5
# TODO
# Problem: Hidden NAs
# 1. 'workclass', 'native.country' and 'occupation', (' ?') 
# 2. 'capital.gain' and 'capital.loss', (0)
# Solution: Ignore these 4 features for now 




## ------ Step 1: Drop 5, 15, 30% of the values ----
r1 <- raw_data_train %>%
  select(-c(workclass, occupation, capital.gain, capital.loss, fnlwgt, native.country))

# Randomly elect __% of the rows with replacement
rows_5 <-sample(nrow(r1), 0.05*nrow(r1), replace = TRUE)
rows_15 <-sample(nrow(r1), 0.15*nrow(r1), replace = TRUE)
rows_30 <-sample(nrow(r1), 0.3*nrow(r1), replace = TRUE)

# ------- If % defined as #NA/#rows: --------
# Fill in NAs within each row: 
r1NA_5 <- r1
r1NA_15 <- r1
r1NA_30 <- r1

for(i in 1:length(rows_5)){
  rand_col <- sample(ncol(r1)-1,1) ## CHECK: Use the same random col?
  r1NA_5[rows_5[i],rand_col] <- NA
}

for(i in 1:length(rows_15)){
  rand_col <- sample(ncol(r1)-1,1) ## CHECK: Use the same random col?
  r1NA_15[rows_15[i],rand_col] <- NA
}

for(i in 1:length(rows_30)){
  rand_col <- sample(ncol(r1)-1,1) ## CHECK: Use the same random col?
  r1NA_30[rows_30[i],rand_col] <- NA
}

# ------- If % defined as #NA/#all_entries: --------
# use missforest to seed 
library(missForest)
r1NA_5 <- prodNA(r1, noNA = 0.05)
r1NA_15 <- prodNA(r1, noNA = 0.15)
r1NA_30 <- prodNA(r1, noNA = 0.3)
## ----- Step 1.5: Describe NAs (not much to show for MCAR) ------

# Use MICE:
library(mice)
md.pattern(r1NA_5) #NOTE: evidence for MCAR
md.pattern(r1NA_15)
md.pattern(r1NA_30)
## Histogram of NA Count per row
# Calculate NA Matrix and num_NA per row
narows_5 <- rowSums(is.na(r1NA_5))
narows_15 <- rowSums(is.na(r1NA_15))
narows_30 <- rowSums(is.na(r1NA_30))

# Plot
hist(narows_5, main = "Number of NA Values in Each Row", xlab = "NA Count")
hist(narows_15, main = "Number of NA Values in Each Row", xlab = "NA Count")
hist(narows_30, main = "Number of NA Values in Each Row", xlab = "NA Count")

## Income vs NA_per_row
boxplot(narows_30 ~ r1NA_30$income) # mostly zero
 
# TODO for other cases: Check distibution of each dimension
hist(raw_data_train)
library(vcd)
mosaic(raw_data_train$workclass)


## ----- Step 2: Imputation -------- 
# 4 methods for categorical 
# for continuous
# correlation method already employed in packages? 
# compare the 5 packages
# plus na.omit

