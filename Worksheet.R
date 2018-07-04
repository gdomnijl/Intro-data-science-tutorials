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
# TODO: how to simulate systematically missing values

# Assume MAR: 
# TODO

# Step 1.5 
# Describe the NAs:
# Freq NA distribution across rows
# NAs with outcome variable

# Step 2
# NA imputation
## layered imputaiton:
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

##-------------------------- Implementation -----------------------------------------------------------------

## Step 0.5
# TODO
# Problem: Hidden NAs
# 1. 'workclass', 'native.country' and 'occupation', (' ?') 
# 2. 'capital.gain' and 'capital.loss', (0)
# Solution: Ignore these 4 features for now 


# Check distibution of each dimension
hist(raw_data_train)
library(vcd)
mosaic(raw_data_train$workclass)

## Step 1: Drop 5% of the values
r1 <- raw_data_train %>%
  select(-c(workclass, occupation, capital.gain, capital.loss, fnlwgt, native.country))

# Randomly elect 0.5% of the rows with replacement
rows_5 <-sample(nrow(r1), 0.05*nrow(r1), replace = TRUE)
# Fill in NAs within each row: CHECK IF TRULY RANDOM
r1_NA <- r1
for(i in 1:length(rows_5)){
  rand_col <- sample(ncol(r1)-1,1)
  r1_NA[rows_5[i],rand_col] <- NA
}

## Step 1.5: Describe NAs

## Calculate NA Matrix
na_matrix <- is.na(r1_NA)
narows2 <- rowSums(na_matrix)

## Histogram of NA Count in each row
hist(narows2, main = "Number of NA Values in Each Row", xlab = "NA Count")
