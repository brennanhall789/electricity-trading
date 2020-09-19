library(xts)
library(dplyr)
library(mice)
library(rgr)

vars <- read.csv('tios-quant-homework-data-2020/machine-learning-variables.csv',na.strings = c('NA',''))
target <- read.csv('tios-quant-homework-data-2020/machine-learning-target.csv')
target <- target[-which(is.na(target$target_value)),]
test_data <- read.csv('tios-quant-homework-data-2020/machine-learning-test-data.csv')
test_data_lower <- read.csv('tios-quant-homework-data-2020/machine-learning-test-data-lowerbound.csv')
test_data_upr <- read.csv('tios-quant-homework-data-2020/machine-learning-test-data-upperbound.csv')

## since number of rows in target data does not match number of rows in
## variables data, join data sets to match target data set.
joined_data=(left_join(target,vars, by='time'))

## determine if any columns have exceptionally high rate of missingness
rat=c()
for(j in 1:ncol(joined_data)){
  countNA = sum(is.na(joined_data[,j]))
  rat[j]=countNA/nrow(joined_data)
}
max(rat) 
## highest ratio of NA values is 0.08% so removing columns 
## based on this will lead to loss of information


#### Dimensionality Redution ####
vars_joined <- joined_data[,-c(1,2)]
### cor matrix - find highly collinear variables
cor_matrix <- cor(vars_joined, use = 'na.or.complete')
for(i in 1:nrow(cor_matrix)){
  for(j in 1:ncol(cor_matrix)){
    if(cor_matrix[i,j]>.99){
      if(i!=j){
        print(c(i,j))
      }
    }
  }
}
cc = c(33,34,35,87,88)
# first need to impute missing values using multiple imputation
# removing var_33, var_34, var_87, var_88 for perfect collinearity 
# of var_37, var_41, var_99, var_100, respectively
vars_joined_imp <- mice(vars_joined[-cc], m=1,
                        method='pmm', printFlag = FALSE)
vars_joined_imp <- mice::complete(vars_joined_imp)
# check all NAs have been imputed
rat=c()
for(j in 1:ncol(vars_joined_imp)){
  countNA = sum(is.na(vars_joined_imp[,j]))
  rat[j]=countNA
}
which(rat>0)

# check there is no more perfect collinearity
cor_matrix_imp <- cor(vars_joined_imp, use = 'na.or.complete')
for(i in 1:nrow(cor_matrix_imp)){
  for(j in 1:ncol(cor_matrix_imp)){
    if(cor_matrix_imp[i,j]>.99){
      if(i!=j){
        print(c(i,j))
      }
    }
  }
}

# next, standardize variables
stdz <- function(x){
  z = (x - mean(x, na.rm = T))/sd(x, na.rm = T)
}
vars_joined_imp = remove.na(vars_joined_imp)$x
vars_joined_imp_z = apply(vars_joined_imp, MARGIN = 2, FUN = stdz)

#### repeat for test data and bound data
test_imp <- mice(test_data[,-c(1,cc+1)], m=1,
                        method='pmm', printFlag = FALSE)
test_imp <- mice::complete(test_imp)
test_imp = remove.na(test_imp)$x
test_imp_z = apply(test_imp, MARGIN = 2, FUN = stdz)
colnames(test_imp_z) <- colnames(test_imp)

test_imp_lower <- mice(test_data_lower[,-c(1,cc+1)], m=1,
                       method='pmm', printFlag = FALSE)
test_imp_lower <- mice::complete(test_imp_lower)
test_imp_lower = remove.na(test_imp_lower)$x
test_imp_lower_z = apply(test_imp_lower, MARGIN = 2, FUN = stdz)
colnames(test_imp_lower_z) <- colnames(test_imp_lower)

test_imp_upper <- mice(test_data_upr[,-c(1,cc+1)], m=1,
                       method='pmm', printFlag = FALSE)
test_imp_upper <- mice::complete(test_imp_upper)
test_imp_upper = remove.na(test_imp_upper)$x
test_imp_upper_z = apply(test_imp_upper, MARGIN = 2, FUN = stdz)
colnames(test_imp_upper_z) <- colnames(test_imp_upper)

