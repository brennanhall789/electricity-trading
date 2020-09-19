# source('feature_data_prep.R')
library(glmnet)


#### PCA ####
# use correlation matrix since variance of variables differ greatly 
# and no knowledge of scale
pca_res <- princomp(vars_joined_imp_z, cor = TRUE)
expl_var <- round(pca_res$sdev^2/sum(pca_res$sdev^2)*100)

plot(cumsum(expl_var),ylab = 'Cumulative Explained Variance');abline(v=37,col='red')

## use components that have marginal effect on explained variance (vars 1:37)
## This is a BIG dimensionality reduction
vars_pca <- vars_joined_imp_z %*% pca_res$loadings[,1:37]


#### Modeling ####
## Regression
hist(joined_data$target_value,xlim = c(0,50),breaks = 'FD',prob=T,
     main = "Histogram of target_value", xlab = "target_value")
fit <- density(joined_data$target_value)
N <- nrow(joined_data)
x_new <- rnorm(N, sample(joined_data$target_value, size = N, replace = TRUE), fit$bw)
lines(density(x_new),col='blue')
legend('topright',legend = 'Gaussian density estimate', lty=1,col='blue')
## target values seem to have Gaussian shape but large outliers

fit1 <- lm(joined_data$target_value ~ vars_pca)
#plot(fit1)

lambda_fm1 <- car::boxCox(fit1, family='yjPower', plotit=F)
lambda_max <- lambda_fm1$x[which.max((lambda_fm1$y))]
transform_target <- car::yjPower(joined_data$target_value, lambda = lambda_max)

fit2 <-  lm(transform_target ~ vars_pca)
#plot(fit2)

med_devs2 <- sqrt(abs(residuals(fit2) - mean(residuals(fit2))))
w2 <- lm(med_devs2 ~ vars_pca)
weight2 <- fitted(w2)
fit2_weighted <- lm(transform_target ~ vars_pca, weights = weight2)
#plot(fit2_weighted)


## Ridge regression
lambda_seq <- 10^seq(10,-10, by = -0.001)
cv_output <- cv.glmnet(vars_pca, transform_target)
lam = cv_output$lambda.min

ridge_fit <- glmnet(vars_pca, joined_data$target_value, lambda = lam, alpha = 0)
rsq_ridge = ridge_fit$dev.ratio
ridge_fit <- glmnet(vars_pca, transform_target, lambda = lam, alpha = 0)
rsq_ridge = ridge_fit$dev.ratio
# note: transforming target value improves model fit
