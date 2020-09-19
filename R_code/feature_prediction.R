source('feature_data_prep.R')
source('feature_model.R')
source('boostrap_pred.R')
library(ggplot2)

#### Predictions ####
# transform test_data into principal components
test_pca <- predict(pca_res, newdata = test_imp_z)

#### Point Prediction ####
ridge_pred <- predict(ridge_fit, s = lam, newx = test_pca[,1:37])
rid_fitted <- predict(ridge_fit, s = lam, newx = vars_pca)

rid_res = transform_target - rid_fitted
summary(rid_res)
plot(rid_res,type = 'l')


#### Prediction Intervals ####
boot_est <- bootstrap_preds(ridge_fit, test_pca[,1:37], 10000)

bootstrap_CIs <- matrix(0, nrow = nrow(test_pca), ncol = 2)
for (i in 1:ncol(boot_est)) {
  bootstrap_CIs[i,1]  <- quantile(boot_est[,i], 0.025,na.rm = T)
  bootstrap_CIs[i,2]  <- quantile(boot_est[,i], 0.975, na.rm = T)
}
plot(ridge_pred, type = 'l', ylab = 'target prediction', xlab='Time')
lines(bootstrap_CIs[,1],col='blue');lines(bootstrap_CIs[,2],col='red')

#### Predictions from Bounds ####

range = test_imp_upper - test_imp_lower
summary(range)

#naive case: take midway point of upr and lwr bounds = (up-lwr)/2 + lwr
mid = (test_imp_upper - test_imp_lower)/2 + test_imp_lower
mid_z <- apply(mid, MARGIN = 2, FUN = stdz)
mid_pca <- predict(pca_res, newdata = mid_z)

## point predictions
ridge_pred_bound <- predict(ridge_fit, s = lam, newx = mid_pca[,1:37])
rid_fitted_bound <- predict(ridge_fit, s = lam, newx = vars_pca)

rid_res_bound = transform_target - rid_fitted
summary(rid_res_bound)
plot(rid_res_bound,type = 'l')

## bootstrapped prediction interval
boot_est_bd <- bootstrap_preds(ridge_fit, mid_pca[,1:37], 10000)

bootstrap_bd_CIs <- matrix(0, nrow = nrow(mid_pca), ncol = 2)
for (i in 1:ncol(boot_est_bd)) {
  bootstrap_bd_CIs[i,1]  <- quantile(boot_est_bd[,i], 0.025,na.rm = T)
  bootstrap_bd_CIs[i,2]  <- quantile(boot_est_bd[,i], 0.975, na.rm = T)
}
plot(ridge_pred_bound, type = 'l', ylab = 'target prediction', xlab='Time')
lines(bootstrap_bd_CIs[,1],col='blue');lines(bootstrap_bd_CIs[,2],col='red')

## compare point predictions with bounds predictions
pt_v_bd_diff <- ridge_pred - ridge_pred_bound
plot(pt_v_bd_diff, type = 'l', ylab = 'Point vs. Bounds Prediction Difference')

#### save predictions to .csv
predictions <- data.frame(Estimate = ridge_pred, Lower = bootstrap_CIs[,1], Upper = bootstrap_CIs[,2])
predictions_bd <- data.frame(Estimate = ridge_pred_bound, Lower = bootstrap_bd_CIs[,1], Upper = bootstrap_bd_CIs[,2])
write.csv(predictions, 'predictions.csv')
write.csv(predictions_bd, 'bound_predictions.csv')


