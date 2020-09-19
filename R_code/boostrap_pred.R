bootstrap_preds  <- function(model, newx, ndraws) {
  preds <- matrix(0, nrow = ndraws, ncol = nrow(newx))
  
  for (i in 1:ndraws) {
    ## For each bootstrap draw, we bootstrap the data by sampling
    ## observations with replacement.
    bootstrap_idx   <- sample(seq(nrow(newx)),
                              nrow(newx),
                              replace = TRUE)
    bootstrap_data   <- newx[bootstrap_idx,]
    
    ## make predictions and save point estimates
    preds[i,] <- predict(model, s=model$lambda, newx = bootstrap_data)
  }

  return(preds)
}
