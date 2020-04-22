#'Function for getting predicted values from cross-basis gam
#'
#'This is not a very reusable function outside of these analyses.  It makes a lot of assumptions about variables in the environment.
#'
library(rlang)
mypred <- function(Q, L, df, colname, model) {
  Q_name <- enquo(Q)
  L_name <- enquo(L)
  
  testvals <- seq(min(Q), max(Q), length.out = 200)
  Q_new <- matrix(mean(df[[colname]]), nrow = length(testvals), ncol = ncol(Q))
  L_new <- L[1:length(testvals), ]
  day_post_new <- rep(mean(df$day_post), length(testvals))
  day_new <- rep(mean(df$day), length(testvals))
  interharvest_id_new <- rep("newlevel", length(testvals))
  plant_new <- rep("newlevel", length(testvals))
  counter_new <- rep("newlevel", length(testvals))
  
  resp <- array(dim = c(length(testvals), ncol(Q)))
  rownames(resp) <- testvals
  
  #loop through columns of matrix, replace with testvals, predict fitted.
  for(i in 1:ncol(Q_new)) {
    P1_i <- Q_new
    P1_i[, i] <- testvals
    resp[, i] <-
      suppressWarnings( #new levels of random effects are on purpose
        predict(
          model,
          newdata = list2(
            !!Q_name := P1_i,
            !!L_name := L_new,
            day_post = day_post_new,
            day = day_new,
            interharvest_id = interharvest_id_new,
            plant = plant_new,
            counter = counter_new
          ),
          type = "response"
        )
      )
  }
  out <-
    resp %>%
    as_tibble(rownames = "x") %>%
    pivot_longer(
      cols = starts_with("V"),
      names_to = "lag",
      names_prefix = "V",
      values_to = "fitted"
    ) %>%
    mutate(lag = as.double(lag), x = as.double(x)) 
  return(out)
}
# mypred(Q = Q_min_temp, L = L, df = hoppers, colname = "temp_min", model = m_hopper_min_temp)
