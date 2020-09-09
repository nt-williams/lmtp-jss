
# Replication script for lmtp package JSS paper

# the following code may be used to install lmtp and sl3
# install.packages(c("lmtp", "devtools", "earth", "ranger"))
# devtools::install_github("tlverse/sl3")

library(lmtp)
library(sl3)
library(earth)

# Example 1 ---------------------------------------------------------------



# Example 2 ---------------------------------------------------------------

lrnrs <- make_learner_stack(Lrnr_glm,
                            Lrnr_ranger, 
                            Lrnr_earth)

# Example 3 ---------------------------------------------------------------



# Example 4 ---------------------------------------------------------------

x <- c("A_1", "A_2", "A_3", "A_4")
tv <- list(c("L_1"), c("L_2"), c("L_3"), c("L_4"))
y <- "Y"

shift <- function(data, trt) {
  (data[[trt]] - 1) * (data[[trt]] - 1 >= 1) + 
    data[[trt]] * (data[[trt]] - 1 < 1)
}

dynamic_shift <- function(data, trt) {
  if (trt == "A_1") {
    shift(data, trt)
  } else {
    ifelse(data[[sub("A", "L", trt)]] == 1, 
           shift(data, trt),
           data[[trt]])
  }
}

set.seed(540)
tml <- lmtp_tmle(sim_t4, x, y, time_vary = tv, shift = dynamic_shift, folds = 3, 
                  learners_outcome = lrnrs, learners_trt = lrnrs)
print(tml)

set.seed(697)
sdr <- lmtp_sdr(sim_t4, x, y, time_vary = tv, shift = dynamic_shift, folds = 3, 
                learners_outcome = lrnrs, learners_trt = lrnrs)
print(sdr)

