
# Replication script for the lmtp package JSS paper

# the following code may be used to install lmtp and sl3
# install.packages(c("lmtp", "devtools", "earth", "ranger"))
# devtools::install_github("tlverse/sl3")

library(lmtp)
library(sl3)
library(earth)

# Section: Creating Treatment Policies ------------------------------------

baseline <- c("W_1", "W_2")
trt <- c("A_1", "A_2")
time_vary <- list(c("L_11", "L_12"), 
                  c("L_21", "L_22"))
create_node_list(trt = trt, baseline = baseline, 
                 time_vary = time_vary, tau = 2)

# Example 1 ---------------------------------------------------------------

x <- c("A_1", "A_2", "A_3", "A_4")
tv <- list(c("L_1"), c("L_2"), c("L_3"), c("L_4"))
y <- "Y"
shift <- function(data, trt) {
    (data[[trt]] - 1) * (data[[trt]] - 1 >= 1) + 
        data[[trt]] * (data[[trt]] - 1 < 1)
}

set.seed(4524)
lmtp_tmle(sim_t4, x, y, time_vary = tv, shift = shift)

set.seed(642)
lmtp_sdr(sim_t4, x, y, time_vary = tv, shift = shift)

# Example 2 ---------------------------------------------------------------

x <- c("A1", "A2")
cen <- c("C1", "C2")
tv <- list(c("L1"), c("L2"))
y <- "Y"
shift <- function(data, trt) {
  data[[trt]] + 0.5
}

lrnrs <- make_learner_stack(Lrnr_glm,
                            Lrnr_ranger, 
                            Lrnr_earth)

set.seed(624)
tml <- lmtp_tmle(sim_cens, x, y, time_vary = tv, 
                 cens = cen, shift = shift, learners_trt = lrnrs, 
                 learners_outcome = lrnrs, folds = 3)
print(tml)

set.seed(76535)
sdr <- lmtp_sdr(sim_cens, x, y, time_vary = tv, 
                cens = cen, shift = shift, learners_trt = lrnrs, 
                learners_outcome = lrnrs, folds = 3)
print(sdr)

set.seed(649)
tml_obs <- lmtp_tmle(sim_cens, x, y, time_vary = tv, 
                     cens = cen, shift = NULL, learners_trt = lrnrs, 
                     learners_outcome = lrnrs, folds = 3)

lmtp_contrast(tml, ref = tml_obs)

set.seed(23454)
sdr_obs <- lmtp_sdr(sim_cens, x, y, time_vary = tv, 
                    cens = cen, shift = NULL, learners_trt = lrnrs, 
                    learners_outcome = lrnrs, folds = 3)

lmtp_contrast(sdr, ref = sdr_obs)

# Example 3 ---------------------------------------------------------------

x <- "trt"
w <- c("W1", "W2")
cen <- paste0("C.", 0:5)
y <- paste0("Y.", 1:6)

set.seed(5423)
tml1 <- lmtp_tmle(sim_point_surv, x, y, w, cens = cen, 
                  learners_trt = lrnrs, learners_outcome = lrnrs,
                  shift = static_binary_on, folds = 3)

set.seed(6354)
tml0 <- lmtp_tmle(sim_point_surv, x, y, w, cens = cen, 
                  learners_trt = lrnrs, learners_outcome = lrnrs,
                  shift = static_binary_off, folds = 3)

lmtp_contrast(tml1, ref = tml0, type = "rr")

set.seed(432)
sdr1 <- lmtp_sdr(sim_point_surv, x, y, w, cens = cen, 
                 learners_trt = lrnrs, learners_outcome = lrnrs,
                 shift = static_binary_on, folds = 3)

set.seed(143)
sdr0 <- lmtp_sdr(sim_point_surv, x, y, w, cens = cen, 
                 learners_trt = lrnrs, learners_outcome = lrnrs,
                 shift = static_binary_off, folds = 3)

lmtp_contrast(sdr1, ref = sdr0, type = "rr")

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
lmtp_tmle(sim_t4, x, y, time_vary = tv, shift = dynamic_shift, folds = 3, 
          learners_outcome = lrnrs, learners_trt = lrnrs)

set.seed(697)
lmtp_sdr(sim_t4, x, y, time_vary = tv, shift = dynamic_shift, folds = 3, 
         learners_outcome = lrnrs, learners_trt = lrnrs)
