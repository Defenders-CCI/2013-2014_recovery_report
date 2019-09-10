library(rio)
library(stringr)
library(tidyverse)

dat <- import("2013-2014_recovery_report.xlsx")
names(dat)

############################################################
# estimated time-to-recovery
############################################################
table(dat$est_time_recov)
dat$est_time_recov_num <- ifelse(
  dat$est_time_recov == "N/A",
  NA,
  dat$est_time_recov
)
dat$est_time_recov_num <- ifelse(
  dat$est_time_recov_num == "UNK",
  NA,
  dat$est_time_recov_num
)
dat$est_time_recov_num <- str_replace_all(dat$est_time_recov_num, 
                                          ", UNK|UNK, |Exempt", 
                                          "")

# Replace one species' multiple choice:
dat$est_time_recov_num <- str_replace_all(dat$est_time_recov_num, "18, ", "")

table(dat$est_time_recov_num)
qq <- filter(dat, is.na(dat$est_time_recov_num))
dat$est_time_recov_num <- as.numeric(dat$est_time_recov_num)
summary(dat$est_time_recov_num)
median_est_time <- median(dat$est_time_recov_num, na.rm = TRUE)

############################################################
# estimated cost
############################################################
table(dat$est_cost_recov)
est_costs <- as.data.frame(table(dat$est_cost_recov), 
                           stringsAsFactors = FALSE) %>%
  rename(cost = Var1, n_spp = Freq)
est_costs$cost_num <- str_replace_all(est_costs$cost, "\\*\\*|, UNK|, UN|\\*", "")
est_costs$cost_num <- gsub(x = est_costs$cost_num, 
                           pattern = "N/A|Exempt|UNK",
                           replacement = NA)
est_costs$cost_num <- gsub(x = est_costs$cost_num, 
                           pattern = "5,000, 16,590,000",
                           replacement = "16,590,000")
est_costs$cost_num <- gsub(x = est_costs$cost_num,  pattern = ",", replacement = "")
est_costs$cost_num <- as.numeric(est_costs$cost_num)
est_costs$cost_per_spp <- est_costs$cost_num / est_costs$n_spp

# re-join with original data:
d2 <- left_join(dat, est_costs, by = c("est_cost_recov" = "cost"))

# total estimated recovery cost for spp *with* estimates
sum(d2$cost_per_spp, na.rm = TRUE)

# estimated cost per spp
est_per_spp <- sum(d2$cost_per_spp, na.rm = TRUE) / sum(!is.na(d2$cost_per_spp))
overall_est_past <- est_per_spp * length(d2$species)
overall_est_cur <- est_per_spp * 1662
overall_est_cur / median_est_time

# Now let's get some confidence intervals:
with_est <- filter(d2, !is.na(d2$cost_per_spp))
resamp_vals <- lapply(1:1000, FUN = function(x) {
  re <- sample_n(with_est, size = 481, replace = TRUE)
  tot_cost <- sum(re$cost_per_spp) / 481
  return(tot_cost)
}) %>% unlist()

quantile(resamp_vals, c(0.01, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99))
quantile(resamp_vals, c(0.025, 0.975))
LCL <- quantile(resamp_vals, 0.025) * 1662
UCL <- quantile(resamp_vals, 0.975) * 1662
LCL / median_est_time
UCL / median_est_time

saveRDS(d2, "data/2013-2014_recovery_report_clean_v1.rds")
