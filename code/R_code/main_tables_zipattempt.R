
# Construct predicted values 
# Automate procedure for selecting B
# Automate looping over outcomes

library(optrdd)
library(tidyverse)
library(vroom)

# main_data <- vroom("~/Dropbox/GPW/data/final_county_level.csv",
#                    delim = ",",
#                    col_types = cols(
#                      quarter = col_double(),
#                      state = col_character(),
#                      census_code = col_double(),
#                      age = col_double(),
#                      total_bal_sl = col_double(),
#                      total_sl_past_due = col_double(),
#                      total_pay_sl = col_double(),
#                      bankruptcy_flag = col_double(),
#                      foreclosure_flag = col_double(),
#                      population = col_double(),
#                      avg_riskscore = col_double(),
#                      total_debts_in_collection = col_double(),
#                      n_coll_12_gt0_tot = col_double(),
#                      total_in_collection = col_double(),
#                      q_coll_12_gt0_tot = col_double(),
#                      total_bal_total = col_double(),
#                      total_bal_past_due = col_double(),
#                      total_bal_mort = col_double(),
#                      total_bal_heloc = col_double(),
#                      total_bal_cc = col_double(),
#                      total_bal_auto = col_double(),
#                      total_bal_consumer_finance = col_double(),
#                      total_bal_retail = col_double(),
#                      total_bal_other = col_double(),
#                      total_pay_auto = col_double(),
#                      total_pay_cc = col_double(),
#                      total_pay_consumer_finance = col_double(),
#                      total_pay_mort = col_double(),
#                      total_pay_heloc = col_double(),
#                      total_pay_retail = col_double(),
#                      total_pay_other = col_double(),
#                      total_auto_past_due = col_double(),
#                      total_cc_past_due = col_double(),
#                      total_consumer_finance_past_due = col_double(),
#                      total_mort_past_due = col_double(),
#                      total_heloc_past_due = col_double(),
#                      total_retail_past_due = col_double(),
#                      total_other_past_due = col_double(),
#                      balance_ttl = col_double(),
#                      balance_ttl_delinq = col_double(),
#                      balance_mort = col_double(),
#                      balance_mort_delinq = col_double(),
#                      payment_mort = col_double(),
#                      balance_heloc = col_double(),
#                      balance_heloc_delinq = col_double(),
#                      payment_heloc = col_double(),
#                      balance_cc = col_double(),
#                      balance_cc_delinq = col_double(),
#                      payment_cc = col_double(),
#                      balance_auto = col_double(),
#                      balance_auto_delinq = col_double(),
#                      payment_auto = col_double(),
#                      balance_consumer_finance = col_double(),
#                      balance_consumer_finance_delinq = col_double(),
#                      payment_consumer_finance = col_double(),
#                      balance_retail = col_double(),
#                      balance_retail_delinq = col_double(),
#                      payment_retail = col_double(),
#                      balance_other = col_double(),
#                      balance_other_delinq = col_double(),
#                      payment_other = col_double(),
#                      balance_sl = col_double(),
#                      balance_sl_delinq = col_double(),
#                      payment_sl = col_double(),
#                      payment_ttl_nonsl = col_double(),
#                      payment_ttl = col_double(),
#                      q_coll_12_gt0 = col_double(),
#                      n_coll_12_gt0 = col_double(),
#                      q_coll_12 = col_double(),
#                      n_coll_12 = col_double(),
#                      bpt = col_double(),
#                      fc = col_double(),
#                      new_bal_mort_dq = col_double(),
#                      new_bal_heloc_dq = col_double(),
#                      new_bal_cc_dq = col_double(),
#                      new_bal_auto_dq = col_double(),
#                      new_bal_consumer_finance_dq = col_double(),
#                      new_bal_retail_dq = col_double(),
#                      new_bal_other_dq = col_double(),
#                      new_bal_sl_dq = col_double(),
#                      new_bal_ttl_dq = col_double()
#                    ))

main_data <- read_csv("~/Dropbox/GPW/data/final_zip_level5080.csv",
                   col_types = cols(
                     state = col_character(),
                     zipcode = col_double(),
                     age = col_double(),
                     total_bal_sl = col_double(),
                     total_sl_past_due = col_double(),
                     total_pay_sl = col_double(),
                     bankruptcy_flag = col_double(),
                     foreclosure_flag = col_double(),
                     population = col_double(),
                     avg_riskscore = col_double(),
                     total_debts_in_collection = col_double(),
                     n_coll_12_gt0_tot = col_double(),
                     total_in_collection = col_double(),
                     q_coll_12_gt0_tot = col_double(),
                     total_bal_total = col_double(),
                     total_bal_past_due = col_double(),
                     total_bal_mort = col_double(),
                     total_bal_heloc = col_double(),
                     total_bal_cc = col_double(),
                     total_bal_auto = col_double(),
                     total_bal_consumer_finance = col_double(),
                     total_bal_retail = col_double(),
                     total_bal_other = col_double(),
                     total_pay_auto = col_double(),
                     total_pay_cc = col_double(),
                     total_pay_consumer_finance = col_double(),
                     total_pay_mort = col_double(),
                     total_pay_heloc = col_double(),
                     total_pay_retail = col_double(),
                     total_pay_other = col_double(),
                     total_auto_past_due = col_double(),
                     total_cc_past_due = col_double(),
                     total_consumer_finance_past_due = col_double(),
                     total_mort_past_due = col_double(),
                     total_heloc_past_due = col_double(),
                     total_retail_past_due = col_double(),
                     total_other_past_due = col_double(),
                     newb = col_double(),
                     newf = col_double(),
                     balance_ttl = col_double(),
                     balance_ttl_delinq = col_double(),
                     balance_mort = col_double(),
                     balance_mort_delinq = col_double(),
                     payment_mort = col_double(),
                     balance_heloc = col_double(),
                     balance_heloc_delinq = col_double(),
                     payment_heloc = col_double(),
                     balance_cc = col_double(),
                     balance_cc_delinq = col_double(),
                     payment_cc = col_double(),
                     balance_auto = col_double(),
                     balance_auto_delinq = col_double(),
                     payment_auto = col_double(),
                     balance_consumer_finance = col_double(),
                     balance_consumer_finance_delinq = col_double(),
                     payment_consumer_finance = col_double(),
                     balance_retail = col_double(),
                     balance_retail_delinq = col_double(),
                     payment_retail = col_double(),
                     balance_other = col_double(),
                     balance_other_delinq = col_double(),
                     payment_other = col_double(),
                     balance_sl = col_double(),
                     balance_sl_delinq = col_double(),
                     payment_sl = col_double(),
                     payment_ttl_nonsl = col_double(),
                     payment_ttl = col_double(),
                     q_coll_12_gt0 = col_double(),
                     n_coll_12_gt0 = col_double(),
                     q_coll_12 = col_double(),
                     n_coll_12 = col_double(),
                     bpt = col_double(),
                     fc = col_double(),
                     new_bpt = col_double(),
                     new_fc = col_double(),
                     new_bal_mort_dq = col_double(),
                     new_bal_heloc_dq = col_double(),
                     new_bal_cc_dq = col_double(),
                     new_bal_auto_dq = col_double(),
                     new_bal_consumer_finance_dq = col_double(),
                     new_bal_retail_dq = col_double(),
                     new_bal_other_dq = col_double(),
                     new_bal_sl_dq = col_double(),
                     new_bal_ttl_dq = col_double(),
                     pop = col_double()
                   ))

outcome_vars <- c("q_coll_12", "n_coll_12", "q_coll_12_gt0", 
                  "avg_riskscore", "balance_mort_delinq",
                  "balance_cc_delinq", "balance_ttl_delinq",
                  "new_bpt", "new_fc")

# Step 0: Collapse data to correct setting
# Step 1: Select outcomes
# Step 2: Determine max B approxmiation for given outcome
# Step 3: Estimate optrdd

reg <- main_data %>% 
  select(one_of(outcome_vars), age, pop, zipcode, state) %>% 
  mutate(age = age - 1) %>%
  mutate(new_fc = new_fc*100, new_bpt= new_bpt*100, 
         q_coll_12_gt0 = q_coll_12_gt0 *100) %>%
  filter(age > 55 & age < 75) %>% 
  filter(age != 65) %>% 
  group_by(age,  zipcode, state) %>%
  gather(variable, outcome, -age, -pop, -zipcode, -state) %>%
  group_by(age,  zipcode, state, variable) %>%
  summarize(outcome = weighted.mean(outcome, pop),
            population = sum(pop))  %>%
  mutate(W = as.numeric(age > 65)) 

# Calculate max B
overall_reg <- main_data %>% 
  select(one_of(outcome_vars), age, pop, zipcode, state) %>% 
  mutate(age = age - 1) %>%
  mutate(new_fc = new_fc*100, new_bpt= new_bpt*100, 
         q_coll_12_gt0 = q_coll_12_gt0 *100) %>%
  filter(age > 40 & age < 75) %>% 
  filter(age != 65) %>% 
  gather(variable, outcome, -age, -pop, -zipcode, -state) %>%
  group_by(age,  variable) %>%
  summarize(outcome = weighted.mean(outcome, pop),
            population = sum(pop))  %>%
  mutate(W = as.numeric(age > 65)) %>%
  group_by(variable, W) %>%
  arrange(variable, age) %>%
  mutate(diff = outcome - lag(outcome, default = NA)) %>%
  mutate(B = diff - lag(diff, default = NA)) 

B_reg <- c()
varnames <- c()
for (i in seq(1,length(outcome_vars))) {
  varname = outcome_vars[i]
  modelfit <- lm(outcome ~ age + I(age^2), 
                 data = overall_reg %>% filter( variable == varname) %>% filter(W == 0))
  B_reg <- c(4*abs(modelfit$coefficients[3]), B_reg)
  varnames <- c(varname, varnames)
}
  
max_B = overall_reg %>%
  group_by(variable) %>%
  summarize(max_B = max(abs(B), na.rm=TRUE)) %>% right_join(tibble(variable = varnames, B_reg = B_reg))

model_output <- vector("list",length(outcome_vars))
for (i in seq(1,length(outcome_vars))) {
  varname = outcome_vars[i]
  data_input <- reg %>% filter(variable== varname) 
  out.1 = optrdd(X=data_input$age, Y=data_input$outcome, W=data_input$W, 
                 max.second.derivative =max_B$B_reg[max_B$variable == varname], 
                 estimation.point = 65, optimizer="mosek", try.elnet.for.sigma.sq = TRUE)      
  print(out.1); plot(out.1, xlim = c(55, 75))
  model_output[[i]] <- out.1
}

# Loop over the variables in outcome_vars
file_output <- c()
for (i in seq(1,length(outcome_vars))) {
  varname = outcome_vars[i]
  print(varname)
  out.1 = model_output[[i]]
  print(out.1)
  data_input <- reg %>% filter(variable== varname) 
  model_fit <- bind_rows(tibble( age     = round(out.1$gamma.fun.0$xx), 
                                 y_minus = out.1$gamma.fun.0$gamma),
                         tibble( age     = round(out.1$gamma.fun.1$xx), 
                                 y_plus = out.1$gamma.fun.1$gamma))
  
  file_output <- c(paste0(varname, " & ", 
                          signif(out.1$tau.hat, digits=2)," & ", "[ ",
                          signif(out.1$tau.hat-out.1$tau.plusminus, digits=2),
                          ", & ", 
                          signif(out.1$tau.hat+out.1$tau.plusminus, digits=2),
                          " ] \\\\"),
                   file_output)
  predicted_val <- data_input %>% right_join(model_fit) %>%
    mutate(y_hat_minus = -1* y_minus * outcome,
           y_hat_plus = y_plus * outcome) %>%
    ungroup() %>%
    summarize(y_hat_minus = sum(y_hat_minus, na.rm=TRUE),
              y_hat_plus = sum(y_hat_plus, na.rm=TRUE)) %>%
    mutate(age = 65) %>%
    gather(y_hat, age) %>%
    rename(outcome = age) %>%
    mutate(age = 65)
  ggplot(data=data_input, aes(x = age, y = outcome, color = as.factor(W))) +
    stat_summary(fun.y = "mean", geom = "point") +
    geom_point(data = predicted_val, aes(y = outcome, x = age), color = c("#0072b2", "#d55e00") ) +
    theme_minimal() +
    scale_color_manual(guide = FALSE, values= c("#0072b2", "#d55e00")) +
    labs(x = "Age", y="", title = varname)
  ggsave(paste0("~/Dropbox/GPW/graphs/", varname, "optrdd_impute.pdf"))
}

fileConn<-file("~/Dropbox/GPW/tables/main_rd_optrdd.tex")
writeLines(file_output, fileConn)
close(fileConn)


# 1) Drop Counties with age bins < 10
# 2) Recut the data into PUMAs which are equal population sized
# 3) 

