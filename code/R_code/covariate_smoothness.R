
# Construct predicted values 
# Automate procedure for selecting B
# Automate looping over outcomes

library(optrdd)
library(tidyverse)
library(vroom)

# main_data <- vroom("~/Dropbox/Research/GPW/data/ACS_microdata_collapsed_by_age_county.csv",
#                    delim = ",",
#                    col_types = cols(
#                      countyfip = col_double(),
#                      age = col_double(),
#                      own_dwelling = col_double(),
#                      fam_size_1 = col_double(),
#                      fam_size_2 = col_double(),
#                      fam_size_more = col_double(),
#                      married = col_double(),
#                      educ_less_than_12 = col_double(),
#                      educ_high_school = col_double(),
#                      educ_some_college = col_double(),
#                      educ_full_college = col_double(),
#                      educ_gtr_college = col_double(),
#                      employed = col_double(),
#                      female = col_double(),
#                      inctot = col_double(),
#                      population = col_double()
#                    ))

main_data <- read_csv("~/Dropbox/Research/GPW/data/ACS_microdata_collapsed_by_age.csv")

#outcome_vars <- c("own_dwelling", "married", "employed", "inctot", 
#                  "fam_size_1", "fam_size_2", "fam_size_more", "female")

outcome_vars <- c("inctot")


# Step 0: Collapse data to correct setting
# Step 1: Select outcomes
# Step 2: Determine max B approximation for given outcome
# Step 3: Estimate optrdd

reg <- main_data %>% select(one_of(outcome_vars), age, population) %>% 
  mutate(age = age - 1) %>%
  mutate(own_dwelling = own_dwelling*100, married= married*100, employed = employed*100,
         fam_size_1 = fam_size_1*100, fam_size_2 = fam_size_2*100, fam_size_more = fam_size_more*100,
         female = female*100) %>%
  filter(age >= 50 & age <= 80) %>% 
  group_by(age) %>%
  gather(variable, outcome, -age, -population) %>%
  group_by(age, variable) %>%
  summarise(outcome = weighted.mean(outcome, population),
            population = sum(population))  %>%
  mutate(W = as.numeric(age >= 65)) 

# Calculate max B
overall_reg <- main_data %>% select(one_of(outcome_vars), age, population) %>% 
  mutate(age = age - 1) %>%
  mutate(own_dwelling = own_dwelling*100, married= married*100, employed = employed*100,
         fam_size_1 = fam_size_1*100, fam_size_2 = fam_size_2*100, fam_size_more = fam_size_more*100,
         female = female*100) %>%
  filter(age >= 50 & age <= 80) %>% 
  gather(variable, outcome, -age, -population) %>%
  group_by(age,  variable) %>%
  summarise(outcome = weighted.mean(outcome, population),
            population = sum(population))  %>%
  mutate(W = as.numeric(age >= 65)) %>%
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
  summarise(max_B = max(abs(B), na.rm=TRUE)) %>% right_join(tibble(variable = varnames, B_reg = B_reg))

model_output <- vector("list",length(outcome_vars))
for (i in seq(1,length(outcome_vars))) {
  varname = outcome_vars[i]
  data_input <- reg %>% filter(variable== varname) 
  out.1 = optrdd(X=data_input$age, Y=data_input$outcome, W=data_input$W, 
                 max.second.derivative =max_B$B_reg[max_B$variable == varname], 
                 estimation.point = 65, optimizer="mosek", try.elnet.for.sigma.sq = TRUE)      
  print(out.1); plot(out.1, xlim = c(50, 80))
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
    summarise(y_hat_minus = sum(y_hat_minus, na.rm=TRUE),
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
  ggsave(paste0("~/Dropbox/Research/GPW/graphs/", varname, "optrdd_impute.pdf"))
}

fileConn<-file("~/Dropbox/Research/GPW/tables/main_rd_optrdd_cov.tex")
writeLines(file_output, fileConn)
close(fileConn)


# 1) Drop Counties with age bins < 10
# 2) Recut the data into PUMAs which are equal population sized
# 3) 

