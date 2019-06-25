
# Construct predicted values 
# Automate procedure for selecting B
# Automate looping over outcomes

library(optrdd)
library(tidyverse)
library(vroom)
library(lfe)

# use final_zip_level5080
# gen zip3 = floor(zipcode /100)
# collapse total_bal_sl-new_bal_ttl_dq (rawsum) pop = population [aweight=population ], by(zip3 state age )
# outsheet using final_zip3_level.csv, comma names replace


outcome_vars <- c("q_coll_12", "n_coll_12", "q_coll_12_gt0", 
                  "balance_ttl_delinq",
                  "balance_cc_delinq",
                  "balance_mort_delinq",
                  "avg_riskscore", "fc","bpt")
outcome_var_labels <- c("Total Collections (\\$)", "Number of Collections", 
                        "Collections > 0", "Total", "Credit Card", "Mortgage",
                        "Risk Score", "Foreclosure", "Bankruptcy")

#main_data <- read_csv("~/Dropbox/GPW/data/final_zip3_level.csv") %>% 
#  filter(state != "PR" & state != "FM" & state != "GU" & state != "VI") %>%
#  select(one_of(outcome_vars), age, pop, zip3, state) %>% 
#  rename(location = zip3) %>%
#  group_by(age,  location, state) %>%
#  gather(variable, outcome, -age, -pop, -location, -state) %>%
#  group_by(age,  location, state, variable)

main_data <- vroom("~/Dropbox/GPW/data/final_county_level.csv", delim=",")

main_data <- main_data %>% 
  filter(state != "PR" & state != "FM" & state != "GU" & 
           state != "VI" & state != "AS" & state != "MP" & state != "MH" &
           state != "MH" & state != "PW"  & state != "OA") %>%
  select(one_of(outcome_vars), age, population, census_code, state) %>% 
  rename(location = census_code) %>%
  mutate(age = age - 1) %>%
  mutate(fc = fc*100, bpt= bpt*100, 
         q_coll_12_gt0 = q_coll_12_gt0 *100) %>%
  gather(variable, outcome, -age, -population, -location, -state) %>%
  group_by(age,  location, state, variable) %>%
  summarize(outcome = weighted.mean(outcome, population),
            population = sum(population))
  




# Step 0: Collapse data to correct setting
# Step 1: Select outcomes
# Step 2: Determine max B approxmiation for given outcome
# Step 3: Estimate optrdd

reg <- main_data %>% 
  filter(age > 55 & age < 75) %>% 
  filter(age != 65) %>% 
  mutate(W = as.numeric(age > 65)) %>%
  group_by(location, state) %>%
  mutate(location_pop = min(population)) 

# Calculate max B
overall_reg <-main_data %>% 
  filter(age > 40 & age < 80) %>% 
  filter(age != 65) %>% 
  group_by(age,  variable) %>%
  summarize(outcome = weighted.mean(outcome, population),
            population = sum(population))  %>%
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
                 data = overall_reg %>% 
                   filter( variable == varname) %>% 
                   filter(W == 0))
  B_reg <- c(4*abs(modelfit$coefficients[3]), B_reg)
  varnames <- c(varname, varnames)
}
  
max_B = overall_reg %>%
  group_by(variable) %>%
  summarize(max_B = max(abs(B), na.rm=TRUE)) %>% 
  right_join(tibble(variable = varnames, B_reg = B_reg))

model_output <- vector("list",length(outcome_vars))
model_output_linear <- vector("list",length(outcome_vars))
model_output_quad<- vector("list",length(outcome_vars))
for (i in seq(1,length(outcome_vars))) {
  varname = outcome_vars[i]
  data_input <- reg %>% filter(variable== varname) 
  out.1 = optrdd(X=data_input$age, Y=data_input$outcome, W=data_input$W, 
                 max.second.derivative =max_B$B_reg[max_B$variable == varname], 
                 estimation.point = 65, 
                 optimizer="mosek", 
                 try.elnet.for.sigma.sq = TRUE,
                 verbose = FALSE)      
  print(out.1); plot(out.1, xlim = c(55, 75))
  model_output[[i]] <- out.1
  model_output_linear[[i]] <- felm(outcome ~ t + W + t*W,
                                   data = data_input %>% 
                                     mutate(t = age - 65) %>% 
                                     filter(abs(t) < 3))
  model_output_quad[[i]] <- felm(outcome ~ t + I(t^2) + W + t*W + I(t^2)*W ,
                                   data = data_input %>% 
                                     mutate(t = age - 65) %>% 
                                     filter(abs(t) < 5))
}

# Loop over the variables in outcome_vars
file_output <- c()
for (i in seq(1,length(outcome_vars))) {
  varname = outcome_vars[i]
  print(varname)
  out.1 = model_output[[i]]
  out.linear = model_output_linear[[i]]
  out.quad = model_output_quad[[i]]
  print(out.1)
  data_input <- reg %>% filter(variable== varname) 
  model_fit <- bind_rows(tibble( age     = round(out.1$gamma.fun.0$xx), 
                                 y_minus = out.1$gamma.fun.0$gamma),
                         tibble( age     = round(out.1$gamma.fun.1$xx), 
                                 y_plus = out.1$gamma.fun.1$gamma))
  mean_64 <- mean(data_input$outcome[data_input$age == 64])
  if (i == 1) {
    file_output <- c(file_output, paste0( "Debt Collections ", " &",
                                          " & ", " & ", " & ", " \\\\"))
  }
  if (i == 4) {
    file_output <- c(file_output, paste0( "Delinquent Debt ", " &",
                                          " & ", " & ", " & ", " \\\\"))
  }
  var_label = outcome_var_labels[i]
  if ( i < 7 ) {
    var_label = paste0(" \\hspace{10pt}", outcome_var_labels[i])
  }
  file_output <- c(file_output, 
                   paste0(var_label, " & " , 
                          signif(mean_64, digits=2)," & ",
                          signif(out.1$tau.hat, digits=2)," & ",
                          signif(out.linear$coefficients[3], digits=2)," & ",
                          signif(out.quad$coefficients[4], digits=2),
                          "\\\\"))
  file_output <- c(file_output, 
                   paste0("   ", " & ", "  ", " & ",
                          "[ ",
                          signif(out.1$tau.hat-out.1$tau.plusminus, digits=2),
                          ",",
                          signif(out.1$tau.hat+out.1$tau.plusminus, digits=2),
                          " ] & ",
                          signif(out.linear$rse[3], digits=2)," & ",
                          signif(out.quad$rse[4], digits=2),
                          "\\\\"))
  
  
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

states <- unique(reg$state)
model_output_state <- vector("list",length(states))
model_output_state2 <- vector("list",length(states))
for (i in seq(1,length(states))) {
  statename = states[i]
  varname = "q_coll_12"
  data_input <- reg %>% filter(variable== varname & state == statename)
  out.1 = optrdd(X=data_input$age, Y=data_input$outcome, W=data_input$W, 
                 max.second.derivative =max_B$B_reg[max_B$variable == varname], 
                 estimation.point = 65, 
                 optimizer="mosek", 
                 verbose = FALSE)      
  print(out.1); plot(out.1, xlim = c(55, 75))
  model_output_state[[i]] <- out.1
  varname = "bpt"
  data_input <- reg %>% filter(variable== varname & state == statename)
  out.1 = optrdd(X=data_input$age, Y=data_input$outcome, W=data_input$W, 
                 max.second.derivative =max_B$B_reg[max_B$variable == varname], 
                 estimation.point = 65, 
                 optimizer="mosek", 
                 verbose = FALSE)      
  print(out.1); plot(out.1, xlim = c(55, 75))
  model_output_state2[[i]] <- out.1
}

file_output <- c()
for (i in seq(1,length(states))) {
  statename = states[i]
  varname = "q_coll_12"
  out.1 = model_output_state[[i]]
  
  if (is.null(out.1$tau.hat) == FALSE) {
    data_input <- reg %>% filter(variable== varname & state == statename)
    model_fit <- bind_rows(tibble( age     = round(out.1$gamma.fun.0$xx), 
                                   y_minus = out.1$gamma.fun.0$gamma),
                           tibble( age     = round(out.1$gamma.fun.1$xx), 
                                   y_plus = out.1$gamma.fun.1$gamma))
    predicted_val <- data_input %>% right_join(model_fit) %>%
      mutate(y_hat_minus = -1* y_minus * outcome,
             y_hat_plus = y_plus * outcome) %>%
      ungroup() %>%
      summarize(y_hat_minus = sum(y_hat_minus, na.rm=TRUE),
                y_hat_plus = sum(y_hat_plus, na.rm=TRUE)) %>%
      mutate(age = 65) %>%
      gather(y_hat, age) %>%
      rename(outcome = age) %>%
      mutate(age = 65) %>% 
      spread(y_hat, outcome)
    var_label = paste0(" \\hspace{10pt}", states[i])
    parta <- paste0(var_label, " & " , 
                    signif(predicted_val$y_hat_minus, digits = 2), " & ",
                    signif(predicted_val$y_hat_plus, digits = 2), " & ",
                    signif(out.1$tau.hat, digits=2)," & ",
                    " ",  " & ")
  }
  else {
    var_label = paste0(" \\hspace{10pt}", states[i])
    parta <- paste0(var_label, " & " , 
                    " ", " & ",
                    " ", " & ",
                    " ", " & ",
                    " ",  "& ")
  } 
  varname = "bpt"
  out.1 = model_output_state2[[i]]
  
  if (is.null(out.1$tau.hat) == FALSE) {
    data_input <- reg %>% filter(variable== varname  & state == statename)
    model_fit <- bind_rows(tibble( age     = round(out.1$gamma.fun.0$xx), 
                                   y_minus = out.1$gamma.fun.0$gamma),
                           tibble( age     = round(out.1$gamma.fun.1$xx), 
                                   y_plus = out.1$gamma.fun.1$gamma))
    predicted_val <- data_input %>% right_join(model_fit) %>%
      mutate(y_hat_minus = -1* y_minus * outcome,
             y_hat_plus = y_plus * outcome) %>%
      ungroup() %>%
      summarize(y_hat_minus = sum(y_hat_minus, na.rm=TRUE),
                y_hat_plus = sum(y_hat_plus, na.rm=TRUE)) %>%
      mutate(age = 65) %>%
      gather(y_hat, age) %>%
      rename(outcome = age) %>%
      mutate(age = 65) %>% 
      spread(y_hat, outcome)
    partb <- paste0(" ", " & " , 
                    signif(predicted_val$y_hat_minus, digits = 2), " & ",
                    signif(predicted_val$y_hat_plus, digits = 2), " & ",
                    signif(out.1$tau.hat, digits=2)," & ",
                    " ",  "\\\\" )
  }
  else {
    var_label = paste0(" \\hspace{10pt}", states[i])
    partb <- paste0(var_label, " & " , 
                    " ", " & ",
                    " ", " & ",
                    " ", " & ",
                    " ",  "\\\\")
  }   
    file_output <- c(file_output, paste0(parta, partb))
}

fileConn<-file("~/Dropbox/GPW/tables/states_rd_optrdd.tex")
writeLines(file_output, fileConn)
close(fileConn)
