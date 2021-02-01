
varname = "has_insurance"
ntl_est = national_acs_estimates %>% 
  mutate(age = 65) 
ntl_data =  acs_puma_data$acs_puma_data_collapsed %>% 
  gather(variable, outcome, -age) %>%
  filter(age != 65) %>%
  zap_labels()  
reg_est = state_acs_estimates %>% filter(state != "DC")  
reg_data = state_data_list$state_acs_data %>%
  filter(age != 65) %>%
  zap_labels() %>% filter(state != "DC") 

estimated_all = ntl_est %>% 
  filter(variable == varname) %>%
  mutate(state = "US") %>%
  bind_rows(reg_est  %>% 
              filter(variable == varname)) %>%
  mutate(age = 65) %>%
  mutate(significant = test_sig(ci_lower, ci_upper)) 
plot_data_all = ntl_data %>% 
  filter(variable == varname) %>%
  mutate(outcome = outcome ) %>%
  mutate(state = "US") %>%
  bind_rows(reg_data %>% 
              filter(variable == varname) %>%
              left_join(estimated_all %>% 
                          select(state, lower_estimate))
  )
plot_data_all2 = plot_data_all %>% mutate(infer = "data") %>%
  bind_rows(
    estimated_all  %>% select(variable, lower_estimate, upper_estimate, state) %>% 
      mutate(upper_estimate = upper_estimate, 
             lower_estimate = lower_estimate) %>%
      gather(key, outcome, -variable, -state) %>%
      mutate(age = case_when(key == "lower_estimate" ~  64.9, TRUE ~ 65.1)) %>%
      mutate(infer = case_when(key == "lower_estimate" ~ "infer_left",
                               key == "upper_estimate" ~ "infer_right"))
  )
g = ggplot() + 
  geom_point(data = plot_data_all2 %>% filter(state == "US" & infer == "data"),
            aes(y = outcome, x=age, group = factor(state)),  
            color =dred, size = 4) 

g +
  scale_shape_manual(values =c("circle", "triangle", "triangle filled")) +
  #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
  theme_classic() +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),      
        axis.title.x = element_text(size=20)) 

ggsave("graphs/presentation_has_insuranceA.pdf", width = 7, height = 3.5)
g = ggplot() + 
  geom_point(data = plot_data_all2 %>% filter(state == "US"),
             aes(y = outcome, x=age, group = factor(state), shape = factor(infer)),  
             color =dred, size = 4, show.legend = FALSE)  
  

paste_val = make_paste_vals(national_acs_estimates, 
                            state_acs_estimates_var,
                            varname)
g +
  scale_shape_manual(values =c("circle", "triangle", "triangle filled")) +
  #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
  theme_classic() +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),    
        axis.title.x = element_text(size=20)) + 
  annotate("text" , x = 75, y = 0.95, 
           label=paste_val$paste_val, size = 6, 
           hjust = 1, vjust = 1)

ggsave("graphs/presentation_has_insuranceB.pdf", width = 7, height = 3.5)

g = ggplot() +
  geom_point(data = plot_data_all2, 
            aes(y = outcome, x=age, group = factor(state), shape = factor(infer)),  
            color = "grey",  alpha = 0.4, size = 1, show.legend = FALSE) + 
  geom_point(data = plot_data_all2 %>% filter(state == "US"),
             aes(y = outcome, x=age, group = factor(state), shape = factor(infer)),  
             color =dred, size = 4, show.legend = FALSE) 

g +
  scale_shape_manual(values =c("circle", "triangle", "triangle filled")) +
  #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
  theme_classic() +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),        
        axis.title.x = element_text(size=20)) + 
  annotate("text" , x = 75, y = 0.95, 
           label=paste_val$paste_val, size = 6, 
           hjust = 1, vjust = 1)

ggsave("graphs/presentation_has_insuranceC.pdf", width = 7, height = 3.5)


g = ggplot() +
  geom_point(data = plot_data_all2, 
             aes(y = outcome, x=age, group = factor(state), shape = factor(infer)),  
             color = "grey",  alpha = 0.4, size = 1, show.legend = FALSE) + 
  geom_point(data = plot_data_all2 %>% filter(state %in% c("MA", "TX")), 
             aes(y = outcome, x=age, group = factor(state), shape = factor(infer), 
                 color = factor(state)),
             size = 4, show.legend = FALSE) + 
  geom_point(data = plot_data_all2 %>% filter(state == "US"),
             aes(y = outcome, x=age, group = factor(state), shape = factor(infer)),  
             color =dred, size = 4, show.legend = FALSE) 

g +
  scale_shape_manual(values =c("circle", "triangle", "triangle filled")) +
  scale_color_manual(values =c("black", dblue)) +
  #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
  theme_classic() +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),     
        axis.title.x = element_text(size=20)) + 
  annotate("text" , x = 66.5, y = 0.845, 
           label="TX", size = 6, color = dblue,
           hjust = 1, vjust = 1) + 
  annotate("text" , x = 64.5, y = 1, 
           label="MA", size = 6, color = "black",
           hjust = 1, vjust = 1) +
  annotate("text" , x = 66.5, y = 0.91, 
           label="US", size = 6, color = dred,
           hjust = 1, vjust = 1)
ggsave("graphs/presentation_has_insuranceD.pdf", width = 7, height = 3.5)


g = ggplot() + 
  # geom_line(data = plot_data_all2, 
  #           aes(y = outcome, x=age, group = factor(state)),  
  #           color = "grey",  alpha = 0.4, size = 1) +
  geom_point(data = plot_data_all2, 
             aes(y = outcome, x=age, group = factor(state),
                 shape = factor(infer)),  
             color = "grey",  alpha = 0.4, 
             size = 3, show.legend = FALSE) +
  # geom_line(data = plot_data_all2 %>% filter(state == "US"), 
  #           aes(y = outcome, x=age, group = factor(state)),  
  #           color = dred,size = 2) +
  geom_point(data = plot_data_all2 %>% filter(state == "US"), 
             aes(y = outcome, x=age, group = factor(state),
                 shape = factor(infer)),  
             color = dred,size = 4, show.legend = FALSE) +
  scale_shape_manual(values =c("circle", "triangle", "triangle filled")) +
  #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
  theme_classic() +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),      
        axis.title.x = element_text(size=20)) 
g +
  #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
  theme_classic() +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),    
        axis.title.x = element_text(size=20)) + 
  annotate("text" , x = 75, y = 0.95, 
           label=paste_val$paste_val, size = 6, 
           hjust = 1, vjust = 1)

ggsave("graphs/presentation_has_insuranceE.pdf", width = 7, height = 3.5)


g = ggplot() + 
  geom_line(data = plot_data_all2,
            aes(y = outcome, x=age, group = factor(state)),
            color = "grey",  alpha = 0.4, size = 1) +
  geom_point(data = plot_data_all2, 
             aes(y = outcome, x=age, group = factor(state),
                 shape = factor(infer)),  
             color = "grey",  alpha = 0.4, 
             size = 3, show.legend = FALSE) +
  geom_line(data = plot_data_all2 %>% filter(state == "US"),
            aes(y = outcome, x=age, group = factor(state)),
            color = dred,size = 1) +
  geom_point(data = plot_data_all2 %>% filter(state == "US"), 
             aes(y = outcome, x=age, group = factor(state),
                 shape = factor(infer)),  
             color = dred,size = 4, show.legend = FALSE) +
  scale_shape_manual(values =c("circle", "triangle", "triangle filled")) +
  #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
  theme_classic() +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),   
        axis.title.x = element_text(size=20)) 

g +
  #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
  theme_classic() +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),       
        axis.title.x = element_text(size=20)) + 
  annotate("text" , x = 75, y = 0.95, 
           label=paste_val$paste_val, size = 6, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 0.89, 
           label=paste_val$paste_val2, size =6, 
           hjust = 1, vjust = 1)

ggsave("graphs/presentation_has_insurance.pdf", width = 7, height = 3.5)


varname = "employed"
g =make_fig1(national_acs_estimates_cov,
          acs_puma_data$acs_puma_data_cov_collapsed %>% 
            gather(variable, outcome, -age) %>%
            filter(age != 65) %>%
            zap_labels(), 
          state_acs_estimates_cov %>% filter(state != "DC"),
          state_data_list$state_acs_data_cov %>%
            filter(age != 65) %>%
            zap_labels() %>% filter(state != "DC"),
          varname)
paste_val = make_paste_vals(national_acs_estimates_cov, 
                            state_acs_estimates_cov_var,
                            varname)
g +
  #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
  theme_classic() +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),      
        axis.title.x = element_text(size=20)) + 
  annotate("text" , x = 75, y = 0.75, 
           label=paste_val$paste_val, size = 6, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 0.57, 
           label=paste_val$paste_val2, size =6, 
           hjust = 1, vjust = 1)

ggsave("graphs/presentation_employed.pdf", width = 7, height = 3.5)

varname = "inctot"
g =make_fig1(national_acs_estimates_cov,
             acs_puma_data$acs_puma_data_cov_collapsed %>% 
               gather(variable, outcome, -age) %>%
               filter(age != 65) %>%
               zap_labels(), 
             state_acs_estimates_cov %>% filter(state != "DC"),
             state_data_list$state_acs_data_cov %>%
               filter(age != 65) %>%
               zap_labels() %>% filter(state != "DC"),
             varname)
paste_val = make_paste_vals(national_acs_estimates_cov, 
                            state_acs_estimates_cov_var,
                            varname)
g +
  #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
  theme_classic() +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),     
        axis.title.x = element_text(size=20)) + 
  annotate("text" , x = 75, y = 70000, 
           label=paste_val$paste_val, size = 6, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 57000, 
           label=paste_val$paste_val2, size =6, 
           hjust = 1, vjust = 1)

ggsave("graphs/presentation_inctot.pdf", width = 7, height = 3.5)



varname = "new_bpt"
g = make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed, 
              state_estimates %>% filter(state != "DC"),
              state_data_list$state_data %>% filter(state != "DC"),
              varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "%")
g + 
  annotate("text" , x = 75, y = 0.3, 
           label=paste_vals$paste_val, size = 6, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 0.22, 
           label=paste_vals$paste_val2, size = 6, 
           hjust = 1, vjust = 1) +
  labs(y = "",
       x = "Age") +
  theme(text = element_text(size=24),     
        axis.title.x = element_text(size=20)) 

ggsave("graphs/presentation_new_bpt.pdf", width = 7, height = 3.5)

varname = "avg_riskscore"
g = make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed, 
              state_estimates %>% filter(state != "DC"),
              state_data_list$state_data %>% filter(state != "DC"),
              varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "")
g + 
  annotate("text" , x = 75,  y = 730,
           label=paste_vals$paste_val, size = 6, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 700, 
           label=paste_vals$paste_val2, size = 6, 
           hjust = 1, vjust = 1) +
  theme(text = element_text(size=24),     
        axis.title.x = element_text(size=20)) 
ggsave("graphs/presentation_avg_riskscore.pdf", width = 7, height = 3.5)


varname = "q_coll_12"
g = make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed, 
              state_estimates %>% filter(state != "DC"),
              state_data_list$state_data %>% filter(state != "DC"),
              varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "$")
g + 
  annotate("text" , x = 75, y = 325, 
           label=paste_vals$paste_val, size = 6, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 230, 
           label=paste_vals$paste_val2, size = 6, 
           hjust = 1, vjust = 1) +
  theme(text = element_text(size=24),     
        axis.title.x = element_text(size=20)) 
ggsave("graphs/presentation_q_coll_12.pdf", width = 7, height = 3.5)

varname = "balance_cc_delinq_scale"
g = make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed, 
              state_estimates %>% filter(state != "DC"),
              state_data_list$state_data %>% filter(state != "DC"),
              varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "")
g + 
  annotate("text" , x = 75, y = 0.18, 
           label=paste_vals$paste_val, size = 6, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 0.135, 
           label=paste_vals$paste_val2, size = 6, 
           hjust = 1, vjust = 1)  +
  theme(text = element_text(size=24),     
        axis.title.x = element_text(size=20)) 
ggsave("graphs/presentation_balance_cc_delinq_scale.pdf", width = 7, height = 3.5)

dose_response_data = state_estimates %>% 
  filter(variable == "has_insurance" | variable == "q_coll_12") %>%
  select(state, variable, tau, se) %>%
  filter(state != "DC") %>%
  spread(variable, tau) %>%
  left_join(estimated_all %>% filter(variable == "has_insurance" | variable == "q_coll_12") %>%
            select(state, variable, tau) %>%
              filter(state != "US") %>%
              spread(variable, tau)) %>%
  mutate(q_coll_12 = q_coll_12*-1) %>%
  left_join(state_pop_data)

summary(lm(data = dose_response_data, formula = q_coll_12 ~ has_insurance, weight = pop))

g = ggplot(data = dose_response_data) + 
  geom_text(aes(y = q_coll_12, x = has_insurance, label = state)) + 
  geom_smooth(aes(y = q_coll_12, x = has_insurance, weight = pop ), se = FALSE, method = "lm")
g + 
  theme_classic() +
  theme(text = element_text(size=20),     
        axis.title.x = element_text(size=16)) +
  labs(y = "",
       x = "Estimated Effect on Insurance Rates") 

ggsave("graphs/presentation_dose_response.pdf", width = 4.5, height = 3.5)
