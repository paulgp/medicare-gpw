

library(readr)
library(tidyverse)
library(ggrepel)
state_collapse <- read_csv("~/Dropbox/GPW/data/state_collapse.csv")
state_collapse_rd <- read_csv("~/Dropbox/GPW/data/state_collapse_rd.csv")
state_collapse_placebo <- read_csv("~/Dropbox/GPW/data/state_collapse_63_64.csv")

state_order <- state_collapse %>% filter(!is.na(state_name)) %>% 
  arrange(desc(q_coll_12)) %>% select(state_name) %>% mutate(state_name = str_to_title(state_name))

ggplot(data = state_collapse %>% filter(!is.na(state_name) & str_to_title(state_name) %in% state_order$state_name) %>% 
         mutate(state_name = str_to_title(state_name))) +
  geom_col(aes(y = q_coll_12, x = factor(state_name, levels=state_order$state_name)), 
               fill =  "#007CB2", color="black") +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 280)) +
  scale_x_discrete(breaks=state_order$state_name[seq(1,51,2)]) +
  labs(x="", y="", title="Per-capita annual collections debt across states", fill="")
ggsave("~/Dropbox/GPW/graphs/q_coll_12_5564_match.pdf", width = 12, height = 7)

state_order <- state_collapse %>% filter(!is.na(state_name)) %>% 
  arrange(desc(pctui_young)) %>% select(state_name) %>% mutate(state_name = str_to_title(state_name))

ggplot(data = state_collapse %>% filter(!is.na(state_name) & str_to_title(state_name) %in% state_order$state_name) %>% 
         mutate(state_name = str_to_title(state_name))) +
  geom_col(aes(y = pctui_young, x = factor(state_name, levels=state_order$state_name)), 
           fill =  "#007CB2", color="black") +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.25)) +
  scale_x_discrete(breaks=state_order$state_name[seq(1,51,2)]) +
  labs(x="", y="", title="Near-elderly uninsurance rates", fill="")
ggsave("~/Dropbox/GPW/graphs/pctui_5564.pdf", width = 12, height = 7)


ggplot(data = state_collapse %>% filter(!is.na(state_name)) %>% mutate(state_name = str_to_title(state_name))) +
  geom_text(aes(y = q_coll_12, x = pctui_young, label = state), color =  "#007CB2", size = 6) +
  theme_minimal() +
  theme(text = element_text(size=18)) +
  labs(x="Near-elderly uninsurance rates", y="", title="Correlation of collections debt and uninsurance rates", fill="")    
ggsave("~/Dropbox/GPW/graphs/q_coll_12_vs_pctui_states_intro.pdf", width = 12, height = 7)

state_order <- state_collapse_rd %>% filter(!is.na(state_name) & young_elderly == 0) %>% 
  arrange(desc(q_coll_12)) %>% select(state_name) %>% mutate(state_name = str_to_title(state_name))

ggplot(data = state_collapse_rd %>% filter(!is.na(state_name) & young_elderly == 0) %>%
         mutate(state_name = str_to_title(state_name))
       ) +
  geom_col(aes(y = q_coll_12, x = factor(state_name, levels=state_order$state_name)), 
           color = "black", fill =  "white") +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18)) +
  scale_x_discrete(breaks=state_order$state_name[seq(1,50,2)]) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 280)) +
  labs(x="", y="", title="Per-capita annual collections debt across states at 65, pre-Medicare", fill="")
ggsave("~/Dropbox/GPW/graphs/q_coll_12_65minus.pdf", width = 12, height = 7)


theta1 <- state_collapse_rd %>% group_by(young_elderly) %>% 
  summarize(var = var(q_coll_12)) %>%
  spread(young_elderly, var) %>% mutate(theta1 = round(`1`/`0`,2))
label_theta1 <- paste("Theta1:", theta1$theta1)

ggplot(data = state_collapse_rd %>% filter(!is.na(state_name)) %>%
         mutate(state_name = str_to_title(state_name))
  ) +
  geom_col(aes(y = q_coll_12, x = factor(state_name, levels=state_order$state_name), 
               fill =  as.factor(young_elderly), color=as.factor(young_elderly), 
               alpha=as.factor(young_elderly)),
           stat="identity",position ="identity") +
  #annotate("text", x = "Pennsylvania", y = 200, label = label_theta1, size = 14) +
  scale_colour_manual(values=c("black","black")) +
  scale_fill_manual(values=c("white","#007CB2")) +
  scale_alpha_manual(values=c(.2, .6)) +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18), legend.position="none") +
  scale_x_discrete(breaks=state_order$state_name[seq(1,50,2)]) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 280)) +
  labs(x="", y="", title="Per-capita annual collections debt across states at 65",
       subtitle= "with and without Medicare",
       fill="", color= "", alpha = "")
ggsave("~/Dropbox/GPW/graphs/q_coll_12_65_plusminus.pdf", width = 12, height = 7)


state_order_placebo <- state_collapse_placebo %>% filter(!is.na(state_name) & age== 62) %>% 
  arrange(desc(q_coll_12)) %>% select(state_name) %>% mutate(state_name = str_to_title(state_name))


theta1 <- state_collapse_placebo %>% filter(age < 64) %>% group_by(age) %>% 
  summarize(var = var(q_coll_12)) %>%
  spread(age, var) %>% mutate(theta1 = round(`63`/`62`,2))
label_theta1 <- paste("Theta1:", theta1$theta1)

ggplot(data = state_collapse_placebo %>% filter(!is.na(state_name) & age < 64) %>%
         mutate(state_name = str_to_title(state_name))
) +
  geom_bar(aes(y = q_coll_12, x = factor(state_name, levels=state_order_placebo$state_name), 
               fill =  as.factor(age), color=as.factor(age), 
               alpha=as.factor(age)),
           stat="identity",position ="identity") +
  #annotate("text", x = "Pennsylvania", y = 200, label = label_theta1) +
  scale_colour_manual(values=c("black","black")) +
  scale_fill_manual(values=c("white","#007CB2")) +
  scale_alpha_manual(values=c(.2, .6)) +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18), legend.position="none") +
  scale_x_discrete(breaks=state_order_placebo$state_name[seq(1,52,2)]) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 280)) +
  labs(x="", y="", title="Per-capita annual collections debt",
       subtitle="across states at 62 and 63",
       fill="", color= "", alpha = "")
ggsave("~/Dropbox/GPW/graphs/q_coll_12_placebo.pdf", width = 12, height = 7)


### Border design
state_collapse2 <- state_collapse_rd %>% 
  filter(!is.na(state_name) & !is.na(q_coll_12_border)) %>%
  mutate(state_name = str_to_title(state_name)) %>%
  select(q_coll_12_border, q_coll_12, young_elderly, state_name) %>%
  rename(q_coll_12_nonborder = q_coll_12) %>%
  gather(location, q_coll_12,  -state_name, -young_elderly) %>%
  unite(location_young, location, young_elderly) %>%
  spread(location_young, q_coll_12)

state_order <- state_collapse2 %>% 
  arrange(desc(q_coll_12_border_0)) %>% select(state_name) %>% 
  mutate(state_name = str_to_title(state_name))

label_theta2 <- paste("Theta2:", .705 )
ggplot(data = state_collapse2 %>%
         mutate(state_name = str_to_title(state_name)),
       aes(x = factor(state_name, levels=state_order$state_name))
) +
  geom_bar(aes(y = q_coll_12_border_0), 
           color = "black", fill = "white", alpha = 0.2,
           stat="identity",position ="identity") +
  geom_bar(aes(y = q_coll_12_border_1), 
           color = "black", fill = "#007CB2", alpha = 0.6,
           stat="identity",position ="identity") +
  #annotate("text", x = "Pennsylvania", y = 200, label = label_theta2) +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18), legend.position="none") +
  scale_x_discrete(breaks=state_order$state_name[seq(1,52,2)]) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 280)) +
  labs(x="", y="", title="Per-capita annual collections debt at state borders at 65,",
       subtitle = "with and without Medicare", 
       fill="", color= "", alpha = "")
ggsave("~/Dropbox/GPW/graphs/q_coll_12_65_plusminus_border.pdf", width = 12, height = 7)


state_order <- state_collapse2 %>% 
  arrange(desc(q_coll_12_nonborder_0)) %>% select(state_name) %>% 
  mutate(state_name = str_to_title(state_name))

label_theta3 <- paste("Theta3:", .52 )
ggplot(data = state_collapse2 %>%
         mutate(state_name = str_to_title(state_name)),
       aes(x = factor(state_name, levels=state_order$state_name))
) +
  geom_bar(aes(y = q_coll_12_nonborder_0), 
           color = "black", fill = "white", alpha = 0.2,
           stat="identity",position ="identity") +
  geom_bar(aes(y = q_coll_12_border_1), 
           color = "black", fill = "#007CB2",  alpha = 0.6, 
           stat="identity",position ="identity") +
  #annotate("text", x = "Pennsylvania", y = 200, label = label_theta3) +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18), legend.position="none") +
  scale_x_discrete(breaks=state_order$state_name[seq(1,52,2)]) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 280)) +
  labs(x="", y="", title="Per-capita annual collections debt,",
       subtitle = "all without Medicare, border without Medicare", 
       fill="", color= "", alpha = "")
ggsave("~/Dropbox/GPW/graphs/q_coll_12_65_plusminus_border_theta3.pdf", width = 12, height = 7)


