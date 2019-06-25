

library(readr)
library(tidyverse)
library(ggrepel)
state_collapse <- read_csv("~/Dropbox/GPW/data/state_collapse.csv")
state_collapse_rd <- read_csv("~/Dropbox/GPW/data/state_collapse_rd.csv")
state_collapse_placebo <- read_csv("~/Dropbox/GPW/data/state_collapse_63_64.csv")

state_order <- state_collapse_rd %>% filter(!is.na(state_name) & young_elderly == 0) %>% 
  arrange(desc(avg_riskscore)) %>% select(state_name) %>% mutate(state_name = str_to_title(state_name))

ggplot(data = state_collapse_rd %>% filter(!is.na(state_name) & young_elderly == 0) %>%
         mutate(state_name = str_to_title(state_name))
       ) +
  geom_point(aes(y = avg_riskscore, x = factor(state_name, levels=state_order$state_name)), 
           color = "black", fill =  "white") +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18)) +
  scale_x_discrete(breaks=state_order$state_name[seq(1,50,2)]) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x="", y="", title="Per-capita annual collections debt across states at 65, pre-Medicare", fill="")
ggsave("~/Dropbox/GPW/graphs/avg_riskscore_65minus.pdf", width = 12, height = 7)


theta1 <- state_collapse_rd %>% group_by(young_elderly) %>% 
  summarize(var = var(avg_riskscore)) %>%
  spread(young_elderly, var) %>% mutate(theta1 = round(`1`/`0`,2))
label_theta1 <- paste("Theta1:", theta1$theta1)

ggplot(data = state_collapse_rd %>% filter(!is.na(state_name)) %>%
         mutate(state_name = str_to_title(state_name))
  ) +
  geom_point(aes(y = avg_riskscore, x = factor(state_name, levels=state_order$state_name), 
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
  scale_y_continuous(expand = c(0, 0)) +
  labs(x="", y="", title="Per-capita annual collections debt across states at 65",
       subtitle= "with and without Medicare",
       fill="", color= "", alpha = "")
ggsave("~/Dropbox/GPW/graphs/avg_riskscore_65_plusminus.pdf", width = 12, height = 7)


state_order_placebo <- state_collapse_placebo %>% filter(!is.na(state_name) & age== 62) %>% 
  arrange(desc(avg_riskscore)) %>% select(state_name) %>% mutate(state_name = str_to_title(state_name))


theta1 <- state_collapse_placebo %>% filter(age < 64) %>% group_by(age) %>% 
  summarize(var = var(avg_riskscore)) %>%
  spread(age, var) %>% mutate(theta1 = round(`63`/`62`,2))
label_theta1 <- paste("Theta1:", theta1$theta1)

ggplot(data = state_collapse_placebo %>% filter(!is.na(state_name) & age < 64) %>%
         mutate(state_name = str_to_title(state_name))
) +
  geom_point(aes(y = avg_riskscore, x = factor(state_name, levels=state_order_placebo$state_name), 
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
  scale_y_continuous(expand = c(0, 0)) +
  labs(x="", y="", title="Per-capita annual collections debt",
       subtitle="across states at 62 and 63",
       fill="", color= "", alpha = "")
ggsave("~/Dropbox/GPW/graphs/avg_riskscore_placebo.pdf", width = 12, height = 7)


### Border design
state_collapse2 <- state_collapse_rd %>% 
  filter(!is.na(state_name) & !is.na(avg_riskscore_border)) %>%
  mutate(state_name = str_to_title(state_name)) %>%
  select(avg_riskscore_border, avg_riskscore, young_elderly, state_name) %>%
  rename(avg_riskscore_nonborder = avg_riskscore) %>%
  gather(location, avg_riskscore,  -state_name, -young_elderly) %>%
  unite(location_young, location, young_elderly) %>%
  spread(location_young, avg_riskscore)

state_order <- state_collapse2 %>% 
  arrange(desc(avg_riskscore_border_0)) %>% select(state_name) %>% 
  mutate(state_name = str_to_title(state_name))

label_theta2 <- paste("Theta2:", .705 )
ggplot(data = state_collapse2 %>%
         mutate(state_name = str_to_title(state_name)),
       aes(x = factor(state_name, levels=state_order$state_name))
) +
  geom_point(aes(y = avg_riskscore_border_0), 
           color = "black", fill = "white", alpha = 0.2,
           stat="identity",position ="identity") +
  geom_point(aes(y = avg_riskscore_border_1), 
           color = "black", fill = "#007CB2", alpha = 0.6,
           stat="identity",position ="identity") +
  #annotate("text", x = "Pennsylvania", y = 200, label = label_theta2) +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18), legend.position="none") +
  scale_x_discrete(breaks=state_order$state_name[seq(1,52,2)]) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x="", y="", title="Per-capita annual collections debt at state borders at 65,",
       subtitle = "with and without Medicare", 
       fill="", color= "", alpha = "")
ggsave("~/Dropbox/GPW/graphs/avg_riskscore_65_plusminus_border.pdf", width = 12, height = 7)


state_order <- state_collapse2 %>% 
  arrange(desc(avg_riskscore_nonborder_0)) %>% select(state_name) %>% 
  mutate(state_name = str_to_title(state_name))

label_theta3 <- paste("Theta3:", .52 )
ggplot(data = state_collapse2 %>%
         mutate(state_name = str_to_title(state_name)),
       aes(x = factor(state_name, levels=state_order$state_name))
) +
  geom_point(aes(y = avg_riskscore_nonborder_0), 
           color = "black", fill = "white", alpha = 0.2,
           stat="identity",position ="identity") +
  geom_point(aes(y = avg_riskscore_border_1), 
           color = "black", fill = "#007CB2",  alpha = 0.6, 
           stat="identity",position ="identity") +
  #annotate("text", x = "Pennsylvania", y = 200, label = label_theta3) +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18), legend.position="none") +
  scale_x_discrete(breaks=state_order$state_name[seq(1,52,2)]) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x="", y="", title="Per-capita annual collections debt,",
       subtitle = "all without Medicare, border without Medicare", 
       fill="", color= "", alpha = "")
ggsave("~/Dropbox/GPW/graphs/avg_riskscore_65_plusminus_border_theta3.pdf", width = 12, height = 7)


