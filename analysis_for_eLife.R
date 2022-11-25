
library(tidyverse)
library(patchwork)
#These run the models

model_run <- function(x) {
  input <- input %>%
    mutate("{{x}}" := as.numeric({{x}}))
  f <- paste0("no_symptoms_12_months ~ ", x)
  f2 <- paste0("mcs_1 ~ ", "`",x,"`")
  f3 <- paste0("mcs_1 ~ ", "`",x,"`")
  m2 <- glm(f , data = input, family = poisson(link = "log"))
  m3 <- glm(f2 , data = input)
  m4 <- glm(f3 , data = input)
  broom::tidy(m2) %>% 
    mutate(model = "symptoms") %>%
    bind_rows(
      broom::tidy(m3) %>% 
        mutate(model = "PCS")) %>%
    
    # ) 
    #   bind_rows(
    #     broom::tidy(m4) %>% 
    #       mutate(model = "MCS")) %>% 
    #     
    
    filter(term != "(Intercept)")
}

model_run_adj <- function(x) {
  input <- input %>%
    mutate("{{x}}" := as.numeric({{x}}))
  f <- paste0("no_symptoms_12_months ~ ", x, " +  age + sex")
  f2 <- paste0("mcs_1 ~ ", "`",x,"`", " +  age + sex")
  f3 <-paste0("mcs_1 ~ ", "`",x,"`", " + age + sex")
  m2 <- glm(f , data = input, family = poisson(link = "log"))
  m3 <- lm(f2 , data = input)
  m4 <- lm(f3 , data = input)
  broom::tidy(m2) %>% 
    mutate(model = "symptoms") %>%
    bind_rows(
      broom::tidy(m3) %>% 
        mutate(model = "PCS")) %>%
    
    # ) %>%
    # bind_rows(
    #   broom::tidy(m4) %>% 
    #     mutate(model = "MCS") 
    #   
    # ) %>%
    filter(term != "(Intercept)") %>%
    filter(term %in% x)
}

#bring in the data 

input <- read_tsv("patient_data.tsv")

#do analysis


pop_res <-map_dfr(total_inc, model_run)

pop_res_adj <-map_dfr(total_inc, model_run_adj) 

#final res


final_res <-pop_res %>% 
  arrange(p.value) %>%
  mutate(adjusted = "undadjusted") %>%
  bind_rows(pop_res_adj %>%
              mutate(adjusted = "adjusted")) %>%
  arrange(p.value) %>%
  filter(model == "symptoms") %>%
  select(-std.error, -statistic, -model) %>%
  mutate(fdr_p = p.adjust(p.value, method = "fdr"), moose_p = if_else(p.value *26>1,1,p.value*26)) %>%
  select(-moose_p) %>%
  pivot_wider(names_from = adjusted, values_from = c(estimate, p.value, fdr_p)) 


#make plots

#generate nice plots

p1<-input %>%
  ggplot(aes(x = pop_cd4_2, y = no_symptoms_12_weeks)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson),
              fill = "deepskyblue4", color = "deepskyblue4", linetype = 2) +
  xlab("Population 2 (CD4)") +
  ylab("Number of symptoms") 


p2<-input %>%
  ggplot(aes(x = pop_cd8_4, y = no_symptoms_12_weeks)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson),
              fill = "deepskyblue4", color = "deepskyblue4", linetype = 2) +
  xlab("Population 4 (CD8)") +
  ylab("Number of symptoms") 



p3 <-input %>%
  ggplot(aes(x = pop_cd4_11, y = no_symptoms_12_weeks)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson),
              fill = "deepskyblue4", color = "deepskyblue4", linetype = 2) +
  xlab("Population 11 (CD4)") +
  ylab("Number of symptoms") 



p4 <-input %>%
  ggplot(aes(x = pop_cd4_3, y = no_symptoms_12_weeks)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson),
              fill = "deepskyblue4", color = "deepskyblue4", linetype = 2) +
  xlab("Population 14 (CD4)") +
  ylab("Number of symptoms") 




(p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")

p5 <- input %>%
  ggplot(aes(x = pop_cd4_2, y = no_symptoms_12_weeks)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson),
              fill = "deepskyblue4", color = "deepskyblue4", linetype = 2) +
  xlab("Population 2 (CD4)") +
  ylab("Number of symptoms") 

p6 <- input %>%
  ggplot(aes(x = pop_cd8_4, y = no_symptoms_12_weeks)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson),
              fill = "deepskyblue4", color = "deepskyblue4", linetype = 2) +
  xlab("Population 4 (CD8)") +
  ylab("Number of symptoms") 


input %>%
  ggplot(aes(x = log(il_4), y = no_symptoms_12_weeks)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson),
              fill = "deepskyblue4", color = "deepskyblue4", linetype = 2) +
  xlab("IL 7") +
  ylab("Number of symptoms") 

