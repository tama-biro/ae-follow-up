
library(tidyverse)
library(EnvStats)
library(lme4)
library(pwr)
library(MuMIn)
library(simglm)
library(pracma)


data <- read.csv(file.choose())


# Test 3
# Mixed model with post-low - C
# For both T1 and T2
data_t3 <- data %>%
  filter(type != 'Diversion' & results > 0) %>%
  group_by(ID) %>%
  mutate(post_low_c = if_else(stddev_1 < stddev_2, 
                              results - mean(results[type == 'Control']),
                              999)) %>%
  ungroup() %>%
  filter(post_low_c != 999)

mmt3_c <- lmer(scale(post_low_c) ~ condition + (1 | ID),
               data = data_t3)

summary(mmt3_c)

r.squaredGLMM(mmt3_c)

# Test 4
# Mixed model with post-low - C
# For both T1 and T2, volatility as predictor
mmt4_c <- lmer(scale(post_low_c) ~ scale(stddev_1) + (1 | ID),
               data = data_t3)

summary(mmt4_c)

r.squaredGLMM(mmt4_c)

#### Post-high ####

# Test 3
# Mixed model with post-high - C
# For both T1 and T2
data_t3 <- data %>%
  filter(type != 'Diversion' & results > 0) %>%
  group_by(ID) %>%
  mutate(post_high_c = if_else(stddev_1 > stddev_2, 
                               results - mean(results[type == 'Control']),
                               999)) %>%
  ungroup() %>%
  filter(post_high_c != 999)

mmt3_c <- lmer(scale(post_high_c) ~ condition + (1 | ID),
               data = data_t3)

summary(mmt3_c)

r.squaredGLMM(mmt3_c)

# Test 4
# Mixed model with post-high - C
# For both T1 and T2, volatility as predictor
mmt4_c <- lmer(scale(post_high_c) ~ scale(stddev_1) + (1 | ID),
               data = data_t3)

summary(mmt4_c)

r.squaredGLMM(mmt4_c)


#### Interaction model ####
data_int <- data %>%
  filter(type != 'Diversion' & results > 0) %>%
  group_by(ID) %>%
  mutate(after_effect = if_else(stddev_1 < stddev_2 | stddev_1 > stddev_2, 
                                results - mean(results[type == 'Control']),
                                999)) %>%
  ungroup() %>%
  filter(after_effect != 999) %>%
  mutate(trial_type = if_else(stddev_1 < stddev_2, 'post-low', 'post-high'),
         trial_type = factor(trial_type),
         after_effect = if_else(trial_type == 'post-high',
                                -after_effect,
                                after_effect))

mm_int <- lmer(scale(after_effect) ~ condition * trial_type + (1 | ID),
               data = data_int)

summary(mm_int)

r.squaredGLMM(mm_int)



#### Power for mixed models ####
power = c()
power2 = c()
power_int = c()
for(i in 1:1000) {
  sim_arguments <- list(
    formula = y ~ 1 + condition * trial_type + (1 | id),
    # fixed = list(condition = list(var_type = 'factor',
    #                           levels = c('T1', 'T2'),
    #                           var_level = 2)),
    # fixed = list(condition = list(var_type = 'continuous',
    #                               mean = 0, sd = 1,
    #                               var_level = 2)),
    fixed = list(
      condition = list(var_type = 'factor',
                       levels = c('T1', 'T2'),
                       var_level = 2),
      trial_type = list(var_type = 'factor',
                        levels = c('post-low', 'post-high'),
                        var_level = 2)
    ),
    randomeffect = list(int_id = list(variance = 1, var_level = 2)),
    sample_size = list(level1 = 18, level2 = 83),
    reg_weights = c(0, .1, .2, 0)
  )
  
  nested_data <- sim_arguments %>%
    simulate_fixed(data = NULL, .) %>%
    simulate_randomeffect(sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments)
  
  model = lmer(y ~ 1 + condition * trial_type + (1 | id),
               data = nested_data)
  
  t1 = summary(model)[[10]][2,4]
  
  if(abs(t1) > 1.96) {
    power[length(power) + 1] = 1
  } else {
    power[length(power) + 1] = 0
  }
  
  t2 = summary(model)[[10]][3,4]

  if(abs(t2) > 1.96) {
    power2[length(power2) + 1] = 1
  } else {
    power2[length(power2) + 1] = 0
  }

  t_int = summary(model)[[10]][4,4]

  if(abs(t_int) > 1.96) {
    power_int[length(power_int) + 1] = 1
  } else {
    power_int[length(power_int) + 1] = 0
  }
}

mean(power)
mean(power2)
mean(power_int)







