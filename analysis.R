
library(tidyverse)
library(EnvStats)
library(lme4)
library(pwr)
library(MuMIn)
library(simglm)

data <- read.csv(file.choose())


#### Post-low AE ####

# Test 1
# After-effect in T1 vs. T2
data_t1 <- data %>%
  filter(type != 'Diversion' & results > 0) %>%
  group_by(ID, condition) %>%
  summarize(post_low_c = 
              mean(results[stddev_1 < stddev_2]) - 
              mean(results[type == 'Control']),
            post_high_c = 
              mean(results[stddev_1 > stddev_2]) - 
              mean(results[type == 'Control'])) %>%
  ungroup() %>%
  mutate(condition = factor(condition, levels = c('T2', 'T1')))

# Check normality (holds for both)
hist(data_t1$post_high_c[data_t1$condition == 'T1'])
shapiro.test(data_t1$post_high_c[data_t1$condition == 'T1'])

psych::describe((data_t1$post_high_c[data_t1$condition == 'T2']))

data_t1$post_high_bc <- boxcoxTransform(data_t1$post_high_c+5, lambda = 3)

sum(data_t1$post_high_c[data_t1$condition == 'T1'] < 0)/
  length(data_t1$post_high_c[data_t1$condition == 'T1'])


t.test(post_high_bc ~ condition, data = data_t1, 
       alternative = 'greater',
       var.equal = TRUE)
t.test(post_low_3 ~ condition, data = data_t1, 
       alternative = 'greater',
       var.equal = TRUE)

wilcox.test(post_high_bc ~ condition, data = data_t1, 
            alternative = 'greater')

# Cohen's D
d_c <- (mean(data_t1$post_high_bc[data_t1$condition == 'T1']) - 
          mean(data_t1$post_high_bc[data_t1$condition == 'T2'])) /
  sqrt(
    (
      sd(data_t1$post_high_bc[data_t1$condition == 'T1'])^2 +
        sd(data_t1$post_high_bc[data_t1$condition == 'T2'])^2
    )
    /2
  )

d_3 <- (mean(data_t1$post_low_3[data_t1$condition == 'T1']) - 
          mean(data_t1$post_low_3[data_t1$condition == 'T2'])) /
  sqrt(
    (
      sd(data_t1$post_low_3[data_t1$condition == 'T1'])^2 +
        sd(data_t1$post_low_3[data_t1$condition == 'T2'])^2
    )
    /2
  )

# Power for medium effect size
pwr.t2n.test(n1 = 56, n2 = 131, d = d_c, alternative = 'less')


# Test 2
# After-effect = 0 in T1 and T2

t.test(data_t1$post_low_c[data_t1$condition == 'T1'], mu = 0)
t.test(data_t1$post_low_3[data_t1$condition == 'T1'], mu = 0)

t.test(data_t1$post_low_c[data_t1$condition == 'T2'], mu = 0)
t.test(data_t1$post_low_3[data_t1$condition == 'T2'], mu = 0)

# Cohen's D
mean(data_t1$post_low_c[data_t1$condition == 'T1']) / 
  sd(data_t1$post_low_c[data_t1$condition == 'T1'])
mean(data_t1$post_low_3[data_t1$condition == 'T1']) / 
  sd(data_t1$post_low_3[data_t1$condition == 'T1'])

mean(data_t1$post_low_c[data_t1$condition == 'T2']) / 
  sd(data_t1$post_low_c[data_t1$condition == 'T2'])
mean(data_t1$post_low_3[data_t1$condition == 'T2']) / 
  sd(data_t1$post_low_3[data_t1$condition == 'T2'])

pwr.t.test(n = 131, d = 0.2, type = 'one.sample')

# Test 3
# Mixed model with post-low - C
# For both T1 and T2
data_t3 <- data %>%
  filter(type != 'Diversion' & results > 0) %>%
  group_by(ID) %>%
  mutate(post_low_c = if_else(stddev_1 < stddev_2, 
                              results - mean(results[type == 'Control']),
                              999),
         post_low_3 = if_else(stddev_1 < stddev_2, 
                              results - 3,
                              999)) %>%
  ungroup() %>%
  filter(post_low_c != 999)

mmt3_c <- lmer(scale(post_low_c) ~ condition + (1 | ID),
               data = data_t3)

summary(mmt3_c)

r.squaredGLMM(mmt3_c)

mmt3_c2 <- lmer(scale(post_low_c) ~ condition + scale(time) + (1 | ID),
               data = data_t3)

summary(mmt3_c2)

AIC(mmt3_c, mmt3_c2)

r.squaredGLMM(mmt3_c2)

mmt3_3 <- lmer(scale(post_low_3) ~ condition + scale(time) + (1 | ID),
               data = data_t3)

summary(mmt3_3)

# Test 4
# Mixed model with post-low - C
# For both T1 and T2, volatility as predictor
mmt4_c <- lmer(scale(post_low_c) ~ scale(stddev_1) + (1 | ID),
               data = data_t3)

summary(mmt4_c)

r.squaredGLMM(mmt4_c)

data_t4 <- data_t3 %>%
  mutate(volatility = if_else(
    stddev_1 == 0.02, -1,
    if_else(
      stddev_1 == 0.04, 0, 1
    )
  ))

mmt4_c2 <- lmer(scale(post_low_c) ~ volatility + (1 | ID),
               data = data_t4)

summary(mmt4_c2)

r.squaredGLMM(mmt4_c2)


# Plot
data_plot <- data %>%
  filter(type != 'Diversion') %>%
  group_by(ID, condition) %>%
  summarize(post_low = mean(results[stddev_1 < stddev_2]),
            control = mean(results[type == 'Control'])) %>%
  ungroup() %>%
  mutate(line_col = ifelse(post_low > control, 'After-effect', 'No after-effect')) %>%
  pivot_longer(c('control', 'post_low'), names_to = 'type',
               values_to = 'value')

# v1
ggplot(data_plot[data_plot$condition == 'T2', ], aes(ID, value)) +
  geom_line(aes(group=ID, col = line_col)) +
  geom_point(aes(shape = type)) +
  labs(x = 'Participant number', y = 'Reported Volatility') +
  scale_shape_manual(values = c(18, 8), labels = c('Control', 'Post-low'),
                     name = 'Trial type') +
  scale_color_manual(values = c('#0fbfd6', '#18191a'), name = 'Direction') +
  scale_y_continuous(limits = c(1.5, 5)) +
  theme_minimal() +
  guides(color = guide_legend(order = 2), shape = guide_legend(order = 1))

ggsave('difference_postlow_control_T2.png', height = 6, width = 12)


data_plot2 <- data %>%
  filter(type != 'Diversion') %>%
  group_by(ID, condition) %>%
  summarize(post_low = mean(results[stddev_1 < stddev_2])) %>%
  ungroup()

ggplot(data_plot2[data_plot2$condition == 'T2', ], aes(ID, post_low)) +
  geom_hline(yintercept = 3, alpha = 0.8) +
  geom_point(color = 'darkblue', alpha = 0.7) +
  labs(x = 'Participant number', y = 'Reported Volatility') +
  theme_minimal()

ggsave('difference_postlow_3_T1.png', height = 6, width = 12)


#### Interactions ####

# T-test
data_int_t <- data %>%
  filter(type != 'Diversion' & results > 0) %>%
  group_by(ID, condition) %>%
  summarize(after_effect = -(mean(results[stddev_1 > stddev_2]) - 
                          mean(results[type == 'Control'])) -
                         (mean(results[stddev_1 < stddev_2]) - 
                          mean(results[type == 'Control']))) %>%
  ungroup()

t.test(after_effect ~ condition, data = data_int_t,
       var.equal = TRUE)

d_c <- (mean(data_int_t$after_effect[data_int_t$condition == 'T1']) - 
          mean(data_int_t$after_effect[data_int_t$condition == 'T2'])) /
  sqrt(
    (
      sd(data_int_t$after_effect[data_int_t$condition == 'T1'])^2 +
        sd(data_int_t$after_effect[data_int_t$condition == 'T2'])^2
    )
    /2)

# Mixed model
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

# Plot interaction
data_plot <- data_int %>%
  group_by(condition, trial_type) %>%
  summarize(ae_mean = mean(after_effect),
            se = se(after_effect)) %>%
  ungroup()

ggplot(data_plot, aes(x = condition, 
                        y = ae_mean,
                        fill = trial_type)) +
  geom_bar(stat='identity', position = position_dodge(0.7),
           width = .4) +
  geom_errorbar(aes(ymin = ae_mean - se,
                    ymax = ae_mean + se),
                width = 0.1, position = position_dodge(0.7)) +
  scale_fill_manual(name = 'Trial type',
                    values = c('#2b2bd6', '#ff5e5e')) +
  labs(y = 'After-effect', x = 'Condition') +
  theme_minimal()


ggsave('after-effect_difference.png', width = 10, height = 8)

#### Power for mixed models ####
power = c()
for(i in 1:100) {
  sim_arguments <- list(
    formula = y ~ 1 + condition + (1 | id),
    fixed = list(condition = list(var_type = 'factor',
                              levels = c('T1', 'T2'),
                              var_level = 2)),
    # fixed = list(condition = list(var_type = 'continuous',
    #                               mean = 0,
    #                               sd = 1,
    #                               var_level = 2)),
    randomeffect = list(int_id = list(variance = 1, var_level = 2)),
    sample_size = list(level1 = 9, level2 = 186),
    reg_weights = c(0, -.2)
  )
  
  nested_data <- sim_arguments %>%
    simulate_fixed(data = NULL, .) %>%
    simulate_randomeffect(sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments)
  
  model = lmer(y ~ 1 + condition+ (1 | id),
                data = nested_data)
  
  t = summary(model)[[10]][2,4]
  
  if(t < -1.68) {
    power[length(power) + 1] = 1
  } else {
    power[length(power) + 1] = 0
  }
}

mean(power)



#### Post-high Mixed models ####

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

psych::skew(data_t3$post_high_c)

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

data_t4 <- data_t3 %>%
  mutate(volatility = if_else(
    stddev_1 == 0.02, -1,
    if_else(
      stddev_1 == 0.04, 0, 1
    )
  ))

mmt4_c2 <- lmer(scale(post_low_c) ~ volatility + (1 | ID),
                data = data_t4)

summary(mmt4_c2)

r.squaredGLMM(mmt4_c2)





