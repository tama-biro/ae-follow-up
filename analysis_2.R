
library(tidyverse)
library(EnvStats)
library(lme4)
library(pwr)
library(effectsize)
library(MESS)
library(MuMIn)
library(simglm)

data <- read.csv(file.choose())

#### Sanity check diversion ####
data %>%
  filter(results > 0 & type == 'Diversion' & condition == 'T2') %>%
  group_by(stddev_2, results) %>%
  summarize(count = n())

d <- data %>%
  filter(
    results > 0
#    & results < 4
    & type == 'Diversion'
    & condition == 'T1'
    ) %>%
  group_by(ID, results) %>%
  summarize(count = n()) %>%
  ungroup %>%
  mutate(fraction_5 = sum(count[results == 5])/sum(count),
         fraction_4 = sum(count[results == 4])/sum(count),
         only_5 = sum(count == max(count) & results == 5)/length(unique(ID)))


# Exclude
data <- data %>%
  filter(!(ID %in% c(26, 173, 181, 183)))

#### Post-low AE ####

# Test 1
# After-effect in T1 vs. T2
data_t1 <- data %>%
  filter(type != 'Diversion' & results > 0 & experiment %in% c('', 'S5')) %>%
  group_by(ID, condition) %>%
  summarize(post_low_c =
              mean(results[stddev_1 < stddev_2]) -
              mean(results[type == 'Control']),
            post_high_c =
              mean(results[type == 'Control']) -
              mean(results[stddev_1 > stddev_2])) %>%
  ungroup() %>%
  mutate(condition = factor(condition, levels = c('T2', 'T1')))

psych::describeBy(data_t1$post_high_c, group = data_t1$condition)

# Check normality (holds for both)
hist(data_t1$post_low_c[data_t1$condition == 'T2'])
shapiro.test(data_t1$post_low_c[data_t1$condition == 'T2'])

sum(data_t1$post_low_c[data_t1$condition == 'T1'] > 0)/
  length(data_t1$post_low_c[data_t1$condition == 'T1'])

# Box-Cox transformation
data_t1$post_low_c <- boxcoxTransform(data_t1$post_low_c + 1, 3)
data_t1$post_high_c <- boxcoxTransform(data_t1$post_high_c + 1, 3)

t.test(post_low_c ~ condition, data = data_t1, 
       alternative = 'less',
       var.equal = TRUE)

wilcox.test(post_low_c ~ condition, data = data_t1, 
            alternative = 'less')

# Cohen's D
d_c <- (mean(data_t1$post_low_c[data_t1$condition == 'T1']) - 
          mean(data_t1$post_low_c[data_t1$condition == 'T2'])) /
  sqrt(
    (
      sd(data_t1$post_low_c[data_t1$condition == 'T1'])^2 +
        sd(data_t1$post_low_c[data_t1$condition == 'T2'])^2
    )
    /2
  )


# Power for medium effect size
pwr.t2n.test(n1 = 56, n2 = 31, d = .50, alternative = 'greater')


# Test 2
# After-effect = 0 in T1 and T2

t.test(data_t1$post_low_c[data_t1$condition == 'T1'], mu = 0)
t.test(data_t1$post_low_c[data_t1$condition == 'T2'], mu = 0)

# Cohen's D
mean(data_t1$post_low_c[data_t1$condition == 'T1']) / 
  sd(data_t1$post_low_c[data_t1$condition == 'T1'])

mean(data_t1$post_low_c[data_t1$condition == 'T2']) / 
  sd(data_t1$post_low_c[data_t1$condition == 'T2'])

pwr.t.test(n = 31, d = .241, type = 'one.sample')

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
# Mixed model with C - post-high
# For both T1 and T2
data_t3 <- data %>%
  filter(type != 'Diversion' & results > 0) %>%
  group_by(ID) %>%
  mutate(post_high_c = if_else(stddev_1 > stddev_2, 
                              mean(results[type == 'Control']) - results,
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

# Plot
data_plot <- data %>%
  filter(type != 'Diversion' & results != 0) %>%
  group_by(ID, condition) %>%
  summarize(post_low = mean(results[stddev_1 < stddev_2]),
            control = mean(results[type == 'Control'])) %>%
  ungroup() %>%
  mutate(line_col = ifelse(post_low > control, 
                           'After-effect', 
                           'No after-effect')) %>%
  pivot_longer(c('control', 'post_low'), names_to = 'type',
               values_to = 'value')

# v1 - post-low
ggplot(data_plot[data_plot$condition == 'T2', ], aes(ID, value)) +
  geom_line(aes(group=ID, col = line_col)) +
  geom_point(aes(shape = type)) +
  labs(x = 'Participant number', y = 'Reported Volatility') +
  scale_shape_manual(values = c(18, 8), labels = c('Control', 'Post-low'),
                     name = 'Trial type') +
  scale_color_manual(values = c('#0fbfd6', '#18191a'), name = 'Direction') +
  scale_y_continuous(limits = c(1, 5)) +
  theme_minimal() +
  guides(color = guide_legend(order = 2), shape = guide_legend(order = 1))

ggsave('difference_postlow_control_T2.png', height = 6, width = 12)


# v1 - post-high
data_plot <- data %>%
  filter(type != 'Diversion' & results != 0) %>%
  group_by(ID, condition) %>%
  summarize(control = mean(results[type == 'Control']),
            post_high = mean(results[stddev_1 > stddev_2])) %>%
  ungroup() %>%
  mutate(line_col = ifelse(post_high < control, 
                             'After-effect', 
                             'No after-effect')) %>%
  pivot_longer(c('control', 'post_high'), names_to = 'type',
               values_to = 'value')

ggplot(data_plot[data_plot$condition == 'T1', ], aes(ID, value)) +
  geom_line(aes(group=ID, col = line_col)) +
  geom_point(aes(shape = type)) +
  labs(x = 'Participant number', y = 'Reported Volatility') +
  scale_shape_manual(values = c(18, 8), labels = c('Control', 'Post-high'),
                     name = 'Trial type') +
  scale_color_manual(values = c('#0fbfd6', '#18191a'), name = 'Direction') +
  scale_y_continuous(limits = c(1, 5)) +
  theme_minimal() +
  guides(color = guide_legend(order = 2), shape = guide_legend(order = 1))

ggsave('difference_posthigh_control_T1.png', height = 6, width = 12)

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
power2 = c()
power_int = c()
for(i in 1:100) {
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
    sample_size = list(level1 = 18, level2 = 188),
    reg_weights = c(0, .2, .2, .09)
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


#### Sensitivity analysis ####

# Run pwr for min effect size
pwr.t2n.test(n1 = 56, n2 = 132, power = .8, sig.level = .05)

pwr.t.test(n = 56, power = .8, sig.level = .1, type = 'one.sample')

# Plot power as a function of alpha and effect size

effect <- seq(0, 1, by = .1)
alpha <- c(0.001, 0.01, 0.05, 0.1)
power <- NA

param_grid <- expand_grid(effect, alpha, power)

for (i in 1:nrow(param_grid)) {
  eff <- param_grid$effect[i]
  alph <- param_grid$alpha[i]
  
  powow <- pwr.t2n.test(n1 = 56, n2 = 31, d = eff, sig.level = alph)
  
  param_grid$power[i] <- powow$power
  
}

param_grid


# Test 4
# Chi-squared test
# Proportion with positive post-low in T1 vs. T2
data_t4 <- data %>%
  filter(
    type != 'Diversion'
    & results > 0
#    & condition == 'T2'
#    & experiment %in% c('', 'S5')
    ) %>%
  group_by(ID, condition) %>%
  summarize(post_low_c =
              if_else(mean(results[stddev_1 < stddev_2]) -
                        mean(results[type == 'Control']) > 0, 1, 0),
            post_high_c =
              if_else(mean(results[type == 'Control']) -
                        mean(results[stddev_1 > stddev_2]) > 0, 1, 0)) %>%
  ungroup() %>%
  mutate(condition = factor(condition, levels = c('T2', 'T1')))


chisq.test(data_t4$condition, data_t4$post_high_c)

fisher.test(data_t4$condition, data_t4$post_low_c,
            alternative = 'greater')

table(data_t4$condition, data_t4$post_high_c)

pwr.chisq.test(w = 0.3, N = 87, df = 1)

mc_nem_dat <- table(data_t4$post_low_c, data_t4$post_high_c) %>%
  as.matrix()

mcnemar.test(mc_nem_dat)

power_mcnemar_test(n = 132, paid=.15, psi = NULL,
                   alternative = 'one.sided', power = 0.9,
                   sig.level = .05)

# Test 5
# Paired t-test for post-high - post-low
data_t5 <- data %>%
  filter(
    type != 'Diversion'
    & results > 0
    & condition == 'T2'
    ) %>%
  group_by(ID) %>%
  summarize(post_low = mean(results[stddev_1 < stddev_2]) -
                        mean(results[type == 'Control']),
            post_high = mean(results[type == 'Control']) -
                        mean(results[stddev_1 > stddev_2])) %>%
  ungroup() %>%
  filter(post_low > 0 | post_high > 0) %>%
  pivot_longer(c(post_low, post_high), names_to = "trial_type",
               values_to = "after_effect") %>%
  mutate(trial_type = factor(trial_type, levels = c("post_high", "post_low")))

t.test(after_effect ~ trial_type, data = data_t5,
       paired = TRUE, alternative = 'greater')

cohens_d(after_effect ~ trial_type, data = data_t5,
         paired = TRUE, alternative = 'greater')

pwr.t.test(n = 179, d = .2, type = 'paired', alternative = 'greater')

pwr.t.test(n = 178, power = 0.8, sig.level = .05,
           type = 'paired', alternative = 'greater')


