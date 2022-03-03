
library(tidyverse)
library(lme4)

data <- read.csv(file.choose())


# Test 1
# After-effect in T1 vs. T2
data_t1 <- data %>%
  filter(type != 'Diversion') %>%
  group_by(ID, condition) %>%
  summarize(post_low_c = 
              mean(results[stddev_1 < stddev_2]) - 
              mean(results[type == 'Control']),
            post_low_3 = 
              mean(results[stddev_1 < stddev_2]) - 3) %>%
  ungroup()


t.test(post_low_c ~ condition, data = data_t1, 
       alternative = 'greater',
       var.equal = TRUE)
t.test(post_low_3 ~ condition, data = data_t1, 
       alternative = 'greater',
       var.equal = TRUE)

# Test 2
# After-effect = 0 in T1 and T2

t.test(data_t1$post_low_c[data_t1$condition == 'T1'], mu = 0)
t.test(data_t1$post_low_3[data_t1$condition == 'T1'], mu = 0)

t.test(data_t1$post_low_c[data_t1$condition == 'T2'], mu = 0)
t.test(data_t1$post_low_3[data_t1$condition == 'T2'], mu = 0)

# Test 3
# Mixed model with post-low - 3
# For both T1 and T2
data_t3 <- data %>%
  filter(type != 'Diversion') %>%
  group_by(ID) %>%
  mutate(post_low_c = if_else(stddev_1 < stddev_2, 
                              results - mean(results[type == 'Control']),
                              999),
         post_low_3 = if_else(stddev_1 < stddev_2, 
                              results - 3,
                              999)) %>%
  ungroup() %>%
  filter(post_low_c != 999)

mmt3_c <- lmer(post_low_c ~ condition + time + (1 | ID),
               data = data_t3)

summary(mmt3_c)

mmt3_3 <- lmer(post_low_3 ~ condition + time + (1 | ID),
               data = data_t3)

summary(mmt3_3)

# Test 3.5
# Mixed model with volatility as factor
mmt3_5_c <- lmer(post_low_c ~ factor(stddev_1) + time + (1 | ID),
               data = data_t3)

summary(mmt3_5_c)

mmt3_5_3 <- lmer(post_low_3 ~ factor(stddev_1) + time + (1 | ID),
               data = data_t3)

summary(mmt3_5_3)


# Test 4
# Mixed model with post-low - 3
# For only T2 with more control variables
mmt4_c <- lmer(post_low_c ~ factor(stddev_1) + time + gender + age + 
                 (1 | ID), data = data_t3[data_t3$condition == 'T2', ])

summary(mmt4_c)

mmt4_3 <- lmer(post_low_3 ~ factor(stddev_1) + time + gender + age + 
                 (1 | ID), data = data_t3[data_t3$condition == 'T2', ])

summary(mmt4_3)



# Plot
data_plot <- data %>%
  filter(type != 'Diversion') %>%
  group_by(ID, condition) %>%
  summarize(post_low = mean(results[stddev_1 < stddev_2]),
            control = mean(results[type == 'Control'])) %>%
  ungroup() %>%
  pivot_longer(c('control', 'post_low'), names_to = 'type',
               values_to = 'value')

# v1
ggplot(data_plot, aes(ID, value)) +
  geom_line(aes(group=ID), col = 'gray') +
  geom_point(aes(shape = type)) +
  facet_wrap('condition', ncol = 2, scales = 'free_x') +
  labs(x = 'Participant number', y = 'Reported Volatility') +
  scale_shape_manual(values = c(18, 8)) +
  theme_minimal()

ggsave('difference_postlow_control.png', height = 6, width = 12)


data_plot2 <- data %>%
  filter(type != 'Diversion') %>%
  group_by(ID, condition) %>%
  summarize(post_low = mean(results[stddev_1 < stddev_2])) %>%
  ungroup()

ggplot(data_plot, aes(ID, value)) +
  geom_hline(yintercept = 3, alpha = 0.8) +
  geom_point(color = 'darkblue', alpha = 0.7) +
  facet_wrap('condition', ncol = 2, scales = 'free_x') +
  labs(x = 'Participant number', y = 'Reported Volatility') +
  theme_minimal()

ggsave('difference_postlow_3.png', height = 6, width = 12)


