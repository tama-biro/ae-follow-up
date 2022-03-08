
library(tidyverse)
library(lme4)
library(pwr)

data <- read.csv(file.choose())


# Test 1
# After-effect in T1 vs. T2
data_t1 <- data %>%
  filter(type != 'Diversion' & time > 0 & results > 0) %>%
  group_by(ID, condition) %>%
  summarize(post_low_c = 
              mean(results[stddev_1 < stddev_2]) - 
              mean(results[type == 'Control']),
            post_low_3 = 
              mean(results[stddev_1 < stddev_2]) - 3) %>%
  ungroup()

# Check normality (holds for both)
hist(data_t1$post_low_c[data_t1$condition == 'T2'])
shapiro.test(data_t1$post_low_c[data_t1$condition == 'T2'])

t.test(post_low_c ~ condition, data = data_t1, 
       alternative = 'greater',
       var.equal = TRUE)
t.test(post_low_3 ~ condition, data = data_t1, 
       alternative = 'greater',
       var.equal = TRUE)

wilcox.test(post_low_c ~ condition, data = data_t1, 
            alternative = 'greater')

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
pwr.t2n.test(n1 = 56, n2 = 101, d = d_c, alternative = 'greater')


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

pwr.t.test(n = 56, d = 0.673, type = 'one.sample')

# Test 3
# Mixed model with post-low - C
# For both T1 and T2
data_t3 <- data %>%
  filter(type != 'Diversion' & time > 0 & results > 0) %>%
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

mmt3_3 <- lmer(scale(post_low_3) ~ condition + scale(time) + (1 | ID),
               data = data_t3)

summary(mmt3_3)

# Test 4
# Mixed model with post-low - C
# For both T1 and T2, volatility as predictor
mmt4_c <- lmer(scale(post_low_c) ~ factor(stddev_1) + (1 | ID),
               data = data_t3)

summary(mmt4_c)

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
ggplot(data_plot[data_plot$condition == 'T1', ], aes(ID, value)) +
  geom_line(aes(group=ID, col = line_col)) +
  geom_point(aes(shape = type)) +
  labs(x = 'Participant number', y = 'Reported Volatility') +
  scale_shape_manual(values = c(18, 8), labels = c('Control', 'Post-low'),
                     name = 'Trial type') +
  scale_color_manual(values = c('black', 'gray'), name = 'Direction') +
  theme_minimal()

ggsave('difference_postlow_control_T1.png', height = 6, width = 12)


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


