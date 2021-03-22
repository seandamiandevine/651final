library(dplyr); library(reshape2)
library(ggplot2); library(ggpubr)
library(lme4)
library(sjPlot)
rm(list =ls())

# Load data -----------------------------------------------------
d <- read.csv('data.csv')


# Visualise Effects -------------------------------------------------------
piccplot <- 
  d %>% 
  group_by(subject, condition, size, timebin) %>% 
  summarise(pFat = mean(key_press)) %>%
  filter(timebin == 1 | timebin == 4) %>% 
  summarise(change = pFat[timebin==4] - pFat[timebin==1]) %>% 
  ggplot(aes(x = size, y = change, color = condition)) + 
  stat_summary(fun.y = mean, geom = 'point', position = position_dodge(0.9), alpha=0.5) + 
  stat_summary(fun.data = mean_se, geom = 'errorbar', position = position_dodge(0.9), alpha=0.5) + 
  stat_summary(fun.y = mean, geom = 'line', position = position_dodge(0.9)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 600), 
                     labels = c('Very\nThin', 'Very\nOverweight')) + 
  scale_colour_manual(values=c("#0066CC", '#990000')) +
  labs(x = '', 
       y = '% Change in Overweight Judgements\n(First 200 Trials \u2013 Last 200 Trials)', 
       colour = 'Condition') + 
  geom_hline(yintercept = 0) + 
  theme_bw()

piccplot_zoom <- 
  d %>% 
  group_by(subject, condition, size, timebin) %>% 
  summarise(pFat = mean(key_press)) %>%
  filter(timebin == 1 | timebin == 4) %>% 
  summarise(change = pFat[timebin==4] - pFat[timebin==1]) %>% 
  filter(size == 320) %>% 
  ggplot(aes(x = condition, y = change, fill=condition)) + 
  stat_summary(fun=mean, geom='bar') + 
  stat_summary(fun.data=mean_se, geom='errorbar', width=0.00) + 
  scale_x_discrete(limits = c('Increase', 'Stable'), labels=c('', '')) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_fill_manual(values=c("#0066CC", '#990000')) +
  labs(x = '', y = '') + 
  theme_bw() + 
  theme(axis.ticks.x = element_line(size = 0))

ggarrange(piccplot, piccplot_zoom, labels = c('a', 'b'), common.legend = T,
          legend = 'bottom', widths = c(2, 1))


# Analyse Effects ---------------------------------------------------------
# * Specify models to be tested ####
# m0.X are intercept only models to test random effects (slopes and intercepts)
# m1 is a main effects model to test the main effect of each predictor w/o interaction terms
# m2.X are 2-way interaction models to test the significance of 2-way interaction terms
# m3 is a 3-way interaction model to test the principal effect of interest 
# More complex models may not converge: use lme4::allFit(model) to find best optimizer if this happens
glm0 <- glm(key_press ~ 1, family = 'binomial', data=d)
m0.1 <- glmer(key_press ~ 1 + (1|subject), family='binomial', data=d)
m0.2 <- glmer(key_press ~ 1 + (trial0|subject), family='binomial', data=d)
m1   <- glmer(key_press ~ condition + trial0 + size0 + (trial0|subject), family='binomial', data=d)
m2.1 <- glmer(key_press ~ condition + trial0 + size0 + trial0:size0 + (trial0|subject), family='binomial', data=d)
m2.2 <- glmer(key_press ~ condition + trial0 + size0 + trial0:size0 + condition:trial0 + (trial0|subject), family='binomial', data=d, glmerControl(optimizer = 'bobyqa'))
m2.3 <- glmer(key_press ~ condition + size0 + trial0 + trial0:size0 + condition:trial0 + condition:size0 + (trial0|subject), family='binomial', data=d, glmerControl(optimizer = 'bobyqa'))
m3   <- glmer(key_press ~ condition * size0 * trial0 + (trial0|subject), family='binomial', data=d, glmerControl(optimizer = 'bobyqa'))

# * Test model significance ####
# anova(m0.1, glm0) # Is random intercepts sig.? Yes 
# anova(m0.2, m0.1) # Is random slopes sig.? Yes
# anova(m1, m0.2)   # Are presence of main effects sig.? Yes
# anova(m2.1, m1)   # Is interaction between trial0 and size0 sig.? No
# anova(m2.2, m2.1) # Is interaction between trial0 and condition sig. Yes
# anova(m2.3, m2.2) # Is interaction between condition and size0 sig.? Yes
# anova(m2.3, m3)   # Is interaction between condition, trial0, and size0 sig. Yes

anova(m0.1, m0.2, m1, m2.1, m2.2, m2.3, m3, glm0) 

# Make a nicely formatted regression table of most complex model with sjPlot
tab_model(m3, 
          pred.labels = c('Intercept', 'Condition', 'Trial0', 'Size0', 
                          'Condition \u00D7 Trial0',
                          'Condition \u00D7 Size0',
                          'Trial0 \u00D7 Size0', 
                          'Condition \u00D7 Trial0 \u00D7 Size0'), 
          dv.labels = 'Response\n(1 = Overweight)')

save.image('651_environment.RData') # save environment so you don't have to rerun all this
