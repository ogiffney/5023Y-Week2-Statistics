library(tidyverse)
library(skimr)
library(rstatix)

darwin <- read_csv("Data/darwin.csv")

## Tidy Data
darwin <- darwin %>% 
  pivot_longer(cols=c("Self":"Cross"), 
               names_to="type", 
               values_to="height")

## Histogram
darwin %>% 
  ggplot(aes(x=height))+geom_histogram()

## Scatter Plot (simple)
darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()

### Linear Models
model1 <- lm(height~1, data=darwin)
model1

## Scatter Plot (with dashed line at intercept)
darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_abline(intercept=18.88, 
              slope=0, 
              linetype="dashed")

## Check that the intercept has accurately estimated the mean
darwin %>% 
  summarise(mean=mean(height))

### Second Linear Model to compare two groups
model2 <- lm(height~type, data=darwin)
model2

### Scatter Plot with slope to define linear relationship between two groups
darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

## Set the factor levels and therefore the intercept to whichever group makes the most sense for your analysis.
darwin_relevel <- darwin %>% 
  mutate(type=factor(type, 
                     levels=c("Self",
                              "Cross")))
lm(height~type, data=darwin_relevel)

summary(model2)

### ANOVA

anova_test(height~type, data=darwin)

### Null Hypothesis Testing

pf(q=5.9395, df1=1, df2=28, lower.tail=FALSE)

## The cross-pollinated plants were on average significantly taller (20.2 inches) than the self pollinated plants (17.5 inches) F1,28= 5.94, P = 0.02.
