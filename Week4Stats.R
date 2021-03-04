library(tidyverse)

darwin <- read_csv("Data/darwin.csv")


# Pivot the data from wide to long, set type and pair as factors, set the intercept to type = Self
darwin <- darwin %>% 
  pivot_longer(cols=c("Self":"Cross"), 
               names_to="type", 
               values_to="height") %>% 
  mutate(type=factor(type, 
                     levels=c("Self",
                              "Cross"))) %>% 
  mutate(pair=factor(pair))

## Paired T-tests ##

model <- lm(formula = height ~ type + pair, data = darwin)

summary(aov(model))


## Confidence Intervals of the mean ##

library(emmeans) # A handy package for estimating means from the fit of our model

estimates <- emmeans(model, specs="type") ### here it will take the average of the values across all the pairs to calculate means for type

estimates %>% 
  as_tibble %>% ### emmeans outputs a grid by default, but can easily                       be changed
  ggplot(aes(x=type, 
             y=emmean, 
             colour=type))+
  geom_pointrange(aes(ymin=lower.CL, 
                      ymax=upper.CL))+
  geom_pointrange(aes(ymin=emmean-SE, 
                      ymax=emmean+SE), 
                  size=1.2)

## Use broom to force model summary into dataframe format, then mutate to 
# calculate lower and upper CIs ##

tidymodel1 <- broom::tidy(model) %>% 
  mutate(lwr=((estimate-(std.error*2))),
         upr=(estimate+(std.error*2)))

##  Plot the 95% CI of the estimates against the Intercept ##

tidymodel1 %>% 
  ggplot(aes(x=estimate, 
             y=term))+
  geom_pointrange(aes(xmin=lwr, 
                      xmax=upr))+
  geom_vline(xintercept=0, 
             linetype="dashed")
# Anything which crosses the 0 is not significantly different at P<0.05 
# from the mean calculated for the Intercept = none are sig. different except type.

## Calculating confidence intervals.
tidymodel2 <- broom::tidy(model, conf.int=T) 
tidymodel2[2,] ## only pull out row 2 

tidymodel2 %>% 
  ggplot(aes(x=estimate, 
             y=term))+
  geom_pointrange(aes(xmin=conf.low, 
                      xmax=conf.high))+
  geom_vline(xintercept=0, 
             linetype="dashed")

## Use the Confidence intervals for mean DIFFERENCE to write up these results ##

