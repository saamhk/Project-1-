Bones <- read.csv("/Users/student/Library/Mobile Documents/com~apple~CloudDocs/Uni/Group Project/Bones.csv", header = TRUE)

# Bartlett -------------------------------------------------------------------

bartlett_test <- bartlett.test(Weight ~ Medium, data = Bones)
print(bartlett_test) # p-value of 0.6542, therefore we can assume homogeneity of variances and proceed with ANOVA

# ANOVA -------------------------------------------------------------------

anova <- aov(Weight ~ Medium, data = Bones)
summary(anova) #f-statistic very small so reject H0 that medium does not effect
                          #Weight

# Linear Mixed Effect Model Analysis -------------------------------------------------------------------

#LINEAR-MIXED-EFFECTS MODEL
Bones$Embryo <- as.factor(Bones$Embryo)
Bones$Medium <- as.factor(Bones$Medium)
lme_model <- lmer(Weight ~ Medium + (1|Embryo), data = Bones)

plot(lme_model, main = 'Residuals vs Fitted', xlab = 'Fitted', ylab = 'Residuals') 
#Good random scatter around 0
summary(lme_model)

residuals <- resid(lme_model)
#All residuals very small
sd(residuals) #=12.78211
fitteds <- fitted(lme_model)
confint(lme_model)


#QQ-PLOT 
qqnorm(resid(lme_model))
qqline(resid(lme_model))
#Good fit along line, light tailed so conduct another test


#SHAPIRO-WILK TEST
shapiro.test(resid(lme_model)) 
#p=0.7987 so residuals fit normal distribution
#W=0.97901 which is greater than the critical value for a sample size of 30

#All conditions and fit tests satisfied so accept lme_model as a good fit

# Estimate Plot --------------------------------------------------------------------

#BUILD DATAFRAME
df <- expand.grid(Medium = c('Arginine', 'Histidine', 'Lysine', 'Threonine', 'Valine'), 
                  CompleteWeight = seq(250, 450))


#LINES FOR ESTIMATES
df$AffectedWeight <- with(df, ifelse(Medium == 'Arginine', CompleteWeight -195.86,
                                     ifelse(Medium == 'Histidine', CompleteWeight -155.49,
                                            ifelse(Medium == 'Lysine', CompleteWeight -90.39,
                                                   ifelse(Medium == 'Threonine', CompleteWeight -201.31,
                                                          CompleteWeight -251.29)))))


#LINES FOR BASE OF CI RIBBON
df$LowerCI <- with(df, ifelse(Medium == 'Arginine', CompleteWeight -222.43,
                              ifelse(Medium == 'Histidine', CompleteWeight -183.31,
                                     ifelse(Medium == 'Lysine', CompleteWeight -118.99,
                                            ifelse(Medium == 'Threonine', CompleteWeight + -228.98,
                                                   CompleteWeight -279.36)))))


#LINES FOR TOP OF CI RIBBON
df$UpperCI <- with(df, ifelse(Medium == 'Arginine', CompleteWeight -168.22,
                              ifelse(Medium == 'Histidine', CompleteWeight -127,
                                     ifelse(Medium == 'Lysine', CompleteWeight -62.51,
                                            ifelse(Medium == 'Threonine', CompleteWeight -173.76,
                                                  CompleteWeight -223.7)))))

#PLOT GRAPH
ggplot(df, aes(x = CompleteWeight, y = AffectedWeight, colour = Medium, group = Medium)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI, fill = Medium), alpha = 0.2, colour=NA) +
  geom_point(aes(x=272, y=166), colour = 'springgreen3') +
  geom_point(aes(x=342, y=157), colour = 'tomato1') +
  geom_point(aes(x=379, y=166), colour = 'steelblue2') +
  geom_point(aes(x=398, y=126), colour = 'orchid2') +
  geom_point(aes(x=403, y=280), colour = 'yellow3') +
  labs(
    title = 'Effect of Omitting Amino Acids on Bone Weight Compared to Complete Medium',
    x = 'Complete Medium Weight (micrograms)',
    y = 'Estimated Omission Weight (micrograms)',
    caption = 'Ribbon indicates confidence interval of estimations.
    Points indicate the data of the first 5 embryos.' #4/5 first embryos lie in their expected confidence interval
  )+theme_minimal()



