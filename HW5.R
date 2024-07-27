Blood = read.table("./Blood.txt" , header=TRUE)


# Boxplot by Overwt group
boxplot(Blood$SystolicBP ~ Blood$Overwt, ylab = "Blood pressure", xlab = "Weight group")

# Sample means
tapply(Blood$SystolicBP, Blood$Overwt, mean)

# Sample standard deviations
tapply(Blood$SystolicBP, Blood$Overwt, sd)

# Sample size
tapply(Blood$SystolicBP, Blood$Overwt, length)

# Fit one way ANOVA
mod = aov (SystolicBP ~ as.factor(Overwt), data=Blood)
summary(mod)

# Tukey multiple comparisons
TukeyHSD(mod, ordered = T)

# Plot Tukey comparisons
plot(TukeyHSD(mod, ordered = T))

# Fit two way ANOVA with no interaction
mod2 = aov (SystolicBP ~ as.factor(Overwt)+as.factor(Smoke), data=Blood)
summary(mod2)


# Fit two way ANOVA with with interaction
mod3 = aov (SystolicBP ~ as.factor(Overwt)+as.factor(Smoke)+as.factor(Overwt)*as.factor(Smoke), data=Blood)
summary(mod3)
