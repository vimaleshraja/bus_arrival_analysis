data <- read.csv('MA3740-Final_Project - Sheet1.csv', stringsAsFactors = T)
head(data)

str(data)

attach(data)

levels(Driver)
levels(Trip.Type)

fivenumsum <- function(x){
  summ <- c(mean(x),sd(x),quantile(x))
  names(summ) <- c("Mean","SD","Min","0.25","Median","0.75","Max")
  return(summ)
}

aggregate(Seconds.late~Driver:Trip.Type
          ,FUN = fivenumsum)


boxplot(Seconds.late~Driver:Trip.Type,las =1,
        main = 'Distribution of Interaction terms')

interaction.plot(Trip.Type,Driver,Seconds.late, main="Break periods VS Driver")
interaction.plot(Driver,Trip.Type,Seconds.late, main = "Driver VS Break Periods")


full.model <- aov(Seconds.late~Driver+Trip.Type+Driver:Trip.Type)

time.model

anova(time.model)

oneway.model <- aov(Seconds.late~Trip.Type)

TukeyHSD(full.model)

### Normality Assumption

res <- resid(full.model)

qqnorm(res)
qqline(res)

shapiro.test(res)

### Homoscedascticity

fit.val <- fitted.values(full.model)
plot(full.model)

kruskal.test(data$Seconds.late~data$Trip.Type)

attach(data)
anova(aov(Seconds.late~Driver*Trip.Type))
anova(aov(Seconds.late~Trip.Type))

pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")
