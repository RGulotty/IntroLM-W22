library(tidyverse)
library(stargazer)


set.seed(345)

df1 <- data.frame(ages = rep(c(6, 7), c(100, 100))) %>%
  mutate(height = c(rnorm(100, 33 + 6 * 2, 1.2), rnorm(100, 33 + 7 * 2, 1.2)),
         words = c(rnorm(100, 100 + 6 * 150, 200), rnorm(100, 100 + 7 * 150, 200))
         )


ggplot(df1, aes(height, words))+geom_point()+geom_smooth(method="lm")+theme_bw()


ggplot(df1, aes(height, words, color = as.factor(ages))) + geom_point() +
  geom_smooth(method = "lm") + theme_bw() + labs(color = "ages")


mod1<-lm(height~words, data=df1)
mod2<-lm(height~words+ages, data=df1)

stargazer(mod1, mod2)




cov(df1$height, df1$ages)/sqrt(var(df1$height)*var(df1$ages))

.5*1.2^2+.5*1.2^2+(.5*45^2+.5*47^2)-(.5*45+.5*47)^2



df2<-data.frame(ages=rep(c(6,7), c(100000,100000)))%>%
mutate(height=c(rnorm(100000, 33+6*2, 1.2), rnorm(100000, 33+7*2, 1.2)),
words=c(rnorm(100000, 100+6*150, 200),rnorm(100000, 100+7*150, 200)))

ggplot(df2, aes(height))+geom_density()+geom_function(fun = dnorm,args = list(mean = 45, sd = 1.2), lty=3)+geom_function(fun = dnorm,args = list(mean = 47, sd = 1.2), lty=3)
ggsave("~/Dropbox/LinearModels/LinearModelstexSlides/Day1_review/Heightdensity.png")


ggplot(df2, aes(words))+geom_density()+geom_function(fun = dnorm,args = list(mean = 100+6*150, sd = 200), lty=3)+geom_function(fun = dnorm,args = list(mean = 100+7*150, sd =200), lty=3)
ggsave("~/Dropbox/LinearModels/LinearModelstexSlides/Day1_review/Wordsdensity.png")
