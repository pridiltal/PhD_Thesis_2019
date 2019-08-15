## ---- load
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(anomalous)
library(lvplot)


## ---- EVDchange
# Figure 3 : Extreme value distributions with respect to m
## standard normal
set.seed(100)
m <- c(1, 10, 100, 1000)
max_m <- list()
min_m <- list()
for (i in 1:4)
{
  max <- numeric(100000)
  min <- numeric(100000)
  for (j in 1:100000)
  {
    data <- rnorm(m[i])
    max[j] <- max(data)
    min[j] <- min(data)
  }
  max_m[[i]] <- max
  min_m[[i]] <- min
}
names(max_m) <- (c("EVD, m = 1", "EVD, m = 10", "EVD, m = 100", "EVD, m = 1000"))
names(min_m) <- (c("EVD, m = 1", "EVD, m = 10", "EVD, m = 100", "EVD, m = 1000"))
max_m <- as_tibble(max_m)
min_m <- as_tibble(min_m)
new_data1 <- tidyr::gather(max_m, EVD, value)
new_data2 <- tidyr::gather(min_m, EVD, value)
p3a <- ggplot(new_data1, aes(x = value)) +
  geom_density(aes(group = EVD, fill = EVD), alpha = 0.8) +
  scale_fill_manual(values = c("yellow3", "grey63", "grey36", "black")) +
  xlab("x") +
  ylab("p(x)") 

p3a <- p3a + geom_density(data= new_data2, aes(x = value, group = EVD, fill = EVD), alpha = 0.8) +
  scale_fill_manual(values = c("yellow3", "grey63", "grey36", "black")) +
  xlab("x") +
  ylab("p(x)") +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 12)  ) +
  theme(legend.title = element_blank())


## exponential
set.seed(100)
m <- c(1, 10, 100, 1000)
max_m <- list()
for (i in 1:4)
{
  max <- numeric(100000)
  for (j in 1:100000)
  {
    max[j] <- max(rexp(m[i]))
  }
  max_m[[i]] <- max
}
names(max_m) <- (c("EVD, m = 1", "EVD, m = 10", "EVD, m = 100", "EVD, m = 1000"))
max_m <- as_tibble(max_m)
new_data <- tidyr::gather(max_m, EVD, value)
p3b<-ggplot(new_data, aes(x = value)) +
  geom_density(aes(group = EVD, fill = EVD), alpha = 0.8) +
  scale_fill_manual(values = c("yellow3", "grey63", "grey36", "black")) +
  xlab("x") +
  ylab("p(x)") +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 12) ) +
  theme(legend.title = element_blank())


p <- ggarrange(p3a, p3b, nrow = 1, ncol = 2 , common.legend = TRUE) 
print(p)



## ---- spacings
set.seed(123)
nobs <- 20000
nsamples <- 1000
trials <- matrix(rnorm(nobs*nsamples,0,1),nrow=nsamples)
trials.sorted <- t(apply(trials, 1, function(x) sort(x, decreasing=TRUE)))
trials.means <- apply(trials.sorted, 2, mean)
mean.spacings <- diff(trials.means)
mean.spacings <- -mean.spacings
x <- 1/(1:(nobs-1))
a <- coef(lm(mean.spacings ~ 0 + x))
pred.spacings <- a*x

trial_mean <- tibble::tibble(Rank = 1:10 , order_mean = trials.means[1:10] )




ts_matrix <- trials.sorted[,1:10]
colnames(ts_matrix) <- 1:10
trial_sorted <- as.data.frame(ts_matrix) %>%
  gather(Rank, "order_stat") %>% mutate(Rank = as.numeric(Rank))


p1 <- ggplot(trial_sorted,aes(x=factor(Rank), y=order_stat)) + 
  lvplot::geom_lv(fill = "grey80", colour = "black")+
  ggtitle("(a)")+
  ylab(expression(paste("Order Statistics")))+
  xlab("Rank")+
  stat_summary(fun.y=mean, geom="point", shape=3, size=2)+
  theme(    text = element_text(size=8))



ts_matrix_diff<- t(apply(ts_matrix, 1, (diff) )) * -1
ts_matrix_stdiff<- t(apply(ts_matrix_diff, 1, function(x) x*(1:9)))
data<- reshape2::melt(ts_matrix_stdiff)
p2 <- ggplot(data,aes(x=factor(Var2), y=value)) + 
  lvplot::geom_lv(fill = "grey80", colour = "black")+
  ggtitle("(b)")+
  ylab(expression(paste("Standardized Spacings (",i*D[i],")")))+
  xlab("Rank")+
  stat_summary(fun.y=mean, geom="point", shape=3, size=2)+
  theme(    text = element_text(size=8))   



p<- ggarrange(p1, p2, nrow = 2, heights = c(2, 2) ) 
print(p)
