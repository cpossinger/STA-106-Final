library(purrr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)
require(maps)
require(viridis)
library(plotly)
covid19 = read.table("COVID19.txt", header=T, sep="\t")
covid19$Sex %<>% as.factor
covid19$AgeGroup %<>% as.factor
covid19 %<>% filter(Sex == "Male",AgeGroup == "85 years and over")

states_map <- map_data("state")
states_map %<>% rename(State = region)
states_map$State %<>% str_to_title()

covid19_map <- left_join(states_map,covid19,by = "State")

covid19_map_plot <- ggplot(covid19_map,aes(long,lat,group = group))+ 
  geom_polygon(aes(fill = COVIDProp,text = State),color = "white")+
  scale_fill_viridis_c(option = "C")

covid19_map_plot %<>% ggplotly
covid19_map_plot

influenza_map_plot <- ggplot(covid19_map,aes(long,lat,group = group))+
  geom_polygon(aes(fill = InfluenzaProp,text = State),color = "white")+
  scale_fill_viridis_c(option = "C")

influenza_map_plot %<>% ggplotly
influenza_map_plot

pneumonia_map_plot <- ggplot(covid19_map,aes(long,lat,group = group))+
  geom_polygon(aes(fill = PneumoniaProp,text = State),color = "white")+
  scale_fill_viridis_c(option = "C")
pneumonia_map_plot %<>% ggplotly
pneumonia_map_plot

aov_least_squares_estimates <- function(data,response_var,factor_a,factor_b){
  factor_a_levels <- data %>% extract2(factor_a) %>% unique %>% length
  factor_b_levels <- data %>% extract2(factor_b) %>% unique %>% length
  
  first_col_means <- c(1:factor_b_levels) %>% map2_dbl(rep(1,factor_a_levels),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>% extract2(response_var) %>% mean)
  
  second_col_means <- c(1:factor_b_levels) %>% map2_dbl(rep(2,factor_a_levels),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>% extract2(response_var) %>% mean)
  
  third_col_means <- c(1:factor_b_levels) %>% map2_dbl(rep(3,factor_a_levels),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>% extract2(response_var) %>% mean)
  
  
  factor_level_means <- cbind(first_col_means,second_col_means,third_col_means)
  
  factor_a_means <- factor_level_means %>% apply(MARGIN = 1,mean)
  factor_b_means <- factor_level_means %>% apply(MARGIN = 2,mean)
  
  overall_mean <- data %>% extract2(response_var) %>% mean
  
  alpha_hat <- factor_a_means - overall_mean 
  alpha_hat <- data.frame("Alpha_Hat" = alpha_hat)
  beta_hat <- factor_b_means - overall_mean 
  beta_hat <- data.frame("Beta_Hat" = beta_hat)
  rownames(beta_hat) <- c(1:factor_b_levels)
  
  first_col_gamma <- c(1:factor_a_levels) %>% map2_dbl(rep(1,factor_a_levels),~factor_level_means[.x,.y] - factor_a_means[.x] -factor_b_means[.y] + overall_mean)
  
  second_col_gamma <- c(1:factor_b_levels) %>% map2_dbl(rep(2,factor_b_levels),~factor_level_means[.x,.y] - factor_a_means[.x] -factor_b_means[.y] + overall_mean)
  
  third_col_gamma <- c(1:factor_b_levels) %>% map2_dbl(rep(3,factor_b_levels),~factor_level_means[.x,.y] - factor_a_means[.x] -factor_b_means[.y] + overall_mean)
  
  gamma_hat <- cbind(first_col_gamma,second_col_gamma,third_col_gamma)
  gamma_hat <- data.frame("Gamma_Hat" = gamma_hat)
  colnames(gamma_hat) <- c("Gamma_Hat_1","Gamma_Hat_2","Gamma_Hat_3")
  
  overall_mean <- data.frame("Mu" = overall_mean)
  
  
  return(list(overall_mean,alpha_hat,beta_hat,gamma_hat))
  
}


anova.table <- function(data,response,factor_a,factor_b){
  factor_a_levels <- data %>% extract2(factor_a) %>% unique 
  factor_b_levels <- data %>% extract2(factor_b) %>% unique
  
  
  first_col_means <- c(rep(factor_a_levels,3),factor_a_levels[1]) %>% map2_dbl(rep(factor_b_levels[1],factor_b_levels %>% length),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>%
                                                         extract2(response) %>% mean)
  second_col_means <- c(rep(factor_a_levels,3),factor_a_levels[1]) %>% map2_dbl(rep(factor_b_levels[2],factor_b_levels %>% length),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>%
                                                          extract2(response) %>% mean)
  third_col_means <- c(rep(factor_a_levels,3),factor_a_levels[1]) %>% map2_dbl(rep(factor_b_levels[3],factor_b_levels %>% length),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>%
                                                         extract2(response) %>% mean)
  fourth_col_means <- c(rep(factor_a_levels,3),factor_a_levels[1]) %>% map2_dbl(rep(factor_b_levels[4],factor_b_levels %>% length),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>%
                                                         extract2(response) %>% mean)
  fifth_col_means <- c(rep(factor_a_levels,3),factor_a_levels[1]) %>% map2_dbl(rep(factor_b_levels[5],factor_b_levels %>% length),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>%
                                                         extract2(response) %>% mean)
  sixth_col_means <- c(rep(factor_a_levels,3),factor_a_levels[1]) %>% map2_dbl(rep(factor_b_levels[6],factor_b_levels %>% length),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>%
                                                         extract2(response) %>% mean)
  seventh_col_means <- c(rep(factor_a_levels,3),factor_a_levels[1]) %>% map2_dbl(rep(factor_b_levels[7],factor_b_levels %>% length),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>%
                                                         extract2(response) %>% mean)
  factor_level_means <- cbind(first_col_means,second_col_means,third_col_means,fourth_col_means,fifth_col_means,sixth_col_means,seventh_col_means)
  
  factor_a_means <- factor_level_means %>% apply(MARGIN = 1,mean)
  factor_b_means <- factor_level_means %>% apply(MARGIN = 2,mean)
  
  
  a <- factor_level_means[1,] %>% length
  b <- factor_level_means[,1] %>% length 
  n <- data %>% filter(!!as.symbol(factor_a) == factor_a_levels[1] & !!as.symbol(factor_b) == factor_b_levels[1]) %>% extract2(response) %>% length
  overall_mean <- data %>% extract2(response) %>% mean
  
  
  
  alpha_hat <- factor_a_means - overall_mean 
  beta_hat <- factor_b_means - overall_mean 
  
  
  first_col_gamma <- rep(1:factor_a_levels %>% length,factor_b_levels %>% length) %>% map2_dbl(rep(1,factor_b_levels %>% length),~factor_level_means[.x,.y] - factor_a_means[.x] -factor_b_means[.y] + overall_mean)
  second_col_gamma <- rep(1:factor_a_levels %>% length,factor_b_levels %>% length) %>% map2_dbl(rep(2,factor_b_levels %>% length),~factor_level_means[.x,.y] - factor_a_means[.x] -factor_b_means[.y] + overall_mean)
  third_col_gamma <- rep(1:factor_a_levels %>% length,factor_b_levels %>% length) %>% map2_dbl(rep(3,factor_b_levels %>% length),~factor_level_means[.x,.y] - factor_a_means[.x] -factor_b_means[.y] + overall_mean)
  fourth_col_gamma <- rep(1:factor_a_levels %>% length,factor_b_levels %>% length) %>% map2_dbl(rep(4,factor_b_levels %>% length),~factor_level_means[.x,.y] - factor_a_means[.x] -factor_b_means[.y] + overall_mean)
  fifth_col_gamma <- rep(1:factor_a_levels %>% length,factor_b_levels %>% length) %>% map2_dbl(rep(5,factor_b_levels %>% length),~factor_level_means[.x,.y] - factor_a_means[.x] -factor_b_means[.y] + overall_mean)
  sixth_col_gamma <- rep(1:factor_a_levels %>% length,factor_b_levels %>% length) %>% map2_dbl(rep(6,factor_b_levels %>% length),~factor_level_means[.x,.y] - factor_a_means[.x] -factor_b_means[.y] + overall_mean)
  seventh_col_gamma <- rep(1:factor_a_levels %>% length,factor_b_levels %>% length) %>% map2_dbl(rep(7,factor_b_levels %>% length),~factor_level_means[.x,.y] - factor_a_means[.x] -factor_b_means[.y] + overall_mean)
  gamma_hat <- cbind(first_col_gamma,second_col_gamma,third_col_gamma,fourth_col_gamma,fifth_col_gamma,sixth_col_gamma,seventh_col_gamma)
  
  SSA <- n*b*(sum((alpha_hat)^2))
  SSB <- n*a*(sum((beta_hat)^2))
  SSAB <- n*sum(gamma_hat^2)
  
  
  
  filtered_observations <- c(rep(factor_a_levels[1],factor_b_levels %>% length),rep(factor_a_levels[2],factor_b_levels %>% length)) %>% 
    map2(rep(factor_b_levels[1:factor_b_levels %>% length],factor_a_levels %>% length),~data %>% filter(!!as.symbol(factor_a) == .x & !!as.symbol(factor_b) == .y) %>% extract2(response))
  filtered_means <- c(filtered_means[1,1],filtered_means[1,2],filtered_means[1,3],filtered_means[1,4],filtered_means[1,5],filtered_means[1,6],filtered_means[1,7],
                      filtered_means[2,1],filtered_means[2,2],filtered_means[2,3],filtered_means[2,4],filtered_means[2,5],filtered_means[2,6],filtered_means[2,7])
  SSE <- filtered_observations %>% map2(filtered_means,~sum((.x-.y)^2)) %>% unlist %>% sum
  
  
  SSTO <- SSA+SSB+SSAB+SSE
  
  df_a <- a-1
  df_b <- b-1
  df_ab <- (a-1)*(b-1)
  df_error <- a*b*(n-1)
  df_total <- (n*a*b)-1
  
  MSA <- SSA/df_a
  MSB <- SSB/df_b
  MSAB <- SSAB/df_ab
  MSE <- SSE/df_error
  
  anova_table <- data.frame("SS" = c(SSA,SSB,SSAB,SSE,SSTO),"df" = c(df_a,df_b,df_ab,df_error,df_total),"MS" = c(MSA,MSB,MSAB,MSE,NA))
  rownames(anova_table) <- c("Factor A","Factor B","AB Interactions","Error","Total")
  
  return(anova_table)
}

anova.table(covid19,"COVIDProp","Sex","AgeGroup")

COVID19 = read.table("COVID19.txt", header = T, sep = "\t")
COVID19[652,c(5:7)] = 0


COVID19$Sex = ifelse(COVID19$Sex == "Male", 1, 2)
COVID19$AgeGroup = as.factor(COVID19$AgeGroup)
COVID19$AgeGroup = factor(COVID19$AgeGroup,levels(COVID19$AgeGroup),1:7)
COVID19$AgeGroup = as.integer(COVID19$AgeGroup)
COVID19 = COVID19[-which(COVID19$AgeGroup == 1),]
COVID19 = COVID19[-which(COVID19$AgeGroup == 2),]
COVID19 = COVID19[-which(COVID19$AgeGroup == 3),]
COVID19$AgeGroup = factor(COVID19$AgeGroup,4:7,1:4)
View(COVID19)
Y = COVID19$COVIDProp
factorA = COVID19$AgeGroup
factorB = COVID19$Sex
factorA = as.integer(factorA)
a = length(unique(factorA))
b = length(unique(factorB))
Yijbar = matrix(0,nrow = a,ncol = b)
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yijbar[i,j] = mean(Y[factorA==i&factorB==j])
  }
}
u = mean(Y)
Yidotbar = apply(Yijbar,MARGIN = 1,mean)
Ydotjbar  = apply(Yijbar,MARGIN = 2,mean)
alpha = Yidotbar-u
beta = Ydotjbar-u
gamma = matrix(0,nrow = a,ncol = b)
for(i in 1:a)
{
  for(j in 1:b)
  {
    gamma[i,j] = Yijbar[i,j]-Yidotbar[i]-Ydotjbar[j]+u
  }
}

Yhat = rep(0,length(Y))
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yhat[factorA==i&factorB==j] = Yijbar[i,j]
  }
}

e = Y-Yhat
plot(Yhat, e, pch = 19, xlab = "Fitted Values", ylab = "errors", main = "Residual Plot")
qqnorm(e)
qqline(e)

n = 52
SSA = n*b*sum(alpha^2)
SSB = n*a*sum(beta^2)
SSAB = n*sum(gamma^2)
Yhat = rep(0,length(Y))
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yhat[factorA==i&factorB==j] = Yijbar[i,j]
  }
}
e = Y-Yhat
SSE = sum((Y-Yhat)^2)
SSTotal = sum((Y-u)^2)
AnovaTable = matrix(0,nrow = 5,ncol = 3)
AnovaTable[1,1] = SSA
AnovaTable[1,2] = a-1
AnovaTable[1,3] = SSA/(a-1)
AnovaTable[2,1] = SSB
AnovaTable[2,2] = b-1
AnovaTable[2,3] = SSB/(b-1)
AnovaTable[3,1] = SSAB
AnovaTable[3,2] = (a-1)*(b-1)
AnovaTable[3,3] = SSAB/((a-1)*(b-1))
AnovaTable[4,1] = SSE
AnovaTable[4,2] = a*b*(n-1)
AnovaTable[4,3] = SSE/(a*b*(n-1))
AnovaTable[5,1] = SSTotal
AnovaTable[5,2] = n*a*b-1
AnovaTable[5,3] = '-'
AnovaTable = as.data.frame(AnovaTable)
rownames(AnovaTable) = c('factor A','factor B','interaction AB','Error','Total')
colnames(AnovaTable) = c('SS','df','MS')
library(pander)
pander(pandoc.table(AnovaTable))

Yijmedian = matrix(0,nrow = a,ncol = b)
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yijmedian[i,j] = median(Y[factorA==i&factorB==j])
  }
}

Yijmedian


# median = c(rep(Yijmedian[1,1],52), rep(Yijmedian[1,2],52), rep(Yijmedian[2,1],52), rep(Yijmedian[2,2],52), rep(Yijmedian[3,1],52), rep(Yijmedian[3,2],52), rep(Yijmedian[4,1],52), rep(Yijmedian[4,2],52), rep(Yijmedian[5,1],52), rep(Yijmedian[5,2],52), rep(Yijmedian[6,1],52), rep(Yijmedian[6,2],52), rep(Yijmedian[7,1],52), rep(Yijmedian[7,2],52))


