
setwd("C:/Users/bdi25/all/Desktop/data/Stal")

library(ggplot2)
library(psych)

### Знакомство с данными

stal <- read.csv("low-alloy steels.csv")
stal <- stal[-627,c(-1,-14,-15,-17,-19,-20)]
hts <- rbind(head(stal),tail(stal))
str(stal)

#Проверка на дубликаты данных и удаление их
duplicated(stal)
sum(duplicated(stal))
bet <- unique(bet)

#Проверка на пропущенные значения 
sum(is.na(stal))

#Получение статистической сводки по набору данных.
desstal <- describe(stal)
desstal <- desstal[,-1]


#Переименнуюем прочность бетона Tensile.Strength..MPa.на Ten.Str
#Переименнуюем Температуру Temperature...C. на Temp
names(stal)[14] <- "Ten.Str"
names(stal)[13] <- "Temp"


#Анализ выходной переменной
ggplot(stal,aes(x=Ten.Str))+
  geom_histogram(aes(y = ..density..),fill="yellow",col='black',binwidth = 8)+
  xlab("Прочность стали на растяжение\n(Мпа)") + ylab("") +
  ggtitle("Гистограмма распределения прочности стали")+
  geom_density(col = "darkblue")

list_proch <- as.list(summary(stal$Ten.Str))
d_proch <- as.data.frame(list_proch)          #Просто базовая сводка по данным
row.names(d_proch)[1] <- "Прочность стали"

ggplot(stal, aes(sample=Ten.Str)) + stat_qq() + stat_qq_line()+
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
  ggtitle("QQ plot нормальности данных")

shapiro.test(stal$Ten.Str)


#Анализ входных параметров 
library(cowplot)

dis_cor_pl <- function(x){
  ss <- summary(x,stal)
  #ss1 <- summary(aov(Ten.Str~x,stal))
  ss2 <- cor.test(~Ten.Str+x,stal)
  
  options(repr.plot.width = 14, repr.plot.height = 7) 
  plot_1 <- ggplot(stal,aes(x))+
geom_histogram(aes(y = ..density..),fill="yellow",col='black',binwidth = 0.1)+
  ylab("")+geom_density(col = "darkblue") 
  plot_2 <- ggplot(stal, aes(x,Ten.Str)) + geom_point()
  bottom_row <- plot_grid(plot_2,labels = c('B'), label_size = 12)
  pp <- plot_grid(plot_1, bottom_row, labels = c('A', ''), 
                  label_size = 12, ncol = 1)
  
  print(ss)
  #print(ss1)
  print(pp)
  
  return(ss2)
}

dis_cor_pl(stal$V)

library(GGally)
ggpairs(stal)


#Деление выборки на обучающую и тестовую
library(caret)
in_train <- createDataPartition(y = stal$Ten.Str, p=0.75, list = FALSE)
h_train <- stal[in_train,]
h_test <- stal[-in_train,]



#Построение множественной регрессии
library(stargazer)

L_mod_all <- lm(Ten.Str~.,h_train)
summary(L_mod_all)
stargazer(L_mod_all,type="html", dep.var.labels = "Ten.Str", 
          covariate.labels = "", out = "lin_mod_x.html")

Lmod_is <- lm(Ten.Str~C+Mn+Mo+Cu+V+Temp+Ni,h_train)
summary(Lmod_is)
stargazer(Lmod_is,type="html", dep.var.labels = "Ten.Str", 
          covariate.labels = "", out = "lin_mod_x.html")

Lmod_is1 <- lm(Ten.Str~Mn+Mo+Cu+V+Temp,h_train)
summary(Lmod_is1)
stargazer(Lmod_is1,type="html", dep.var.labels = "Ten.Str", 
          covariate.labels = "", out = "lin_mod_x.html")

model_null <- lm(Ten.Str ~ 1, data = h_train)
L_step_for <- step(model_null, scope = list(lower = model_null, 
                                upper = L_mod_all), direction = "forward")
summary(L_step_for)
stargazer(L_step_for, type="html", dep.var.labels = "Ten.Str", 
          covariate.labels = "",out = "lin_mod_x.html")

L_step_bac <- step(L_mod_all, direction = 'backward')
summary(L_step_bac)
stargazer(L_step_bac,type="html", dep.var.labels = "Ten.Str", 
          covariate.labels = "", out = "lin_mod_x.html")

aic_data_lm <- data.frame(All=AIC(L_mod_all),Isprav=AIC(Lmod_is),
      Isprav1=AIC(Lmod_is1), step_for=AIC(L_step_for), Last=AIC(L_step_bac))
row.names(aic_data_lm)[1] <- "AIC"

r.sq.lm <- data.frame(L_mod_all=summary(L_mod_all)$adj.r.squared, 
                      Lmod_is=summary(Lmod_is)$adj.r.squared,
                   Lmod_is1=summary(Lmod_is1)$adj.r.squared,
                   L_step_for=summary(L_step_for)$adj.r.squared,
                   L_step_bac=summary(L_step_bac)$adj.r.squared)
row.names(r.sq.lm)[1] <- "R.squared"

L_mod <-Lmod_is #Переименовали для удобства записи


#создадим фрейм с даными прочности и предсказанными значениями моделей 
prognoz <- data.frame(Ten.Str = h_test$Ten.Str)
prognoz$lin.reg <- predict(L_mod,h_test)
plin <- rbind(head(prognoz),tail(prognoz))


#Проверка модели на тестовой выбоки

#Коэффицент детерминации 
r.sq.l <- data.frame(lin.reg=summary(Lmod_is)$adj.r.squared)
row.names(r.sq.l)[1] <- "R.squared"
r.sq.all <- data.frame(lin.reg=summary(Lmod_is)$adj.r.squared)
row.names(r.sq.all)[1] <- "R.squared"

#Проверка корреляции
cor.test(prognoz$Ten.Str, prognoz$lin.reg)
lin.cor <- data.frame(Корреляционная.зависимость = 0.7939)
cor.tochn <- data.frame(lin.reg=0.7939, reg.tree=0.8265, 
                        random.for=0.9444, poly_10=0.9219)
row.names(cor.tochn)[1] <- "Корреляционная.зависимость"

#Среднее абсолютное процентное отклоение
mad.lin <- data.frame(lin.reg=mean(abs(prognoz$lin.reg - 
                                         prognoz$Ten.Str)/prognoz$Ten.Str))
row.names(mad.lin)[1] <- "MAPE"
mad_all <- data.frame(mad.lin)
row.names(mad_all)[1] <- "MAPE"

#Средняя абсолютная ошибка прогноза
prognozlin.reg <- data.frame(Ten.Str=prognoz$Ten.Str, lin.reg=prognoz$lin.reg)
mape.lin <- data.frame(lin.reg=mean(apply(prognozlin.reg, 1, min)/
                                      apply(prognozlin.reg, 1, max)))
row.names(mape.lin)[1] <- "MaxMin"
mape_all <- data.frame(mape.lin)
row.names(mape_all)[1] <- "MaxMin"

#RMSE
RMSE_lm <- data.frame(rmse.lin=sqrt(mean((prognoz$Ten.Str-prognoz$lin.reg)^2)))
row.names(RMSE_lm)[1] <- "RMSE"
RMSE <- data.frame(lin.reg=RMSE_lm)
row.names(RMSE)[1] <- "RMSE"


#Диагностика модели
d_gr <- data.frame(Lmod_pred=L_mod$fitted, Lmod_res=L_mod$resid)

#Проверка на гетероскедастичность
library (lmtest)
bptest(L_mod)

#Проверка на линейность
ggplot(d_gr, aes(Lmod_pred, Lmod_res)) + 
  geom_point(size = 1.5) + geom_hline(yintercept=0, col = 'red', lwd = 1)
#Независимость остатков 
d_gr$obs_number <- 1:nrow(d_gr)
ggplot(d_gr, aes(x = obs_number, y = Lmod_res)) + 
  geom_point(size = 1.5) + geom_smooth()
#Норм. распр. остатков
qqnorm(L_mod$residuals)
qqline(L_mod$residuals)
ggplot(d_gr, aes(x = Lmod_res)) + 
  geom_histogram(binwidth = 15, fill = 'white', col = 'black')
shapiro.test(L_mod$residuals)


par(mfrow=c(2,2))
plot(L_mod)


#Регрессионное дерево
library(rpart)     
library(rpart.plot)
library(dplyr)
par(mfrow=c(1,1))

reg_t <- rpart(Ten.Str ~ ., data = h_train, method  = "anova")
rpart.plot(reg_t)
plotcp(reg_t)

reg_t1 <- rpart(Ten.Str ~ ., data = h_train, 
                method="anova", control=list(cp = 0, xval = 10))
plotcp(reg_t1)
abline(v = 15, lty = "dashed")

reg_t$cptable #Вывод таблицы 

hyper_grid <- expand.grid(minsplit = seq(5, 20, 1), maxdepth = seq(7, 14, 1))

head(hyper_grid)
nrow(hyper_grid)

models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # получаем значения minsplit, maxdepth в строке i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # обучение модели и сохраните в списке
  models[[i]] <- rpart(Ten.Str ~ ., data=h_train, method ="anova",
                       control = list(minsplit = minsplit, maxdepth=maxdepth)
  )
}

# функция для получения оптимального cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# функция для получения минимальной ошибки
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

optimal_tree <- rpart(Ten.Str ~ ., data=h_train, method="anova",
                      control = list(minsplit = 6, maxdepth = 7, cp = 0.01) )
optimal_tree$frame

rpart.plot(optimal_tree)
plotcp(optimal_tree)

target <- h_train$Ten.Str
tree_err <- mean((predict(optimal_tree)-target)^2)
null_err <- mean((mean(target)-target)^2)
tree_R2 <- 1.-tree_err/null_err
r.sq.tree <- data.frame(tree_R2)
row.names(r.sq.tree)[1] <- "R.squared"
r.sq.all$reg.tree <- tree_R2

#создадим фрейм с даными прочности и предсказанными значениями трех моделей 
prognoz$reg.tree <- predict(optimal_tree,h_test)
ht.rt <- rbind(head(prognoz[,-2]),tail(prognoz[,-2]))

#Проверка корреляции
cor.test(prognoz$Ten.Str,prognoz$reg.tree)
cor.tree <- data.frame(Корреляционная.зависимость = 0.9232)
cor.tochn <- data.frame(lin.reg=0.7939, reg.tree=0.9232, 
                        random.for=0.9444, poly_10=0.9219)

#Среднее абсолютное процентное отклоение
mad.reg <- data.frame(reg.tree=mean(abs(prognoz$reg.tree - 
                                          prognoz$Ten.Str)/prognoz$Ten.Str))
row.names(mad.reg)[1] <- "MAPE"
mad_all$reg.tree <- mad.reg$reg.tree

#Средняя абсолютная ошибка прогноза
prognoztr <- data.frame(Ten.Str=prognoz$Ten.Str, reg.tree=prognoz$reg.tree)
mape.tree <- data.frame(reg.tree= mean(apply(prognoztr, 1, min)/
                                         apply(prognoztr, 1, max)))
row.names(mape.tree)[1] <- "MaxMin"
mape_all$reg.tree <- mape.tree$reg.tree

#RMSE
RMSE_reg.tree <- data.frame(rmse.reg.tree=sqrt(mean((prognoz$Ten.Str - 
                                                       prognoz$reg.tree)^2)))
row.names(RMSE_reg.tree)[1] <- "RMSE"
RMSE$reg.tree <- RMSE_reg.tree$rmse.reg.tree 



###Случайный лес
library(randomForest)

#создание модели случайного леса
modelrf <- randomForest(
  formula = Ten.Str ~ .,
  data = h_train
)
#просмотр построенной модели
modelrf

#отображение тестового MSE по количеству деревьев
plot(modelrf)

#нахождение количества деревьев, которые дают наименьший тестовый MSE
which.min(modelrf$mse)
kt <- data.frame(Количество.деревьев = 238)

#нахождение RMSE для построения лучшей модели
sqrt(modelrf$mse[which.min(modelrf$mse)])
nr <- data.frame(Оптимальный.RMSE = 35.99)

#график важности предикторов
varImpPlot(modelrf) 

#настройка модели, нахождение наименьшего Mtry
model_tuned <- tuneRF(
  x=h_train[,-14],
  y=h_train$Ten.Str,
  ntreeTry=500,
  mtryStart=2,
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE #позволяет не показывать процесс в реальном времени
)

best_rf_model <- randomForest(formula=Ten.Str~.,
                              mtry=9,
                              ntree=238,
                              data = h_train)

target <- h_train $Ten.Str
rf_err <- mean((predict(best_rf_model)-target)^2)
null_err <- mean((mean(target)-target)^2)
rf_R2 <- 1.-rf_err/null_err
r.sq.rf <- data.frame(rf_R2)
row.names(r.sq.rf)[1] <- "R.squared"
r.sq.all$rand.for <- rf_R2

varImpPlot(best_rf_model) 
importance.rf <- importance(best_rf_model)

confusion_mtx = table(h_test$Ten.Str, prognoz$rand.for)
confusion_mtx

#создадим фрейм с даными прочности и предсказанными значениями 
prognoz$rand.for <- predict(best_rf_model,h_test)
ht.rf <- rbind(head(prognoz[,c(-2,-3)]),tail(prognoz[,c(-2,-3)]))

#Проверка корреляции
cor.test(prognoz$Ten.Str,prognoz$rand.for)
cor.rf <- data.frame(Корреляционная.зависимость = 0.9859)
cor.tochn <- data.frame(lin.reg=0.7939, reg.tree=0.9139, 
                        random.for=0.9859, poly_10=0.9219)

#Среднее абсолютное процентное отклоение
mad.rf <- data.frame(rand.for=mean(abs(prognoz$rand.for - 
                                         prognoz$Ten.Str)/prognoz$Ten.Str))
row.names(mad.rf)[1] <- "MAPE"
mad_all$rand.for <- mad.rf$rand.for

#Средняя абсолютная ошибка прогноза
prognozrf <- data.frame(Ten.Str=prognoz$Ten.Str, rand.for=prognoz$rand.for)
mape.rf <- data.frame(rand.for= mean(apply(prognozrf, 1, min)/
                                       apply(prognozrf, 1, max)))
row.names(mape.rf)[1] <- "MaxMin"
mape_all$rand.for <- mape.rf$rand.for

#RMSE
RMSE_rf <- data.frame(rmse.rand.for=sqrt(mean((prognoz$Ten.Str - 
                                                 prognoz$rand.for)^2)))
row.names(RMSE_rf)[1] <- "RMSE"
RMSE$rand.for <- RMSE_rf$rmse.rand.for 




#Полиномиальная регрессия

pol2 <- lm(Ten.Str ~ poly(C,2) + poly(Mn,2) +poly(Mo,2) +poly(Cu,2) + 
                     poly(V,2) +poly(Temp,2)+ poly(Ni,2), h_train)
summary(pol2)

pol3 <- lm(Ten.Str ~ poly(C,3) + poly(Mn,3) + poly(Mo,3) +poly(Cu,3) + 
             poly(V,3) +poly(Temp,3)+ poly(Ni,3), h_train)
summary(pol3)


pol4 <- lm(Ten.Str ~ poly(C,4) + poly(Mn,4) +poly(Mo,4) + poly(Cu,4) + 
             poly(V,4) + poly(Temp,4)+poly(Ni,4), h_train)
summary(pol4)

pol5 <- lm(Ten.Str ~ poly(C,5) +  poly(Mn,5) + poly(Mo,5) +poly(Cu,5) + 
             poly(V,5) +poly(Temp,5)+ poly(Ni,5), h_train)
summary(pol5)

pol6 <- lm(Ten.Str ~ poly(C,6) + poly(Mn,6) + poly(Mo,6) + poly(Cu,6) + 
             poly(V,6) +poly(Temp,6)+ poly(Ni,6), h_train)
summary(pol6)

pol7 <- lm(Ten.Str ~ poly(C,7) + poly(Mn,7) +poly(Mo,7) +poly(Cu,7) + 
             poly(V,7) +poly(Temp,7)+poly(Ni,7), h_train)
summary(pol7)

pol8 <- lm(Ten.Str ~ poly(C,8) + poly(Mn,8) +poly(Mo,8) +poly(Cu,8) + 
             poly(V,8) +poly(Temp,8)+poly(Ni,8), h_train)
summary(pol8)

pol9 <- lm(Ten.Str ~ poly(C,9) + poly(Mn,9) +poly(Mo,9) + poly(Cu,9) + 
             poly(V,9) + poly(Temp,9)+ poly(Ni,9), h_train)
summary(pol9)

pol10 <- lm(Ten.Str ~ poly(C,10) +  poly(Mn,10) + poly(Mo,10) + poly(Cu,10)+ 
              poly(V,10) +poly(Temp,10)+ poly(Ni,10), h_train)
summary(pol10)

aic_data_pol <- data.frame(pol2=AIC(pol2),pol3=AIC(pol3), pol4=AIC(pol4),
                           pol5=AIC(pol5),pol6=AIC(pol6),pol7=AIC(pol7),
                           pol8=AIC(pol8),pol9=AIC(pol9),pol10=AIC(pol10))
row.names(aic_data_pol)[1] <- "AIC"

r.sq.polall <- data.frame(
  pol2=summary(pol2)$adj.r.squared, pol3=summary(pol3)$adj.r.squared,
  pol4=summary(pol4)$adj.r.squared,pol5=summary(pol5)$adj.r.squared,
  pol6=summary(pol6)$adj.r.squared, pol7=summary(pol7)$adj.r.squared,
  pol8=summary(pol8)$adj.r.squared, pol9=summary(pol9)$adj.r.squared,
  pol10=summary(pol10)$adj.r.squared)
row.names(r.sq.polall)[1] <- "R.squared"


#создадим фрейм с даными прочности и предсказанными значениями 
prognoz$poly_4 <- predict(pol4,h_test)
ht.pol4 <- rbind(head(prognoz[,c(1,7)]),tail(prognoz[,c(1,7)]))

r.sq.pol4 <- data.frame(poly_4=summary(pol4)$adj.r.squared)
row.names(r.sq.pol4)[1] <- "R.squared"
r.sq.all$poly_4 <- r.sq.pol4$poly_4

#Проверка корреляции
cor.test(prognoz$Ten.Str,prognoz$poly_4)
cor.pol4 <- data.frame(Корреляционная.зависимость = 0.9357)
cor.tochn <- data.frame(lin.reg=0.7939, reg.tree=0.9139,
                        random.for=0.9842, poly_10=0.9357)

#Среднее абсолютное процентное отклоение
mad.pol4 <- data.frame(pol4=mean(abs(prognoz$poly_4 - 
                                       prognoz$Ten.Str)/prognoz$Ten.Str))
row.names(mad.pol4)[1] <- "MAPE"
mad_all$poly_4 <- mad.pol4$pol4

#Средняя абсолютная ошибка прогноза
prognoz4 <- data.frame(Ten.Str=prognoz$Ten.Str, poly_4=prognoz$poly_4)
mape.pol4 <- data.frame(poly_4= mean(apply(prognoz4, 1, min)/
                                       apply(prognoz4, 1, max)))
row.names(mape.pol4)[1] <- "MaxMin"
mape_all$pol_4 <- mape.pol4$poly_4

#RMSE
RMSE_pol4 <- data.frame(rmse.pol4=sqrt(mean((prognoz$Ten.Str
                                             - prognoz$poly_4)^2)))
row.names(RMSE_pol4)[1] <- "RMSE"
RMSE$poly_4 <- RMSE_pol4$rmse.pol4


ht.prog <- rbind(head(prognoz),tail(prognoz))

RMSE <- RMSE[,c(-3,-5)]

### Модель нейроной сети 

library("neuralnet")

set.seed(80) # сеем семя

max <-  apply(stal , 2 , max)
min <-  apply(stal, 2 , min)

# скалируем данные

trainNN <-  as.data.frame(scale(h_train,
                                center = min,
                                scale = max - min))

testNN <- as.data.frame(scale(h_test,
                              center = min,
                              scale = max - min))

# Построенние базовой модели

NN <-  neuralnet(Ten.Str ~ C+Si+Mn+P+S+Ni+Cr+Mo+Cu+V+Al+N+Temp, 
                 data=trainNN, linear.output = F)
plot(NN)

NNprognoz <-data.frame(NN =neuralnet::compute(NN,testNN[,c(1:13)])$net.result)

#посмотрим на R2 и RMSE

r.sq.NN <- data.frame(NN_R2=postResample(NNprognoz,testNN[,14])[2])
row.names(r.sq.NN)[1] <- "R.squared"
r.sq.NN_all <- r.sq.NN

RMSE_NN <- data.frame(rmse.NN=sqrt(mean((testNN$Ten.Str - NNprognoz$NN)^2)))
row.names(RMSE_NN)[1] <- "RMSE"
RMSE_NN_all <- RMSE_NN


#Построение модели с 5тью скрытымии узлами

NN5 = neuralnet(Ten.Str ~ C+Si+Mn+P+S+Ni+Cr+Mo+Cu+V+Al+N+Temp, 
                data=trainNN, hidden = 5, linear.output = F)

plot(NN5)

NNprognoz$NN5 <- (neuralnet::compute(NN5, testNN[,c(1:13)]))$net.result


r.sq.NN5 <- data.frame(NN5_R2= postResample(NNprognoz$NN5,testNN[,14])[2])
row.names(r.sq.NN5)[1] <- "R.squared"
r.sq.NN_all$NN5_R2 <- postResample(NNprognoz$NN5,testNN[,14])[2]

RMSE_NN5 <- data.frame(rmse.NN5=sqrt(mean((testNN$Ten.Str-NNprognoz$NN5)^2)))
row.names(RMSE_NN5)[1] <- "RMSE"
RMSE_NN_all$RMSE_NN5 <- RMSE_NN5$rmse.NN5


#Построение модели с 15тью скрытымии узлами

NN15 = neuralnet(Ten.Str ~ C+Si+Mn+P+S+Ni+Cr+Mo+Cu+V+Al+N+Temp, 
                 data=trainNN, hidden = c(5,10), linear.output = F)

plot(NN15)

NNprognoz$NN15 <- neuralnet::compute(NN15, testNN[,c(1:13)])$net.result


r.sq.NN15 <- data.frame(NN15=postResample(NNprognoz$NN15,testNN[,14])[2])
row.names(r.sq.NN15)[1] <- "R.squared"
r.sq.NN_all$NN15_R2 <- postResample(NNprognoz$NN15,testNN[,14])[2]

RMSE_NN15 <-data.frame(rmse.NN15=sqrt(mean((testNN$Ten.Str-NNprognoz$NN15)^2)))
row.names(RMSE_NN15)[1] <- "RMSE"
RMSE_NN_all$RMSE_NN15 <- RMSE_NN15$rmse.NN15

#Построение с 8ью скрытыми узлами

NN8 = neuralnet(Ten.Str ~ ., 
                data=trainNN, hidden = c(12,6), linear.output = F)
NN18 <- NN8
plot(NN18)

NNprognoz$NN18 <- neuralnet::compute(NN8, testNN[,c(1:13)])$net.result

r.sq.NN18 <- data.frame(NN18_R2=postResample(NNprognoz$NN18,testNN[,14])[2])
row.names(r.sq.NN18)[1] <- "R.squared"
r.sq.NN_all$NN18_R2 <- postResample(NNprognoz$NN18,testNN[,14])[2]

RMSE_NN18 <-data.frame(rmse.NN18=sqrt(mean((testNN$Ten.Str-NNprognoz$NN18)^2)))
row.names(RMSE_NN18)[1] <- "RMSE"
RMSE_NN_all$RMSE_NN18 <- RMSE_NN18$rmse.NN18
RMSE$NN18 <- sqrt(mean((h_test$Ten.Str - prognoz$NN18)^2))

#Обратная нормализация данных

prognozNN <- data.frame(NN18=NNprognoz$NN18 * (max(h_train$Ten.Str)
                            - min(h_train$Ten.Str)))+ min(h_train$Ten.Str)
prognoz$NN18 <- prognozNN$NN18
ht.NN <- rbind(head(prognoz[,c(1,6)]),tail(prognoz[,c(1,6)]))

#Проверка корреляции
cor.test(prognoz$Ten.Str,prognoz$NN18)
cor.NN18 <- data.frame(Корреляционная.зависимость = 0.9832)
cor.tochn <- data.frame(lin.reg=0.7939, reg.tree=0.9139, random.for=0.9842, 
                        poly_4=0.9357, NN18=0.9832)
row.names(cor.tochn)[1] <- "Корреляционная.зависимость"

#Среднее абсолютное процентное отклоение
mad.NN18 <- data.frame(NN18=mean(abs(prognoz$NN18 - 
                                       prognoz$Ten.Str)/prognoz$Ten.Str))
row.names(mad.NN18)[1] <- "MAPE"
mad_all$NN18 <- mad.NN18$NN18

#Средняя абсолютная ошибка прогноза
prognozNN18 <- data.frame(Ten.Str=prognoz$Ten.Str, NN18=prognoz$NN18)
mape.NN18 <- data.frame(NN18= mean(apply(prognozNN18, 1, min)/
                                     apply(prognozNN18, 1, max)))
row.names(mape.NN18)[1] <- "MaxMin"
mape_all$NN18 <- mape.NN18$NN18

ht.prog <- rbind(head(prognoz),tail(prognoz))


RMSE$NN18 <- sqrt(mean((h_test$Ten.Str - prognoz$NN18)^2))

prognoz <- prognoz[,-5]

mape_all <- mape_all[,-4]
row.names(mape_all)[1] <- "MaxMin"

mad_all <- mad_all[,-4]
row.names(mad_all)[1] <- "MAPE"










