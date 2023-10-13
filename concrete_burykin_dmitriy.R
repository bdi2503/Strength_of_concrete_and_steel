
### Знакомство с данными

bet <- read.csv("concrete_data.csv")
hb <- head(bet)
tb <- tail(bet)
str(bet)

#Проверка на дубликаты данных и удаление их
sum(duplicated(bet))
bet <- unique(bet)

#Проверка на пропущенные значения 
sum(is.na(bet))

#Получение статистической сводки по набору данных.
debet <- describe(bet)
debet <- debet[,-2]

#Анализ выходной переменной
ggplot(bet,aes(x=Strength))+
  geom_histogram(aes(y = ..density..),fill="yellow",col='black',binwidth = 8)+
  xlab("Прочность бетона на сжатие\n(Мпа)") + ylab("") +
  ggtitle("Гистограмма распределения прочности бетона")+
  geom_density(col = "darkblue")

list_proch <- as.list(summary(bet$Strength))
d_proch <- as.data.frame(list_proch)        #Просто базовая сводка по данным
row.names(d_proch)[1] <- "Прочность бетона"

ggplot(bet, aes(sample=Strength)) + stat_qq() + stat_qq_line()+
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
  ggtitle("QQ plot нормальности данных")

shapiro.test(bet$Strength)

#Преобразование Бокса-Кокса
library(MASS)
box_tuning <- boxcox(bet$Strength ~ 1,
                     lambda = seq(-6,6,0.1) )

box_frame <- data.frame(box_tuning$x,box_tuning$y)
colnames(box_frame) <- c('lambda','LogLikelihood')

box_frame <- box_frame[order(-box_frame$LogLikelihood),]
bx1 <- box_frame[1,]
opt_lambda <- box_frame[1,'lambda']

target <- bet$Strength 
bet$Cox.Strength <- (target^opt_lambda - 1)/opt_lambda

ggplot(bet,aes(x=Cox.Strength))+
  geom_histogram(aes(y = ..density..),fill="yellow",col='black',binwidth = 1)+
  xlab("Прочность бетона на сжатие\n(Мпа) (Бокса-Кокса") + ylab("") +
  ggtitle("Гистограмма распределения прочности бетона \n
          после преобразования Бокса-Кокса")+
  geom_density(col = "darkblue")
ggplot(bet, aes(sample=Cox.Strength)) + stat_qq() + stat_qq_line()+
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
  ggtitle("QQ plot нормальности данных (Бокс-Кокс)")

shapiro.test(bet$Cox.Strength)

#Анализ входных параметров 
library(cowplot)

dis_cor_pl <- function(x){
  ss <- summary(x,bet)
  ss1 <- summary(aov(Strength~x,bet))
  ss2 <- summary(aov(Cox.Strength~x,bet))
  ss3 <- cor.test(~Strength+x,bet)
  ss4 <- cor.test(~Cox.Strength+x,bet)
  
  options(repr.plot.width = 14, repr.plot.height = 7) 
  plot_1 <- ggplot(bet,aes(x))+
    geom_histogram(aes(y = ..density..), fill="yellow",
                   col='black',binwidth = 20)+ ylab("")+
    geom_density(col = "darkblue") 
  plot_2 <- ggplot(bet, aes(x,Strength)) + geom_point()
  plot_3 <- ggplot(bet, aes(x,Cox.Strength)) + geom_point()
  bottom_row <- plot_grid(plot_2, plot_3, labels = c('B', 'C'),label_size = 12)
  pp <- plot_grid(plot_1, bottom_row, labels = c('A', ''), 
                  label_size = 12, ncol = 1)
  
  print(ss)
  print(ss3)
  print(ss4)
  print(pp)
  
  return(c(ss1,ss2))
}
dis_cor_pl(bet$Age)

ggpairs(bet)

#Деление выборки на обучающую и тестовую
library(caret)
in_train <- createDataPartition(y = bet$Cox.Strength, p=0.75, list = FALSE)
h_train <- bet[in_train,]
h_train <- h_train[,-9]
h_test <- bet[-in_train,]
h_test <- h_test[,-9]

#Построение линейной регресии с одним предиктором
lin_plot <- function(x){
  mod <- lm(Cox.Strength~x,h_train)
  ss <- summary(mod)
  gg <- ggplot(h_train, aes(Cox.Strength, x))+
    geom_point(size = 1)+
    geom_smooth(method = "lm")
 stargazer(mod,type="html", 
                 dep.var.labels = "Cox.Strength", 
                 covariate.labels = "x",
                 out = "lin_mod_x.html")
  print(ss)
  print(gg)
  }
lin_plot(h_train$Age)

#Построение множественной регрессии
L_mod_all <- lm(Cox.Strength~.,h_train)
summary(L_mod_all)
stargazer(L_mod_all,type="html", dep.var.labels = "Cox.Strength", 
          covariate.labels = "", out = "lin_mod_x.html")

L_mod_is1 <- lm(Cox.Strength~Cement+Blast.Furnace.Slag+
                  Fly.Ash+Superplasticizer+Age, h_train)
summary(L_mod_is1)
stargazer(L_mod_is1, type="html", dep.var.labels = "Cox.Strength", 
          covariate.labels = "",out = "lin_mod_x.html")

model_null <- lm(Cox.Strength ~ 1, data = h_train)
L_step_for <- step(model_null, scope = list(lower = model_null, 
                  upper = L_mod_all), direction = "forward")
summary(L_step_for)
stargazer(L_step_for, type="html", dep.var.labels = "Cox.Strength", 
          covariate.labels = "",out = "lin_mod_x.html")

L_step_bac <- step(L_mod_all, direction = 'backward')
summary(L_step_bac)
stargazer(L_step_bac, type="html", dep.var.labels = "Cox.Strength", 
          covariate.labels = "",out = "lin_mod_x.html")

L_last <- lm(Cox.Strength~Cement+Blast.Furnace.Slag+Water+Fly.Ash+Age, h_train)
summary(L_last)
stargazer(L_last, type="html", dep.var.labels = "Cox.Strength", 
          covariate.labels = "",out = "lin_mod_x.html")

anova(L_mod_all, L_step_for)

aic_data <- data.frame(All=AIC(L_mod_all),step_for=AIC(L_step_for), 
                       Last=AIC(L_last))
row.names(aic_data)[1] <- "AIC"

L_mod <-L_step_for #Переименовали для удобства записи

#Проверка модели на тестовой выбоки

pr_t <- predict(L_mod, h_test)

srav_pr <- data.frame(Cox.Strength=h_test$Cox.Strength,predict=pr_t)
hs <- head(srav_pr)
ht <- tail(srav_pr)
sht <- rbind(hs,ht)

actual_pred <- data.frame(cbind(Cox.Strength=h_test$Cox.Strength,
                                predicteds=pr_t))
cor.test(actual_pred$Cox.Strength,actual_pred$predicteds)
dcacpr <- data.frame(Корреляционная.зависимость = 0.7883)

mn_ac <- mean(apply(actual_pred, 1, min)/apply(actual_pred, 1, max))
mape <- mean(abs(actual_pred$predicteds - 
                   actual_pred$Cox.Strength)/actual_pred$Cox.Strength)

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
  geom_histogram(binwidth = 3, fill = 'white', col = 'black')
shapiro.test(L_mod$residuals)


par(mfrow=c(2,2))
plot(L_mod)


#Регрессионное дерево
library(rpart)     
library(rpart.plot)

reg_t <- rpart(Cox.Strength ~ ., data = h_train, method  = "anova")
rpart.plot(reg_t)
plotcp(reg_t)

reg_t1 <- rpart(Cox.Strength ~ ., data = h_train, 
                method="anova", control=list(cp = 0, xval = 10))
plotcp(reg_t1)
abline(v = 15, lty = "dashed")

reg_t$cptable #Вывод таблицы 

hyper_grid <- expand.grid(minsplit = seq(5, 20, 1), maxdepth = seq(8, 15, 1))

head(hyper_grid)
nrow(hyper_grid)

models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # получаем значения minsplit, maxdepth в строке i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # обучение модели и сохраните в списке
  models[[i]] <- rpart(Cox.Strength ~ ., data=h_train, method ="anova",
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

optimal_tree <- rpart(Cox.Strength ~ ., data=h_train, method="anova",
  control = list(minsplit = 9, maxdepth = 12, cp = 0.01) )
optimal_tree$frame

rpart.plot(optimal_tree)
plotcp(optimal_tree)

target <- h_train$Cox.Strength
tree_err <- mean((predict(optimal_tree)-target)^2)
null_err <- mean((mean(target)-target)^2)
tree_R2 <- 1.-tree_err/null_err
tree_R2

#создадим фрейм с даными прочности и предсказанными значениями трех моделей 
prognoz <- data.frame(Strength = h_test$Cox.Strength)
prognoz$lin.reg <- predict(L_mod,h_test)
prognoz$reg.tree <- predict(optimal_tree,h_test)

hs <- head(prognoz)
ht <- tail(prognoz)
sht <- rbind(hs,ht)

ht.tree <- rbind(head(prognoz[,c(1,3)]),tail(prognoz)[,c(1,3)])

cor.test(prognoz$Strength,prognoz$reg.tree)
dcacpr <- data.frame(Корреляционная.зависимость = 0.8265)

prognoztr <- data.frame(Strength=prognoz$Strength, reg.tree=prognoz$reg.tree)
mn_ac <- mean(apply(prognoztr, 1, min)/apply(prognoztr, 1, max))
mape <- mean(abs(prognoz$reg.tree - prognoz$Strength)/prognoz$Strength)




###Случайный лес
library(randomForest)

#создание модели случайного леса
modelrf <- randomForest(
  formula = Cox.Strength ~ .,
  data = h_train
)
#просмотр построенной модели
modelrf

#отображение тестового MSE по количеству деревьев
plot(modelrf)

#нахождение количества деревьев, которые дают наименьший тестовый MSE
which.min(modelrf$mse)
kt <- data.frame(Количество.деревьев = 447)

#нахождение RMSE для построения лучшей модели
sqrt(modelrf$mse[which.min(modelrf$mse)])
nr <- data.frame(Оптимальный.RMSE = 1.3583)

#график важности предикторов
varImpPlot(modelrf) 

#настройка модели, нахождение наименьшего Mtry
model_tuned <- tuneRF(
  x=h_train[,-9],
  y=h_train$Cox.Strength,
  ntreeTry=500,
  mtryStart=2,
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE #позволяет не показывать процесс в реальном времени
)

best_rf_model <- randomForest(formula=Cox.Strength~.,
                              mtry=6,
                              ntree=447,
                              data = h_train)

target <- h_train $Cox.Strength
rf_err <- mean((predict(best_rf_model)-target)^2)
null_err <- mean((mean(target)-target)^2)
rf_R2 <- 1.-rf_err/null_err
rf_R2

varImpPlot(best_rf_model) 
importance(best_rf_model)



#создадим фрейм с даными прочности и предсказанными значениями модели

prognoz$random.for <- predict(best_rf_model,h_test)

ht.tree <-  rbind(head(prognoz[,c(1,4)]),tail(prognoz)[,c(1,4)])
hs <- head(prognoz)
ht <- tail(prognoz)
sht <- rbind(hs,ht)

cor.test(prognoz$Strength,prognoz$random.for)
dcacpr <- data.frame(Корреляционная.зависимость = 0.9444)

prognozrf <- data.frame(Strength=prognoz$Strength, rf=prognoz$random.for)
mn_ac <- mean(apply(prognozrf, 1, min)/apply(prognozrf, 1, max))
mape <- mean(abs(prognoz$random.for - prognoz$Strength)/prognoz$Strength)


#Обратное преобразование Бокса-Кокса

prochnost.betona <- (prognoz*0.6+1)^(1/0.6)

#Сохранение таблици в Ворд формате 

hs <- head(prochnost.betona)
ht <- tail(prochnost.betona)
sht <- rbind(hs,ht)


#Полиномиальная регрессия

pol2 <- lm(Cox.Strength ~ poly(Cement,2)+poly(Age,2)+poly(Superplasticizer,2)+
             poly(Blast.Furnace.Slag,2)+poly(Fly.Ash,2)+poly(Water,2), h_train)
summary(pol2)

pol3 <- lm(Cox.Strength ~ poly(Cement,3)+ poly(Age,3)+poly(Superplasticizer,3)+
             poly(Blast.Furnace.Slag,3)+poly(Fly.Ash,3)+poly(Water,3), h_train)
summary(pol3)


pol4 <- lm(Cox.Strength ~ poly(Cement,4)+poly(Age,4)+poly(Superplasticizer,4)+
             poly(Blast.Furnace.Slag,4)+poly(Fly.Ash,4)+poly(Water,4), h_train)
summary(pol4)


pol5 <- lm(Cox.Strength ~ poly(Cement,5)+poly(Age,5)+poly(Superplasticizer,5)+
             poly(Blast.Furnace.Slag,5)+poly(Fly.Ash,5)+poly(Water,5), h_train)
summary(pol5)


pol6 <- lm(Cox.Strength ~ poly(Cement,6)+poly(Age,6)+poly(Superplasticizer,6)+
             poly(Blast.Furnace.Slag,6)+poly(Fly.Ash,6)+poly(Water,6), h_train)
summary(pol6)

pol7 <- lm(Cox.Strength ~ poly(Cement,7)+poly(Age,7)+poly(Superplasticizer,7)+
             poly(Blast.Furnace.Slag,7)+poly(Fly.Ash,7)+poly(Water,7), h_train)
summary(pol7)

pol8 <- lm(Cox.Strength ~ poly(Cement,8)+poly(Age,8)+poly(Superplasticizer,8)+
             poly(Blast.Furnace.Slag,8)+poly(Fly.Ash,8)+poly(Water,8), h_train)
summary(pol8)

pol9 <- lm(Cox.Strength ~ poly(Cement,9)+poly(Age,9)+poly(Superplasticizer,9)+
             poly(Blast.Furnace.Slag,9)+poly(Fly.Ash,9)+poly(Water,9), h_train)
summary(pol9)

pol10 <- lm(Cox.Strength ~ poly(Cement,10)+poly(Age,10)+
        poly(Superplasticizer,10)+poly(Blast.Furnace.Slag,10)+poly(Fly.Ash,10)+
        poly(Water,10), h_train)
summary(pol10)

aic_data_pol <- data.frame(pol2=AIC(pol2),pol3=AIC(pol3), pol4=AIC(pol4),
                           pol5=AIC(pol5),pol6=AIC(pol6),pol7=AIC(pol7),
                           pol8=AIC(pol8),pol9=AIC(pol9),pol10=AIC(pol10))
row.names(aic_data_pol)[1] <- "AIC"
                           
r_kvd_pol <- data.frame(
  pol2=summary(pol2)$adj.r.squared, pol3=summary(pol3)$adj.r.squared,
  pol4=summary(pol4)$adj.r.squared,pol5=summary(pol5)$adj.r.squared,
  pol6=summary(pol6)$adj.r.squared, pol7=summary(pol7)$adj.r.squared,
  pol8=summary(pol8)$adj.r.squared, pol9=summary(pol9)$adj.r.squared,
  pol10=summary(pol10)$adj.r.squared)
row.names(r_kvd_pol)[1] <- "R.squared"

pred_pol4 <- predict(pol4,h_test)
prognoz$poly_4 <- predict(pol4,h_test)
ht.pol4 <- rbind(head(prognoz[,c(1,6)]), tail(prognoz[,c(1,6)]))

cor.test(prognoz$Strength,prognoz$poly_4)
dcacpr <- data.frame(Корреляционная.зависимость = 0.915)

prognoz4 <- data.frame(Strength=prognoz$Strength, pol4=prognoz$poly_4)
mn_ac <- mean(apply(prognoz4, 1, min)/apply(prognoz4, 1, max))
mape <- mean(abs(prognoz$poly_4 - prognoz$Strength)/prognoz$Strength)

#Среднее абсолютное процентное отклоение
mape.pol4 <- data.frame(pol4=mean(abs(prognoz$poly_4 - 
                                        prognoz$Strength)/prognoz$Strength))
row.names(mape.pol4)[1] <- "MAPE"


#Средняя абсолютная ошибка прогноза
mm.pol4 <- data.frame(poly_4= mean(apply(prognoz4, 1, min)/
                                     apply(prognoz4, 1, max)))
row.names(mm.pol4)[1] <- "MinMax"

RMSE_pol4 <- data.frame(poly_4=RMSE_pol)
row.names(RMSE_pol4)[1] <- "RMSE"
RMSE$poly_4 <- RMSE_pol4$poly_4
RMSE <- RMSE[,-4]

RMSE_lm <- sqrt(mean((prognoz$Strength - prognoz$lin.reg)^2))
print(RMSE_lm)

RMSE_tree <- sqrt(mean((prognoz$Strength - prognoz$reg.tree)^2))
print(RMSE_tree)

RMSE_rf <- sqrt(mean((prognoz$Strength - prognoz$random.for)^2))
print(RMSE_rf)

RMSE_pol <- sqrt(mean((prognoz$Strength - prognoz$poly_4)^2))
print(RMSE_pol)

RMSE <- data.frame(lin.reg=RMSE_lm, reg.tree=RMSE_tree, 
                   random.for=RMSE_rf, poly_10=RMSE_pol)
row.names(RMSE)[1] <- "RMSE"


cor.tochn <- data.frame(lin.reg=0.7883, reg.tree=0.8265,
                        random.for=0.9444, poly_10=0.915)
row.names(cor.tochn)[1] <- "Корреляционная.зависимость"




#Среднее абсолютное процентное отклоение
mad_all <- data.frame(
  lin.reg=mean(abs(prognoz$lin.reg - prognoz$Strength)/prognoz$Strength),
  reg.tree=mean(abs(prognoz$reg.tree - prognoz$Strength)/prognoz$Strength), 
  random.for=mean(abs(prognoz$random.for - prognoz$Strength)/prognoz$Strength), 
  poly_4=mean(abs(prognoz$poly_4 - prognoz$Strength)/prognoz$Strength))
row.names(mad_all)[1] <- "MAD"

#Средняя абсолютная ошибка прогноза

prognozlin.reg <- data.frame(Strength=prognoz$Strength,lih.reg=prognoz$lin.reg)

mape_all <- data.frame(
  lin.reg=mean(apply(prognozlin.reg, 1, min)/apply(prognozlin.reg, 1, max)),
  reg.tree= mean(apply(prognoztr, 1, min)/apply(prognoztr, 1, max)), 
  random.for=mean(apply(prognozrf, 1, min)/apply(prognozrf, 1, max)), 
  poly_10=mean(apply(prognoz4, 1, min)/apply(prognoz4, 1, max)))
row.names(mape_all)[1] <- "MAPE"

mn_ac <- mean(apply(actual_pred, 1, min)/apply(actual_pred, 1, max))


##### Неиросети

install.packages("neuralnet")
library("neuralnet")
library("ROSE")

set.seed(80) # сеем семя

max <-  apply(h_train , 2 , max)
min <-  apply(h_train, 2 , min)

# скалируем данные

trainNN <-  as.data.frame(scale(h_train,
                             center = min,
                             scale = max - min))

testNN <- as.data.frame(scale(h_test,
                                   center = min,
                                   scale = max - min))

# Построенние базовой модели

NN <-  neuralnet(Cox.Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water
               + Superplasticizer + Coarse.Aggregate + Fine.Aggregate + Age, 
               data=trainNN, linear.output = F)
plot(NN)

NNprognoz <- data.frame(NN=neuralnet::compute(NN, testNN[,c(1:8)])$net.result)

#посмотрим на R2 и RMSE

r.sq.NN <- data.frame(NN_R2=postResample(NNprognoz,testNN[,9])[2])
row.names(r.sq.NN)[1] <- "R.squared"
r.sq.NN_all <- r.sq.NN

RMSE_NN <- data.frame(
  rmse.NN=sqrt(mean((testNN$Cox.Strength - NNprognoz$NN)^2)))
row.names(RMSE_NN)[1] <- "RMSE"
RMSE_NN_all <- RMSE_NN


#Построение модели с 5тью скрытымии узлами

NN5 = neuralnet(Cox.Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water
               + Superplasticizer + Coarse.Aggregate + Fine.Aggregate + Age, 
               data=trainNN, hidden = 5, linear.output = F)

plot(NN5)

NNprognoz$NN5 <- (neuralnet::compute(NN5, testNN[,c(1:8)]))$net.result


r.sq.NN5 <- data.frame(NN5_R2= postResample(NNprognoz$NN5,testNN[,9])[2])
row.names(r.sq.NN5)[1] <- "R.squared"
r.sq.NN_all$NN5_R2 <- postResample(NNprognoz$NN5,testNN[,9])[2]

RMSE_NN5 <- data.frame(
  rmse.NN5=sqrt(mean((testNN$Cox.Strength - NNprognoz$NN5)^2)))
row.names(RMSE_NN5)[1] <- "RMSE"
RMSE_NN_all$RMSE_NN5 <- RMSE_NN5$rmse.NN5


#Построение модели с 15тью скрытымии узлами

NN15 = neuralnet(Cox.Strength ~ ., 
                data=trainNN, hidden = c(5,10), linear.output = F)

plot(NN15)

NNprognoz$NN15 <- neuralnet::compute(NN15, testNN[,c(1:8)])$net.result


r.sq.NN15 <- data.frame(NN15=postResample(NNprognoz$NN15,testNN[,9])[2])
row.names(r.sq.NN15)[1] <- "R.squared"
r.sq.NN_all$NN15_R2 <- postResample(NNprognoz$NN15,testNN[,9])[2]

RMSE_NN15 <- data.frame(
  rmse.NN15=sqrt(mean((testNN$Cox.Strength - NNprognoz$NN15)^2)))
row.names(RMSE_NN15)[1] <- "RMSE"
RMSE_NN_all$RMSE_NN15 <- RMSE_NN15$rmse.NN15

#Построение с 8ью скрытыми узлами

NN8 = neuralnet(Cox.Strength ~ ., 
                 data=trainNN, hidden = 8, linear.output = F)

plot(NN8)

NNprognoz$NN8 <- neuralnet::compute(NN8, testNN[,c(1:8)])$net.result


r.sq.NN8 <- data.frame(NN8_R2=postResample(NNprognoz$NN8,testNN[,9])[2])
row.names(r.sq.NN8)[1] <- "R.squared"
r.sq.NN_all$NN8_R2 <- postResample(NNprognoz$NN8,testNN[,9])[2]

RMSE_NN8 <- data.frame(
  rmse.NN8=sqrt(mean((testNN$Cox.Strength - NNprognoz$NN8)^2)))
row.names(RMSE_NN8)[1] <- "RMSE"
RMSE_NN_all$RMSE_NN8 <- RMSE_NN8$rmse.NN8
RMSE$NN8 <- RMSE_NN8$rmse.NN8
RMSE_NN8 <- mean((prognoz$Strength - prognoz$NN8)^2)
RMSE$NN8 <- mean((prognoz$Strength - prognoz$NN8)^2)


#Обратная нормализация данных

prognozNN <- (NNprognoz$NN8 * 
                (max(h_train$Cox.Strength) - min(h_train$Cox.Strength)))
                   + min(h_train$Cox.Strength)
prognoz$NN8 <- prognozNN


#Проверка корреляции
cor.test(prochnost.betona$Strength,prochnost.betona$NN8)
cor.NN8 <- data.frame(Корреляционная.зависимость = 0.9413)
cor.tochn <- data.frame(lin.reg=0.7883, poly_4=0.915, 
                        reg.tree=0.8265, random.for=0.9444,  NN8 =0.9413)
row.names(cor.tochn)[1] <- "Корреляционная зависимость"

#Среднее абсолютное процентное отклоение
mape.NN8 <- data.frame(NN8=mean(abs(prognoz$NN8 - 
                                      prognoz$Strength)/prognoz$Strength))
row.names(mape.NN8)[1] <- "MAPE"
mape_all$NN8 <- mape.NN8$NN8

#Средняя абсолютная ошибка прогноза
prognozNN8 <- data.frame(Strength=prognoz$Strength, NN8=prognoz$NN8)
mm.NN8 <- data.frame(NN8= mean(apply(prognozNN8, 1, min)/
                                 apply(prognozNN8, 1, max)))
row.names(mm.NN8)[1] <- "MinMax"
mm_all$NN8 <- mm.NN8$NN8



prognoz <- prognoz[,-5]
prochnost.betona <- (prognoz*0.6+1)^(1/0.6)
ht.NN <- rbind(head(prochnost.betona[,c(1,6)]),tail(prochnost.betona[,c(1,6)]))
ht.proch <- rbind(head(prochnost.betona),tail(prochnost.betona))

# NN = neuralnet(Cox.Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water
#                + Superplasticizer + Coarse.Aggregate + Fine.Aggregate + Age, 
#                data=trainNN, hidden = 4, linear.output = F)
# 
# redict_testNN = neuralnet::compute(NN, testNN[,c(1:8)])
# 
# # чтобы заново нормализовать данные
# predict_testNN = (predict_testNN$net.result * (max(df$Cox.Strength) - min(df$Cox.Strength)))
#                   + min(df$Cox.Strength)
# 
# predict_testNN

MinMax_all <- data.frame(lin.reg=0.8414, poly_4=0.8963, reg.tree=0.8558, random.for=0.9159, NN8=0.8837)
row.names(MinMax_all)[1] <- "MinMax"

MAPEALL <- data.frame(lin.reg=0.2052, poly_4=0.1186, reg.tree=0.17, random.for=0.0969, NN8=0.1185)
row.names(MAPEALL)[1] <- "MAPE"

r.sq.all <- data.frame(lin.reg=0.58, poly_4=0.8703, reg.tree=0.7676, random.for=0.9035, NN8=0.8916)
row.names(r.sq.all)[1] <- "R.squared"

#########################################################################




