################################################################################
#Гусева Дарья э402 ВКР "Гендерное неравенство и рынок искусства в России"

################################################################################
#                               Скачиваем данные

#install.packages("readxl")
library(readxl)
DATA <- read_excel("C:/Users/Daria/Documents/VKR.xlsx")
View(DATA)

#добавляем переменную логарифма, возраста и периода признания
DATA$LN_HIGH_estimate_USD_2017 <- log(DATA$HIGH_estimate_USD_2017)
DATA$LN_LOW_estimate_USD_2017 <- log(DATA$LOW_estimate_USD_2017)
DATA$LN_Market_price_USD_2017 <- log(DATA$Market_price_USD_2017)
DATA$AGE <- (DATA$Year_of_creation - DATA$Painter_date_of_birth)
DATA$acknowledgement <- (DATA$Year_of_sale - DATA$Year_of_creation)

################################################################################
#                               Описание данных

summary(DATA[c('LOW_estimate_USD_2017', 'HIGH_estimate_USD_2017', 
               'Market_price_USD_2017', 'Painter_date_of_birth',
               'Year_of_creation', 'Size_cm2', 'Year_of_sale'
               )])


DATA_W <- dplyr::filter(DATA, Sex == 1)

summary(DATA_W[c('LOW_estimate_USD_2017', 'HIGH_estimate_USD_2017', 
               'Market_price_USD_2017', 'Painter_date_of_birth',
               'Year_of_creation', 'Size_cm2', 'Year_of_sale'
)])


DATA_M <- dplyr::filter(DATA, Sex == 0)

summary(DATA_M[c('LOW_estimate_USD_2017', 'HIGH_estimate_USD_2017', 
                 'Market_price_USD_2017', 'Painter_date_of_birth',
                 'Year_of_creation', 'Size_cm2', 'Year_of_sale'
)])

#видим разницу в средних, есть основания для гипотез, проверяем 

#                           Редактируем выборку

#избавимся от выбросов по размеру
#install.packages("ggplot2")
library(ggplot2)

ggplot ((DATA), aes(x = Size_Hight_cm)) + 
  geom_histogram(bins = 100, na.rm = FALSE, color = "black", fill = 'navyblue')+
  theme_minimal() +
  labs(x = 'высота, см', y='количество работ')
DATA <- dplyr::filter(DATA, Size_Hight_cm > 2)
DATA <- dplyr::filter(DATA, Size_Hight_cm < 250)

#избавимся от выбросов для оценок 

library(ggplot2)
ggplot ((DATA), aes(x = LN_HIGH_estimate_USD_2017)) + 
  geom_histogram(bins = 500, na.rm = FALSE, color = "black", fill = 'navyblue')+ 
  theme_minimal() + labs(x = 'логарифм оценки ауккциона', y='Количество работ')

DATA <- dplyr::filter(DATA, LN_HIGH_estimate_USD_2017<15)

#                        Строим графики для анализа

#гистограмма для анализа оценок
DATAHist <- dplyr::filter(DATA, HIGH_estimate_USD_2017>1)
DATAHist <- dplyr::filter(DATA, HIGH_estimate_USD_2017<10000)
ggplot ((DATAHist), aes(x = HIGH_estimate_USD_2017)) + 
  geom_histogram(bins = 50, na.rm = FALSE, color = "black", fill = 'navyblue') + 
  theme_minimal() + labs(x = 'верхняя оценка ауккциона, долл. США', 
                         y = 'Количество работ')

DATAHist_W <- dplyr::filter(DATA_W, HIGH_estimate_USD_2017<30000)
DATAHist_M <- dplyr::filter(DATA_M, HIGH_estimate_USD_2017<30000)

#гистограмма для оценок, м и ж
ggplot()+geom_histogram(bins = 50, data=DATAHist_W, 
                        aes(x=HIGH_estimate_USD_2017, fill="женщины"),
                        alpha = 0.3) +
  geom_histogram(bins = 50, data=DATAHist_M, 
                 aes(x=HIGH_estimate_USD_2017, fill = "мужчины"), 
                 alpha = 0.3) + 
  theme_minimal() + labs(x = 'верхняя оценка ауккциона, долл. США', 
                         y = 'Количество работ')

#гистограмма для распределения по периодам написания работы, м и ж
ggplot()+geom_histogram(bins = 50, data=DATAHist_W, 
                        aes(x=Year_of_creation, fill="женщины"),
                        alpha = 0.3) +
  geom_histogram(bins = 50, data=DATAHist_M, 
                 aes(x=Year_of_creation, fill = "мужчины"), 
                 alpha = 0.3) +
  theme_minimal() + labs(x = 'Год создания работы', 
                         y = 'Количество работ')

#гистограмма для распределения по ценам продажи, м и ж
DATAHist_W <- dplyr::filter(DATA_W, Market_price_USD_2017<30000)
DATAHist_M <- dplyr::filter(DATA_M, Market_price_USD_2017<30000)
ggplot()+geom_histogram(bins = 50, data=DATAHist_W, 
                        aes(x=Market_price_USD_2017, fill="женщины"),
                        alpha = 0.5) +
  geom_histogram(bins = 50, data=DATAHist_M, 
                 aes(x=Market_price_USD_2017, fill = "мужчины"), 
                 alpha = 0.3) +
  theme_minimal() + labs(x = 'Цена продажи, долл. США', 
                         y = 'Количество работ')

################################################################################
#                       Проверка контрольных переменных

CONTROLS <- subset(DATA, select = c(Sex,
                                      Painting,
                                      Sculpture,
                                      Photos,
                                      Others,
                                      AU_Russia_and_Eastern_Europe,
                                      AU_America_and_Canada,
                                      Prestigious_auction_house,
                                      Size_cm2,
                                      Painter_date_of_birth,
                                      Year_of_sale,
                                      Year_of_creation
                                      ))

#install.packages("corrplot")
library('corrplot')
corrplot(cor(CONTROLS))
cor(CONTROLS)
corrplot.mixed
#Близкая к 0 корреляция между всеми переменными, кроме года создания работы и 
#года рождения художника, что логично

#Добавим переменную возраста создания работы и периода признания 
CONTROLS <- subset(DATA, select = c(Sex,
                                    Painting,
                                    Sculpture,
                                    Photos,
                                    Others,
                                    AU_Russia_and_Eastern_Europe,
                                    AU_America_and_Canada,
                                    Prestigious_auction_house,
                                    Size_cm2,
                                    acknowledgement,
                                    AGE
))

#View(CONTROLS)

corrplot(cor(CONTROLS))
cor(CONTROLS)
corrplot.mixed

#Возраст не коррелирует с годом(периодом) написания работы, 
#поэтому контролируем на него, вместо года рождения художника

#то же самое, только с регионами для продаж
CONTROLS <- subset(DATA, select = c(Sex,
                                    Painting,
                                    Sculpture,
                                    Photos,
                                    Others,
                                    M_Russia_and_Eastern_Europe,
                                    Prestigious_auction_house,
                                    Size_cm2,
                                    acknowledgement,
                                    AGE
))

corrplot(cor(CONTROLS))
cor(CONTROLS)
corrplot.mixed
#Близкая к 0 корреляция между всеми переменными


################################################################################
#                                  МОДЕЛИ

#для табличек
#install.packages("stargazer")
library('stargazer')

#строим регрессию
#install.packages ("lmtest")
library('lmtest')

### ### ### ###              Для верхней оценки                  ### ### ### ###

modelAU_1 <- lm(HIGH_estimate_USD_2017 ~ Sex +   
                  Painting +
                  Sculpture +
                  Photos +
                  Others +
                  AU_Russia_and_Eastern_Europe +
                  AU_America_and_Canada +
                  Size_cm2 +
                  Prestigious_auction_house +
                  acknowledgement +
                  AGE,
                  data=DATA
)
summary(modelAU_1)

#Проводим тест Бокса-Кокса, чтобы понять, нужен ли нам логарифм

#install.packages ("MASS")
library('MASS')
boxcox(modelAU_1)

#Тест показал, что с высокой вероятностью лямбда близка к 0,
#строим логарифмически-линейную модель

modelAU_2 <- lm(LN_HIGH_estimate_USD_2017 ~ Sex +
                  Painting +
                  Sculpture + 
                  Photos +
                  Others +
                  AU_Russia_and_Eastern_Europe +
                  AU_America_and_Canada +
                  Size_cm2 +
                  Prestigious_auction_house +
                  acknowledgement +
                  AGE,
                data=DATA
)

summary(modelAU_2)

#сравниваем модели с логарифмом и без
AIC(modelAU_1, modelAU_2)
#полулогарифмическая модель лучше

modelAU_2.0 <- lm(LN_HIGH_estimate_USD_2017 ~ Sex +
                  Painting +
                  Sculpture + 
                  Photos +
                  Others +
                  AU_Russia_and_Eastern_Europe +
                  AU_America_and_Canada +
                  Size_cm2 +
                  AGE,
                data=DATA
)

summary(modelAU_2.0)
stargazer(modelAU_2, modelAU_2.0,
          title="Сравнение короткой и длинной регрессий", type="text", 
          column.labels=c("длинная", "короткая"), 
          df=FALSE, digits=2)
#Исправленный R2 больше в длинной модели
#проводим тест на короткую и длинную:
waldtest(modelAU_2.0, modelAU_2)
#отвергаем гипотезу, что коэфф. при добавленных переменных равны нулю, а значит
#контролируем на созданную переменную - период признания, и премиум от 
#принадлежности к престижному аукционному дому

### ### ### ###              Для нижней оценки                   ### ### ### ###

modelAU_3 <- lm(LN_LOW_estimate_USD_2017 ~ Sex +
                  Painting +
                  Sculpture + 
                  Photos +
                  Others +
                  AU_Russia_and_Eastern_Europe +
                  AU_America_and_Canada +
                  Size_cm2 +
                  Prestigious_auction_house +
                  acknowledgement +
                  AGE,
                data=DATA
)
summary(modelAU_3)


#Сводная табличка для оценок
stargazer(modelAU_2, modelAU_3,
          title="АУКЦИОННЫЕ ОЦЕНКИ", type="text", 
          column.labels=c("Модель верхняя оценка", "Модель нижняя оценка"), 
          df=FALSE, digits=2)

################################################################################
#                              Проводим тесты

#дисп не очень большие, но все же проверим на мультиколлинеарность:
#install.packages("car")
library('car')
vif(modelAU_2)
vif(modelAU_3)
#мультиколлинеарности нет, VIF < 5 для всех независимых переменных

#Однако, мы еще не можем делать вывод о значимости всех коэфф.

#тест Бреуша-Пагана на отсутствие гетероскедастичности
bptest(modelAU_2)
bptest(modelAU_3)
#В данных присутствует гетероскедастичность и нам необходимо использовать
#робастные ст. ош.

#Используем функциюю для робастных (к гетероскедастичности) стандартных ошибок:
#install.packages("sandwich")
library('sandwich')

cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(modelAU_2,
          title="АУКЦИОННЫЕ ОЦЕНКИ", type="text", 
          column.labels=c("Модель верхняя оценка"), 
          se=list(cse(modelAU_2)),  
          df=FALSE, digits=4)

stargazer(modelAU_2, modelAU_3,
          title="АУКЦИОННЫЕ ОЦЕНКИ", type="text", 
          column.labels=c("Модель верхняя оценка", "Модель нижняя оценка"), 
          se=list(cse(modelAU_2), cse(modelAU_3)),
          df=FALSE, digits=2)
#переменная пола остается значимой (как и другие переменные)

################################################################################
#                               МЭТЧИНГ

#Шаг первый: сопостовляем художников и проверяем баланс кованиатов
#install.packages("MatchIt")
library('MatchIt')
matched_obj <- matchit( Sex ~    
                       Painting +
                       Sculpture +
                       Graphic_arts +
                       Photos +
                       Others +
                       AU_Russia_and_Eastern_Europe +
                       AU_America_and_Canada +
                       AU_Europe +
                       Size_cm2 +
                       acknowledgement +
                       AGE,
                     data = DATA, method = "nearest", distance ="glm",
                     ratio = 1,
                     replace = FALSE)
matched_obj
summary(matched_obj)

#Баланса достигли по всем переменным, Std. Mean Diff.близка к нулю

#собираем сопоставленные данные
DATA_Matched <- match.data(matched_obj)

#Шаг второй: проводим регрессию
#Веса замэтченных художников = 1

### ### ### ###              Для верхней оценки                  ### ### ### ###
modelAU_Matching_1 <- lm(LN_HIGH_estimate_USD_2017 ~ Sex, 
                       data = DATA_Matched, weights = weights)


### ### ### ###              Для нижней оценки                   ### ### ### ###
modelAU_Matching_2 <- lm(LN_LOW_estimate_USD_2017 ~ Sex, 
                         data = DATA_Matched, weights = weights)

#Проводим тест, используя кластерные робастные ст. ош.
coeftest(modelAU_Matching_1, vcov. = vcovCL, cluster = ~subclass)
coeftest(modelAU_Matching_2, vcov. = vcovCL, cluster = ~subclass)

stargazer(modelAU_Matching_1, modelAU_Matching_2,
          title="АУКЦИОННЫЕ ОЦЕНКИ МЭТЧИНГ", type="text", 
          column.labels=c("Модель верхняя оценка Мэтчд", 
                          "Модель нижняя оценка Мэтчд"), 
          se=list(cse(modelAU_Matching_1), cse(modelAU_Matching_2)),
          df=FALSE, digits=2)


### ### ### ###              Для рыночных оценок                 ### ### ### ###

#избавимся от выбросов для рын цен 

library(ggplot2)
ggplot ((DATA), aes(x = LN_Market_price_USD_2017)) + 
  geom_histogram(bins = 500, na.rm = FALSE, color = "black", fill = 'navyblue')+ 
  theme_minimal() + labs(x = 'логарифм цены', y='количество работ')

DATA_MARKET <- dplyr::filter(DATA, LN_Market_price_USD_2017<12.5)

#строим регрессию
modelM <- lm(LN_Market_price_USD_2017 ~ Sex +   
                  Painting +
                  Sculpture +
                  Photos +
                  Others +
                  M_Russia_and_Eastern_Europe +
                  Size_cm2 +Size_cm2 +
                  Prestigious_auction_house +
                  acknowledgement +
                  AGE,
                data=DATA
)

stargazer(modelM,
          title="Рынок", type="text", 
          column.labels=c("Модель продажи"), 
          se=list(cse(modelM)),
          df=FALSE, digits=2)

#переменная пола не значима для продаж

stargazer(modelAU_2, modelAU_3, modelM,
          title="Оценки и Продажи", type="text", 
          column.labels=c("Модель верхняя оценка", 
                          "Модель нижняя оценка",
                          "Модель продажи"), 
          se=list(cse(modelAU_2), cse(modelAU_3), 
                  cse(modelM)),
          df=FALSE, digits=2, decimal.mark = ",", out = "regression_1.html")

#                                 Мэтчинг
#Шаг первый: сопостовляем художников и проверяем баланс ковариатов

matched_obj_2 <- matchit( Sex ~    
                          Painting +
                          Sculpture +
                          Graphic_arts +
                          Photos +
                          Others +
                          M_Russia_and_Eastern_Europe +
                          Size_cm2 +
                          Year_of_sale +
                          Year_of_creation +
                          AGE,
                        data = DATA, method = "nearest", distance ="glm",
                        ratio = 1,
                        replace = FALSE)
summary(matched_obj_2)

#Баланса достигли по всем переменным, Std. Mean Diff.близка к нулю

#собираем сопоставленные данные
DATA_Matched_Market <- match.data(matched_obj_2)

#Шаг второй: проводим регрессию
#Веса замэтченных художников = 1

modelM_Matching <- lm(LN_Market_price_USD_2017 ~ Sex, 
                         data = DATA_Matched, weights = weights)

#Проводим тест, используя кластерные робастные ст. ош.
coeftest(modelM_Matching, vcov. = vcovCL, cluster = ~subclass)

stargazer(modelAU_Matching_1, modelAU_Matching_2, modelM_Matching,
          title="Оценки и Продажи Мэтчинг", type="text", 
          column.labels=c("Модель верхняя оценка Мэтчд", 
                          "Модель нижняя оценка Мэтчд",
                          "Модель продажи Мэтчд"), 
          se=list(cse(modelAU_Matching_1), cse(modelAU_Matching_2), 
                  cse(modelM_Matching)),
          df=FALSE, digits=2)

stargazer(modelAU_Matching_1, modelAU_Matching_2, modelM_Matching,
          title="Оценки и Продажи Мэтчинг", 
          column.labels=c("Модель верхняя оценка Мэтчд", 
                          "Модель нижняя оценка Мэтчд",
                          "Модель продажи Мэтчд"), 
          se=list(cse(modelAU_Matching_1), cse(modelAU_Matching_2), 
                  cse(modelM_Matching)),
          df=FALSE, digits=2, decimal.mark = ",", out = "regression_2.html")

#при мэтчинге переменная пола значима только для оценок аукционов



################################################################################
################################################################################
#                              Анализ данных опроса

#                    проверяем баланс ковариатов для опроса

#рассчитываем стандартное отклонение (Standard Deviation (sd))

DATA_B <- read_excel("C:/Users/Daria/Documents/VKR Balance.xlsx")

library('tableone')
table1 <- CreateTableOne(vars=c('sex', 'Eighteenplus', 'Twentyfiveplus', 'thirtyplus', 'fortyfiveplus', 'bachelor', 'student',
                                'doesntwork','works','hasexperience','rich','uppermiddleclass','middleclass','poor'), 
                         strata = 'treatment', data=DATA_B, test=TRUE)
table1

#проверяем баланс, проводим Хи-квадрат тест (сводит к одной статистике)
#install.packages("RItools")
library('RItools')
xBalance(treatment~  
           sex + 
           Eighteenplus +
           Twentyfiveplus +
           thirtyplus +
           student +
           works +
           hasexperience +
           rich +
           uppermiddleclass +
           middleclass, data=DATA_B, report = 'chisquare.test')
#баланс есть, статистика <25

#                       опрос часть 2

DATA_S <- read_excel("C:/Users/Daria/Documents/VKR survey.xlsx")
View(DATA_S)
library('lmtest')

DATA_S_W <- dplyr::filter(DATA_S, artistsex == 1)

model_2.1 <- lm(estimate ~  
                treatment,
              data = DATA_S_W)
summary(model_2.1)

DATA_S_T <- dplyr::filter(DATA_S, treatment == 1)

model_2.2 <- lm(estimate ~  
                  artistsex,
              data = DATA_S_T)
summary(model_2.2)

DATA_S_UT <- dplyr::filter(DATA_S, treatment == 0)

model_2.3 <- lm(estimate ~  
                  artistsex,
                data = DATA_S_UT)
summary(model_2.3)

stargazer(model_2.1, model_2.2, model_2.3,
          title="Опрос часть 2", 
          column.labels=c("влияние знаний о поле на оценку", 
                          "разница в оценках работ женщин в Т-группе",
                          "разница в оценках работ женщин в контроле"),
          df=FALSE, digits=2, decimal.mark = ",", out = "regression_3.html")

#                       опрос часть 3

model_3 <- lm(agree ~  
                  treatment,
                data = DATA_B)
summary(model_3)
