date_df<-read.csv(file='/Users/qylikys/R/R_practice/Kursach/transactions.csv', 
                     header=T, sep='\t', row.names=NULL)
# Предобработка данных ----------------------------------------------------
date_df <- data.frame(date_df)
df <- data.frame(date_df)
df$Сумма.продажи <- as.numeric(gsub('NULL', '0', df$Сумма.продажи))
df$Сумма.без.скидки <- as.numeric(gsub('NULL', '0', df$Сумма.без.скидки))
df$Дата.транзакции <- as.Date(df$Дата.транзакции, format="%d/%m/%Y")
date_df$Дата.транзакции <- as.Date(date_df$Дата.транзакции, format="%d/%m/%Y")
View(date_df)
n <- nrow(df)
if(!("dplyr" %in% installed.packages())){
  install.packages("dplyr") 
}
library(dplyr)
tail(df)
prelast_month <- as.Date('01/09/2022', format = "%d/%m/%Y")
df <- filter(df, 
             Дата.транзакции < prelast_month &
             Кол.во - as.integer(Кол.во) == 0 &
             Сумма.без.скидки != 0)
View(df)
tail(df)
# Группировка по чекам ----------------------------------------------------

sale_sum <- aggregate(df, Сумма.продажи ~ Чек, FUN=sum) #Общая сумма продажи
marja_sum <- aggregate(df, Маржа ~ Чек, FUN=sum) #Общая маржа
count_pos <- aggregate(df, Кол.во ~ Чек, FUN=sum) #Кол-во позиций в чеке
unique_pos <- summarise(group_by(df, Чек), 
                        Уникальные.товары=length(unique(Товар))) #Уникальные
client <- summarise(group_by(df, Чек), Клиент=Клиент[1]) #Клиенt
date <- summarise(group_by(df, Чек), Дата.транзакции=Дата.транзакции[1])
df_check <- data.frame(date[2],
                       sale_sum[1],
                       client[2],
                       unique_pos[2],
                       count_pos[2],
                       sale_sum[2],
                       marja_sum[2])
df_check <- df_check[order(df_check$Дата.транзакции, 
                            decreasing = F), ] 
View(df_check)
# Группировка по клиентам -------------------------------------------------
mean_int <- function(arr){
  summ = 0
  n = length(arr)
  if (n != 1){
    for (i in (1:(n-1))){
      day_diff = as.numeric(gsub(' days', '', 
                            as.Date(arr[i+1], 
                                format = "%d/%m/%Y") - 
                            as.Date(arr[i],
                                format = "%d/%m/%Y")))
      summ = summ + day_diff
    }
  }
  else{
    summ = 0
  }
  return(signif(summ / n, digits = 4))
}

sale_sum <- aggregate(df_check, Маржа ~ Клиент, FUN=sum) #Принес
loss_sum <- summarise(group_by(df_check, Клиент), Унес=
                      (sum(Сумма.продажи) - sum(Маржа))) #Унес
count_check <- aggregate(df_check, Чек ~ Клиент, FUN=length) #Кол-во чеков
first_date <- summarise(group_by(df_check, Клиент), Посещение1=
                          Дата.транзакции[1]) #Первое посещение
diff_date <- summarise(group_by(df_check, Клиент), Разница= as.numeric(
                         gsub(' days', '',
                              Дата.транзакции[length(Дата.транзакции)] - 
                              Дата.транзакции[1]))) #Разница посл. и 1 дня 
passed_time <- summarise(group_by(df_check, Клиент), Прошло= as.numeric(
                         gsub(' days', '',
                              prelast_month - 
                              Дата.транзакции[length(Дата.транзакции)]))) #Прошло дней
date_int <- summarise(group_by(df_check, Клиент), Средний.интервал=
                        mean_int(Дата.транзакции))
df_client <- data.frame(sale_sum[1],
                       count_check[2],
                       loss_sum[2],
                       sale_sum[2],
                       first_date[2],
                       date_int[2],
                       diff_date[2],
                       passed_time[2]
                       )
View(df_client)

# пока так ----------------------------------------------------------------
last_month <- as.Date('01/10/2022', format = "%d/%m/%Y")
df_train <- filter(date_df, 
                   Дата.транзакции >=
                     prelast_month &
                   Дата.транзакции <
                     last_month &
                   Кол.во - as.integer(Кол.во) == 0 &
                   Сумма.без.скидки != 0)
View(df_train)

new_month <- data.frame(Клиент = unique(df_train$Клиент), Присутствие = 1)


df_client_train <- dplyr::left_join(df_client, new_month, by = "Клиент")
df_client_train[is.na(df_client_train)] <- 0
View(df_client_train)
model <-  glm(Присутствие ~ ., 
              data=df_client_train, family = binomial(link="logit"))
View(model)
summary(model)
# основная выборка + предпоследний месяц ----------------------------------

new_df <- data.frame(date_df)
new_df$Сумма.продажи <- as.numeric(gsub('NULL', '0', new_df$Сумма.продажи))
new_df$Сумма.без.скидки <- as.numeric(gsub('NULL', '0', new_df$Сумма.без.скидки))
new_df$Дата.транзакции <- as.Date(new_df$Дата.транзакции, format="%d/%m/%Y")
new_df <- filter(new_df, 
             Дата.транзакции < last_month &
               Кол.во - as.integer(Кол.во) == 0 &
               Сумма.без.скидки != 0)
#Группируем по чекам
sale_sum <- aggregate(new_df, Сумма.продажи ~ Чек, FUN=sum) #Общая сумма прод
marja_sum <- aggregate(new_df, Маржа ~ Чек, FUN=sum) #Общая маржа
count_pos <- aggregate(new_df, Кол.во ~ Чек, FUN=sum) #Кол-во позиций в чеке
unique_pos <- summarise(group_by(new_df, Чек), 
                        Уникальные.товары=length(unique(Товар))) #Уникальные 
client <- summarise(group_by(new_df, Чек), Клиент=Клиент[1]) #Клиенt
date <- summarise(group_by(new_df, Чек), Дата.транзакции=Дата.транзакции[1])
new_df_check <- data.frame(date[2],
                       sale_sum[1],
                       client[2],
                       unique_pos[2],
                       count_pos[2],
                       sale_sum[2],
                       marja_sum[2])
new_df_check <- new_df_check[order(new_df_check$Дата.транзакции, 
                           decreasing = F), ] 
View(new_df_check)
#Группируем по клиентам
sale_sum <- aggregate(new_df_check, Маржа ~ Клиент, FUN=sum) #Принес
loss_sum <- summarise(group_by(new_df_check, Клиент), Унес=
                        (sum(Сумма.продажи) - sum(Маржа))) #Унес
count_check <- aggregate(new_df_check, Чек ~ Клиент, FUN=length) #Кол-во чеков
first_date <- summarise(group_by(new_df_check, Клиент), Посещение1=
                          Дата.транзакции[1]) #Первое посещение
diff_date <- summarise(group_by(new_df_check, Клиент), Разница= as.numeric(
  gsub(' days', '',
       Дата.транзакции[length(Дата.транзакции)] - 
         Дата.транзакции[1]))) #Разница посл. и 1 дня 
passed_time <- summarise(group_by(new_df_check, Клиент), Прошло= as.numeric(
  gsub(' days', '',
       prelast_month - 
         Дата.транзакции[length(Дата.транзакции)]))) #Прошло дней
date_int <- summarise(group_by(new_df_check, Клиент), Средний.интервал=
                        mean_int(Дата.транзакции))
new_df_client <- data.frame(sale_sum[1],
                        count_check[2],
                        loss_sum[2],
                        sale_sum[2],
                        first_date[2],
                        date_int[2],
                        diff_date[2],
                        passed_time[2]
)
View(new_df_client)

# Клиенты последнего месяца -----------------------------------------------
new_df <- data.frame(date_df)
new_df$Сумма.продажи <- as.numeric(gsub('NULL', '0', new_df$Сумма.продажи))
new_df$Сумма.без.скидки <- as.numeric(gsub('NULL', '0', 
                                           new_df$Сумма.без.скидки))
new_df$Дата.транзакции <- as.Date(new_df$Дата.транзакции, format="%d/%m/%Y")
new_df <- filter(new_df, 
                 Дата.транзакции >= last_month &
                   Кол.во - as.integer(Кол.во) == 0 &
                   Сумма.без.скидки != 0)
#Группируем по чекам
sale_sum <- aggregate(new_df, Сумма.продажи ~ Чек, FUN=sum) #Общая сумма продаж
marja_sum <- aggregate(new_df, Маржа ~ Чек, FUN=sum) #Общая маржа
count_pos <- aggregate(new_df, Кол.во ~ Чек, FUN=sum) #Кол-во позиций в чеке
unique_pos <- summarise(group_by(new_df, Чек), 
                        Уникальные.товары=length(unique(Товар))) #Уникальные п
client <- summarise(group_by(new_df, Чек), Клиент=Клиент[1]) #Клиенt
date <- summarise(group_by(new_df, Чек), Дата.транзакции=Дата.транзакции[1])
new_df_check <- data.frame(date[2],
                           sale_sum[1],
                           client[2],
                           unique_pos[2],
                           count_pos[2],
                           sale_sum[2],
                           marja_sum[2])
new_df_check <- new_df_check[order(new_df_check$Дата.транзакции, 
                                   decreasing = F), ] 
View(new_df_check)
#Группируем по клиентам
sale_sum <- aggregate(new_df_check, Маржа ~ Клиент, FUN=sum) #Принес
loss_sum <- summarise(group_by(new_df_check, Клиент), Унес=
                        (sum(Сумма.продажи) - sum(Маржа))) #Унес
count_check <- aggregate(new_df_check, Чек ~ Клиент, FUN=length) #Кол-во чеков
first_date <- summarise(group_by(new_df_check, Клиент), Посещение1=
                          Дата.транзакции[1]) #Первое посещение
diff_date <- summarise(group_by(new_df_check, Клиент), Разница= as.numeric(
  gsub(' days', '',
       Дата.транзакции[length(Дата.транзакции)] - 
         Дата.транзакции[1]))) #Разница посл. и 1 дня 
passed_time <- summarise(group_by(new_df_check, Клиент), Прошло= as.numeric(
  gsub(' days', '',
         as.Date('01/11/2022', format="%d/%m/%Y") - 
         Дата.транзакции[length(Дата.транзакции)]))) #Прошло дней
date_int <- summarise(group_by(new_df_check, Клиент), Средний.интервал=
                        mean_int(Дата.транзакции))
new_df_client <- data.frame(sale_sum[1],
                            count_check[2],
                            loss_sum[2],
                            sale_sum[2],
                            first_date[2],
                            date_int[2],
                            diff_date[2],
                            passed_time[2]
)
View(new_df_client)

# Прогнозируем на последний месяц -----------------------------------------

predictResult <- predict(model, newdata = new_df_client, type="response")
typeof(predictResult)
predictResult
difference <- dplyr::left_join(new_df_client, predictResult, by = "Клиент")
difference[is.na(difference)] <- 0
View(difference)
