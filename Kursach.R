df <-  read.csv(file='/Users/qylikys/R/R_practice/Kursach/transactions.csv', header=T, sep='\t', row.names=NULL)
df <- data.frame(df)
View(df)
nrow(df)
df[8]
if(!("dplyr" %in% installed.packages())){
  install.packages("dplyr") 
}
library(dplyr)
#Тут ошибка скорее всего не понимаю почему
df %>%
  group_by(Магазин) %>% # группируем по месяцам
  summarise('Кол-во покупок' = length(Сумма.продажи))
df %>%
  group_by(Магазин) %>% # группируем по месяцам
  summarise("Средний чек" = mean(as.numeric(
    gsub('NULL', '0', df$Сумма.продажи)
    , na.rm = T)))
df %>%
  group_by(Магазин) %>% # группируем по месяцам
  summarise("Медиана чека" = median(as.numeric(
    gsub('NULL', '0', df$Сумма.продажи)
    , na.rm = T)))
df %>%
  group_by(Магазин) %>% # группируем по месяцам
  summarise("Средний маржа" = mean(as.numeric(
    gsub('NULL', '0', df$Маржа)
    , na.rm = T)))

#Выбираем рандомного клиента из таблицы
df_rand_cl <- filter(df, Клиент == df$Клиент[sample(1:length(df$Клиент), 1)])
#В какие магазины ходил и сколько раз
counter <- nrow(df_rand_cl)
count_purchase <- df_rand_cl %>%
  group_by(Магазин) %>% # 
  summarise(count_purchase = length(Сумма.продажи))
count_purchase
#Сколько тратил деняк
mean_check <- df_rand_cl %>%
  group_by(Магазин) %>% # 
  summarise(mean_check = mean(as.numeric(
    gsub('NULL', '0', df_rand_cl$Сумма.продажи)
    , na.rm = T)))
mean_check
median_check <- df_rand_cl %>%
  group_by(Магазин) %>% 
  summarise(median_check = median(as.numeric(
    gsub('NULL', '0', df_rand_cl$Сумма.продажи)
    , na.rm = T)))
median_check
#Мат ожидание прибыли
ver_prib <- count(filter(df_rand_cl, Маржа >= 0)) / counter
ver_ubit <- 1 - ver_prib
E <- 0
for (i in range(1, counter)){
  if (df_rand_cl$Маржа[i] >= 0){
    E <- E + df_rand_cl$Маржа[i] * ver_prib
  }
  else{
    E <- E + df_rand_cl$Маржа[i] * ver_ubit
  }
}
shop_loss <- df_rand_cl %>%
  group_by(Магазин) %>% # 
  summarise(shop_loss = median(as.numeric(gsub('NULL', '0', 
                                               df_rand_cl$Сумма.продажи)) 
                               - as.numeric(gsub('NULL', '0', 
                                                 df_rand_cl$Сумма.без.скидки))), 
            na.rm = T)
client_df <- data_frame(Client=df_rand_cl$Клиент[1], 
                    Count_purchase=count_purchase[2],
                    Mean_check=mean_check[2],
                    Median_check=median_check[2],
                    Pribil=E,
                    Loss=shop_loss[2])
View(client_df)
#Строю график по марже
df$month <- format( as.Date (df$Дата.транзакции, format=" %d/%m/%Y "),"%m")
df$year <- format( as.Date (df$Дата.транзакции, format=" %d/%m/%Y "),"%Y")
new_df <- aggregate(Маржа ~ month + year,       
          df,
          FUN = sum)
day <- c(rep('01', nrow(new_df)))
new_df$date <-paste(day, new_df$month, new_df$year, sep=".")
View(new_df)
date <- new_df$date
marja <- new_df$Маржа
date <- strptime(new_df$date, '%d.%m.%Y')
marja <- log(as.numeric(new_df$Маржа))
plot(date, marja, ylab='Маржа, ln', xlab='Дата', type = "l")
#График по утратам магазина
df$Сумма.продажи <- gsub('NULL', '0', df$Сумма.продажи)
df$Сумма.без.скидки <- gsub('NULL', '0', df$Сумма.без.скидки)
df$loss <- as.numeric(df$Сумма.без.скидки) - as.numeric(df$Сумма.продажи)
View(new_df_count)
new_df_count <- aggregate(loss ~ month + year,       
                    df,
                    FUN = sum)
loss <- new_df_count$loss
plot(date, loss, ylab='Потери магазина, руб', xlab='Дата', type = "l")
