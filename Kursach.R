date_df<-read.csv(file='/Users/qylikys/R/R_practice/Kursach/transactions.csv', 
                     header=T, sep='\t', row.names=NULL)
# Предобработка данных ----------------------------------------------------
df <- data.frame(date_df)
df$Сумма.продажи <- as.numeric(gsub('NULL', '0', df$Сумма.продажи))
df$Сумма.без.скидки <- as.numeric(gsub('NULL', '0', df$Сумма.без.скидки))
View(df)
n <- nrow(df)
if(!("dplyr" %in% installed.packages())){
  install.packages("dplyr") 
}
library(dplyr)
tail(df)
df <- filter(df, 
             Дата.транзакции < '01/09/2022' &
               Кол.во - as.integer(Кол.во) == 0 &
               Сумма.без.скидки != 0)
View(df)

# Группировка по чекам ----------------------------------------------------
sale_sum <- aggregate(df, Сумма.продажи ~ Чек, FUN=sum) #Общая сумма продажи
marja_sum <- aggregate(df, Маржа ~ Чек, FUN=sum) #Общая маржа
count_pos <- aggregate(df, Кол.во ~ Чек, FUN=sum) #Кол-во позиций в чеке
unique_pos <- summarise(group_by(df, Чек), 
                        'Уникальные.товары'=length(unique(Товар))) #Уникальные позиции
client <- summarise(group_by(df, Чек), 'Клиент'=unique(Клиент)) #Клиент
df_check <- data.frame(sale_sum[1],
                       client[2],
                       unique_pos[2],
                       count_pos[2],
                       sale_sum[2],
                       marja_sum[2])
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
  return(round(summ / n))
}

sale_sum <- aggregate(df_check, Маржа ~ Клиент, FUN=sum) #Принес
loss_sum <- summarise(group_by(df_check, Клиент), 'Унес'=
                      (sum(Сумма.продажи) - sum(Маржа))) #Унес
count_check <- aggregate(df_check, Чек ~ Клиент, FUN=length) #Кол-во чеков
first_date <- summarise(group_by(df, Клиент), 'Посещение1'=
                          Дата.транзакции[1]) #Первое посещение
diff_date <- summarise(group_by(df, Клиент), "Разница"=
                       (as.Date(Дата.транзакции[length(Дата.транзакции)], 
                               format = "%d/%m/%Y") - 
                       as.Date(Дата.транзакции[1], 
                                 format = "%d/%m/%Y"))) #Разница посл. и 1 дня 
passed_time <- summarise(group_by(df, Клиент), "Прошло"=
                           (as.Date('01/09/2022', 
                                    format = "%d/%m/%Y") - 
                              as.Date(Дата.транзакции[length(Дата.транзакции)], 
                                      format = "%d/%m/%Y"))) #Прошло дней
date_int <- summarise(group_by(df, Клиент), 'Средний интервал'=
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
