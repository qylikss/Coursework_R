date_df <-  read.csv(file='C:/Users/PC15/Downloads/transactions.csv', header=T, sep='\t', row.names=NULL)

# Предобработка данных ----------------------------------------------------
df <- data.frame(date_df)
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
df_check <- df %>%
  group_by(Чек) %>%
  summarise(summ <- sum(as.numeric(Сумма.продажи)),
            marja <- sum(Маржа),
            uniqe <- sum(Кол.во) - count(Кол.во),
            count_check <- count(Чек))
sale_sum <- aggregate(df, as.numeric(Сумма.продажи) ~ Чек, FUN=sum)
View(sale_sum)
marja_sum <- aggregate(df, Маржа ~ Чек, FUN=sum)
View(marja_sum)
sale_sum <- aggregate(df, as.numeric(Сумма.продажи) ~ Чек, FUN=sum)
sale_sum <- summarise(group_by(df, Чек), uniqe <- sum(Кол.во) - count(Кол.во))
check_count <- summarise(group_by(df, Чек), count_check=count(Чек))
View(check_count)