#Libraries

library(dplyr)
library(rvest)
library(readxl)
library(rtweet)
 
# Changed note again
df_branches <- read_xlsx('data/df_branchs.xlsx')
txts <- readxl::read_xlsx('data/texts.xlsx')


# Scraping and Preparing Data
page_usd <- read_html('https://azn.day.az/az/rates/usd')
page_eur <- read_html('https://azn.day.az/az/rates/eur')
page_rub <- read_html('https://azn.day.az/az/rates/rub')

df_usd <- page_usd %>%
  html_nodes('#table-rates') %>%
  html_table() %>%
  .[[1]] %>%
  tibble() %>%
  mutate(Bank_Buys =as.numeric(.[[2]]),       #Storing Bank_Buys and Buys_Sells
         Bank_Sells=as.numeric(.[[3]])) %>%   #Values as numeric
  select(1,7,8) %>% 
  rename(Bank_Name=1)


df_eur <- page_eur %>%
  html_nodes('#table-rates') %>%
  html_table() %>%
  .[[1]] %>%
  tibble() %>%
  mutate(Bank_Buys =as.numeric(.[[2]]),      #Storing Bank_Buys and Buys_Sells
         Bank_Sells=as.numeric(.[[3]])) %>%  #Values as numeric
  select(1,7,8) %>% 
  rename(Bank_Name=1)


df_rub <- page_rub %>%
  html_nodes('#table-rates') %>%
  html_table() %>%
  .[[1]] %>%
  tibble() %>%
  mutate(Bank_Buys =as.numeric(.[[2]]),       #Storing Bank_Buys and Buys_Sells
         Bank_Sells=as.numeric(.[[3]])) %>%   #Values as numeric
  select(1,7,8) %>% 
  rename(Bank_Name=1)



#Dropping Ireelevant Banks

v_reg_usd <- c('AFB.','Melli','Avrasiya','Premium','Nax..van',
               'ASB','Pak.stan','Az.rp','Xalq','Eliko')
v_reg_eur <- c('AFB.','Melli','Avrasiya','Premium','Nax..van',
               'ASB','Az.rp','Xalq','Eliko')
v_reg_rub <- c('AFB.','Melli','Avrasiya','Premium','Nax..van',
               'ASB','Az.rp','Eliko')

vec_drp_usd <- Filter(length,sapply(v_reg_usd,function(x) grep(x, df_usd$Bank_Name)))
vec_drp_eur <- Filter(length,sapply(v_reg_eur,function(x) grep(x, df_eur$Bank_Name)))
vec_drp_rub <- Filter(length,sapply(v_reg_rub,function(x) grep(x, df_rub$Bank_Name)))


df_usd_final <- df_usd %>% slice(-unlist(vec_drp_usd)) %>% 
  left_join(df_branches,by=c('Bank_Name'='Bank_Name_Org'))

df_eur_final <- df_eur %>% slice(-unlist(vec_drp_eur)) %>% 
  left_join(df_branches,by=c('Bank_Name'='Bank_Name_Org'))

df_rub_final <- df_rub %>% slice(-unlist(vec_drp_rub)) %>% 
  left_join(df_branches,by=c('Bank_Name'='Bank_Name_Org'))


# Buy Values
usd_val_buy <- df_usd_final %>%
  arrange(-Bank_Buys,-Branch_Num) %>%
  select(Bank_Name,Bank_Name_Az,Bank_Buys) %>%
  slice(1)

eur_val_buy <-df_eur_final %>%
  arrange(-Bank_Buys,-Branch_Num) %>%
  select(Bank_Name,Bank_Name_Az,Bank_Buys) %>%
  slice(1)

rub_val_buy <-df_rub_final %>%
  arrange(-Bank_Buys,-Branch_Num) %>%
  select(Bank_Name,Bank_Name_Az,Bank_Buys) %>%
  slice(1)


# Sell Values
usd_val_sell <- df_usd_final %>%
  arrange(Bank_Sells,-Branch_Num) %>%
  select(Bank_Name,Bank_Name_Az,Bank_Sells) %>%
  slice(1)

eur_val_sell <-df_eur_final %>%
  arrange(Bank_Sells,-Branch_Num) %>%
  select(Bank_Name,Bank_Name_Az,Bank_Sells) %>%
  slice(1)

rub_val_sell <-df_rub_final %>%
  arrange(Bank_Sells,-Branch_Num) %>%
  select(Bank_Name,Bank_Name_Az,Bank_Sells) %>%
  slice(1)


# Twitter

appname <- Sys.getenv('APPNAME_TWT')
key <- Sys.getenv('KEY_TWT')
secret <- Sys.getenv('SECRET_TWT')
access_token <- Sys.getenv('ACCESS_TOKEN_TWT')
access_secret <- Sys.getenv('ACCESS_SECRET_TWT')


twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

 
 
f_currency <- function(x){ #Standardize currency decimals
  ifelse(nchar(x)==6,x,paste0(x,
                              paste(rep('0',6-nchar(x)),collapse = '')))
}


bold_twt <- function(dt){ # Encoding characters to bold
  x <- as.character(dt)
  full_char<- substring(x, 1:nchar(x), 1:nchar(x))

  chars = c('0','1','2','3','4','5','6','7','8','9','-')
  bold_chars = c("\U0001d7ec","\U0001d7ed","\U0001d7ee",
                 "\U0001d7ef","\U0001d7f0","\U0001d7f1",
                 "\U0001d7f2","\U0001d7f3","\U0001d7f4",
                 "\U0001d7f5","-"  )

  final_chr <- c()
  for(i in full_char){
    if(i %in% chars){
      ind <- which(chars %in% i)
      final_chr <- c(final_chr,bold_chars[ind])
    }
  }
  final_char <- paste(final_chr,collapse = '')
  return(final_char)

}


post_text <- paste0('\n\U0001F4C5 ',bold_twt(Sys.Date()),' ',
                    '\n\n\U0001F4C8',txts[[1]][1], '\n\U0001F1FA\U0001F1F8USD: ', usd_val_buy$Bank_Name_Az,
                    ' - ',f_currency(usd_val_buy$Bank_Buys),'\n\U0001F1EA\U0001F1FAEUR: ',eur_val_buy$Bank_Name_Az,
                    ' - ',f_currency(eur_val_buy$Bank_Buys), '\n\U0001F1F7\U0001F1FARUB: ',rub_val_buy$Bank_Name_Az,
                    ' - ',f_currency(rub_val_buy$Bank_Buys),
                    '\n\n\U0001F4C9',txts[[1]][2],'\n\U0001F1FA\U0001F1F8USD: ',
                    usd_val_sell$Bank_Name_Az,' - ',f_currency(usd_val_sell$Bank_Sells),
                    '\n\U0001F1EA\U0001F1FAEUR: ', eur_val_sell$Bank_Name_Az,' - ',f_currency(eur_val_sell$Bank_Sells),
                    '\n\U0001F1F7\U0001F1FARUB: ', rub_val_sell$Bank_Name_Az,' - ',f_currency(rub_val_sell$Bank_Sells),
                    '\n')





post_tweet(post_text)
