library(data.table)
library(dplyr)
library(reshape)
library(ggplot2)
library(reshape2)


setwd('D:\\DeveloperAPIopinionMining\\data\\newdaya\\TwitterPostsAPIVersionChangeData')


data <- fread('transformed_tweets_data.csv')

data%>%
  mutate(add_param = grepl('add*param',  text,ignore.case=TRUE),
         del_param = grepl('del*param',  text,ignore.case=TRUE),
         chn_param = grepl('add*param',  text,ignore.case=TRUE),
         add_method = grepl('add*method',text,ignore.case=TRUE),
         del_method = grepl('del*method',text,ignore.case=TRUE),
         chn_method = grepl('add*method',text,ignore.case=TRUE),
         )%>%
  group_by(ver)%>%
  summarise(
    add_param =   sum(add_param ),
      del_param = sum(del_param ),
      chn_param = sum(chn_param ),
      add_method =sum(add_method),
      del_method =sum(del_method),
      chn_method =sum(chn_method)
  )



twit_result = data%>%
  mutate(add_param = grepl('add*param',  text,ignore.case=TRUE),
         del_param = grepl('del*param',  text,ignore.case=TRUE),
         chn_param = grepl('add*param',  text,ignore.case=TRUE),
         add_method = grepl('add*method',text,ignore.case=TRUE),
         del_method = grepl('del*method',text,ignore.case=TRUE),
         chn_method = grepl('add*method',text,ignore.case=TRUE),
  )%>%
  group_by(ver)%>%
  summarise(
    add_param =   sum(add_param ),
    del_param = sum(del_param ),
    chn_param = sum(chn_param ),
    add_method =sum(add_method),
    del_method =sum(del_method),
    chn_method =sum(chn_method)
  )
twit_result_trn = melt(twit_result, id=c("ver"))


ggplot(data=twit_result_trn, aes(x=ver, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()












setwd('D:\\DeveloperAPIopinionMining\\data\\newdaya\\aspnet_transformed_data')


data <- fread('aspnet_transformed_data.csv')

head(data)

dim(data)


aspnet_result = data%>%
  mutate(add_param = grepl('add*param',  text,ignore.case=TRUE),
         del_param = grepl('del*param',  text,ignore.case=TRUE),
         chn_param = grepl('add*param',  text,ignore.case=TRUE),
         add_method = grepl('add*method',text,ignore.case=TRUE),
         del_method = grepl('del*method',text,ignore.case=TRUE),
         chn_method = grepl('add*method',text,ignore.case=TRUE),
  )%>%
  group_by(ver)%>%
  summarise(
    add_param =   sum(add_param ),
    del_param = sum(del_param ),
    chn_param = sum(chn_param ),
    add_method =sum(add_method),
    del_method =sum(del_method),
    chn_method =sum(chn_method)
  )
aspnet_result_trn = melt(aspnet_result, id=c("ver"))


ggplot(data=aspnet_result_trn, aes(x=ver, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

