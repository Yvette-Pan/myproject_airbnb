### DATA EXPLORATION ###

# load data
users = read.csv('/Users/yuranpan/desktop/BA_Class/Hmk/Week7/dataset/users.csv', header = T)

dim(users)
head(users)
str(users)
summary(users)
# gender has unknown and other
#other columns for date_account_created, date_first_booking, language, affiliate_channel/provider,
# first_affilate_tracked, first_device_type, first_browser, country

table(is.na(users))
colSums(is.na(users))
# 87990 NA in age column

##### DATA PRE-PROCESSING ######
## 1. Country desitination
summary(users$country_destination)
prop.table(table(users$country_destination))

library(ggplot2)
ggplot(users, aes(country_destination))+ geom_bar(fill='purple')

# create a new field that groups all contries into US, outside US, and NDF
users$country = users$country_destination
users$country = factor(ifelse(users$country== 'US', 'US',
                              ifelse(users$country== 'NDF', 'NDF','Outside US')))
unique(users$country)
prop.table(table(users$country))
ggplot(users, aes(country)) + geom_bar(fill = 'purple')

# 1.get users sign up method and sign up app
library(dplyr)
summary(users$signup_method)
prop.table(table(users$signup_method))
ggplot(users,aes(users$signup_method)) + geom_bar(fill = 'red')

summary(users$signup_app)
prop.table(table(users$signup_app))
ggplot(users,aes(users$signup_app)) + geom_bar(fill = 'red')

# 2. pre-processing gender
prop.table(table(users$gender))
class(users$gender)
# convert unknown to na, otherwise it will effect visualization result. R will automatically wipe out
users$gender = factor(ifelse(users$gender == '-unknown-', '', as.character(users$gender)))
summary(users$gender)
unique(users$gender)
# fill in gender missing value based on country distribution 

#2.1 country = NDF

users%>% count(country, gender) %>% group_by(gender)

users %>% count(country, gender) %>% filter(gender!= '' & country == 'NDF') %>% ungroup %>% 
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
# select gender is na with NDF 
df_ndf = users[users$gender=='' & users$country =='NDF',]
set.seed(1)
df_ndf_f = users %>% filter(gender == ''& country =='NDF') %>% sample_n(round(0.54*66670,0))
female = df_ndf_f$id
male = setdiff(df_ndf$id,df_ndf_f$id)

users$gender = ifelse(users$id %in% female, 'FEMALE', 
                      ifelse(users$id %in% male, 'MALE',as.character(users$gender)))
users %>% filter(country == 'NDF') %>% count(gender)

#2.2 country = US

users %>% count(country, gender) %>% filter(gender!= '' & country == 'US') %>% ungroup %>% 
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
df_ndf = users[users$gender=='' & users$country =='US',]
set.seed(1)
df_ndf_f = users %>% filter(gender == ''& country =='US') %>% sample_n(round(0.54*20109,0))
female = df_ndf_f$id
male = setdiff(df_ndf$id,df_ndf_f$id)

users$gender = ifelse(users$id %in% female, 'FEMALE', 
                      ifelse(users$id %in% male, 'MALE',as.character(users$gender)))
users %>% filter(country == 'US') %>% count(gender)


#2.3 country outside US
users %>% count(country, gender) %>% filter(gender!= '' & country == 'Outside US') %>% ungroup %>% 
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
df_ndf = users[users$gender=='' & users$country =='Outside US',]
set.seed(1)
df_ndf_f = users %>% filter(gender == ''& country =='Outside US') %>% sample_n(round(0.53*8909,0))
female = df_ndf_f$id
male = setdiff(df_ndf$id,df_ndf_f$id)

users$gender = ifelse(users$id %in% female, 'FEMALE', 
                      ifelse(users$id %in% male, 'MALE',as.character(users$gender)))
users %>% filter(country == 'Outside US') %>% count(gender)


# 3. create a new languate include only english and non-english
summary(users$language)
users$lang = users$language
users$lang = factor(ifelse(users$lang =='en','en','non-en'))
unique(users$lang)
ggplot(users, aes(users$lang)) + geom_bar(fill = 'pink')
head(users)

# 4. create a new affiliate_prov including direct, google, criaglist,bing, facebook, and others
summary(users$affiliate_provider)
ggplot(users, aes(users$affiliate_provider)) + geom_bar(fill = 'blue')
users$affilate_prov = users$affiliate_provider
users$affilate_prov = factor(ifelse(users$affilate_prov =='direct', 'direct',
                                    ifelse(users$affilate_prov =='google', 'google',
                                           ifelse(users$affilate_prov =='craigslist','craigslist',
                                                  ifelse(users$affilate_prov =='bing','bing',
                                                         ifelse(users$affilate_prov == 'facebook','facebook','others'))))))
unique(users$affilate_prov)
summary(users$affilate_prov)

ggplot(users, aes(users$affilate_prov)) + geom_bar(fill = 'blue')

#5. pre-processing age
summary(users$age)
# max age of 2014 and min age of 1 is unrealistic. assuming 2014 is birth year
users$age = ifelse((users$age >= 1924)&(users$age <= 2014),2017-users$age, users$age)
summary(users$age)
users$age = ifelse((users$age >=10)&(users$age <= 100),users$age, NA)
summary(users$age)

# age distribution
users$age_dist = users$age
users$age_dist = factor(ifelse((users$age_dist>=15)&(users$age_dist<26),'15-25',
                               ifelse((users$age_dist>=26)&(users$age_dist<37),'26-36',
                                      ifelse((users$age_dist>=37)&(users$age_dist<48),'37-47',  
                                             ifelse((users$age_dist>=48)&(users$age_dist<59),'48-58',
                                                    ifelse((users$age_dist>=59)&(users$age_dist<70),'59-69',
                                                           ifelse((users$age_dist>=70)&(users$age_dist<81),'70-80',
                                                                  ifelse((users$age_dist>=81)&(users$age_dist<92),'81-91',
                                                                         ifelse((users$age_dist>=92),'92+','other')))))))))
unique(users$age_dist)
head(users)

users %>% count(age_dist, country) %>% filter(age_dist != '' & country == 'NDF') %>% ungroup %>%
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
# for those whose destination is NDF, majority with age in 26-36 range
users %>% count(age_dist, country) %>% filter(age_dist != '' & country == 'US') %>% ungroup %>%
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
# same with destination being US
users %>% count(age_dist, country) %>% filter(age_dist != '' & country == 'Outside US') %>% ungroup %>%
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
# Same as above

# 6. affilate channel
summary(users$affiliate_channel)
ggplot(users,aes(users$affiliate_channel)) + geom_bar(fill = 'orange')

# 7. first browser
summary(users$first_browser)
ggplot(users, aes(users$first_browser))+ geom_bar(fill = 'blue')
# to see only top 6
count_first_browser = users %>% count(first_browser)
count_first_browser %>% select(first_browser, n) %>% arrange(desc(n))
top_six_browser = count_first_browser %>% select(first_browser,n) %>%
  arrange(desc(n)) %>% head(n = 6)
top_six_browser
ggplot(top_six_browser,aes(x = first_browser, y = n)) + geom_bar(stat = 'identity', fill = 'forest green')


# 8. first device type
summary(users$first_device_type)
ggplot(users, aes(users$first_device_type)) + geom_bar(fill = 'purple')
# see the rank of the device type
count_device_type = users %>% count(first_device_type) %>% select(first_device_type, n) %>%
  arrange(desc(n))
# see percentage of each device type with country desitination of NDF
users %>% count(country, first_device_type) %>% filter(country =='NDF') %>%ungroup %>%
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
# see percentage of each device type with country desitination of US
users %>% count(country, first_device_type) %>% filter(country =='US') %>%ungroup %>%
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
# see percentage of each device type with country desitination of Outside US
users %>% count(country, first_device_type) %>% filter(country =='Outside US') %>%ungroup %>%
  group_by(country) %>% mutate(pct = round(n/sum(n),2))
# mac desktop and windows desktop are mostly used device 

# 9. breakout data created date by year, month, date
library(stringr)
dac = as.data.frame(str_split_fixed(users$date_account_created,'/',3))
dac
users['dac_year'] = dac[,3]
users['dac_month']= dac[,1]
users['dac_day']= dac[,2]
head(users)

# 10. break out data first booking date
dfb =  as.data.frame(str_split_fixed(users$date_first_booking,'/',3))
dfb
users['dfb_year'] = dfb[,3]
users['dfb_month']=dfb[,1]
users['dfb_day']= dfb[,2]
head(users)

# 11.1 calculate days between account created vs first booking
require(lubridate)
dt_created = mdy(users$date_account_created)
dt_first_booking = mdy(users$date_first_booking)
users$dt_diff = ifelse(dt_first_booking>=dt_created,dt_first_booking - dt_created,'')
class(users$dt_diff)
users$dt_diff = as.numeric((users$dt_diff))
summary(users$dt_diff)
names(users)
users %>% count(dt_diff)

hist(users$dt_diff)
# 11.2 group date difference into 
users$dt_diff_range = ifelse(users$dt_diff ==0, 'Sameday_booking',
                             ifelse((users$dt_diff > 0)&(users$dt_diff<=7),'2-7 days',
                             ifelse((users$dt_diff > 7)&(users$dt_diff <= 30),'8-30 days',
                             ifelse((users$dt_diff > 30)&(users$dt_diff <=90),'31-90 days','>3 months'))))
                               

table(users$dt_diff_range)


# 12 create conversation rate 
users$booked = ifelse(users$country == 'NDF',0,1)


# write CSV in R
write.csv(users, file = '/Users/yuranpan/desktop/BA_Class/Hmk/Week7/dataset/users_clean.csv')

##############################
##### Data Visualization ######
##############################

# ggplot 1: country vs gender
library(ggplot2)
library(dplyr)
ggplot(users, aes(users$gender)) + geom_bar(fill = 'blue')
users %>% count(country, gender)
g1 = users %>% count(country, gender) %>% ungroup %>% group_by(country) %>%
  mutate(pct = round(n/sum(n),2))
g1
ggplot(g1, aes(x = country, y = pct)) + geom_bar(stat = 'identity', aes(fill = country))+
  facet_grid(.~gender)+
  geom_text(aes(label = round(pct,2),y = pct + 0.03)) +
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  scale_fill_brewer(palette = 'Set2')

# insights: there's no significant difference bewteen country destination for both male and female
# female is more likly to book a trip than male, around 6-8 pct

# pie chart: for country vs gender
summary(factor(users$gender))
df_gender <- data.frame(users$gender)
lbl = paste(round(prop.table(table(users$gender)*100),2), '%', sep= '')
lbl
slices = c(114624,98545,282)
lbls = c('FEMALE','MALE','OTHER')
pct = round(slices/sum(slices)*100,2)
lbls = paste(lbls,pct)
lbls = paste(lbls,'%',sep='')
pie(slices,lable = lbls,col = rainbow(length(lbls)),main ='Country vs Gender')


# ggplot2: country vs age
summary(users$age_dist)
users %>% count(country, age_dist)
g2 = users %>%count(country, age_dist) %>% filter(age_dist != '') %>% ungroup %>% group_by(country) %>%
  mutate(pct = round(n/sum(n),2))
g2
ggplot(g2, aes(x = country, y = pct)) + geom_bar(stat = 'identity', aes(fill = country))+
  facet_grid(.~age_dist)+
  geom_text(aes(label = round(pct,2),y = pct + 0.03)) +
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  scale_fill_brewer()
# insights: the age distribution between 26-36 is most likely to travel
# after 36, the older people get, the less likely they will travel

# ggplot3: country cs sign up method
summary(users$signup_method)
users %>% count(country, signup_method)
g3 = users %>%count(country, signup_method) %>% filter(signup_method != '') %>% ungroup %>% group_by(country) %>%
  mutate(pct = round(n/sum(n),2))
g3
ggplot(g3, aes(x = country, y = pct)) + geom_bar(stat = 'identity', aes(fill = country))+
  facet_grid(.~signup_method)+
  geom_text(aes(label = round(pct,2),y = pct + 0.03)) +
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  scale_fill_brewer(palette = 'Set3')
# insights: most people will sigh up through home page
# there's no sigficant differece in desitnations across sign up method

#ggplot4: country vs language
summary(users$lang)
users %>% count(country, lang)
g4 = users %>%count(country, lang) %>% filter(lang!= '') %>% ungroup %>% group_by(country) %>%
  mutate(pct = round(n/sum(n),2))
g4
ggplot(g4, aes(x = country, y = pct)) + geom_bar(stat = 'identity', aes(fill = country))+
  facet_grid(.~lang)+
  geom_text(aes(label = round(pct,2),y = pct + 0.03)) +
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  scale_fill_brewer()
# insights: sinificant difference in language , english counts the most.

# gglot5: country vs affiliate provider
summary(users$affilate_prov)
users %>% count(country, affilate_prov)
g5 = users %>%count(country, affilate_prov) %>% filter(affilate_prov!= '') %>% ungroup %>% group_by(country) %>%
  mutate(pct = round(n/sum(n),2))
g5
ggplot(g5, aes(x = country, y = affilate_prov)) + geom_bar(stat = 'identity', aes(fill = country))+
  facet_grid(.~affilate_prov)+
  geom_text(aes(label = round(pct,2),y = pct + 0.03)) +
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  scale_fill_brewer(palette = 'Set2')+
  theme_dark()
# google, facebook, craiglist, and bing take up the most 

#ggplot6: country vs signup app
summary(users$signup_app)
users %>% count(country, signup_app)
g6 = users %>%count(country, signup_app) %>% filter(signup_app!= '') %>% ungroup %>% group_by(country) %>%
  mutate(pct = round(n/sum(n),2))
g6
ggplot(g6, aes(x = country, y = signup_app)) + geom_bar(stat = 'identity', aes(fill = country))+
  facet_grid(.~signup_app)+
  geom_text(aes(label = round(pct,2),y = pct + 3.4)) +
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  scale_fill_brewer()+ theme_dark()

# mostly from web, may need to improve mobile features

#ggplot7: country vs  first device type
summary(users$first_device_type)
users %>% count(country, first_device_type)
g7 = users %>%count(country, first_device_type) %>% filter(first_device_type!= ''& country =='NDF') %>% ungroup %>% group_by(country) %>%
  mutate(pct = round(n/sum(n),2))
g7
ggplot(g7, aes(x = country, y = pct)) + geom_bar(stat = 'identity', aes(fill = first_device_type))+
  ylim(0,1)+
  facet_grid(.~first_device_type)+
  geom_text(aes(label = round(pct,2),y = pct + 9)) +
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  scale_fill_brewer()

# insight: most people use mac and windows desktop when country is NDF

#ggplot8:  country vs first browswer
summary(users$first_browser)
g8 = users %>% count(country, first_browser) %>% filter(country =='NDF') %>%
  ungroup %>% group_by(country) %>% mutate(pct = round(n/sum(n),2))
g8


#########################################
###########  Random Forest ##############
#########################################


# import csv
data = read.csv('/Users/yuranpan/desktop/BA_Class/Hmk/Week7/dataset/users_clean.csv', header = T)
head(data)
class(data)
da = data.frame(data)
class(da)

library(dplyr)

#filter out irrelevant columns
data1 = select(da, gender : dfb_day)
del_col = c('age_dist','dfb_year','dfb_month','dfb_day','affiliate_provider','country_destination')
data1 = data1[,-which(names(data1)%in% del_col)]
names(data1)

# check missing and NA values
colSums(is.na(data1))
attach(data1)
str(data1)

# convert data type for each column
data1$signup_flow = factor(data1$signup_flow)
data1$dac_year = factor(data1$dac_year)
data1$dac_month = factor(data1$dac_month)
data1$dac_day = factor(data1$dac_day)
colSums(is.na(data1))

summary(data1$age)

data1$age = ifelse(is.na(data1$age),36.59, data1$age)
summary(data1$age)

colSums(is.na(data1))

# split data into training and testing
set.seed(123)
samp = sample(1:nrow(data1), 0.7 * nrow(data1))
train = data1[samp,]
test = data1[-samp,]

### random forest
library(randomForest)
set.seed(100)
head(train)
rf = randomForest(country~., data = train, ntree = 100, importance = T, na.action = na.omit)
rf 
#oob error is 37.97%

# importance plot
importance(rf)
varImpPlot(rf)

# check the model with the test
# confusion matrix

pred = predict(rf, newdata = test)
pred
pred_prob = predict(rf, newdata = train, type = 'response')
pred_prob

# misclassificatoin model
table = table(pred, test$country)
table  # also known as confusion matrix
class(table)

# accuracy
sum(diag(table))/sum(table)
# overall accuracy is 62.77%

# calculate misclassification rate
test.err <- 1 - sum(diag(table))/sum(table)
test.err
# missclassification rate is 37.23%
