# let's find out the working directory
getwd()


# let's install the packages we don't have currently
install.packages('treemap')
install.packages("wordcloud")


# let's load the necessary library for this project
library('treemap')
library('lubridate')
library('plyr')
library('dplyr')
library('corrplot')
library('wordcloud')
library('ggplot2')
library('tidyverse')


# let's read the data into a dataframe called df
df<-read.csv("ted_main.csv", header=TRUE, sep=',')

# let's view the dataframe
View(df)

# let's find the dimention of this dataframe
dim(df)

#let's find the data structure of this dataframe
str(df)

#let's change the date from unix date stamp to actual date
df$film_date<-as_datetime(df$film_date, tz='UTC')
df$published_date<-as_datetime(df$published_date, tz='UTC')
View(df)

# let's find the summary statistics for only numeric columns
nCol<-c('comments','duration','languages', 'num_speaker','views')
df3<-df[nCol]
View(df3)
summary(df3)


#now let's find the pearson correlation of these numeric columns
cor(df3)

# now let's see the correlation plot of this numeric columns
# In purticular, let's determine the correlation between views and languages
# or does having more views mean that they are translated into more languages
corrplot(cor(df3), method='circle', type = 'lower', addCoef.col='red')


# let's chart a treechart of main_speaker, languages, and views togather
df2<-df%>%select('main_speaker','languages','views')%>%group_by('main_speaker')%>%arrange(desc(views))%>%head(10)
View(df2)
dim(df2)

field<-c('main_speaker','views')
df2$fieldChange <- do.call('paste',c(df2[field], sep="\nViews - "))
field<-c('fieldChange','languages')
df2$fieldChange<-do.call('paste',c(df2[field],sep="\nLanguages - "))
View(df2)

#?treemap
treemap(df2, index ='fieldChange', vSize='views', 
        title = 'Treemap of main_speaker based on languages and views')







# Word Cloud Display for Speaker Occupation
# let's display top occupation employing word cloud
temp<-table(df$speaker_occupation)
temp<-as.data.frame(temp)
temp<-temp[with(temp, order(-Freq)),]
View(temp)

set.seed(1234)
wordcloud(words = temp$Var1, freq = temp$Freq, min.freq = 1,
          max.words=30, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))







# Display Heatmap of num of tedtalk for each year and each month
# let's display number of talk for each year as heatmap
df4<-aggregate(df$num_speaker, by=list(df$film_date), sum)
colnames(df4)<-c('Date', 'num_talk')
View(df4)
df4['year']<-year(df4$Date)
df4['month']<-month(df4$Date)
df4['day']<-day(df4$Date)
df5<-aggregate(df4$num_talk, by=list(df4$month, df4$year), sum)
colnames(df5)<-c('month', 'year', 'num_talk')
View(df5)

#Assign color variables
col1 = "#d8e1cf" 
col2 = "#438484"
ggplot(df5, aes(month, year)) + geom_tile(aes(fill = num_talk),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Number of TEDTalks")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Heatmap of number of TEDtalks by year and month",
       x = "Year", y = "Month") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# let see the same information in heatmap using simple line chart
# draw line chart
ggplot( data = df4, aes( Date, num_talk)) + geom_line()
# let's see the same informatio for year
df6<-aggregate(df4[,2], by=list(df4$year), FUN=sum)
colnames(df6)<-c('year','Num_of_TEDtalks')
View(df6)
options(repr.plot.width = 8, repr.plot.height = 4)
ggplot(data=df6, aes(year, Num_of_TEDtalks))+
  geom_line(color='blue', size=2)+
  geom_point()+
  geom_text(aes(label=Num_of_TEDtalks), vjust=3, color='red', size=3)+
  ggtitle("Ted Talks on different years")




# let's find the top tag
tags<-df$tags
tags<-as.character(tags)
View(tags)


# let's display top ratings in bubble chart

