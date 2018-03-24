
mental_health <- read_csv("Working_Sheet_mental-heath-in-tech-2016.csv")
mental_health

barplot(mental_health$AGE, na.rm=T)


mental_health$AGE

library(tm)

install.packages("tm")

mental_health$AGE


mental_health$GENDER[mental_health$GENDER == 'male' | mental_health$GENDER == 'Malr'| mental_health$GENDER == 'mail'|  mental_health$GENDER == 'Male.'| mental_health$GENDER == 'Male' | mental_health$GENDER == 'm' | mental_health$GENDER == 'MALE' | mental_health$GENDER == 'Male ' | mental_health$GENDER == 'man' | mental_health$GENDER == 'Sex is male' | mental_health$GENDER == 'M|' | mental_health$GENDER == 'Dude'] <- "M"
mental_health$GENDER[mental_health$GENDER == 'F' | mental_health$GENDER == 'Female'| mental_health$GENDER == 'female'|  mental_health$GENDER == 'Female assigned at birth'| mental_health$GENDER == 'Female or Multi-Gender Femme' | mental_health$GENDER == 'I identify as female.' | mental_health$GENDER == 'woman' | mental_health$GENDER == 'fem' | mental_health$GENDER == 'f' | mental_health$GENDER == ' Female' | mental_health$GENDER =='Female (props for making this a freeform field, though)' | mental_health$GENDER == 'female-bodied; no feelings about gender'] <- "M"

unique(mental_health$GENDER)
barplot(mental_health$AGE)

####################################################################################################################################################################

#### INSTALL REQUIRED PACKAGES ####
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
library(maps)
library(mapdata)

##### READ DATA ######
dat <- read.csv("mental-heath-in-tech-2016_20161114.csv")
data
dim(data)
head(names(data))
library('plyr')

df <- rename(dat, 
                job_loc = What.country.do.you.work.in.,
                us_job_loc = What.US.state.or.territory.do.you.live.in.,
                us_state = What.US.state.or.territory.do.you.work.in.,
                age = What.is.your.age.,
                gender = What.is.your.gender.,
                dis_sup =  Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..,
                dis_cow =  Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.)
df %>% select(job_loc, us_job_loc, us_state, age, gender, dis_sup, dis_cow) %>% str()


##### PLOTTING AGE ######

hist(data2$age)

mean(data2$age)

ggplot(data2, aes(x = age), bins=2) + geom_histogram(binwidth = 50)

ggplot(data2, aes(x = dis_cow),colour='red') + geom_histogram(binwidth = 50,stat = "count")
ggplot(data2, aes(x = dis_sup),colour='red') + geom_histogram(binwidth = 50,stat = "count")


?ggplot
##### CREATING A MAP #######
usa <- map_data("usa") 

usa <- map_data("usa") # we already did this, but we can do it again
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) + guides(fill=FALSE)


states <- map_data("state")
states
dim(states)

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)


###### Age and mental health discussion ANALYSIS ########

# Extracting total number of people in older age (>35)
older_population = filter(data2, age > '35') 
length(older_population$age)                    # Count of ppl > 35 years old

# Extracting total number of people in younger age (<=35)

younger_population = filter(data2, age <= '35') # Count of ppl <= 35 years old
length(younger_population$age)

# Total number of people confortable discussing mental health with their coworkers and their supervisors
PlotBars <- function(Var,Name){qplot(factor(Var),
        data=data2,geom="bar",fill=factor(Var),main=Name)}

PlotBars(data2$dis_sup,"Would you feel comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor")
PlotBars(data2$dis_cow,"Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.")

# Boxplot depicting realtionship between age(old and young) and discussion with coworkers

ggplot(older_population, aes(x=dis_cow, y=age, fill=age)) +
  geom_boxplot()                                               

ggplot(younger_population, aes(x=dis_cow, y=age, fill=age)) +
  geom_boxplot()

ggplot(older_population, aes(x=dis_sup, y=age, fill=age)) +
  geom_boxplot()                                               

ggplot(younger_population, aes(x=dis_sup, y=age, fill=age)) +
  geom_boxplot()

# 
PlotBars <- function(Var,Name){qplot(factor(Var),
                                     data=younger_population,geom="bar",fill=factor(Var),main=Name)}

PlotBars(younger_population$dis_cow,"Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.")
PlotBars(younger_population$dis_sup,"Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.")

PlotBars <- function(Var,Name){qplot(factor(Var),
                                     data=older_population,geom="bar",fill=factor(Var),main=Name)}

PlotBars(older_population$dis_cow,"Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.")
PlotBars(older_population$dis_sup,"Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.")


###############What are the other salient aspects of the data (e.g. geospatial factors, text content, etc.)###########################




