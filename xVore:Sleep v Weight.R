#Install packages----
install.packages("tidyverse")
install.packages("reshape2")

#And, the libraries----
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)


#Finding fun data to play with----
data()

#LakeHuron
#USArrests
#rivers
#uspop
#starwars
#storms
#billboard

#Graph1----
?starwars
glimpse(starwars)
View(starwars)
unique(starwars$species)

?msleep
glimpse(msleep)
View(msleep)

colnames(msleep)

#Creating a graph1----
sleep <- msleep %>% 
  select(name,vore,awake,sleep_total,bodywt) %>% 
  drop_na(vore) %>% 
  mutate(wt_lbs = (bodywt * 2.2)) %>% 
  arrange(-wt_lbs)
  
View(sleep)

gsleep <- sleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(Lower = min(sleep_total),
            Average = mean(sleep_total),
            Upper = max(sleep_total),
            Weight = mean(wt_lbs))%>% 
  arrange(Average)
View(gsleep)

sleep %>% 
  ggplot(aes(sleep_total,wt_lbs))+
  geom_point(shape = 8)+
  facet_wrap(~vore, scales = 'free')+
  scale_x_continuous(breaks = seq(0,20, by = 5))
View(sleep)


#Average Weight & Sleep by Diet----
gsleep%>% 
  ggplot(aes(Average,Weight))+
  geom_point(size = 2.5,shape = 8, aes(color = vore,))+
  scale_x_continuous(breaks = 3)+
  labs(title = "Averages",
       x= "Avg Sleep by Diet Type",
       y= "Avg Weight by Diet Type")+
  theme_bw()

#Combining Data Sets on One Graph----
ggplot(sleep,aes(sleep_total,bodywt))+
  geom_jitter(size = 1.5, aes(color = vore))+
  geom_point(data=gsleep,aes(Average,Weight, color = vore),
             size = 3.5, shape = 13,
             )+
  facet_grid(vore~., scales = 'free')+
  labs(x = "Sleep",
       y= "Body Wt (kG)")+
  scale_color_manual(values = c("#4F9FCE", "#A5165D","#A5F37D","#D25512"), 
                    name = c("Avg Wt & Sleep"),
                    labels = c("carnivore","herbivore",
                               "insectivore","omnivore"))+
  theme_bw()

#Graph 2 Max v Min Sleep Patterns----
colnames(gsleep)

 MinMax <-gsleep %>% 
  ggplot(aes(vore,Average))+
  geom_point(aes(color = vore),
             size = 3.5,
             shape = 8)+
   geom_errorbar(aes(ymin = Lower, ymax = Upper, color = vore))+
   labs(title = "Difference in Sleep Totals",
        caption = "data from famous msleep",
        x= "-Vore",
        y= "Hours of Sleep")+
   scale_color_manual(values = c("#4F9FCE", "#A5165D","#A5F37D","#D25512"))+
   theme_bw()+
   theme(text = element_text('Courier'),
         plot.title = element_text(hjust = 0.5),
         plot.caption = element_text(face = 'italic',
                                     color = "#581845"),
         legend.position = 'none')
  MinMax
 

  
  
  

