library(tidyverse)
library(tidytext)
library(patchwork)
library(cowplot)
library(ggimage)
library(showtext)
font_add_google(name = "Arvo", family = "Arvo")
showtext_auto()
animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')
#plot 1: tree
# tree leaves: of tree: step one, filter animals rescued from braches using str word extraction from "final_description" and then 
#insert ggimage of animals as well using geom_point 
#right_tree:
branch <- animal_rescues %>% as_tibble() %>% unnest_tokens(word, final_description) %>% filter(word %in% c("branch","branches"))
animal_type<- branch %>% select(c("cal_year","animal_group_parent")) %>% group_by(cal_year,animal_group_parent) %>% count(animal_group_parent) %>% summarise(avg= mean(n)) %>% arrange(desc(avg)) %>% filter(row_number()==1)
image<- c( "/Users/thivina/Documents/TidyTuesday06-29/img/cat.png","/Users/thivina/Documents/TidyTuesday06-29/img/cat.png","/Users/thivina/Documents/TidyTuesday06-29/img/cat.png","/Users/thivina/Documents/TidyTuesday06-29/img/cat.png","/Users/thivina/Documents/TidyTuesday06-29/img/bird.png", "/Users/thivina/Documents/TidyTuesday06-29/img/cat.png", "/Users/thivina/Documents/TidyTuesday06-29/img/bird.png", "/Users/thivina/Documents/TidyTuesday06-29/img/cat.png","/Users/thivina/Documents/TidyTuesday06-29/img/dog.png","/Users/thivina/Documents/TidyTuesday06-29/img/bird.png")
animal_type<-cbind(animal_type,image)
ggplot(animal_type, aes(cal_year, avg,)) + geom_col(width = 0.5, fill="darkgreen") +geom_text(aes(cal_year,avg,label=cal_year),hjust=1, vjust=0.5, colour="yellow") + geom_image(aes(image=animal_type$...4), hjust=0,size=0.07)  + theme_minimal() + coord_flip() + theme(panel.grid = element_blank(),axis.title.y =element_blank(),axis.ticks.y=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.line.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), plot.background = element_rect(fill = "burlywood", colour=NA))

#left_tree
property_type<- branch %>% select(c("cal_year","property_category")) %>% group_by(cal_year,property_category) %>% count(property_category) %>% summarise(avg= mean(n)) %>% arrange(desc(avg)) %>% filter(row_number()==1)
image_loc<- c( "/Users/thivina/Documents/TidyTuesday06-29/img/outdoors.png","/Users/thivina/Documents/TidyTuesday06-29/img/outdoors.png","/Users/thivina/Documents/TidyTuesday06-29/img/house.png","/Users/thivina/Documents/TidyTuesday06-29/img/outdoors.png","/Users/thivina/Documents/TidyTuesday06-29/img/outdoors.png", "/Users/thivina/Documents/TidyTuesday06-29/img/house.png", "/Users/thivina/Documents/TidyTuesday06-29/img/outdoors.png", "/Users/thivina/Documents/TidyTuesday06-29/img/house.png","/Users/thivina/Documents/TidyTuesday06-29/img/outdoors.png","/Users/thivina/Documents/TidyTuesday06-29/img/outdoors.png")
property_type<-cbind(property_type,image_loc)
ggplot(property_type,aes(cal_year, avg)) + geom_col(width = 0.5, fill="darkgreen") + geom_image(aes(image=property_type$...5), hjust=1,size=0.09) + geom_text(aes(label=cal_year),hjust=0, vjust=0.5, colour="yellow") + theme_minimal() + coord_flip()  + scale_y_reverse() + theme( panel.grid = element_blank(),  axis.title.y =element_blank(),axis.ticks.y=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.line.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), plot.background = element_rect(fill = "burlywood", colour=NA))



#tree trunk: step one, filter animals rescued from trunks using str word extraction from "final_description" and then 
#insert ggimage of animals as well using geom_point 
trunk <- animal_rescues %>% as_tibble() %>% unnest_tokens(word, final_description) %>% filter(word %in% c("tree","trees"))

trunk_df<- trunk %>% select(c("cal_year","animal_group_parent")) %>% group_by(cal_year,animal_group_parent) %>% count(animal_group_parent) %>% summarise(avg= mean(n)) %>% arrange(desc(avg))  %>% ungroup() %>% filter(row_number()<4)

image_trunk<- c( "/Users/thivina/Documents/TidyTuesday06-29/img/cat.png","/Users/thivina/Documents/TidyTuesday06-29/img/cat.png","/Users/thivina/Documents/TidyTuesday06-29/img/cat.png")
trunk_df<-cbind(trunk_df,image_trunk)
ggplot(trunk_df,aes(cal_year, avg)) + geom_col(width = 1, fill="saddlebrown") + geom_text(aes(label=cal_year),hjust=0.5, vjust=3, colour="tan") +  geom_image(aes(image=image_trunk), hjust=5,size=0.08) + theme_void() + theme(plot.background = element_rect(fill = "burlywood", colour=NA))




#putting it together: 

ggplot() + theme_void() +  theme(plot.background = element_rect(fill = "burlywood", colour=NA)) + draw_image( image = "/Users/thivina/Documents/TidyTuesday06-29/img/trunk.jpeg", x=0,y=0,width = 1, height=0.7) + draw_image( image = "/Users/thivina/Documents/TidyTuesday06-29/img/right_branch.jpeg", x=0.25,y=0.5,width = 1, height=0.4) + draw_image( image = "/Users/thivina/Documents/TidyTuesday06-29/img/left_branch.jpeg", x=-0.25,y=0.5,width = 1, height=0.4) +  draw_label("Rescued from trees", x = 0.5, y = 1, size = 35,fontface = "bold",fontfamily = "Arvo") +  draw_label("Key finding: \nThe year 2020 recorded the most \ntree branch rescues escpially of \ncats followed by the year 2021", x = 0.75, y = 0.4, size = 10,fontface = "bold",fontfamily = "Arvo",colour="darkgreen") +  draw_label("Key finding: \nThe year 2020 recorded the most \ntree branch rescues escpially from \noutdoor environments \nfollowed by the year 2021", x = 0.25, y = 0.4, size = 10,fontface = "bold",fontfamily = "Arvo", colour="darkgreen") +  draw_label("Graphics:beana_vina,Source:London.gov", x = 0.5, y = 0, size = 7,fontface = "bold",fontfamily = "Arvo", colour="grey20") + draw_label("Key finding: \nThe year 2011 recorded the most \ntree trunk rescues escpially of cats", x = 0.5, y = 0.8, size = 10,fontface = "bold",fontfamily = "Arvo",colour="saddlebrown") +  ggsave(path = "img", filename = "final.png", dpi = 128, width = 15, height =8.4 )

