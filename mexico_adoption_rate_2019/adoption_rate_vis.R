devtools::install_github("hrbrmstr/waffle")

library(waffle)
library(tidyverse)
library(magrittr)
library(cowplot)
library(beepr)
library(grid)
library(extrafont)

children <- data.frame(status=c(rep("not adopted",33111),rep("adopted",7)))
            

a<-children %>%
  count(status) %>% arrange(desc(n)) %>% 
  ggplot(aes(fill = status, values = n)) +
  geom_waffle(n_rows = 180,color="white")+
  scale_fill_manual(values=c("red1","gray50"),guide=FALSE)+
  labs(title="Adoptions in Mexico")+
  labs(title="In 2019, 7 children were adopted in Mexico, out of the +33,000 living in children's homes")+
  coord_fixed()+
  theme_void()+
  theme(plot.title = element_text(family="Roboto Condensed",face="bold"))

  additional_elements <- ggdraw(a)
  additional_elements <- additional_elements + draw_line(x=c(0.886,0.895,0.924),y=c(0.908,0.918,0.918),color="red1")
  additional_elements <- additional_elements + draw_line(x=c(0.886,0.895,0.924),y=c(0.705,0.715,0.715),color="gray50")
  additional_elements <- additional_elements + draw_label("Adopted\nchildren",x=0.926,y=0.918,color="red1",size=7,hjust = 0)
  additional_elements <- additional_elements + draw_label("Nonadopted\nchildren",x=0.926,y=0.715,color="gray50",size=7,hjust = 0)
  additional_elements <- additional_elements + draw_label("Data: INEGI - CAAS, DIF\ngithub.com/Rorevilla",x=0.878,y=0.028,color="gray50",size=6,hjust = 1)
  
  
  ggsave("a.png",plot=additional_elements)
  beep()
