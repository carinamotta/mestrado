
#facilitar a carregar pacotes 
package.list <- c("here", 
                  "tidyverse",
                  "devtools",
                  "ggpubr",
                  "ggplot2")

#instalar pacotes novos (se ainda não estiver presente)
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#carregar pacotes
for(i in package.list){library(i, character.only = T)}

#load spreadsheet
pg <- readr::read_csv(here::here("guajava.csv"))

##1 Correlation between Original Age X Number of P. guajava----__---------------
#plot correlation between original age (current age - 6 years) and the orginal
#number of P. guajava found and tagged in the plot

#test for normality
shapiro.test(pg$og_pg) #W = 0.68761, p-value = 0.0006237, not normal

#log transform number of pg since not normal
pg$log_og_pg=log(pg$og_pg)

#test for normality again 
shapiro.test(pg$log_og_pg) #W = 0.88984, p-value = 0.1689, normal!

num_pg <- ggscatter(pg, x = "og_age", y = "log_og_pg", 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "plot age", ylab = "log(number of P. guajava)")+
          geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
          geom_point(size =2, alpha = 1) 

num_pg

ggsave(
  filename = here::here("num_pg.png"),
  plot = num_pg, 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 720
)


##2 Correlation between Current Age X % Mortality of P. guajava-----------------

#calculate mortality
pg$mort=((abs((pg$current_pg)-(pg$og_pg)))/(pg$og_pg))*100

#test for normality
shapiro.test(pg$mort) #W = 0.8646, p-value = 0.08643, normal!

mort <- ggscatter(pg, x = "current_age", y = "mort", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "plot age", ylab = "% mortality") +
        geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
        geom_point(size =2, alpha = 1) 

mort

ggsave(
  filename = here::here("mort.pdf"),
  plot = mort, 
  width = 28, 
  height = 20, 
  units = "cm", 
  dpi = 720
)

         