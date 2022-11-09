# qualificação
# September 09, 2022
# Carina Isabella Motta

#1 LOAD PACKAGES----------------------------------------------------------------

# a vector listing package names needed for importing the DNA sequences,
#calculating genetic distance, calculated geographic distance, and performing
#a Mantel test

package.list <- c("here", #so I don't have to deal with setting a WD
                  "vegan", #mantel test
                  "tidyverse", #data cleaning
                  "dplyr", #data cleaning
                  "stringr", #data cleaning
                  "ggplot2", #mantel test visualization 
                  "remotes",
                  
)

# Install devtools if not available
if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")

# Install traitdata package from Github
remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T)

#installing the packages if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}

library(traitdata)

####
#LOAD DATA

load_dados <- readr::read_csv(here::here("qualificação", "dados", "dados.csv"))

dados <- as_tibble(load_dados)

subset_dados <- dados %>% select(1:4, 9:10, 20)

filter_dados <- filter(subset_dados, fc == "y")

#edit species names to coincide with elton_traits

filter <- filter_dados %>% 
  mutate(especie = str_replace_all(especie, c("Stilpnia cayana", 
                                              "Myiothlypis flaveola",
                                              "Dryobates passerinus"), 
                                            c("Tangara cayana", 
                                              "Basileuterus flaveolus",
                                              "Veniliornis passerinus")))
                                      
                                

data(elton_birds)

subset_diet <- elton_birds %>% select(8:9, 17, 21)

subset_diet$especie<- with(subset_diet, paste(Genus, Species))



#left join

dados_diet <- merge(x=filter, y=subset_diet,
                    by="especie", all.x =T)

