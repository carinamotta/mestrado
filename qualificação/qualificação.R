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
                  "psych",
                  "matrix"
                  
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

library(Matrix)

####
#LOAD DATA

load_dados <- readr::read_csv(here::here("qualificação", "dados", "dados7.csv"))

dados <- as_tibble(load_dados)

subset_dados <- dados %>% select(1:5, 10:11, 21, 23)

fezes <- filter(subset_dados, fc == "y")

#edit species names to coincide with elton_traits

spp_corr1 <- fezes %>% 
  mutate(especie = str_replace_all(especie, "Stilpnia cayana", 
                                            "Tangara cayana"))

spp_corr1 <- spp_corr1 %>% 
  mutate(especie = str_replace_all(especie, "Myiothlypis flaveola", 
                                   "Basileuterus flaveolus"))


spp_corr2 <- spp_corr1 %>% 
  mutate(especie = str_replace_all(especie, c("Dryobates passerinus",
                                              "Clibanornis rectirostris"), 
                                            c("Veniliornis passerinus",
                                              "Hylocryptus rectirostris")))

spp_corr3 <- spp_corr2 %>% 
  mutate(especie = str_replace_all(especie, "Asemospiza fuliginosa", 
                                            "Tiaris fuliginosus"))



# "Dryobates passerinus" Dryobates passerinus
#"Veniliornis passerinus"

#add_mes <- spp_corr3 %>%  
  #mutate(mes = case_when(grepl("Jul", data) ~ "julho",
                           #grepl("Aug", data, ignore.case = TRUE) ~"agosto",
                           #grepl("Sep", data, ignore.case = TRUE) ~ "setembro",
                           #grepl("Oct", data, ignore.case = TRUE) ~ "outubro",
                           #grepl("Nov", data, ignore.case = TRUE) ~ "novembro"))

goiaba <- spp_corr3 %>%  
  mutate(goiaba = case_when(grepl("207", parcela) ~ "sem goiaba",
                            grepl("20", parcela) ~ "com goiaba",
                            grepl("22", parcela) ~ "com goiaba",
                            grepl("84", parcela) ~ "com goiaba",
                            grepl("87", parcela) ~ "com goiaba",
                            grepl("88", parcela) ~ "com goiaba",
                            grepl("90", parcela) ~ "com goiaba",
                            grepl("206", parcela) ~ "com goiaba",
                            grepl("64", parcela) ~ "sem goiaba",
                            grepl("56", parcela) ~ "sem goiaba"))
         
      


#recent <- test[-c(145, 146), ] 
                                    
#test <- filter %>%  
  #mutate(collection = case_when(                        
                         #grepl("Aug", data) ~"1",
                         #grepl("Sep", data) ~ "2",
                         #grepl("Oct", data) ~ "3",
                         #grepl("Nov", data) ~ "3"))


data(elton_birds)

elton_traits <- readr::read_tsv(here::here("qualificação", "dados","BirdFuncDat.txt"))

subset_diet <- elton_traits %>% select(4, 8, 16, 20)

#subset_diet$especie<- with(subset_diet, paste(Genus, Species))

colnames(subset_diet)[2] ="especie"


#left join

dados_diet <- merge(x=goiaba, y=subset_diet,
                    by="especie", all.x =T)

colnames(dados_diet)[13] ="guild"

colnames(dados_diet)[11] ="familia"

colnames(dados_diet)[9] ="recap"

dados_diet <- dados_diet %>%  
  mutate(cons_fruto = case_when(`Diet-Fruit`> 0 ~ "sim",
                                `Diet-Fruit` == 0 ~ "não",))

list_spp1 <- distinct(dados_diet, especie, familia, cons_fruto, guild, .keep_all = FALSE)

list <- 
  dados_diet  %>%  
  group_by(especie)  %>%  
  summarise(numero = n())

#list_spp1$spp_no <- 1:nrow(list_spp1) 

final_list <- merge(x=list_spp1, y=list,
                        by="especie", all.x =T)

write.csv(final_list,"table.csv")

dados_diet <- dados_diet %>%  
  mutate(rede_hora = (haf - har)/3600)

dados_diet$rede_hora <- as.numeric(dados_diet$rede_hora)

summary(dados_diet)

group_by_parcela <- 
  dados_diet %>%                             #Applying group_by &summarise
  group_by(parcela) %>%
  summarise(especie = n_distinct(especie), indivíduos = n_distinct(amostra), 
            taxa_cap = (n_distinct(amostra)/(sum(rede_hora)/(n_distinct(amostra)/n_distinct(data)))),
                        rede_hora = sum(rede_hora)/(n_distinct(amostra)/n_distinct(data)),
            recap = sum(recap))


write.csv(group_by_parcela, "summary.csv")


#DF1 <- dados_diet 
#DF1$month_year <- format(as.Date(DF1$data, format = "%d-M-%Y"), "%Y-%m")



duration(num = NULL, units = "seconds", ...)

#describeBy(dados_diet, group=dados_diet$parcela, fast=TRUE)

curve_all = specaccum(dados_diet, method = "random", 
                      permutations = 100)

create_binary_matrix(x = dados_diet,
                     )
spp1 <- xtabs(~especie+dia, data=dados_diet)

write.csv(spp1, "x_tabs.csv")

spp1 <- as.tibble(spp)

group_by_data <- 
  dados_diet %>%                             #Applying group_by &summarise
  group_by(data, parcela) %>%
  summarise(especie = n_distinct(especie))
  



df <- dados_diet[order(dados_diet$dia),]

summary(dados_diet)



filtered <- df %>%
  filter(duplicated(especie) == FALSE)

group_by_data <- 
  filtered %>%                             #Applying group_by &summarise
  group_by(dia) %>%
  summarise(especie = n())

group_by_data$z <- 0

sum <- 0

for (i in 1:nrow(group_by_data)) {
              #print(i)
              sum <- (group_by_data[i, 2] + sum) 
              group_by_data[i, 3] <- sum
}

colnames(group_by_data)[3] ="cumsum"

ggplot(group_by_data, aes(dia, cumsum)) + 
  geom_point()


com_goiaba <- filtered %>%
  filter(goiaba == "com goiaba")

sem_goiaba <- filtered %>%
  filter(goiaba == "sem goiaba")

com_goiaba_grouped <- 
  com_goiaba %>%                             #Applying group_by &summarise
  group_by(dia) %>%
  summarise(especie = n())

com_goiaba_grouped$z <- 0

sum <- 0

for (i in 1:nrow(com_goiaba_grouped)) {
  #print(i)
  sum <- (com_goiaba_grouped[i, 2] + sum) 
  com_goiaba_grouped[i, 3] <- sum
}

colnames(com_goiaba_grouped)[3] ="cumsum"



sem_goiaba_grouped <- 
  sem_goiaba %>%                             #Applying group_by &summarise
  group_by(dia) %>%
  summarise(especie = n())

sem_goiaba_grouped$z <- 0

sum <- 0

for (i in 1:nrow(sem_goiaba_grouped)) {
  #print(i)
  sum <- (sem_goiaba_grouped[i, 2] + sum) 
  sem_goiaba_grouped[i, 3] <- sum
}

colnames(sem_goiaba_grouped)[3] ="cumsum"

(plot1 <- ggplot(com_goiaba_grouped, aes(dia, cumsum)) + 
    geom_point() + geom_line(color = "magenta") +
    geom_point(data = sem_goiaba_grouped) + 
    geom_line(data = sem_goiaba_grouped, color = "green") +
    theme_classic() +
    xlab("sampling day")+
    ylab("cummulative number of species") +
    labs())

