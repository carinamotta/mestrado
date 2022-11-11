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
                  "bipartite"
)



#installing the packages if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}


####
#LOAD DATA

load_dados <- readr::read_csv(here::here("qualificação", "dados", "dados8.csv"))

dados <- as_tibble(load_dados)

subset_dados <- dados %>% select(1:5, 10:11, 21, 23:24)

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



elton_traits <- readr::read_tsv(here::here("qualificação", "dados","BirdFuncDat.txt"))

subset_diet <- elton_traits %>% select(4, 8, 16, 20)

#subset_diet$especie<- with(subset_diet, paste(Genus, Species))

colnames(subset_diet)[2] ="especie"


#left join

dados_diet <- merge(x=goiaba, y=subset_diet,
                    by="especie", all.x =T)

colnames(dados_diet)[14] ="guild"

colnames(dados_diet)[12] ="familia"

colnames(dados_diet)[10] ="recap"

dados_diet <- dados_diet %>%  
  mutate(cons_fruto = case_when(`Diet-Fruit`> 0 ~ 1,
                                `Diet-Fruit` == 0 ~ 0,))

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
            recap = sum(recap),
            seeds = sum(seeds),
            fruit = sum(cons_fruto))


write.csv(group_by_parcela, "summary.csv")


#DF1 <- dados_diet 
#DF1$month_year <- format(as.Date(DF1$data, format = "%d-M-%Y"), "%Y-%m")



#duration(num = NULL, units = "seconds", ...)

#describeBy(dados_diet, group=dados_diet$parcela, fast=TRUE)

#CUMMULATIVE NUMBER OF SPECIES TOTAL 

#fix order 

df_order <- dados_diet[order(dados_diet$dia),]

#get rid of any species that appear for a second time, only first occurence

filtered <- df_order %>%
  filter(duplicated(especie) == FALSE)

#group by day

group_by_dia <- 
  filtered %>%                             #Applying group_by &summarise
  group_by(dia) %>%
  summarise(especie = n())

#create new column 
group_by_dia$cumspp_t <- 0

#define place for for loop to store values 
sum <- 0

#for loop to calculate cumulative number of spp.

for (i in 1:nrow(group_by_dia)) {
              sum <- (group_by_dia[i, 2] + sum) 
              group_by_dia[i, 3] <- sum
}

#get rid of spp. column 

group_by_dia <- group_by_dia %>%
  select(-especie)

ggplot(group_by_dia, aes(dia, cumspp_t)) + 
  geom_point()

## GOIABA presence/absence 

#create new df grouping data by goiaba presence

com_goiaba <- filtered %>%
  filter(goiaba == "com goiaba")

com_goiaba_grouped <- 
  com_goiaba %>%                             #Applying group_by &summarise
  group_by(dia) %>%
  summarise(especie = n())

com_goiaba_grouped$cumspp <- 0

sum <- 0

for (i in 1:nrow(com_goiaba_grouped)) {
  #print(i)
  sum <- (com_goiaba_grouped[i, 2] + sum) 
  com_goiaba_grouped[i, 3] <- sum
}

#get rid of species column 
com_goiaba_grouped <- com_goiaba_grouped %>%
  select(-especie)

#add value to last day to make graph line go all the way to the end 
com_goiaba_grouped[nrow(com_goiaba_grouped) + 1,] <- list(32, 25)

#now create df without goiaba 

sem_goiaba <- filtered %>%
  select(goiaba == "sem goiaba")

sem_goiaba_grouped <- 
  sem_goiaba %>%                             #Applying group_by &summarise
  group_by(dia) %>%
  summarise(especie = n())

sem_goiaba_grouped$cumspp <- 0

sum <- 0

for (i in 1:nrow(sem_goiaba_grouped)) {
  #print(i)
  sum <- (sem_goiaba_grouped[i, 2] + sum) 
  sem_goiaba_grouped[i, 3] <- sum
}

#get rid of species column 
sem_goiaba_grouped <- sem_goiaba_grouped %>%
  select(-especie)

#merge two df into same df

#create new datafram with all the days 

#dia <- seq(1, 32, by = 1)

#dia_df <- data.frame(dia)

#df_goiaba <- merge(x=dia_df, y=com_goiaba_grouped,
                #by="dia", all.x =T)

#df_goiaba <- merge(x=df_goiaba, y=sem_goiaba_grouped,
                   #by="dia", all.x =T)

#complete(df_goiaba)

#plot1 <- ggplot(df_goiaba, aes(dia, cumspp_cg, cumspp_sg)) + 
    #geom_point(df_goiaba$cumspp_cg) + geom_line() +
    #theme_classic() +
    #xlab("sampling day")+
    #ylab("cummulative number of species") +
    #labs()



(plot1 <- ggplot(com_goiaba_grouped, aes(dia, cumspp)) + 
    geom_point() + geom_line(color = "magenta") +
    geom_point(data = sem_goiaba_grouped) + 
    geom_line(data = sem_goiaba_grouped, color = "green") +
    theme_classic() +
    xlab("sampling day")+
    ylab("cummulative number of species") +
    labs())
plot1

#----------------GRAPH BY GUILD

group_by_guild <- 
  filtered %>%                             #Applying group_by &summarise
  group_by(dia, guild) %>%
  summarise(especie = n())

##INVERT  

invert <- group_by_guild %>%
  filter(guild == "Invertebrate")

invert <- invert %>%
  select(-guild)

sum <- 0

invert$cummspp <- 0

for (i in 1:nrow(invert)) {
  #print(i)
  sum <- (invert[i, 2] + sum) 
  invert[i, 3] <- sum
}

invert <- invert %>%
  select(-especie)



##omnivore

omn <- group_by_guild %>%
  filter(guild == "Omnivore")

omn <- omn %>%
  select(-guild)

sum <- 0

omn$cummspp <- 0

for (i in 1:nrow(omn)) {
  #print(i)
  sum <- (omn[i, 2] + sum) 
  omn[i, 3] <- sum
}

omn <- omn %>%
  select(-especie)

omn[nrow(omn) + 1,] <- list(32, 8)

##FruiNect

fru <- group_by_guild %>%
  filter(guild == "FruiNect")

fru <- fru %>%
  select(-guild)

sum <- 0

fru$cummspp <- 0

for (i in 1:nrow(fru)) {
  #print(i)
  sum <- (fru[i, 2] + sum) 
  fru[i, 3] <- sum
}

fru <- fru %>%
  select(-especie)

fru[nrow(fru) + 1,] <- list(32, 4)

##PlantSeed

plant <- group_by_guild %>%
  filter(guild == "PlantSeed")

plant <- plant %>%
  select(-guild)

sum <- 0

plant$cummspp <- 0

for (i in 1:nrow(plant)) {
  #print(i)
  sum <- (plant[i, 2] + sum) 
  plant[i, 3] <- sum
}

plant <- plant %>%
  select(-especie)

plant[nrow(plant) + 1,] <- list(32, 2)

##PLOT

(plot3 <- ggplot(invert, aes(dia, cummspp)) + 
    geom_point() + geom_line(color = "blue") +
    geom_point(data = omn) + 
    geom_line(data = omn, color = "red") +
    geom_point(data = fru) + 
    geom_line(data = fru, color = "purple") +
    geom_point(data = plant) + 
    geom_line(data = plant, color = "green") +
    geom_point(data = group_by_data) + 
    geom_line(data = group_by_data, color = "black") +
    theme_classic() +
    xlab("sampling day")+
    ylab("cummulative number of species") +
    labs()+
    theme(legend.text = element_text(colour="blue", size=10, 
                                     face="bold")))


###-------GRAPH BY FRUIT CONSUMING OR NOT 


group_by_fru_cons <- 
  filtered %>%                             #Applying group_by &summarise
  group_by(dia, cons_fruto) %>%
  summarise(especie = n())


fru_yes <- group_by_fru_cons %>%
  filter(cons_fruto == "sim")

fru_yes <- fru_yes %>%
  select(-cons_fruto)

sum <- 0

fru_yes$cummspp <- 0

for (i in 1:nrow(fru_yes)) {
  #print(i)
  sum <- (fru_yes[i, 2] + sum) 
  fru_yes[i, 3] <- sum
}

fru_yes <- fru_yes %>%
  select(-especie)

fru_yes[nrow(fru_yes) + 1,] <- list(32, 23)


##not fruit consuming


fru_no <- group_by_fru_cons %>%
  filter(cons_fruto == "não")

fru_no <- fru_no %>%
  select(-cons_fruto)

sum <- 0

fru_no$cummspp <- 0

for (i in 1:nrow(fru_no)) {
  #print(i)
  sum <- (fru_no[i, 2] + sum) 
  fru_no[i, 3] <- sum
}

fru_no <- fru_no %>%
  select(-especie)


##plot

(plot2 <- ggplot(fru_yes, aes(dia, cummspp)) + 
    geom_point() + geom_line(color = "blue") +
    geom_point(data = fru_no) + 
    geom_line(data = fru_no, color = "red") +
    geom_point(data = group_by_data) + 
    geom_line(data = group_by_data, color = "black") +
    theme_classic() +
    xlab("sampling day")+
    ylab("cummulative number of species") +
    labs())


## 

dia <- seq(1, 32, by = 1)

dia_df <- data.frame(dia)

df_fru <- merge(x=dia_df, y=fru_no,
                    by="dia", all.x =T)

fezes <- filter(subset_dados, fc == "y")

###INTERACTION WEB
summary(subset_dados)
#rename the plots in the order i want 
mat <- dados_diet %>% 
  mutate(parcela = str_replace_all(parcela, "207", 
                                 "09"))
mat <- mat %>% 
  mutate(parcela = str_replace_all(parcela, "87", 
                                   "02"))

mat <- mat %>% 
  mutate(parcela = str_replace_all(parcela, "206", 
                                   "06"))

mat <- mat %>% 
  mutate(parcela = str_replace_all(parcela, "20", 
                                   "01"))

mat <- mat %>% 
  mutate(parcela = str_replace_all(parcela, "22", 
                                   "03"))

mat <- mat %>% 
  mutate(parcela = str_replace_all(parcela, "90", 
                                   "04"))

mat <- mat %>% 
  mutate(parcela = str_replace_all(parcela, "84", 
                                   "05"))

mat <- mat %>% 
  mutate(parcela = str_replace_all(parcela, "88", 
                                   "07"))

mat <- mat %>% 
  mutate(parcela = str_replace_all(parcela, "64", 
                                   "08"))

mat <- mat %>% 
  mutate(parcela = str_replace_all(parcela, "56", 
                                   "10"))
#as.character(mat$parcela)


inter_mat <- subset_dados %>%
  filter(fc == "y") %>% #only feces collected
  group_by(parcela) %>% 
  arrange(parcela, .by_group=T)

inter_mat <- mat %>%
  #filter(fc == "y") %>% #only feces collected
  #arrange(desc(cons_fruto)) %>%
  group_by(parcela, especie) %>% 
  summarise(amostras = n_distinct(amostra)) %>% # this just combines all the quantities from the observations into one value per family
  #arrange(Order) %>% #sinc I imagine you will want to visualize this in order of different insect orders, this and the next line of code make sure that happens
  pivot_wider(names_from = especie, #makes this in to a matrix with this column name
              values_from = amostras) %>% #and values in cells filled with this column
  column_to_rownames(var = "parcela")  #then, we set the row names to our flower visitor families

inter_mat[is.na(inter_mat)] <- 0
# to plot this as a web, I use the plotweb function from bipartite
# you can find out more about this function by typing ?plotweb
# in your R console. 

inter_mat2 <- mat %>%
  filter(fc == "y") %>% #only feces collected
  arrange(guild, by.group=T) %>%
  group_by(parcela, especie) %>% 
  summarise(amostras = n_distinct(amostra)) %>% # this just combines all the quantities from the observations into one value per family
  #arrange(Order) %>% #sinc I imagine you will want to visualize this in order of different insect orders, this and the next line of code make sure that happens
  pivot_wider(names_from = especie, #makes this in to a matrix with this column name
             values_from = amostras) %>% #and values in cells filled with this column
  column_to_rownames(var = "parcela")  #then, we set the row names to our flower visitor families

inter_mat[is.na(inter_mat)] <- 0

??plotweb



# I have it here with some of the defaults changed so it looks 
# like it does, 
plotweb(inter_mat,
        method = "normal", # makes sure the families are in order I set them by the order of insects
        low.spacing = 0.05, # spreads out the bottom of the graph
        low.y=-1,
        text.rot=90,
        y.lim = c(-1.5, 4)) #squishes the m


