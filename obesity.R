library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggjoy)

Obesity <- read_csv("C:/Users/Utente/Desktop/R_project/Obesity.csv")
## Rinomo delle colonne
 setnames(Obesity, old = "Gender", new = "Genere")
 setnames(Obesity, old = "Age", new = "Età")
 setnames(Obesity, old = "Height", new = "Altezza")
 setnames(Obesity, old = "Weight", new = "Peso")
 setnames(Obesity, old = "family_history_with_overweight", new = "Storia_Famigliale_Con_SovraPeso")
 setnames(Obesity, old = "FAVC", new = "Consumo_Alimenti_Alto_Calori")
 setnames(Obesity, old = "FCVC", new = "Frequenze_Consumo_Verdure")
 setnames(Obesity, old = "NCP", new = "Numero_pasti_principale")
 setnames(Obesity, old = "CAEC", new = "Consumo_Cibo_tra_pasti")
 setnames(Obesity, old = "SMOKE", new = "Fumatore")
 setnames(Obesity, old = "CH2O", new = "Consumo_giornaliero_acqua")
 setnames(Obesity, old = "SCC", new = "Monitario_Consumo_Calorie")
 setnames(Obesity, old = "FAF", new = "Frequenza_Attività_Fisica")
 setnames(Obesity, old = "TUE", new = "Tempo_Utulizzo_dispositivi_Tecnologici")
 setnames(Obesity, old = "CALC", new = "Consumo_Alcool")
 setnames(Obesity, old = "MTRANS", new = "Trasporto_Utilizzato")
 setnames(Obesity, old = "NObeyesdad", new = "Categorie_Peso")
 
## ordinamento dei dati 
Obesity$Genere <- factor(Obesity$Genere) 
Obesity$Storia_Famigliale_Con_SovraPeso <- factor(Obesity$Storia_Famigliale_Con_SovraPeso)
Obesity$Consumo_Alimenti_Alto_Calori <- factor(Obesity$Consumo_Alimenti_Alto_Calori)
Obesity$Consumo_Cibo_tra_pasti <- factor(Obesity$Consumo_Cibo_tra_pasti)
Obesity$Fumatore <- Obesity$Fumatore
Obesity$Monitario_Consumo_Calorie <- factor(Obesity$Monitario_Consumo_Calorie)
Obesity$Consumo_Alcool <- factor(Obesity$Consumo_Alcool)
Obesity$Trasporto_Utilizzato <- factor(Obesity$Trasporto_Utilizzato)
Obesity$Categorie_Peso <- factor(Obesity$Categorie_Peso)

##rinomo dei levels 
Obesity$Genere <- ifelse(Obesity$Genere == "Male", "Maschio", "Femmina")
Obesity$Storia_Famigliale_Con_SovraPeso <-ifelse(Obesity$Storia_Famigliale_Con_SovraPeso == "yes", "Si", "No")
Obesity$Consumo_Alimenti_Alto_Calori <- ifelse(Obesity$Consumo_Alimenti_Alto_Calori == "yes", "Si", "No")
Obesity$Fumatore <- ifelse(Obesity$Fumatore == "yes", "Si", "No")
Obesity$Monitario_Consumo_Calorie <- ifelse(Obesity$Monitario_Consumo_Calorie == "yes", "Si", "No")
levels(Obesity$Consumo_Cibo_tra_pasti) <- c("sempre", "Frequentemente", "No", "Qualche volta")
levels(Obesity$Consumo_Alcool) <- c("Sempre", "Frequentemente", "No", "Qualche volta")
levels(Obesity$Trasporto_Utilizzato)<- c("Machina", "Bici", "Motociclo", "Trasporto bublico", "A piedi")
levels(Obesity$Categorie_Peso) <- c("Peso_insufficiente", "Peso_normale ", "Obesità_di_tipo_I", "Obesità_di_tipo_II",
                                "Obesità_di_tipo_III", "Sovrappeso_di_levello_I", "Sovrappeso_di_levello_II")

##riordino dei levles 
Obesity$Consumo_Cibo_tra_pasti <- relevel(Obesity$Consumo_Cibo_tra_pasti, ref ="No", "Qualch  volta", "Frequentemente","sempre" )
Obesity$Consumo_Alcool <- relevel(Obesity$Consumo_Alcool, ref = "No", "Qualch  volta", "Frequentemente","sempre"  )
Obesity$Trasporto_Utilizzato <- relevel(Obesity$Trasporto_Utilizzato, ref ="A piedi","Bici", "Motociclo", "Machina", "Trasporto bublico")
Obesity$Categorie_Peso <- relevel(Obesity$Categorie_Peso, ref = "Peso_insufficiente", "Peso_normale ","Sovrappeso_di_levello_I","Sovrappeso_di_levello_II",
                              "Obesità_di_tipo_I", "Obesità_di_tipo_II", "Obesità_di_tipo_III" )



## visuallizzazione dei dati 

###1_fattori di rischio 
## principali fattori di rischio associati all'obesità

#calcoliamo l'indice  di massa corporea (IMC) che ci permmette di capire 
Obesity$Indice_massa_Corporea <- Obesity$Peso / Obesity$Altezza^2

#rapporto tra il peso e l'altezza per categorie di genere in funzione del (IMC)
ggplot(data = Obesity, mapping = aes(x = Peso, y =Altezza,color = Genere)) +
  geom_point(mapping = aes(size= Indice_massa_Corporea)) + 
  geom_smooth(se = FALSE)+
  xlab("Peso") +
  ylab("Altezza") +
  ggtitle(" rapporto tra l'alteza e il Peso") +
  scale_color_manual(values = c("orange1","grey50")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

 # si osserva più donne con altezza basso e con peso molto elevato con un IMC maggiore di 40 e peso >120
 #mentre si vedi Maschi con più grande altezze ma meno maschi con peso >120

##l'endamento nell'IMC della storia famigliale con sovrapeso
ggplot(Obesity, aes(x =Indice_massa_Corporea, fill = Storia_Famigliale_Con_SovraPeso)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("grey50","brown1"))+
  labs(title = "rapporto tra l'indice di massa corporea e la storia familiare con sovrappeso",
       x = "Indice di Massa Corporea", y = "Densità", fill = "Storia famgliale con Sovrapeso") +
  theme_minimal()
  #si osserva un andamento d'ince di massa corperea molto elevata da quelli con storia famigliale con sovrappeso
  
  #verifichiamo le peecentuale di categorie di peso da quelli con storia famigliale con sovrapeso
  #indice di massa corperea che ci permette di ragruppare le diverse categorie di peso
  Obesity<- Obesity %>%
    mutate(Gruppo_Peso = case_when(
      Indice_massa_Corporea  <= 18.5                                ~ "Sottopeso",
      Indice_massa_Corporea > 18.5 & Indice_massa_Corporea <= 24.9  ~ "Normopeso",
      Indice_massa_Corporea > 24.9 & Indice_massa_Corporea <= 29.9  ~ "Sovrappeso",
      Indice_massa_Corporea > 29.9 & Indice_massa_Corporea <= 34.9  ~ "Obesità di grado 1",
      Indice_massa_Corporea > 34.9 & Indice_massa_Corporea <= 39.9  ~ "Obesità di grado 2",
      Indice_massa_Corporea > 39.9                                  ~ "Obesità di grado 3 "
    ))
  Obesity$Gruppo_Peso<- factor(Obesity$Gruppo_Peso)
  Obesity$Gruppo_Peso <- relevel(Obesity$Gruppo_Peso, ref = "Sottopeso",   "Normopeso ", "Sovrappeso", "Obesità di grado 1", "Obesità di grado 2", 
                                 "Obesità di grado 3 " )
  #filtriamo per storia famigliale = Si e verifichiamo le percentuale 
  Obesity_proporzione_0 <- Obesity %>%
    filter(Storia_Famigliale_Con_SovraPeso=="Si") %>%
    group_by(Gruppo_Peso) %>%
    summarise(Frequenza = n()) %>%
    mutate(Percentualestoriafamigliale = Frequenza / sum(Frequenza) * 100)
  #grafico
  ggplot(data = Obesity_proporzione_0, aes(x = "", y = Percentualestoriafamigliale, fill = Gruppo_Peso)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = c("grey","green" ,"orange", "lightblue","brown1","purple"))+
    labs(fill = "Categorie di peso") +
    geom_text(aes(label = paste0(round(Percentualestoriafamigliale, 1), "%")), position = position_stack(vjust = 0.5))
   #c'è un rischio di obesità elevata da quelli che hanno una storia famigliale con sovrappeso le percentuale sono elevsate da 
   # da quelli con sovrappeso sia con obesità 
  
  
###2_stili di vità 
##obbitudini di vità che possono essere fattore di obesità  

#il rapporto tra  consumo frequente di alimenti ad alto contenuto calorico ell'obesità
#filtriamo quelli che hanno un consumo frequente di alimenti ad alto calorie e che hanno 
#sia un peso normale sia un sovrappeso o un obesità e verifichiamo le percentuale
  Obesity_proporzione_2 <- Obesity %>%
    filter(Consumo_Alimenti_Alto_Calori=="Si" & Indice_massa_Corporea > 18.5) %>%
    group_by(Gruppo_Peso) %>%
    summarise(Frequenza = n()) %>%
    mutate(PercentualeConsumo = Frequenza / sum(Frequenza) * 100)
#grafico
  ggplot(data = Obesity_proporzione_2, aes(x = "", y = PercentualeConsumo, fill = Gruppo_Peso)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("green","darkgoldenrod2" ,"lightblue", "brown1","grey" ))+
  labs(fill = "Categorie di peso") +
  geom_text(aes(label = paste0(round(PercentualeConsumo, 1), "%")), position = position_stack(vjust = 0.5))
  #si osserva che le percentuale sono elevati da quelli  con sovrappeso sia con obesità 
  #quindi il consumo di alimenti ad alto contenuto di calori è fattore di obesità 
  
#vediamo come varie quel raporto tra categorie di sesso 
Obesity%>%
  filter(Consumo_Alimenti_Alto_Calori=="Si" & Indice_massa_Corporea > 20   )%>%
  group_by(Gruppo_Peso, Genere) %>%
  ggplot(aes(x = Gruppo_Peso, y = Indice_massa_Corporea, fill = Genere)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Categorie di peso  ") +
  ylab("Indice di massa corporea") +
  ggtitle("rapporto tra le categorie di genere con indice di massa corpera  > 30 e le categorie di peso ") +
  scale_fill_manual(values = c("orange","lightblue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#il consumo di alimenti ad alto contenuto calorico non sembra essere diversso per questi due categorie 
#però si osserva che solo le femine hanno un obesità di grave con IMC > 50 

##roporto tra il fumatore e l'obesità
 Obesity %>%
  filter(Fumatore=="Si") %>%
  group_by(Gruppo_Peso, Genere) %>%
ggplot(aes(x=Indice_massa_Corporea, y=Gruppo_Peso, fill= Genere))+
  geom_joy(scale=2)+
  scale_y_discrete(expand = c(0.01, 0))+
  scale_x_continuous(expand = c(0.01, 0))+
  theme_joy()+ 
  scale_fill_manual(values = c("orange","grey"))+
  labs(title = "Andamento del percentuale di fumatore per categorie di peso",
       x = "Indice di massa corperea",
       y = "Categorie di Peso")+
  theme(plot.title = element_text(hjust = 0.5))
#abbiamo un adamento d'indice di massa elevata dai Maschi che fumano più che dalle Femine che fumano 
#mentre osserviamo che non ci sono personne di obesità grave  quindi fumare non si presenta come fatore di obesità
 
 
##Attributi relativi alla condizione fisica 
#osserviamo il raporto tra frequenza media di attività fisica e le categorie di peso 
Obesity%>%
  group_by(Gruppo_Peso) %>%
  summarise(Frequenza_media = mean(Frequenza_Attività_Fisica)) %>%
  ggplot(aes(x = Gruppo_Peso, y = Frequenza_media , fill = Gruppo_Peso)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Categorie di peso ") +
  ylab("Frequenza media") +
  ggtitle("Rapporto tra Frequenza media di attività fisica e le categorie di peso") +
  scale_fill_manual(values = c("grey", "green","orange", "blue", "brown1", "purple")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#la frequeza di attiva fisica media sembra essere bassa < 1 per le personne con obesità o con sovrapeso  
#mentre si osserva che la frequeza di attiva fisica media è >1 quindi possiamo dire che le personne con 
#obesità o con sovrappeso non effetuano molto attività fisica

##verifichiamo allora come varia quel frequenza media < 0.7 per intervalli di Età
#i differente intervalli di Età
Obesity<- Obesity %>%
  mutate(Gruppo_Età = case_when(
    Età > 10 & Età <= 20  ~ "10-20",
    Età > 20 & Età <= 30  ~ "20-30",
    Età > 30 & Età <= 40  ~ "30-40",
    Età > 40 & Età <= 50  ~ "40-50",
    Età > 50 & Età <= 60  ~ "50-60",
    Età > 60              ~ "Più_di_60"
  ))
#grafico
  Obesity%>%
  group_by(Gruppo_Età, Gruppo_Peso) %>%
  summarise(Frequenza_media = mean(Frequenza_Attività_Fisica)) %>%
  filter(Frequenza_media<0.7)%>%
  ggplot(aes(x = Gruppo_Età, y = Frequenza_media, fill = Gruppo_Peso)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("intervalli di Eta ") +
  ylab("frequenza dell'attività Fisica") +
  ggtitle("rapporto tra la frequenza dell'attività media fisica e l'età ") +
  scale_fill_manual(values = c("grey", "darkgreen",  "orange", "beige", "brown1" ,"lightblue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  # si oserva solo nell'intervallo [20-30] (intervalli dei giovani) un obesità grave mentre si osserva che 
  #le personne di intervalli di Età [10,20] e [50,60] hanno un frequenza di attività media > 0.7
  #però il rishio di obesità sembra essere elevato nella l'intervallo dei giovani
  
  ## per finir vediamo il mezzo utilizzato dalle personne con obesità
  Obesity_proporzione_7 <- Obesity %>%
    filter(Gruppo_Peso=="Obesità di grado 3 ") %>%
    group_by(Trasporto_Utilizzato) %>%
    summarise(Frequenza = n()) %>%
    mutate(Percentuale = Frequenza / sum(Frequenza) * 100)
   #grafico
  ggplot(data = Obesity_proporzione_7, aes(x = "", y = Percentuale, fill = Trasporto_Utilizzato)) +
    geom_bar(stat = "identity", width = 1) +
    scale_y_continuous(trans = "log10", breaks = c(0.1, 1, 10, 100), labels = scales::percent) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = c("lightblue", "brown1")) +
    labs(fill = "Mezzo di Trasporto") +
    geom_text(aes(label = paste0(round(Percentuale, 1), "%")), position = position_stack(vjust = 0.5))

#si osserva che le perssone  con obesità grave  utilizzano di più il trasporto publico e un può la macchina 
# questo può essere la causa del fatto che hanno una frequenza di attività bassa che provoca l'obesità grave.
  
    