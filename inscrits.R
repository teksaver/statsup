#chargement des données
download.file("http://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-atlas_regional-effectifs-d-etudiants-inscrits/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true","./data/inscrits.csv",method="curl")

library(dplyr)
library(ggplot2)
inscrits <- read.csv2("./data//inscrits.csv",sep=";",header=TRUE,quote="\"")

##nettoyage

#suppression des colonnes de droits
inscrits <- inscrits[,c(1:15,21)] 
#renommage
names(inscrits)<-c("rentree","rentree_univ",
                   "code_geo","nom_geo",
                   "code_regroupement","nom_regroupement",
                   "code_secteur","nom_secteur",
                   "code_sexe", "sexe",
                   "total_inscrits",
                   "a_DUT","nb_DUT",
                   "a_inge","nb_inge",
                   "id_insee_geo")

#transformation des facteurs en entiers           
inscrits$nb_inge <- as.integer(levels(inscrits$nb_inge))[inscrits$nb_inge]
#un facteur permet l'empilement des barres
inscrits$rentree <- factor(inscrits$rentree) 

#filtrage pour récupérer les données des ingénieurs au niveau national sans totaux
ingesFrance <- inscrits %>%
  filter(code_geo=="Pays",code_regroupement!="TOTAL",a_inge=="oui") %>%
  group_by(rentree,code_secteur,secteur=nom_secteur,regroupement=nom_regroupement) %>%
  summarize(nb_inge=sum(nb_inge))

#calcul des totaux par rentrée et par secteur
totaux <- ingesFrance %>% group_by(rentree,secteur) %>%summarise(totaux=sum(nb_inge))

#tracé de l'histogramme du nombre d'inscrits ingénieurs
png("evolutionIngesParSecteurs.png", width = 800, height = 480, units = "px") 
ggplot(ingesFrance,aes(x=rentree:code_secteur,y=nb_inge))+
  geom_bar(stat="identity",aes(fill=regroupement,group=secteur,color=secteur)) + 
  scale_x_discrete(labels=xlab)+
  geom_smooth(data=totaux,method="lm",aes(x=rentree,y=totaux,color=secteur,group=secteur))+
  geom_point(data=totaux,method="lm",aes(x=rentree,y=totaux,color=secteur,group=secteur))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0),legend.title=element_text(size=12),legend.text=element_text(size=11))+
  labs(title="Evolution des inscriptions en écoles d'ingénieurs par secteur",y="Nombre d'élèves ingénieurs",x="année universitaire")
dev.off()


#calcul pour toutes les filières
inscritsFrance <- inscrits %>% filter(code_geo=="Pays",code_regroupement!="TOTAL")
totaux <- inscritsFrance %>% group_by(rentree,secteur=nom_secteur) %>%summarise(totaux=sum(total_inscrits))

#tracé de l'histogramme pour toutes les filières
png("evolutionTous.png", width = 800, height = 480, units = "px") 
  ggplot(totaux,aes(x=rentree,y=totaux,fill=secteur))+
  geom_bar(stat="identity",position="dodge")+
  geom_smooth(method="lm") + 
  theme(legend.title=element_text(size=14),legend.text=element_text(size=12))+
  labs(title="Evolution des inscriptions (toutes filières)",y="Nombre d'étudiants inscrits",x="année universitaire")
dev.off()

