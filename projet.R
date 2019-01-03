#Nous allons étudier la corrélation qu'il peut y avoir entre ces différentes variables (en s'intéressant uniquement à la France) : la consommation de Tabac et d'alcool, le PIB, le taux de maladies professionnelles, le chômage, le nombre d'heures de travail par an et l'espérance de vie.  
#Cela va nous permettre de tester si ces facteurs peuvent avoir une incidence dans la cause de la depréssion. Nous verrons si ces variablent augmentent ou baissent en fonction du temps en parallèle. Ces données ont été récupérées sur le site : "data.gouv.fr", qui fournit beaucoup de bases de données en libre-service.

path = "https://github.com/YannCastellvi92/Project.git/"
#CHOMAGE
df = read.csv(file=paste(path,"Chômage.csv",sep=""), header=TRUE, sep =",")
print(df)
time <- c()
value <- c()
for (row in 1:nrow(df)) {
  time <- c(time,strsplit(toString(df[row,1]),";")[[1]][1])
  value <- c(value,strsplit(toString(df[row,1]),";")[[1]][2])
}

années <- as.numeric(time)
taux_de_chomage <- as.double(value)
plot(années,taux_de_chomage)
print("Le taux de corrélation linéaire entre le taux de chomage et le temps est de :")
print(cor(années,taux_de_chomage))


#HORAIRES PAR AN
df = read.csv(paste(path,"Heures_travaillées_par_an.csv",sep=""), header=TRUE,sep=",")
df <- data.frame(df$ï..LOCATION,df$TIME,df$Value)
names(df)[1]<-"loc"
names(df)[2]<-"time"
names(df)[3]<-"val"

time <- c()
val <- c()

for (row in 1:nrow(df)) {
  if(df[row,"loc"] == "FRA"){
    print(df[row,"loc"])
    time <- c(time,df[row,"time"])
    val <- c(val,df[row,"val"])
  }
}
années <- time
Heures_de_travail <- val
plot(années,Heures_de_travail)
print("Le taux de corrélation linéaire entre le nombre d'heures de travail et le temps est de :")
print(cor(années,Heures_de_travail))

#ALCOOL ET TABAC

df = read.csv(file=paste(path,"Consommation_alcool_tabac.csv",sep=""), header=TRUE, sep =",")
print(df)
time <- c()
value <- c()
time <- 2000:2013
value <- as.numeric(strsplit(toString(df[1,1]),";")[[1]])
années <- time
Somme_dépensée_en_alcool_et_tabac <- value
plot(années,Somme_dépensée_en_alcool_et_tabac)
print("Le taux de corrélation linéaire entre la somme dépenssée par les ménages en alcool et en tabac et le temps est de :")
print(cor(années,Somme_dépensée_en_alcool_et_tabac))

#ESPERANCE DE VIE

df = read.csv(file=paste(path,"Espérance_de_vie.csv",sep=""), header=TRUE, sep ="")
print(df)
time <- c()
value <- c()
time <- 1975:2013
value <- strsplit(toString(df[1,1]),";")[[1]]
value <- gsub(",", ".", value)


value <- as.double(value)
années <- time
espérance_de_vie <- value

plot(années,espérance_de_vie)
print("Le taux de corrélation linéaire entre l'espérance de vie et le temps est de :")
print(cor(années,espérance_de_vie))

#PIB 

df = read.csv(file=paste(path,"PIB.csv",sep=""), header=TRUE, sep ="")
time <- c()
value <- c()
time <- 1989:2010
value <- strsplit(toString(df[1,1]),";")[[1]]
value <- gsub(",", ".", value)


value <- as.double(value)
années <- time
PIB <- value

plot(années,PIB)
print("Le taux de corrélation linéaire entre le PIB et le temps est de :")
print(cor(années,PIB))

#MALADIE

df = read.csv(file=paste(path,"maladies.csv",sep=""), header=TRUE, sep ="")
print(df)
time <- c()
value <- c()
time <- 1984:2014
value <- strsplit(toString(df[1,1]),";")[[1]]
value <- gsub(",", ".", value)


value <- as.double(value)
années <- time
Taux_de_maladies_professionnelles <- value

plot(années,Taux_de_maladies_professionnelles)
print("Le taux de corrélation linéaire entre le taux de maladies professionnelles et le temps est de :")
print(cor(années,Nombre_de_maladies))

#CORRELATIONS :
  
#On va maintenant voir si il y a une correlation entre les différentes variables. Nous ne comparerons que les variables : taux de maladies professionnelle et la consommation de tabac.
#En effet, les autres variables ne sont pas intéressantes car : le PIB et l'espérance de vie n'ont fait qu'augmenter, et pour tester notre hypothèse, il faudrait qu'elles baissent, or ce n'est pas le cas.
# Pour le cas du nombre d'heure de travail par an : il baisse, alors qu'on s'attendrait plutôt à ce qu'il augmente si c'était un facteur important dans le cas de la depréssion.

#On va se limiter, pour la variable taux de maladies professionnelles, aux années entre 2000 et 2013, car nos données sur la somme dépenssée en alcool et tabac ne couvre que cette période.
Taux_de_maladies_professionnelles <- Taux_de_maladies_professionnelles[16:29]
print("Le taux de corrélation linéaire entre le taux de maladies professionnelles et la somme dépenssée par les ménages en alcool et en tabac est de :")
print(cor(Taux_de_maladies_professionnelles,Somme_dépensée_en_alcool_et_tabac))

#La correlation est très forte, cependant, deux variables ne sont pas suffisantes pour tirer des conclusions.

#PARTIE TEST :
  
#Pour tester notre hypothèse de départ qui est : "ces différentes variables sont des facteurs causant la dépression en France", nous nous réferrerons aux différens articles sur Internet expliquant les causes de la dépression.
#Nous ne nous intéresserons qu'aux deux variables dont on a calculé la correlation linéaire.

#On peut donc citer les articles suivants pour tester notre étude :
#L'effet de l'acool sur la depréssion :
#http://www.doctissimo.fr/html/psychologie/mag_2002/mag1101/dossier/ps_6054_depression_alcool.htm

#L'effet du tabac sur la depréssion :
#  http://www.doctissimo.fr/html/dossiers/tabac/articles/2775-tabac-depression.htm

#Ces articles expliquent que l'alcool et le tabac peuvent être des facteurs causant la depréssion mais également pouvant l'agraver.

#Je n'ai en revanche pas trouvé d'articles prouvant le lien entre les maladies professionnelles et la depréssion.

