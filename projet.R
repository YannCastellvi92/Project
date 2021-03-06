#Nous allons �tudier la corr�lation qu'il peut y avoir entre ces diff�rentes variables (en s'int�ressant uniquement � la France) : la consommation de Tabac et d'alcool, le PIB, le taux de maladies professionnelles, le ch�mage, le nombre d'heures de travail par an et l'esp�rance de vie.  
#Cela va nous permettre de tester si ces facteurs peuvent avoir une incidence dans la cause de la depr�ssion. Nous verrons si ces variablent augmentent ou baissent en fonction du temps en parall�le. Ces donn�es ont �t� r�cup�r�es sur le site : "data.gouv.fr", qui fournit beaucoup de bases de donn�es en libre-service.

path = "https://github.com/YannCastellvi92/Project.git/"
#CHOMAGE
df = read.csv(file=paste(path,"Ch�mage.csv",sep=""), header=TRUE, sep =",")
print(df)
time <- c()
value <- c()
for (row in 1:nrow(df)) {
  time <- c(time,strsplit(toString(df[row,1]),";")[[1]][1])
  value <- c(value,strsplit(toString(df[row,1]),";")[[1]][2])
}

ann�es <- as.numeric(time)
taux_de_chomage <- as.double(value)
plot(ann�es,taux_de_chomage)
print("Le taux de corr�lation lin�aire entre le taux de chomage et le temps est de :")
print(cor(ann�es,taux_de_chomage))


#HORAIRES PAR AN
df = read.csv(paste(path,"Heures_travaill�es_par_an.csv",sep=""), header=TRUE,sep=",")
df <- data.frame(df$�..LOCATION,df$TIME,df$Value)
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
ann�es <- time
Heures_de_travail <- val
plot(ann�es,Heures_de_travail)
print("Le taux de corr�lation lin�aire entre le nombre d'heures de travail et le temps est de :")
print(cor(ann�es,Heures_de_travail))

#ALCOOL ET TABAC

df = read.csv(file=paste(path,"Consommation_alcool_tabac.csv",sep=""), header=TRUE, sep =",")
print(df)
time <- c()
value <- c()
time <- 2000:2013
value <- as.numeric(strsplit(toString(df[1,1]),";")[[1]])
ann�es <- time
Somme_d�pens�e_en_alcool_et_tabac <- value
plot(ann�es,Somme_d�pens�e_en_alcool_et_tabac)
print("Le taux de corr�lation lin�aire entre la somme d�penss�e par les m�nages en alcool et en tabac et le temps est de :")
print(cor(ann�es,Somme_d�pens�e_en_alcool_et_tabac))

#ESPERANCE DE VIE

df = read.csv(file=paste(path,"Esp�rance_de_vie.csv",sep=""), header=TRUE, sep ="")
print(df)
time <- c()
value <- c()
time <- 1975:2013
value <- strsplit(toString(df[1,1]),";")[[1]]
value <- gsub(",", ".", value)


value <- as.double(value)
ann�es <- time
esp�rance_de_vie <- value

plot(ann�es,esp�rance_de_vie)
print("Le taux de corr�lation lin�aire entre l'esp�rance de vie et le temps est de :")
print(cor(ann�es,esp�rance_de_vie))

#PIB 

df = read.csv(file=paste(path,"PIB.csv",sep=""), header=TRUE, sep ="")
time <- c()
value <- c()
time <- 1989:2010
value <- strsplit(toString(df[1,1]),";")[[1]]
value <- gsub(",", ".", value)


value <- as.double(value)
ann�es <- time
PIB <- value

plot(ann�es,PIB)
print("Le taux de corr�lation lin�aire entre le PIB et le temps est de :")
print(cor(ann�es,PIB))

#MALADIE

df = read.csv(file=paste(path,"maladies.csv",sep=""), header=TRUE, sep ="")
print(df)
time <- c()
value <- c()
time <- 1984:2014
value <- strsplit(toString(df[1,1]),";")[[1]]
value <- gsub(",", ".", value)


value <- as.double(value)
ann�es <- time
Taux_de_maladies_professionnelles <- value

plot(ann�es,Taux_de_maladies_professionnelles)
print("Le taux de corr�lation lin�aire entre le taux de maladies professionnelles et le temps est de :")
print(cor(ann�es,Nombre_de_maladies))

#CORRELATIONS :
  
#On va maintenant voir si il y a une correlation entre les diff�rentes variables. Nous ne comparerons que les variables : taux de maladies professionnelle et la consommation de tabac.
#En effet, les autres variables ne sont pas int�ressantes car : le PIB et l'esp�rance de vie n'ont fait qu'augmenter, et pour tester notre hypoth�se, il faudrait qu'elles baissent, or ce n'est pas le cas.
# Pour le cas du nombre d'heure de travail par an : il baisse, alors qu'on s'attendrait plut�t � ce qu'il augmente si c'�tait un facteur important dans le cas de la depr�ssion.

#On va se limiter, pour la variable taux de maladies professionnelles, aux ann�es entre 2000 et 2013, car nos donn�es sur la somme d�penss�e en alcool et tabac ne couvre que cette p�riode.
Taux_de_maladies_professionnelles <- Taux_de_maladies_professionnelles[16:29]
print("Le taux de corr�lation lin�aire entre le taux de maladies professionnelles et la somme d�penss�e par les m�nages en alcool et en tabac est de :")
print(cor(Taux_de_maladies_professionnelles,Somme_d�pens�e_en_alcool_et_tabac))

#La correlation est tr�s forte, cependant, deux variables ne sont pas suffisantes pour tirer des conclusions.

#PARTIE TEST :
  
#Pour tester notre hypoth�se de d�part qui est : "ces diff�rentes variables sont des facteurs causant la d�pression en France", nous nous r�ferrerons aux diff�rens articles sur Internet expliquant les causes de la d�pression.
#Nous ne nous int�resserons qu'aux deux variables dont on a calcul� la correlation lin�aire.

#On peut donc citer les articles suivants pour tester notre �tude :
#L'effet de l'acool sur la depr�ssion :
#http://www.doctissimo.fr/html/psychologie/mag_2002/mag1101/dossier/ps_6054_depression_alcool.htm

#L'effet du tabac sur la depr�ssion :
#  http://www.doctissimo.fr/html/dossiers/tabac/articles/2775-tabac-depression.htm

#Ces articles expliquent que l'alcool et le tabac peuvent �tre des facteurs causant la depr�ssion mais �galement pouvant l'agraver.

#Je n'ai en revanche pas trouv� d'articles prouvant le lien entre les maladies professionnelles et la depr�ssion.

