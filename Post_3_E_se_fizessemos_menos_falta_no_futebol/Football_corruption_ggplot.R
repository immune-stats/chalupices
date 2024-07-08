library(ggplot2)

library(gridExtra)

library(ggflags)

file<-'~/Desktop/Chalupices/Post_03_Football_Corruption/average_fouls_per_league_2019_2021.txt'

data<-read.table(file,header=T,sep='\t')

aux<-which(data$Country=='ITA_B'|data$Country=='ESP_2'|data$Country=='ENG_CL'|data$Country=='GER_2'|data$Country=='FRA_2'|is.na(data$Personally_Affected_2019)==TRUE|is.na(data$Average_Fouls)==T)

data<-data[-aux,]

dim(data)

table(data$Country_Ab)

summary(data$Sample_size)

################################
##### Personally Affected ######
################################

cor.test(data$Average_Fouls,data$Personally_Affected_2019,method='spearman')

x.sco<-data$Average_Fouls[data$Country=='SCO']

y.sco<-data$Personally_Affected_2019[data$Country=='SCO']

x.eng<-data$Average_Fouls[data$Country=='ENG']

y.eng<-data$Personally_Affected_2019[data$Country=='ENG']

ggplot(data,aes(x=Average_Fouls,y= Personally_Affected_2019,country=Country_Ab))+geom_flag(size=8)+ylab('Percentagem de indivíduos')+xlim(20,35)+xlab('Média de faltas por jogo')+ylim(0,100)+annotate('text',x=x.eng,y=y.eng+6,label='ENG')+annotate('text',x=x.sco,y=y.sco+6,label='SCO')+ggtitle('Percepção de ter sido afectado pessoalmente por corrupção',subtitle='Correlação=0.743')+theme(plot.title = element_text(size = 20, face = "bold",hjust=0.5))+theme(plot.subtitle = element_text(size = 17.5,hjust=0.5))+theme(axis.text = element_text(size = 20, face = "bold"))+theme(axis.title = element_text(size = 20, face = "bold"))

###################################
##### Corruption in Business ######
###################################

x.sco<-data$Average_Fouls[data$Country=='SCO']

y.sco<-data$Corruption_Business_2019[data$Country=='SCO']

x.eng<-data$Average_Fouls[data$Country=='ENG']

y.eng<-data$Corruption_Business_2019[data$Country=='ENG']

cor.test(data$Average_Fouls,data$Corruption_Business_2019,method='spearman')

ggplot(data,aes(x=Average_Fouls,y= Corruption_Business_2019,country=Country_Ab))+geom_flag(size=8)+ylab('Percentagem de indivíduos com essa opinião')+xlim(20,35)+xlab('Média de faltas por jogo')+ylim(0,100)+annotate('text',x=x.eng,y=y.eng+6,label='ENG')+annotate('text',x=x.sco,y=y.sco+6,label='SCO')+ggtitle('Percepção de haver uma cultura de corrupção nos negócios',subtitle='Correlação=0.664')+theme(plot.title = element_text(size = 20, face = "bold",hjust=0.5))+theme(plot.subtitle = element_text(size = 17.5,hjust=0.5))+theme(axis.text = element_text(size = 20, face = "bold"))+theme(axis.title = element_text(size = 20, face = "bold"))

###################################################################
##### Favouritism/Corrution Hampers Business Competitiveness ######
###################################################################

x.sco<-data$Average_Fouls[data$Country=='SCO']

y.sco<-data$Corruption_Business_2019[data$Country=='SCO']

x.eng<-data$Average_Fouls[data$Country=='ENG']

y.eng<-data$Corruption_Business_2019[data$Country=='ENG']

cor.test(data$Average_Fouls,data$Favouritism_2019,method='spearman')

ggplot(data,aes(x=Average_Fouls,y= Favouritism_2019,country=Country_Ab))+geom_flag(size=8)+ylab('Percentagem de indivíduos com essa opinião')+xlim(20,35)+xlab('Média de faltas por jogo')+ylim(0,100)+annotate('text',x=x.eng,y=y.eng+6,label='ENG')+annotate('text',x=x.sco,y=y.sco+6,label='SCO')+ggtitle('Percepção de favoritismo e corrupção reduzir a competitividade nos negócios',subtitle='Correlação=0.756')+theme(plot.title = element_text(size = 20, face = "bold",hjust=0.5))+theme(plot.subtitle = element_text(size = 17.5,hjust=0.5))+theme(axis.text = element_text(size = 20, face = "bold"))+theme(axis.title = element_text(size = 20, face = "bold"))
