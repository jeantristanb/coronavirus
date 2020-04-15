library(coronavirus)
data(coronavirus)
update_datasets(T)
Country="South Africa"
CoCase<-coronavirus[coronavirus$type=="confirmed",]
CoCaseMean<-aggregate(cases~Country.Region, data=CoCase, sum)
CoCaseMean<-CoCaseMean[order(CoCaseMean$cases),]
Cmt=1
for(Country in unique(coronavirus$Country)){
SACRConf<-coronavirus[coronavirus$type=="confirmed" & coronavirus$Country.Region==Country,]
SACRConf<-SACRConf[order(as.Date(SACRConf$date)),]
SACRConf$CumSumCase<-cumsum(SACRConf$cases)
datefirstcase=SACRConf[SACRConf$CumSumCase==min(SACRConf$CumSumCase[SACRConf$CumSumCase>0]),][1,'date']
SACRDeath<-coronavirus[coronavirus$type=='death' & coronavirus$Country.Region==Country,]
SACRDeath<-SACRDeath[order(as.Date(SACRDeath$date)),]
SACRDeath$CumSumCase<-cumsum(SACRDeath$cases)
datefirstdeath=SACRDeath[SACRDeath$CumSumCase==min(SACRDeath$CumSumCase[SACRDeath$CumSumCase>0]),][1,'date']
CoCaseOtherC<-CoCaseMean[CoCaseMean$Country!=Country,]
ListeOtherCount<-CoCaseOtherC[order(abs(CoCaseMean[CoCaseMean$Country==Country,'cases'] - CoCaseOtherC[CoCaseOtherC$Country!=Country,'cases'])),1][1:6]

my_title <- paste("Coronavirus in",Country)
rmarkdown::render_site()

newhtlm=paste(gsub(' ','_',Country),'.html',sep='')
file.copy('allcountry/index.html', newhtlm)
if(Cmt>3){
q()
}
Cmt<-Cmt+1
}



