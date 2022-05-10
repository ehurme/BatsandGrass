setwd("J:/Auswertung/Tadarida/Tadarida-C-master")
ClassifC1="ClassifC1.R"
AggContacts="AggContacts.R"
AggNbSp="AggNbSp.R"


#get arguments from the command line
args <- commandArgs(trailingOnly = TRUE)

if(length(args)<3) #for local tests
  {
  ClassifC1="J:/Auswertung/Tadarida/Tadarida-C-master/tadaridaC_src/ClassifC1.R"
  AggContacts="J:/Auswertung/Tadarida/Tadarida-C-master/tadaridaC_src/AggContacts.R"
  AggNbSp="J:/Auswertung/Tadarida/Tadarida-C-master/tadaridaC_srcAggNbSp.R"
    args="J:/test/merged_treated_files_HG/txt" #directory containing .ta files
args[1]="J:/test/merged_treated_files_HG/txt"
args[2]="ClassifEsp_tabase3HF_Baden-Wuerttemberg_2022-04-27.learner"
#args[2]="ClassifEsp_LF_180129.learner"
#args[2]="ClassifEspFrance180303.learner"
#args[3]="tabase3_LFXC"
args[3]="N"

#options (HPF = filtre passe-haut / Reduc = r?duction des features par DFA)
args[4]=10 #HPF
args[5]=F #Reduc - obsolete
args[6]=T #TC
args[7]=8409 #block size
#args[8]=1 #start number
#args[9]=2139 #end number number
args[10]="SpeciesList.csv" #species list
args[11]="CNS_tabase3HF_France_IdConc.learner" #name of the species number" classifier
args[12]=T #if species number should be filtered or not
args[13]="Referentiel_seuils_ProbEspHF_.csv"
args[14]=80000 #an additionnal, larger block size to handle memory leaks problem in randomForest
#args[15]="ClassifEsp_tabase3HF_France_Cir_2019-11-26_wiSR.learner"
#args[16]="CNS_RSDB_HF_tabase3HF_sansfiltre_IdTot_wiSR_IdConc.learner"
#args[17]="Referentiel_seuils_RSDB_HF_tabase3HF_sansfiltre_IdTot_wiSR_IdConc__G.csv"
args[18]=1000 #block number 
}

print(getwd())
print(args)

tadir=args[1]
talistot=list.files(tadir,pattern=".ta$",full.names=T)

if(length(talistot)>as.numeric(args[14])*(as.numeric(args[18])-1))
{
  talistot=talistot[(as.numeric(args[14])*(as.numeric(args[18])-1)+1)
                    :(min(length(talistot)
                          ,as.numeric(args[14])*(as.numeric(args[18]))))]
  
  FITA=file.info(talistot)
  talistot=subset(talistot,FITA$size>1000)
  
  
  
  block=as.numeric(args[7])
  
  
  for (r in 1:ceiling(length(talistot)/block))
  {
    args[8]=block*(r-1)+1 #start number
    args[9]=block*r #end number
    source(ClassifC1)
    print(paste(r,ceiling(length(talistot)/block),Sys.time()))
    if(!skip){
      source(AggContacts)
      source(AggNbSp)
    }
    
    print(gc())
    
  }
}
FITA=file.info(talistot)
