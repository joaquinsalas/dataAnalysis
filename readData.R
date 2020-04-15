#ReadData
#This is derive from ReadDataDate
#In ReadDataSelect, we construct a classifier to distinguish between confirmed and discarted based on a 
#set of features with medical meaning.
#2020.04.10
#Joaquin Salas. salas@ieee.org


suppressMessages(suppressWarnings(library(Boruta))) #for boruta, feature selection

library(stringi)


#this function reads the data from a csv file and filters out 
#data with too many missing values

readData <- function (filename, sep, fileEncoding = "") {
  
  #leer el archivo csv
  data <- read.csv(file = filename,  header=TRUE, stringsAsFactors = TRUE,  
                   fileEncoding= fileEncoding, sep =sep)
  
  #data <- read_csv(file = filename)
  
  
  dim(data)
  
  
  
  keep = c(
    "PACIENTE", 
 #   "DESC_DELEGACION", 
  "EDAD_ANO",   
  "EDAD_MESES",                  
 "SEXO",  
 "OCUPACION",   
 #"UNIDAD_DE_ADSCRIPCION",   
 "FECHA_ATENCION", "FECHA_INICIO_CUADRO_CLINICO",               
 "INICIO_SUBITO",                 "FIEBRE",                        "TOS",                          
  "CEFALEA",                       "ODINOFAGIA",                    "ATAQUE_AL_ESTADO_GENERAL"  ,   
   "MIALGIAS",                      "ARTRALGIAS",                    "POSTRACION",                   
   "RINORREA",                      "ESCALOFRÍO",                    "CONGESTION_NASAL",             
   "DISFONIA",                      "DOLOR_ABDOMINAL",               "CONJUNTIVITIS",                
   "DISNEA",                        "CIANOSIS",                      "LUMBALGIA",                    
   "DIARREA",                       "DOLOR_TORACICO",                "POLIPNEA",                     
   "IRRITABILIDAD_MENOS5AÑOS",      "CORIZA",                        "OTROS",                        
   "CONTACTO_OTROS_CASOS",          "ENFERMEDAD_CRONICA",          
   "ANTECED_EPOC",                  "ANTECED_DIABETES",              "ANTECED_ASMA",                 
   "ANTECED_INMUNOSUPRESION",       "ANTECED_TABAQUISMO",            "ANTECED_OBESIDAD",             
   "ANTECED_VIH_EVIH",              "EMBARAZO",                      "SEMANAS_DE_GESTACION",         
   "LACTANCIA",                     "PUERPERIO",                     "DIAS_PUERP",                   
   "ANTECED_OTRO_ANIMAL",           "VIAJE",                                          
   "RECIBIO_VACUNA",                "TIENE_ANTIMICROBIANOS",         "TIENE_ANTIVIRAL",              
                      "ANTECED_CERDOS",               
   "ANTECED_HIPERTENSION",          "ANTECED_CARDIOVASCULAR",        "ANTECED_RENAL",                
   "ANTECED_OTRA",                         "ANT_ENF_HEPATICA_CRONICA",     
   "ANT_ANEMIA_HEMOLITICA",         "ANT_ENF_NEUROLOGICA",                   
   "CONT_PERFAM_VIAJADO_EXTR",     
   "CONT_AMCONFAM_VIVIAN_EXTR",     "RECIBIO_VAC_NEUMOCOCO",  "SE_RECONOCE_INDIGENA",
 "TRABAJADOR_IMSS",
 "INDICADOR_SOSP_COVID", 
 "REALIZO_VIAJE_INI_SINT",
 "REC_TXANTIVIRAN" ,            
         "REALIZO_VIAJE_INI_SINT",
  "DESC_RESULTADO_CONF1", "DESC_RESULTADO_CONF2"
  )
  
  no.factors = c("EDAD_ANO","EDAD_MESES",
                 "ESCALOFRÍO", "IRRITABILIDAD_MENOS5AÑOS" )
  factors = keep[!(keep %in% no.factors)]
  
  

  
  ## Preprocesamiento
  
  #remueve las varibles que por ahora no se tomaranen cuenta
  data.keep = data[, (names(data) %in% c(keep))]
  
  for (factor in factors) {
    #print(stri_enc_mark(factor))
    
    #name = stri_encode(factor,"", "UTF-16")
   # name = factor
    #print(factor)
    #print(stri_enc_mark(name))
    data.keep[,factor] = as.factor(data.keep[,factor])
    
  }
  
 
 data.keep$EDAD_ANO = data.keep$EDAD_ANO + data.keep$EDAD_MESES/12
data.keep$FECHA_ATENCION = dmy(data.keep$FECHA_ATENCION) - dmy(data.keep$FECHA_INICIO_CUADRO_CLINICO)
data.keep$FECHA_ATENCION = as.numeric(data.keep$FECHA_ATENCION, units="days") 
data.keep = data.keep[, !(names(data.keep) %in% c("EDAD_MESES", "FECHA_INICIO_CUADRO_CLINICO"))] 
 
 
  #hay algunas variables con un nan, se hace imputacion de valores
  
  
  data.imp = data.keep
  ## Dado que utilizaremos Boruta para la seleccion de caracteristicas, 
  #reducimos la base de datos mediante la siguiente estrategia. 
  
  #excluir la caracteristica INDICADOR_SOSP_COVID
  X = data.imp[, !(names(data.imp) %in% "INDICADOR_SOSP_COVID")]
  
  #esta variable contendra aquellos sujetos de interes
  #y =as.factor(data$INDICADOR_SOSP_COVID)
  
  sospechosos = which(data$INDICADOR_SOSP_COVID =="SI")
  
  #filtra 
  #y = y[c(sospechosos)]
  
  #mantiene unicamente a los sospechosos 
  X.sospechosos = X[c(sospechosos),]
  
  
  dim(X.sospechosos)
  
  
  

  
  
  #too.many.nan = names(X)[colSums(is.na(X.sospechosos)) > 1]
  

  

  
   
  
 
  
  #hay algunas variables con un nan, se hace imputacion de valores
  #Xpp = fill(Xp, which(colSums (is.na(Xp)) > 0) )  
  
  #dim(Xpp)
  
  #get those rows where the result of at least one is <POSITIVO>
  positivo =  X.sospechosos$DESC_RESULTADO_CONF1 == "POSITIVO" | X.sospechosos$DESC_RESULTADO_CONF2 == "POSITIVO"
  #or at least one <NEGATIVO> but not <positivo> (note the lower case)
  negativo =  (X.sospechosos$DESC_RESULTADO_CONF1 == "NEGATIVO" | X.sospechosos$DESC_RESULTADO_CONF2 == "NEGATIVO") & !(positivo)
  
  #index holds the valid records
  index = which(positivo | negativo)
  
  sum(positivo)
  
  sum(negativo)
  
  #esta es la variable de interes
  y = c(rep(1, length(index))) * positivo[index]
  
  #get the data with valid columns
  Xp = X.sospechosos[, !(colnames(X.sospechosos) %in% c("DESC_RESULTADO_CONF1", "DESC_RESULTADO_CONF2") )]
  
  #and valid rows
  Xpp =Xp[index,]
  
  dim(Xpp)
  
  dataset = list(X = Xpp,  y = y)
  
  return (dataset)
  
  
}



selectFeatures <- function (dataset, posfix) {


  #selecciona las caracteristicas
  boruta.train = suppressWarnings(Boruta(dataset$X, dataset$y, pValue = 0.01, maxRuns = 100, mcAdj = TRUE))


  # 1. Open jpeg file
  filename = paste("boruta-",posfix,".jpg")
  jpeg(filename, width = 700, height = 350)

  plot(boruta.train, xlab = "", xaxt = "n")
  lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
  names(lz) <- colnames(boruta.train$ImpHistory)
  Labels <- sort(sapply(lz,median))
  axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

  # 3. Close the file
  dev.off()



  boruta.final <- TentativeRoughFix(boruta.train)

  #print(final.boruta)

  #get the surviving attributes after feature selection
  attributes = getSelectedAttributes(boruta.final, withTentative = F)

  #print(attributes)
  #plotImpHistory(boruta.train,lw = 2)


  #boruta.final <- TentativeRoughFix(boruta.train)


  #boruta.attributes = getSelectedAttributes(boruta.final, withTentative = FALSE)

  #statistical summary of Boruta run in an attribute centered way.
  boruta.df = attStats(boruta.final)


  confirmed = boruta.df[which(boruta.df$decision == "Confirmed"),]


  rejected = boruta.df[which(boruta.df$decision == "Rejected"),]


  important = data.frame(confirmed[order(-confirmed$meanImp),c(1,2)]);
  unimportant = rejected[order(rejected$meanImp),c(1,2)];
  print(round(important,2))
  #print(round(unimportant,2))

  features <- list(important = important, unimportant = unimportant, attributes = attributes)
  return (features)

}




