
datapath = "C:\\Users\\Public\\Music\\Sample Music\\Input_SampleSheet.csv"


transp = function(){
  # Column FCID
  fcidrow = which(startdata[,1]=="FCID")[1]
  datarows = nrow(bodydata)
  startm = as.matrix(startdata)
  fcidcontent = startm[fcidrow, 2]
  fcidcol = rep(fcidcontent, datarows)
  
  # Column Lane
  
  lanecol = rep(1, datarows)
  
  # Column SampleID

  sampleid = format(c(1:datarows), digits = 3)
  sampleid = chartr( old = " ", new = "0",sampleid)
  sampleid = paste("S", sampleid, sep = "")
  
  # Column SampleRef
  samplerefrow = which(startdata[,1]=="SampleRef")[1]
 samplerefcontent = startm[samplerefrow, 2]
  samplerefcol = rep(samplerefcontent, datarows)  
  
  
  # Column Index
  
  indexcol = paste(bodydata$index, bodydata$index2, sep = "-")
  
  # Column Control
  contrlrow = which(startdata[,1]=="Control")[1]
  contrlcontent = startm[contrlrow, 2]
  contrlfcol = rep(contrlcontent, datarows)  
  
  
  # Column Recipe
  reciperow = which(startdata[,1]=="Recipe")[1]
  recipecontent = startm[reciperow, 2]
  recipecol = rep(recipecontent, datarows)  
  

  # File output
  outheaders = c("FCID","Lane","SampleID","SampleRef","Index","Description","Control",
                 "Recipe","Operator","SampleProject")
  outdata = data.frame(fcidcol, lanecol, sampleid, samplerefcol,
                       indexcol, rep(NA, datarows), contrlfcol,
                       recipecol, rep(NA, datarows), rep(NA, datarows))
  write.table(outdata, file = "midterm.csv", sep = ",", na = "", row.names = FALSE,
              col.names = outheaders)
  
  return(0)
}


if(!file.exists(datapath))
{
  print("File non trovato")

}else
{
  # Reads the first rows, looks for the row with tag [Data],

  startdata = read.csv(datapath, header = FALSE, nrows = 30)
  skipstart = which(startdata$V1=="[Data]")[1]
  
  # Check if there are all columns
  cols = c("Lane","Sample_ID","Sample_Name","Sample_Plate","Sample_Well","I7_Index_ID","index","I5_Index_ID","index2","Sample_Project","Description")
  if(!sum(cols == startdata[(skipstart+1),]) == length(cols))
  {
    print("Non sono presenti tutte le colonne necessarie")
  }else
  {
    # load the file starting from the row with tag [Data]
    # and select the subset before that row
  bodydata = read.csv(datapath, header = TRUE, skip = skipstart)
  startdata = startdata[1:(skipstart-1),]
  
  transp()  
  }

}
