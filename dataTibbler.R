datapath = file.choose(new = FALSE)

trasp = function(p)
{
  # Check if file exists, useful?
  if(!file.exists(p)) return("File non trovato")
  
  # Get the path for the final output
  # and check if the file is a csv
  splpath = strsplit(p, split = "\\", fixed = TRUE, perl = FALSE, useBytes = FALSE)
  filename = splpath[[1]][length(splpath[[1]])]
  splfile = strsplit(filename, split = ".", fixed = TRUE, perl = FALSE, useBytes = FALSE)
  endpath = nchar(filename)
  fullpath = substr(p, 0, (nchar(p) - endpath))
  extens = splfile[[1]][length(splfile[[1]])]
  
  if(!extens == "csv") return("Formato file non corretto")
  
  # Reads the file, looks for the row with tag [Data]
  library(tibble)
  wholedata = read.csv(p, header = FALSE)
  wholedata = as_tibble(wholedata)
  datatag = which(wholedata=="[Data]")[1] + 1
  if(is.na(datatag)) return("Struttura file non corretta")
  
  # Check if there are all columns
  cols = c("Lane","Sample_ID","Sample_Name","Sample_Plate","Sample_Well","I7_Index_ID",
           "index","I5_Index_ID","index2","Sample_Project","Description")
  if(!all(wholedata[(datatag),] == cols) ) return("Non sono presenti tutte le colonne necessarie")

  # Split the headers and the data into two dataframes
  startdata = wholedata[1:datatag,]
  bodydata = wholedata[(datatag+1):(nrow(wholedata)),]
  rm(wholedata)
  
  # Column FCID
  fcidrow = which(startdata=="FCID")[1]
  outrows = nrow(bodydata)
  startm = as.matrix(startdata)
  fcidcontent = startm[fcidrow, 2]
  fcidcol = rep(fcidcontent, outrows)
  
  # Column Lane
  
  lanecol = bodydata$V1
  
  # Column SampleID
  
  sampleid = bodydata$V2
  
  # Column SampleRef
  samplerefrow = which(startdata[,1]=="SampleRef")[1]
  samplerefcontent = startm[samplerefrow, 2]
  samplerefcol = rep(samplerefcontent, outrows)  
  
  
  # Column Index
  
  indexcol = paste(bodydata$V7, bodydata$V9, sep = "-")
  
  # Column Control
  contrlrow = which(startdata[,1]=="Control")[1]
  contrlcontent = startm[contrlrow, 2]
  contrlfcol = rep(contrlcontent, outrows)  
  
  
  # Column Recipe
  reciperow = which(startdata[,1]=="Recipe")[1]
  recipecontent = startm[reciperow, 2]
  recipecol = rep(recipecontent, outrows)  
  
  # Empty columns
  
  desccol = rep(NA, outrows)
  opcol = rep(NA, outrows)
  descrcol = rep(NA, outrows)
  
  # File output
  outheaders = c("FCID","Lane","SampleID","SampleRef","Index","Description","Control",
                 "Recipe","Operator","SampleProject")
  outdata = tibble(fcidcol, lanecol, sampleid, samplerefcol,
                       indexcol, desccol, contrlfcol,
                       recipecol, opcol, descrcol)
  outfile = paste(fullpath, "midterm.csv", sep = "")
  write.table(outdata, file = outfile, quote = FALSE, sep = ",", na = "", row.names = FALSE,
              col.names = outheaders)
  
  return("File scritto correttamente")

}

if(!is.na(datapath))
{
  trasp(datapath)
}else
{
  print("Operazione annullata")
}

