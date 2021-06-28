#---
#ParSD
#Tool to design and analyze particle size distributions
#Copyright (C) 2020, 2021 Jens Fruhstorfer
#
#
#This file is part of ParSD.
#
#ParSD is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#
#ParSD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
#
#See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with ParSD. If not, see <http://www.gnu.org/licenses/>.
#---





createdb <- function(dbfile) {

#return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 (or else) for back to choose path and db-name
  
  while (TRUE) {
  
  message("BEGIN create database function\n")
  
  sizedef <- tkmessageBox(title = "Definition of sieves or particle sizes", message="Would you like to import the particle sizes from a CSV-file? If not, you are asked to input them manually.", icon = "question", type = "yesno")

    if (tclvalue(sizedef) == "yes") {
    
    
      #get dnum and dsizes from file
      
      matfile <- tk_choose.files(caption="Select file with particle sizes", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))   
    if (length(matfile) == 0) { next }
   
    #requirements on file: column 'Diameter' 

    message("  Import particle sizes from file:",matfile)
   
    matdata <- read.csv2(file=matfile,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

    #check for every dsize, if it is included in matdata$Diameter. If not, add row (Percent=NA).
    
    dsizes <- matdata$Diameter
    dnum <- length(dsizes)
      
    message("  Number of component sizes:",dnum,"\n")
      
    } else { #end if sizedef import; start sizedef input
  
  
  message("  Input particle sizes")
  
  
  
  
  vals <- varEntryDialog(vars=c('cntd'), labels=c("Number of sieves or particle sizes:"), title="Sieve number definition", prompt="Define the number of sieves or component sizes.",cancellab='Back to Main menu')
        if(is.null(vals)) { 
        appdb <- 1 #back to Main Menu
        break 
        } 
 
   dnum <- as.numeric(vals[1])
   if (is.na(dnum) || dnum%%1!=0) {
      tkmessageBox(title ='Input error', message="Number of sieves has to be a whole number/integer.", icon = "error", type = "ok")
      next
   }
  
  
  message("  Number of component sizes:",dnum,"\n")
  
  
  
  
  
  
    dsizes <- vector(length=dnum)
    winnr <- ceiling(dnum/10)
    for (i in seq(from=0, to=winnr-1, by=1)) { #bis -1 weil von 0; 0 necessary due to condition-types with i*10+INPUTlineNUMBER
      
      if (i*10+10 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9','d10'), labels=c(paste("Sieve/Particle size ", i*10+1, ":",sep=""), paste("Sieve/Particle size ", i*10+2, ":",sep=""), paste("Sieve/Particle size ", i*10+3, ":",sep=""), paste("Sieve/Particle size ", i*10+4, ":",sep=""), paste("Sieve/Particle size ", i*10+5, ":",sep=""), paste("Sieve/Particle size ", i*10+6, ":",sep=""), paste("Sieve/Particle size ", i*10+7, ":",sep=""), paste("Sieve/Particle size ", i*10+8, ":",sep=""),paste("Sieve/Particle size ", i*10+9, ":",sep=""), paste("Sieve/Particle size ", i*10+10, ":",sep="")), title="Sieve/Particle sizes of the new database",prompt=paste("Define the consecutive sieve sizes from the smallest (Sieve/\nParticle size 1) to the largest (Sieve/Particle size",dnum,"):"),cancellab='Back to Definition of number of sieves or particle sizes')
        if(is.null(vals)) { break }
        
        dsizes[i*10+1] <- as.numeric(vals[1])
        dsizes[i*10+2] <- as.numeric(vals[2])
        dsizes[i*10+3] <- as.numeric(vals[3])
        dsizes[i*10+4] <- as.numeric(vals[4])
        dsizes[i*10+5] <- as.numeric(vals[5])
        dsizes[i*10+6] <- as.numeric(vals[6])
        dsizes[i*10+7] <- as.numeric(vals[7])
        dsizes[i*10+8] <- as.numeric(vals[8])
        dsizes[i*10+9] <- as.numeric(vals[9])
        dsizes[i*10+10] <- as.numeric(vals[10])
      
      } else if (i*10+9 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9'), labels=c(paste("Sieve/Particle size ", i*10+1, ":",sep=""),paste("Sieve/Particle size ", i*10+2, ":",sep=""),paste("Sieve/Particle size ", i*10+3, ":",sep=""),paste("Sieve/Particle size ", i*10+4, ":",sep=""),paste("Sieve/Particle size ", i*10+5, ":",sep=""),paste("Sieve/Particle size ", i*10+6, ":",sep=""),paste("Sieve/Particle size ", i*10+7, ":",sep=""),paste("Sieve/Particle size ", i*10+8, ":",sep=""),paste("Sieve/Particle size ", i*10+9, ":",sep="")), title="Sieve/Particle sizes of the new database",prompt=paste("Define the consecutive sieve sizes from the smallest (Sieve/\nParticle size 1) to the largest (Sieve/Particle size",dnum,"):"),cancellab='Back to Definition of number of sieves or particle sizes')
        if(is.null(vals)) { break }
        
        dsizes[i*10+1] <- as.numeric(vals[1])
        dsizes[i*10+2] <- as.numeric(vals[2])
        dsizes[i*10+3] <- as.numeric(vals[3])
        dsizes[i*10+4] <- as.numeric(vals[4])
        dsizes[i*10+5] <- as.numeric(vals[5])
        dsizes[i*10+6] <- as.numeric(vals[6])
        dsizes[i*10+7] <- as.numeric(vals[7])
        dsizes[i*10+8] <- as.numeric(vals[8])
        dsizes[i*10+9] <- as.numeric(vals[9])
        
      } else if (i*10+8 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8'), labels=c(paste("Sieve/Particle size ", i*10+1, ":",sep=""),paste("Sieve/Particle size ", i*10+2, ":",sep=""),paste("Sieve/Particle size ", i*10+3, ":",sep=""),paste("Sieve/Particle size ", i*10+4, ":",sep=""),paste("Sieve/Particle size ", i*10+5, ":",sep=""),paste("Sieve/Particle size ", i*10+6, ":",sep=""),paste("Sieve/Particle size ", i*10+7, ":",sep=""),paste("Sieve/Particle size ", i*10+8, ":",sep="")), title="Sieve/Particle sizes of the new database",prompt=paste("Define the consecutive sieve sizes from the smallest (Sieve/\nParticle size 1) to the largest (Sieve/Particle size",dnum,"):"),cancellab='Back to Definition of number of sieves or particle sizes')
        if(is.null(vals)) { break }
        
        dsizes[i*10+1] <- as.numeric(vals[1])
        dsizes[i*10+2] <- as.numeric(vals[2])
        dsizes[i*10+3] <- as.numeric(vals[3])
        dsizes[i*10+4] <- as.numeric(vals[4])
        dsizes[i*10+5] <- as.numeric(vals[5])
        dsizes[i*10+6] <- as.numeric(vals[6])
        dsizes[i*10+7] <- as.numeric(vals[7])
        dsizes[i*10+8] <- as.numeric(vals[8])
        
      } else if (i*10+7 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7'), labels=c(paste("Sieve/Particle size ", i*10+1, ":",sep=""),paste("Sieve/Particle size ", i*10+2, ":",sep=""),paste("Sieve/Particle size ", i*10+3, ":",sep=""),paste("Sieve/Particle size ", i*10+4, ":",sep=""),paste("Sieve/Particle size ", i*10+5, ":",sep=""),paste("Sieve/Particle size ", i*10+6, ":",sep=""),paste("Sieve/Particle size ", i*10+7, ":",sep="")), title="Sieve/Particle sizes of the new database",prompt=paste("Define the consecutive sieve sizes from the smallest (Sieve/\nParticle size 1) to the largest (Sieve/Particle size",dnum,"):"),cancellab='Back to Definition of number of sieves or particle sizes')
        if(is.null(vals)) { break }
        
        dsizes[i*10+1] <- as.numeric(vals[1])
        dsizes[i*10+2] <- as.numeric(vals[2])
        dsizes[i*10+3] <- as.numeric(vals[3])
        dsizes[i*10+4] <- as.numeric(vals[4])
        dsizes[i*10+5] <- as.numeric(vals[5])
        dsizes[i*10+6] <- as.numeric(vals[6])
        dsizes[i*10+7] <- as.numeric(vals[7])
        
      } else if (i*10+6 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6'), labels=c(paste("Sieve/Particle size ", i*10+1, ":",sep=""),paste("Sieve/Particle size ", i*10+2, ":",sep=""),paste("Sieve/Particle size ", i*10+3, ":",sep=""),paste("Sieve/Particle size ", i*10+4, ":",sep=""),paste("Sieve/Particle size ", i*10+5, ":",sep=""),paste("Sieve/Particle size ", i*10+6, ":",sep="")), title="Sieve/Particle sizes of the new database",prompt=paste("Define the consecutive sieve sizes from the smallest (Sieve/\nParticle size 1) to the largest (Sieve/Particle size",dnum,"):"),cancellab='Back to Definition of number of sieves or particle sizes')
        if(is.null(vals)) { break }
        
        dsizes[i*10+1] <- as.numeric(vals[1])
        dsizes[i*10+2] <- as.numeric(vals[2])
        dsizes[i*10+3] <- as.numeric(vals[3])
        dsizes[i*10+4] <- as.numeric(vals[4])
        dsizes[i*10+5] <- as.numeric(vals[5])
        dsizes[i*10+6] <- as.numeric(vals[6])
        
      } else if (i*10+5 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5'), labels=c(paste("Sieve/Particle size ", i*10+1, ":",sep=""),paste("Sieve/Particle size ", i*10+2, ":",sep=""),paste("Sieve/Particle size ", i*10+3, ":",sep=""),paste("Sieve/Particle size ", i*10+4, ":",sep=""),paste("Sieve/Particle size ", i*10+5, ":",sep="")), title="Sieve/Particle sizes of the new database",prompt=paste("Define the consecutive sieve sizes from the smallest (Sieve/\nParticle size 1) to the largest (Sieve/Particle size",dnum,"):"),cancellab='Back to Definition of number of sieves or particle sizes')
        if(is.null(vals)) { break }
        
        dsizes[i*10+1] <- as.numeric(vals[1])
        dsizes[i*10+2] <- as.numeric(vals[2])
        dsizes[i*10+3] <- as.numeric(vals[3])
        dsizes[i*10+4] <- as.numeric(vals[4])
        dsizes[i*10+5] <- as.numeric(vals[5])
        
      } else if (i*10+4 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4'), labels=c(paste("Sieve/Particle size ", i*10+1, ":",sep=""),paste("Sieve/Particle size ", i*10+2, ":",sep=""),paste("Sieve/Particle size ", i*10+3, ":",sep=""),paste("Sieve/Particle size ", i*10+4, ":",sep="")), title="Sieve/Particle sizes of the new database",prompt=paste("Define the consecutive sieve sizes from the smallest (Sieve/\nParticle size 1) to the largest (Sieve/Particle size",dnum,"):"),cancellab='Back to Definition of number of sieves or particle sizes')
        if(is.null(vals)) { break }
        
        dsizes[i*10+1] <- as.numeric(vals[1])
        dsizes[i*10+2] <- as.numeric(vals[2])
        dsizes[i*10+3] <- as.numeric(vals[3])
        dsizes[i*10+4] <- as.numeric(vals[4])
        
      } else if (i*10+3 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3'), labels=c(paste("Sieve/Particle size ", i*10+1, ":",sep=""),paste("Sieve/Particle size ", i*10+2, ":",sep=""),paste("Sieve/Particle size ", i*10+3, ":",sep="")), title="Sieve/Particle sizes of the new database",prompt=paste("Define the consecutive sieve sizes from the smallest (Sieve/\nParticle size 1) to the largest (Sieve/Particle size",dnum,"):"),cancellab='Back to Definition of number of sieves or particle sizes')
        if(is.null(vals)) { break }
        
        dsizes[i*10+1] <- as.numeric(vals[1])
        dsizes[i*10+2] <- as.numeric(vals[2])
        dsizes[i*10+3] <- as.numeric(vals[3])
        
      } else if (i*10+2 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2'), labels=c(paste("Sieve/Particle size ", i*10+1, ":",sep=""),paste("Sieve/Particle size ", i*10+2, ":",sep="")), title="Sieve/Particle sizes of the new database",prompt=paste("Define the consecutive sieve sizes from the smallest (Sieve/\nParticle size 1) to the largest (Sieve/Particle size",dnum,"):"),cancellab='Back to Definition of number of sieves or particle sizes')
        if(is.null(vals)) { break }
        
        dsizes[i*10+1] <- as.numeric(vals[1])
        dsizes[i*10+2] <- as.numeric(vals[2])
        
      } else {
        vals <- varEntryDialog(vars=c('d1'), labels=c(paste("Sieve/Particle size ", i*10+1, ":",sep="")), title="Sieve/Particle sizes of the new database",prompt=paste("Define the consecutive sieve sizes from the smallest (Sieve/\nParticle size 1) to the largest (Sieve/Particle size",dnum,"):"),cancellab='Back to Definition of number of sieves or particle sizes')
        if(is.null(vals)) { break }
        
        dsizes[i*10+1] <- as.numeric(vals[1])
      }
      
    } #for
    if(is.null(vals)) { return (2) } #if cancelled, back to dnum definition
    
    #message(" Input component sizes:",dsizes,"\n")

    } #end else sizedef input
    
    
    
    #add first material:
    while (TRUE) {
    
    message("  Add first material\n")
    
      windb <- tktoplevel(bg=hintergrund)
  tkwm.title(windb,"Add first material to new database")
  
  tkraise(windb)

  db <- tclVar(0)

  tkbind(windb,"<Destroy>",function() tclvalue(db)<-1) 
  
  Menu <- tkmenu(windb, bg=menue)           
  tkconfigure(windb, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","03addmat1","03.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())
  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  tkadd(QuMenu, "command", label = "Definition of sieves/particle sizes", command = function() tclvalue(db)<-3)
  tkadd(QuMenu, "command", label = "Database path and name selection", command = function() tclvalue(db)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(db)<-1)
  
  tkgrid(tklabel(windb, bg=hintergrund, text="How to add the material's\nparticle size data?"), pady=10, padx=10, columnspan=2)
  
  rb1 <- tkradiobutton(windb)
  rb2 <- tkradiobutton(windb)
  rb3 <- tkradiobutton(windb)
  rb4 <- tkradiobutton(windb)
  
  rbValue <- tclVar(3)
  
  tkconfigure(rb1,variable=rbValue,value=1)
  tkconfigure(rb2,variable=rbValue,value=2)
  tkconfigure(rb3,variable=rbValue,value=3)
  tkconfigure(rb4,variable=rbValue,value=4)
  
  tkgrid(tklabel(windb,text="Retention Density-curve"), rb1, pady=10, padx=10, columnspan=2)
  tkgrid(tklabel(windb,text="Passthrough Density-curve"), rb2, pady=10, padx=10, columnspan=2)
  tkgrid(tklabel(windb,text="Passthrough Sum-curve (CPFT)"), rb3, pady=10, padx=10, columnspan=2)
  tkgrid(tklabel(windb,text="Retention sum-curve"), rb4, pady=5, padx=10, columnspan=2)
  
  tkgrid(tkbutton(windb, text='Input\nmanually', bg=knoepfe, command=function() tclvalue(db)<-4), tkbutton(windb, text='Import\nfrom CSV', bg=knoepfe, command=function() tclvalue(db)<-5), pady=10, padx=10, columnspan=2)
  
  tkgrid(ttkseparator(windb), columnspan=12, pady=10, padx=10, sticky="we")
  
  tkgrid(tklabel(windb,text="Or import the material data"), tkbutton(windb, text='From another\ndatabase', bg=knoepfe, command=function() tclvalue(db)<-6), pady=10, padx=10, columnspan=2)
  
  
  #warum alle 4 varianten als import, aber nicht als manually... eher vlt erst variante waehlen und dann entscheiden ob import oder manually. und eignetlich bei beiden interpolieren...
  
  tkfocus(windb)

  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(db)

  var2 <- tclvalue(db)
  typadd <- tclvalue(rbValue)
  
  tkdestroy(windb)
  
  message("  Chosen action (1 Main Menu, 2 Database selection, 3 Component sizes definition, 4 Input first material, 5 Import from CSV, 6 Import from database): ",var2)
  message("  Chosen particle size distribution type (1 Retentin-Density, 2 passthrough-Density, 3 Passthrough-Sum, 4 Retention-Sum): ",typadd,"\n")
  
  if (var2 < 4) {
    appdb <- var2
    break
  }
    
   
    
  appdb <- addmatdb(dbfile, dnum, dsizes, typadd, var2)
  message("END add first material funtion\n")
  
  if (appdb < 4) { break } 
  
 
    
    
    }#end while add material
    
    
    if (appdb < 3) { break }
    
    
    
    }#end while define sieve sizes
        
    return (appdb)    
    #1 for main menu; 0 for quit; 2 for again dnum/dbfile definition should not be allowed, I think, because also called from other functions // either adding more, or back to main, after once new database was fully defined.
    
    
}#end createdb-function



addmatdb <- function(dbfile, dnum, dsizes, typadd, var2) {
#input:
#dbfile : db-path-and-file string
#dnum : number of sieves/components
#dsizes : sieve sizes (vector legth = dnum)
#typadd : 1 = Retention Density-curve; 2 = Passthrough Density-curve, 3 = Passthrough Sum-curve (CPFT); 4 = Retention sum-curve
#var2 : 4 = input manually; 5 = import from csv

message("BEGIN add first material function\n")

#while (TRUE) { #braucht man hier nciht mehr; geht wenn dann eh immer zu vorigem dialog zurueck. und nachdem s geschafft ist, geht s zum hauptmenu.

if (typadd == 1) {
addtitle <- "Retention density-curve"
addtext <- "Retention on sole sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 0% to 0%!):"
} else if (typadd == 2) {
addtitle <- "Passthrough density-curve"
addtext <- "Pass through sole sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 0% to 0%!):"
} else if (typadd == 3) {
addtitle <- "Passthrough sum-curve (CPFT)"
addtext <- "Cumulative pass through (CPFT) sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 0% to 100%):"
} else {
addtitle <- "Retention sum-curve"
addtext <- "Cumulative retention on sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 100% to 0%):"
}





if (var2==6) { #import from other db

#choose other db
#choose material from other db


db2file <- tk_choose.files(caption="Select database from which to import material", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))   
    if (length(dbfile) == 0) { return(4) }
    
    message("  Import particle size data from other database: ",db2file)

db2info <- read.csv2(file=db2file,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

db2data <- read.csv2(file=db2file,skip=19,header=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
    colnames(db2data) <- colnames(db2info)
    
    #dnum2 <- length(dbdata$Identifier) #required?
    #dsizes2 <- dbdata$Identifier #required?

MatList2 <- colnames(db2info)[2:length(colnames(db2info))]

id <- tk_select.list(choices=MatList2, preselect=NULL, multiple=FALSE, title="Select material to import:")
    if (id == "") { return(4) }
   
   message("  Import material: ",id)
   
   sizemat2 <- data.frame(cbind(db2data$Identifier,db2data[,id]))
   colnames(sizemat2) <- c("Diameter","Percent")
   #print(sizemat2)   
   
   
   sizeCPFT <- vector(length=length(sizemat2$Diameter))
    sizeCPFT[1] <- 0
    for(x in seq(from=2, to=length(sizemat2$Diameter), by=1)) {
      sizeCPFT[x] <- sizemat2$Percent[x-1]+sizeCPFT[x-1]
    }
    
    sizemat <- data.frame(cbind(sizemat2$Diameter,sizeCPFT))
   colnames(sizemat) <- c("Diameter","Percent")
   #print(sizemat)
   
   sizeinput <- data.frame(cbind(dsizes,rep(NA, dnum)))
    colnames(sizeinput) <- c("Diameter","Percent")
    
    #print(sizeinput)
    
    #print(matdata)
    #print(sizeinput)
    
    matmerge <- merge(sizemat,sizeinput,all=TRUE)
    
    #print(matmerge)
    
    matmerge <- matmerge[!duplicated(matmerge$Diameter), ]
    
    #print(matmerge)
    
    #put first and last percent if necessary
    
    if (is.na(matmerge$Percent[1])) {
      for (m in seq(from=1, to=length(matmerge$Diameter), by=1)) {
        if (!is.na(matmerge$Percent[m])) { 
          matmerge$Percent[1] <- matmerge$Percent[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    if (is.na(matmerge$Percent[length(matmerge$Diameter)])) {
      for (m in seq(from=length(matmerge$Diameter), to=1, by=-1)) {
        if (!is.na(matmerge$Percent[m])) { 
          matmerge$Percent[length(matmerge$Diameter)] <- matmerge$Percent[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    
    #lineares fuellen
    
    i=1
    while (i < length(matmerge$Diameter)) {#<=
    
      if (is.na(matmerge$Percent[i])) {#beginn NA ist element VolMat[i] falls TRUE; VolMat[i-1] unterer Wert fuer Lienarisierung
        for (j in seq(from=i+1, to=length(matmerge$Diameter), by=1)) {
          if (!is.na(matmerge$Percent[j])) {#erstes element nach i, was kein NA mehr ist, ist j; damit VolMat[j] oberer Wert fuer Linearisierung
            
            #Linearisierung fuer alle elemente zwischen i-1 und j
            for (k in seq(from=i, to=j-1, by=1)) {
              matmerge$Percent[k] <- matmerge$Percent[i-1] + (matmerge$Diameter[k]-matmerge$Diameter[i-1])*(matmerge$Percent[j]-matmerge$Percent[i-1])/(matmerge$Diameter[j]-matmerge$Diameter[i-1])
            }
            
            i=j+1#oder ohne +1?
            break #out of for j in seq-loop
            
          }
        }#end for j in seq
        
      } else {
       i=i+1
      }
      
    }#end while i < length(matmerge$Diameter)
    
    #print(matmerge)
    
    #print(dsizes)
    #next: kep only dsizes resp sizeinput
    #bei kumulativ okay, aber nicht bei RETENTION!!!
    matcrop <- matmerge[matmerge$Diameter %in% dsizes,]
    
    #print(matcrop)
    
    
    #CPFT -> RETENTION
    
    VolMat <- vector(length=dnum)    
      
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMat[x] <- matcrop$Percent[x+1]-matcrop$Percent[x]
      }
      VolMat[dnum] <- 0
    
    
    #print(VolMat)
    #VolMat <- matcrop$Percent
   
   
   
   
   
   #CONSTRUCT and SAVE - col1 und col2 required
    
mathead <- c('Identifier','Name','Last modified','Origin/Supplier','Date supplied','Price per MT','True density','Date measured','Measurement method','Specific surface area','Date measured','Measurement method','d10','d25','d50','d75','d90','Date measured','Measurement method')

      #matvars <- c('ID','Name','Last','Origin','Date','Price','Density','DenDate','DenMeas','SSA','SSADate','SSAMeas','d10','d25','d50','d75','d90','dDate','dMeas')

      #matlabs <- c('Unique identifier:','Material name:','Last modified:','Origin/Supplier:','Date supplied:','Price per MT:','True density:','Date measured (Density):','Measurement method (Density):','Specific surface area (SSA):','Date measured (SSA):','Measurement method (SSA):',paste('d(10%) in',tclvalue(dunit),':'),paste('d(25%) in',tclvalue(dunit),':'),paste('d(50%) in',tclvalue(dunit),':'),paste('d(75%) in',tclvalue(dunit),':'),paste('d(90%) in',tclvalue(dunit),':'),'Date measured (Particle sizes):','Measurement method (Particle sizes):')
    
      matpre <- vector(length=19)
      matpre[1] <- id
      for (o in c(2,3,4,5,8,9,11,12,18,19)) {
        if (length(db2info[o-1,id]) == 0) {
          matpre[o] <- ''
        } else {
          matpre[o] <- as.character(db2info[o-1,id])
        }
      }
      for (q in c(6,7,10,13,14,15,16,17)) {
        if (length(db2info[q-1,id]) == 0) {
          matpre[q] <- ''
        } else {
          matpre[q] <- gsub(",",".",db2info[q-1,id]) #...mit as.numeric( gsub ) werdens zahlen statt umformatierten textes
        }
      }
    
test2 <- gsub("NA","",matpre)
    
    
    #make database; save material
    
    col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
col2 <- c(test2[2:length(mathead)],format(round(VolMat, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo

    



} else {#end import from other db; start input or import data separately


if (var2 == 4) {#input manually

message("  Input particle size data manually")

    VolMat <- vector(length=dnum)
    winnr <- ceiling(dnum/10)
    for (i in seq(from=0, to=winnr-1, by=1)) { #bis -1 weil von 0; 0 necessary due to condition-types with i*10+INPUTlineNUMBER
      
      if (i*10+10 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9','d10'), labels=c(
        paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+8], tclvalue(dunit), ":",sep=""),
        paste(addtext, dsizes[i*10+9], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+10], tclvalue(dunit), ":",sep="")), 
        title=addtitle,
        prompt=addpromt,
        cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        VolMat[i*10+6] <- as.numeric(vals[6])
        VolMat[i*10+7] <- as.numeric(vals[7])
        VolMat[i*10+8] <- as.numeric(vals[8])
        VolMat[i*10+9] <- as.numeric(vals[9])
        VolMat[i*10+10] <- as.numeric(vals[10])
      
      } else if (i*10+9 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+8], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+9], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        VolMat[i*10+6] <- as.numeric(vals[6])
        VolMat[i*10+7] <- as.numeric(vals[7])
        VolMat[i*10+8] <- as.numeric(vals[8])
        VolMat[i*10+9] <- as.numeric(vals[9])
        
      } else if (i*10+8 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+8], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        VolMat[i*10+6] <- as.numeric(vals[6])
        VolMat[i*10+7] <- as.numeric(vals[7])
        VolMat[i*10+8] <- as.numeric(vals[8])
        
      } else if (i*10+7 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        VolMat[i*10+6] <- as.numeric(vals[6])
        VolMat[i*10+7] <- as.numeric(vals[7])
        
      } else if (i*10+6 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        VolMat[i*10+6] <- as.numeric(vals[6])
        
      } else if (i*10+5 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        
      } else if (i*10+4 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        
      } else if (i*10+3 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        
      } else if (i*10+2 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        
      } else {
        vals <- varEntryDialog(vars=c('d1'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
      }
      
    } #for
    if(is.null(vals)) { return (4) } #if cancelled, back to amount input type selection
    
    
    #put first and last amount if necessary
    
    #if (is.na(VolMat[1]) || is.na(VolMat[dnum])) {
    #  tkmessageBox(title ="Missing values", message="First or last amount were not given, but are mandatory. Returning to How to add material's particle size data.", icon = "error", type = "ok")
    #  return (4) #back to amount input type selection
    #}
    
    if (is.na(VolMat[1])) {
      for (m in seq(from=1, to=length(VolMat), by=1)) {
        if (!is.na(VolMat[m])) { 
          VolMat[1] <- VolMat[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    if (is.na(VolMat[length(VolMat)])) {
      for (m in seq(from=length(VolMat), to=1, by=-1)) {
        if (!is.na(VolMat[m])) { 
          VolMat[length(VolMat)] <- VolMat[m]
          break
        }
      }
    }
    
    
    
    
    #NAs linear ausgleichen?
    
    if (!all(!is.na(VolMat))) {#wenn min 1 NA, dann: linearisierende fuellung
      tkmessageBox(title ="Missing values", message="At least one missing value was detected. A linear interpolation follows. If this is not wanted but you want to input all values correctly, go 'Back to How to add material's particle size data' in the next dialog.", icon = "warning", type = "ok")
    #  return (4) #back to amount input type selection
    }
    
    #print(VolMat)
    i=1
    while (i < dnum) {#<=
    
      if (is.na(VolMat[i])) {#beginn NA ist element VolMat[i] falls TRUE; VolMat[i-1] unterer Wert fuer Lienarisierung
        for (j in seq(from=i+1, to=dnum, by=1)) {
          if (!is.na(VolMat[j])) {#erstes element nach i, was kein NA mehr ist, ist j; damit VolMat[j] oberer Wert fuer Linearisierung
            
            #Linearisierung fuer alle elemente zwischen i-1 und j
            for (k in seq(from=i, to=j-1, by=1)) {
              VolMat[k] <- VolMat[i-1] + (dsizes[k]-dsizes[i-1])*(VolMat[j]-VolMat[i-1])/(dsizes[j]-dsizes[i-1])
            }
            
            i=j+1#oder ohne +1?
            break #out of for j in seq-loop
            
          }
        }#end for j in seq
        
      } else {
       i=i+1
      }
      
    }#end while i < dnum
    #print(VolMat)
    
    
    
    
    #wenn hiernach normalisiert wird fuer die density-eingaben, muesste es auch gehen (bei input werden keine daten weggeschnitten)
    #die andere variante mit vorher cpft berechnen geht auhc nicht, weil ja NAs uU drin stehen, und die muessen erst weg. und die gehen weg durch die normalisierun
    
    #anders bei den sachen aus ner datei, wo fuer das datei-datenset durchaus die cpfts berechnet werden koennen wie oben
    
    if (typadd == 1 || typadd == 2) {
    
      norm <- sum(VolMat)
      VolMat <- 100*VolMat/norm
    
    }

    
    
    #hier weiter mit umrechnungen, zu... retnetion on sole sieve/component (retention density curve), wie in database gespeichert
    #umrechnung fuer cpft -> retention density schon in verify.R
    
    #typadd : 1 = Retention Density-curve; 2 = Passthrough Density-curve, 3 = Passthrough Sum-curve (CPFT); 4 = Retention sum-curve
    
    VolMatMod <- vector(length=dnum)    

    if (typadd == 1) {
    
      VolMatMod <- VolMat
      
    } else if (typadd == 2) {
    
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatMod[x] <- VolMat[x+1]
      }
      VolMatMod[dnum] <- 0
    
    } else if (typadd == 3) {
      
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatMod[x] <- VolMat[x+1]-VolMat[x]
      }
      VolMatMod[dnum] <- 0
    
    } else {
    
      
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatMod[x] <- VolMat[x]-VolMat[x+1]
      }
      VolMatMod[dnum] <- 0
      
    }
    
    
    
    
    
    
    
} else { #end if input manually; start else import from csv

    matfile <- tk_choose.files(caption="Select material data file", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))   
    if (length(matfile) == 0) { return(4) }
    
    message("  Import particle size data from raw amterial file: ",matfile)
   
    #requirements on file: two columns 'Diameter' and 'Percent'; no NAs, words, ... only numbers 
   
    matdata <- read.csv2(file=matfile,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

    #check for every dsize, if it is included in matdata$Diameter. If not, add row (Percent=NA).
    
    
    #cpft berechnen
    
matcpft <- vector(length=length(matdata$Diameter))    

    if (typadd == 1) {
    
      matcpft[1] <- 0
    for(x in seq(from=2, to=length(matdata$Diameter), by=1)) {
      matcpft[x] <- matdata$Percent[x-1]+matcpft[x-1]
    }
      
    } else if (typadd == 2) {
    
      matcpft[1] <- matdata$Percent[1]
    for(x in seq(from=2, to=length(matdata$Diameter), by=1)) {
      matcpft[x] <- matdata$Percent[x]+matcpft[x-1]
    }
    
    } else if (typadd == 3) {
      
      matcpft <- matdata$Percent
    
    } else {
    
      matcpft <- 100-matdata$Percent
      
    }    
    
   
    
    sizemat <- data.frame(cbind(matdata$Diameter,matcpft))
   colnames(sizemat) <- c("Diameter","Percent")
   #print(sizemat)
   
   sizeinput <- data.frame(cbind(dsizes,rep(NA, dnum)))
    colnames(sizeinput) <- c("Diameter","Percent")
    
    #print(sizeinput)
    
    #print(matdata)
    #print(sizeinput)
    
    matmerge <- merge(sizemat,sizeinput,all=TRUE)
    
    #print(matmerge)
    
    
   
    
    
    
    matmerge <- matmerge[!duplicated(matmerge$Diameter), ]
    
    #print(matmerge)
    
    #put first and last percent if necessary
    
    if (is.na(matmerge$Percent[1])) {
      for (m in seq(from=1, to=length(matmerge$Diameter), by=1)) {
        if (!is.na(matmerge$Percent[m])) { 
          matmerge$Percent[1] <- matmerge$Percent[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    if (is.na(matmerge$Percent[length(matmerge$Diameter)])) {
      for (m in seq(from=length(matmerge$Diameter), to=1, by=-1)) {
        if (!is.na(matmerge$Percent[m])) { 
          matmerge$Percent[length(matmerge$Diameter)] <- matmerge$Percent[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    
    #lineares fuellen
    
    i=1
    while (i < length(matmerge$Diameter)) {#<=
    
      if (is.na(matmerge$Percent[i])) {#beginn NA ist element VolMat[i] falls TRUE; VolMat[i-1] unterer Wert fuer Lienarisierung
        for (j in seq(from=i+1, to=length(matmerge$Diameter), by=1)) {
          if (!is.na(matmerge$Percent[j])) {#erstes element nach i, was kein NA mehr ist, ist j; damit VolMat[j] oberer Wert fuer Linearisierung
            
            #Linearisierung fuer alle elemente zwischen i-1 und j
            for (k in seq(from=i, to=j-1, by=1)) {
              matmerge$Percent[k] <- matmerge$Percent[i-1] + (matmerge$Diameter[k]-matmerge$Diameter[i-1])*(matmerge$Percent[j]-matmerge$Percent[i-1])/(matmerge$Diameter[j]-matmerge$Diameter[i-1])
            }
            
            i=j+1#oder ohne +1?
            break #out of for j in seq-loop
            
          }
        }#end for j in seq
        
      } else {
       i=i+1
      }
      
    }#end while i < length(matmerge$Diameter)
    
    #print(matmerge)
    
    
    #next: kep only dsizes resp sizeinput
    matcrop <- matmerge[matmerge$Diameter %in% dsizes,]
    
    #print(matcrop)
    
    #VolMat <- matcrop$Percent
    
    #print(VolMat)
    
   VolMatMod <- vector(length=dnum)    
      
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatMod[x] <- matcrop$Percent[x+1]-matcrop$Percent[x]
      }
      VolMatMod[dnum] <- 0 
      
      
      

}#end else import from csv







    



    #calculate d10, d25, d50, d75, d90 from cpft
    MatCPFT <- vector(length=dnum)
    MatCPFT[1] <- 0
    for(x in seq(from=2, to=dnum, by=1)) {
      MatCPFT[x] <- VolMatMod[x-1]+MatCPFT[x-1]
    }
    
    d10 <- 0
d25 <- 0
d50 <- 0
d75 <- 0
d90 <- 0

for (v in seq(from=1, to=dnum, by=1)) {
 
 if (d10 == 0 && MatCPFT[v] > 10) { #interpolate with value before for d10
  d10 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (10 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d25 == 0 && MatCPFT[v] > 25) { #single if's because it s also possible that multiple values have to be interpolated between the same pair of data
  d25 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (25 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d50 == 0 && MatCPFT[v] > 50) { 
  d50 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (50 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d75 == 0 && MatCPFT[v] > 75) { 
  d75 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (75 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d90 == 0 && MatCPFT[v] > 90) { 
  d90 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (90 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
}
    
    
    #show material information dialog (siehe ab zeile 902 in verify.R)

    #get infos
mathead <- c('Identifier','Name','Last modified','Origin/Supplier','Date supplied','Price per MT','True density','Date measured','Measurement method','Specific surface area','Date measured','Measurement method','d10','d25','d50','d75','d90','Date measured','Measurement method')

matvars <- c('ID','Name','Last','Origin','Date','Price','Density','DenDate','DenMeas','SSA','SSADate','SSAMeas','d10','d25','d50','d75','d90','dDate','dMeas')

matlabs <- c('Unique identifier (Required!):','Material name:','Last modified:','Origin/Supplier:','Date supplied:','Price per MT:','True density:','Date measured (Density):','Measurement method (Density):','Specific surface area (SSA):','Date measured (SSA):','Measurement method (SSA):',paste('d(10%) in',tclvalue(dunit),':'),paste('d(25%) in',tclvalue(dunit),':'),paste('d(50%) in',tclvalue(dunit),':'),paste('d(75%) in',tclvalue(dunit),':'),paste('d(90%) in',tclvalue(dunit),':'),'Date measured (Particle sizes):','Measurement method (Particle sizes):')

matpre <- c(paste('Material ',paste(Sys.time())),'',paste(Sys.Date()),'','','','','','','','','',round(d10, digits=accuracy),round(d25, digits=accuracy),round(d50, digits=accuracy),round(d75, digits=accuracy),round(d90, digits=accuracy),'','')

matinfo <- varEntryDialog(vars=matvars, labels=matlabs, title="Material information",prompt="Adjust material information:",preset=matpre,cancellab="Back to How to add material's particle size data")
    if(is.null(matinfo)) { return(4) }#if input ok, go on, if not return ?!???

    
    id <- as.character(matinfo[1])
    nam <- as.character(matinfo[2])
    last <- as.character(matinfo[3])
    orig <- as.character(matinfo[4])
    date <- as.character(matinfo[5])
    price <- format(as.numeric(matinfo[6]), decimal.mark=tclvalue(decpoint), na.encode=F)
    td <- format(as.numeric(matinfo[7]), decimal.mark=tclvalue(decpoint), na.encode=F)
    tddate <- as.character(matinfo[8])
    tdmeas <- as.character(matinfo[9])
    ssa <- format(as.numeric(matinfo[10]), decimal.mark=tclvalue(decpoint), na.encode=F)
    ssadate <- as.character(matinfo[11])
    ssameas <- as.character(matinfo[12])
    d10s <- format(as.numeric(matinfo[13]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d25s <- format(as.numeric(matinfo[14]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d50s <- format(as.numeric(matinfo[15]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d75s <- format(as.numeric(matinfo[16]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d90s <- format(as.numeric(matinfo[17]), decimal.mark=tclvalue(decpoint), na.encode=F)
    ddate <- as.character(matinfo[18])
    dmeas <- as.character(matinfo[19])
    #dmax <- as.numeric(vals[1])
    #n <- as.numeric(vals[2])
    
    materinfo <- c(id,nam,last,orig,date,price,td,tddate,tdmeas,ssa,ssadate,ssameas,d10s,d25s,d50s,d75s,d90s,ddate,dmeas)
    test2 <- gsub("NA","",materinfo)
    
    
    #make database; save material
    
    col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
col2 <- c(test2[2:length(mathead)],format(round(VolMatMod, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo



}#end else input or import particle data separately


newmatdb <- data.frame(cbind(col1,col2), row.names=1:length(col1))
newmatdb <- newmatdb[,2, drop=F]
#print(newmatdb)

#save dataframe with new first line (=col.names)
write.table(newmatdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", col.names=paste("Identifier;",id,sep=""), row.names=col1, quote=F)

message("  New database created with first material.")








#info that saved + question if another material should be added, or not; if yes, calling other function reopening database and adding new stuff

    addmore <- tkmessageBox(title = "New database created", message="The new database was succesfully created and the first material added. Do you wish to add more materials to this database now?", icon = "info", type = "yesno")

    message("  Add more materials to new database? Decision: ",addmore,"\n")
    if (tclvalue(addmore) == "yes") { 
      #call to other function
      
      addmaterial(dbfile)#theoretisch sollten dnum und dsizes aus datei entnommen werden koennen.. aber wann halt ist die frage
      message("END add material function\n")
      
    } #else {
      #return (1) #back to main menu????
    #}


#} #while input amounts per sieve

return (1)

#return:
#0 = quit; 1 = main menu
#2 = sieve number definition
#3 = sieve sizes definition
#4 = selection input type for amounts per sieve size

} #end addmatdb function












































addmaterial <- function(dbfile) {

  #siehe auch zeile 786ff in verify.R

  while (TRUE) {
  
  message("BEGIN add material function\n")
  
    #break fuer back to main menu
    #next fuer add another material
  
    dbinfo <- read.csv2(file=dbfile,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

    #read mass% values separately
    dbdata <- read.csv2(file=dbfile,skip=19,header=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
    colnames(dbdata) <- colnames(dbinfo)
    
    #print(dbinfo)
     
    MatList <- colnames(dbinfo)[2:length(colnames(dbinfo))]
  
    dnum <- length(dbdata$Identifier)
    dsizes <- dbdata$Identifier
    
    message("  Number of component sizes: ",dnum)
  
    #weiter ab zeile 133 dieser datei (dbfun.R)
    
    
    windb <- tktoplevel(bg=hintergrund)
  tkwm.title(windb,"Add material to database")
  
  tkraise(windb)

  db <- tclVar(0)

  tkbind(windb,"<Destroy>",function() tclvalue(db)<-1) 
  
  Menu <- tkmenu(windb, bg=menue)           
  tkconfigure(windb, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","06addmat","06.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())
  
  #QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "command", label = "Back to Main Menu", command = function() tclvalue(db)<-1)
  
  tkgrid(tklabel(windb, bg=hintergrund, text="How to add the material's\nparticle size data?"), pady=10, padx=10, columnspan=2)
  
  rb1 <- tkradiobutton(windb)
  rb2 <- tkradiobutton(windb)
  rb3 <- tkradiobutton(windb)
  rb4 <- tkradiobutton(windb)
  
  rbValue <- tclVar(3)
  
  tkconfigure(rb1,variable=rbValue,value=1)
  tkconfigure(rb2,variable=rbValue,value=2)
  tkconfigure(rb3,variable=rbValue,value=3)
  tkconfigure(rb4,variable=rbValue,value=4)
  
  tkgrid(tklabel(windb,text="Retention Density-curve"), rb1, pady=10, padx=10, columnspan=2)
  tkgrid(tklabel(windb,text="Passthrough Density-curve"), rb2, pady=10, padx=10, columnspan=2)
  tkgrid(tklabel(windb,text="Passthrough Sum-curve (CPFT)"), rb3, pady=5, padx=10, columnspan=2)
  tkgrid(tklabel(windb,text="Retention sum-curve"), rb4, pady=5, padx=10, columnspan=2)
  
  #OLD: (vor Leoben)
  #tkgrid(tkbutton(windb, text='Input\nmanually', bg=knoepfe, command=function() tclvalue(db)<-2), tkbutton(windb, text='Import\nfrom CSV', bg=knoepfe, command=function() tclvalue(db)<-3), pady=10, padx=10, columnspan=3)
  
  tkgrid(tkbutton(windb, text='Input\nmanually', bg=knoepfe, command=function() tclvalue(db)<-2), tkbutton(windb, text='Import\nfrom CSV', bg=knoepfe, command=function() tclvalue(db)<-3), pady=10, padx=10, columnspan=2)
  
  tkgrid(ttkseparator(windb), columnspan=12, pady=10, padx=10, sticky="we")
  
  tkgrid(tklabel(windb,text="Or import the material data"), tkbutton(windb, text='From another\ndatabase', bg=knoepfe, command=function() tclvalue(db)<-4), pady=10, padx=10, columnspan=2)
 
  #warum alle 4 varianten als import, aber nicht als manually... eher vlt erst variante waehlen und dann entscheiden ob import oder manually. und eignetlich bei beiden interpolieren...
  
  tkfocus(windb)

  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(db)

  var <- tclvalue(db)
  typadd <- tclvalue(rbValue)
  
  tkdestroy(windb)
  
    message("  Chosen action (1 Main Menu, 2 Input material, 3 Import from CSV, 4 Import from database): ",var)
  message("  Chosen particle size distribution type (1 Retentin-Density, 2 passthrough-Density, 3 Passthrough-Sum, 4 Retention-Sum): ",typadd,"\n")
  
  if (var < 2) {
    #appdb <- var
    break
  }
    
  #appdb <- addmatdb(dbfile, dnum, dsizes, typadd, var2)
  
  #if (appdb < 2) { break } 
  
  
if (typadd == 1) {
addtitle <- "Retention density-curve"
addtext <- "Retention on sole sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 0% to 0%!):"
} else if (typadd == 2) {
addtitle <- "Passthrough density-curve"
addtext <- "Pass through sole sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 0% to 0%!):"
} else if (typadd == 3) {
addtitle <- "Passthrough sum-curve (CPFT)"
addtext <- "Cumulative pass through (CPFT) sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 0% to 100%):"
} else {
addtitle <- "Retention sum-curve"
addtext <- "Cumulative retention on sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 100% to 0%):"
}







if (var==4) { #import from other db

#choose other db
#choose material from other db


db2file <- tk_choose.files(caption="Select database from which to import material", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))   
    if (length(dbfile) == 0) { return(4) }
    
      message("  Import from database: ",db2file)

db2info <- read.csv2(file=db2file,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

db2data <- read.csv2(file=db2file,skip=19,header=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
    colnames(db2data) <- colnames(db2info)
    
    #dnum2 <- length(dbdata$Identifier) #required?
    #dsizes2 <- dbdata$Identifier #required?

MatList2 <- colnames(db2info)[2:length(colnames(db2info))]

id <- tk_select.list(choices=MatList2, preselect=NULL, multiple=FALSE, title="Select material to import:")
    if (id == "") { next }
    
    message("  Import material: ",id)
       
#HIER WEITER
   #merge dsizes and volmat
   
   #print("merge:")
   
   
   
   sizemat2 <- data.frame(cbind(db2data$Identifier,db2data[,id]))
   colnames(sizemat2) <- c("Diameter","Percent")
   #print(sizemat2)   
   
   
   sizeCPFT <- vector(length=length(sizemat2$Diameter))
    sizeCPFT[1] <- 0
    for(x in seq(from=2, to=length(sizemat2$Diameter), by=1)) {
      sizeCPFT[x] <- sizemat2$Percent[x-1]+sizeCPFT[x-1]
    }
    
    sizemat <- data.frame(cbind(sizemat2$Diameter,sizeCPFT))
   colnames(sizemat) <- c("Diameter","Percent")
   #print(sizemat)
   
   sizeinput <- data.frame(cbind(dsizes,rep(NA, dnum)))
    colnames(sizeinput) <- c("Diameter","Percent")
    
    #print(sizeinput)
    
    #print(matdata)
    #print(sizeinput)
    
    matmerge <- merge(sizemat,sizeinput,all=TRUE)
    
    #print(matmerge)
    
    matmerge <- matmerge[!duplicated(matmerge$Diameter), ]
    
    #print(matmerge)
    
    #put first and last percent if necessary
    
    if (is.na(matmerge$Percent[1])) {
      for (m in seq(from=1, to=length(matmerge$Diameter), by=1)) {
        if (!is.na(matmerge$Percent[m])) { 
          matmerge$Percent[1] <- matmerge$Percent[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    if (is.na(matmerge$Percent[length(matmerge$Diameter)])) {
      for (m in seq(from=length(matmerge$Diameter), to=1, by=-1)) {
        if (!is.na(matmerge$Percent[m])) { 
          matmerge$Percent[length(matmerge$Diameter)] <- matmerge$Percent[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    
    #lineares fuellen
    
    i=1
    while (i < length(matmerge$Diameter)) {#<=
    
      if (is.na(matmerge$Percent[i])) {#beginn NA ist element VolMat[i] falls TRUE; VolMat[i-1] unterer Wert fuer Lienarisierung
        for (j in seq(from=i+1, to=length(matmerge$Diameter), by=1)) {
          if (!is.na(matmerge$Percent[j])) {#erstes element nach i, was kein NA mehr ist, ist j; damit VolMat[j] oberer Wert fuer Linearisierung
            
            #Linearisierung fuer alle elemente zwischen i-1 und j
            for (k in seq(from=i, to=j-1, by=1)) {
              matmerge$Percent[k] <- matmerge$Percent[i-1] + (matmerge$Diameter[k]-matmerge$Diameter[i-1])*(matmerge$Percent[j]-matmerge$Percent[i-1])/(matmerge$Diameter[j]-matmerge$Diameter[i-1])
            }
            
            i=j+1#oder ohne +1?
            break #out of for j in seq-loop
            
          }
        }#end for j in seq
        
      } else {
       i=i+1
      }
      
    }#end while i < length(matmerge$Diameter)
    
    #print(matmerge)
    
    
    #next: kep only dsizes resp sizeinput
    #bei kumulativ okay, aber nicht bei RETENTION!!!
    matcrop <- matmerge[matmerge$Diameter %in% dsizes,]
    
    #print(matcrop)
    
    
    #CPFT -> RETENTION
    
    VolMat <- vector(length=dnum)    
      
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMat[x] <- matcrop$Percent[x+1]-matcrop$Percent[x]
      }
      VolMat[dnum] <- 0
    
    
    #print(VolMat)
    #VolMat <- matcrop$Percent
   
   
   
   
   
   #CONSTRUCT and SAVE - col1 und col2 required
    
mathead <- c('Identifier','Name','Last modified','Origin/Supplier','Date supplied','Price per MT','True density','Date measured','Measurement method','Specific surface area','Date measured','Measurement method','d10','d25','d50','d75','d90','Date measured','Measurement method')

      #matvars <- c('ID','Name','Last','Origin','Date','Price','Density','DenDate','DenMeas','SSA','SSADate','SSAMeas','d10','d25','d50','d75','d90','dDate','dMeas')

      #matlabs <- c('Unique identifier:','Material name:','Last modified:','Origin/Supplier:','Date supplied:','Price per MT:','True density:','Date measured (Density):','Measurement method (Density):','Specific surface area (SSA):','Date measured (SSA):','Measurement method (SSA):',paste('d(10%) in',tclvalue(dunit),':'),paste('d(25%) in',tclvalue(dunit),':'),paste('d(50%) in',tclvalue(dunit),':'),paste('d(75%) in',tclvalue(dunit),':'),paste('d(90%) in',tclvalue(dunit),':'),'Date measured (Particle sizes):','Measurement method (Particle sizes):')
    
      matpre <- vector(length=19)
      matpre[1] <- id
      for (o in c(2,3,4,5,8,9,11,12,18,19)) {
        if (length(db2info[o-1,id]) == 0) {
          matpre[o] <- ''
        } else {
          matpre[o] <- as.character(db2info[o-1,id])
        }
      }
      for (q in c(6,7,10,13,14,15,16,17)) {
        if (length(db2info[q-1,id]) == 0) {
          matpre[q] <- ''
        } else {
          matpre[q] <- gsub(",",".",db2info[q-1,id]) #...mit as.numeric( gsub ) werdens zahlen statt umformatierten textes
        }
      }
    
test2 <- gsub("NA","",matpre)
    
    
    #make database; save material
    
    col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
col2 <- c(test2[2:length(mathead)],format(round(VolMat, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo

    



} else {#end import from other db; start input or import data separately








if (var == 2) {#input manually

message("  Input particle size data manually")

    VolMat <- vector(length=dnum)
    winnr <- ceiling(dnum/10)
    for (i in seq(from=0, to=winnr-1, by=1)) { #bis -1 weil von 0; 0 necessary due to condition-types with i*10+INPUTlineNUMBER
      
      if (i*10+10 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9','d10'), labels=c(
        paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+8], tclvalue(dunit), ":",sep=""),
        paste(addtext, dsizes[i*10+9], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+10], tclvalue(dunit), ":",sep="")), 
        title=addtitle,
        prompt=addpromt,
        cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        VolMat[i*10+6] <- as.numeric(vals[6])
        VolMat[i*10+7] <- as.numeric(vals[7])
        VolMat[i*10+8] <- as.numeric(vals[8])
        VolMat[i*10+9] <- as.numeric(vals[9])
        VolMat[i*10+10] <- as.numeric(vals[10])
      
      } else if (i*10+9 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+8], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+9], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        VolMat[i*10+6] <- as.numeric(vals[6])
        VolMat[i*10+7] <- as.numeric(vals[7])
        VolMat[i*10+8] <- as.numeric(vals[8])
        VolMat[i*10+9] <- as.numeric(vals[9])
        
      } else if (i*10+8 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+8], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        VolMat[i*10+6] <- as.numeric(vals[6])
        VolMat[i*10+7] <- as.numeric(vals[7])
        VolMat[i*10+8] <- as.numeric(vals[8])
        
      } else if (i*10+7 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        VolMat[i*10+6] <- as.numeric(vals[6])
        VolMat[i*10+7] <- as.numeric(vals[7])
        
      } else if (i*10+6 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        VolMat[i*10+6] <- as.numeric(vals[6])
        
      } else if (i*10+5 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        VolMat[i*10+5] <- as.numeric(vals[5])
        
      } else if (i*10+4 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        VolMat[i*10+4] <- as.numeric(vals[4])
        
      } else if (i*10+3 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        VolMat[i*10+3] <- as.numeric(vals[3])
        
      } else if (i*10+2 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
        VolMat[i*10+2] <- as.numeric(vals[2])
        
      } else {
        vals <- varEntryDialog(vars=c('d1'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep="")), title=addtitle,prompt=addpromt,cancellab="Back to How to add material's particle size data")
        if(is.null(vals)) { break }
        
        VolMat[i*10+1] <- as.numeric(vals[1])
      }
      
    } #for
    if(is.null(vals)) { next } #if cancelled, back to amount input type selection
    
    
    #put first and last amount if necessary
    
    #if (is.na(VolMat[1]) || is.na(VolMat[dnum])) {
    #  tkmessageBox(title ="Missing values", message="First or last amount were not given, but are mandatory. Returning to How to add material's particle size data.", icon = "error", type = "ok")
    #  return (4) #back to amount input type selection
    #}
    
    if (is.na(VolMat[1])) {
      for (m in seq(from=1, to=length(VolMat), by=1)) {
        if (!is.na(VolMat[m])) { 
          VolMat[1] <- VolMat[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    if (is.na(VolMat[length(VolMat)])) {
      for (m in seq(from=length(VolMat), to=1, by=-1)) {
        if (!is.na(VolMat[m])) { 
          VolMat[length(VolMat)] <- VolMat[m]
          break
        }
      }
    }
    
    
    
    
    
    
    #NAs linear ausgleichen?
    
    if (!all(!is.na(VolMat))) {#wenn min 1 NA, dann: linearisierende fuellung
      tkmessageBox(title ="Missing values", message="At least one missing value was detected. A linear interpolation follows. If this is not wanted but you want to input all values correctly, go 'Back to How to add material's particle size data' in the next dialog.", icon = "warning", type = "ok")
    #  return (4) #back to amount input type selection
    }
    
    #print(VolMat)
    i=1
    while (i < dnum) {#<=
    
      if (is.na(VolMat[i])) {#beginn NA ist element VolMat[i] falls TRUE; VolMat[i-1] unterer Wert fuer Lienarisierung
        for (j in seq(from=i+1, to=dnum, by=1)) {
          if (!is.na(VolMat[j])) {#erstes element nach i, was kein NA mehr ist, ist j; damit VolMat[j] oberer Wert fuer Linearisierung
            
            #Linearisierung fuer alle elemente zwischen i-1 und j
            for (k in seq(from=i, to=j-1, by=1)) {
              VolMat[k] <- VolMat[i-1] + (dsizes[k]-dsizes[i-1])*(VolMat[j]-VolMat[i-1])/(dsizes[j]-dsizes[i-1])
            }
            
            i=j+1#oder ohne +1?
            break #out of for j in seq-loop
            
          }
        }#end for j in seq
        
      } else {
       i=i+1
      }
      
    }#end while i < dnum
    #print(VolMat)
    
    
    
    
    
    
    
    
    
    #wenn hiernach normalisiert wird fuer die density-eingaben, muesste es auch gehen (bei input werden keine daten weggeschnitten)
    #die andere variante mit vorher cpft berechnen geht auhc nicht, weil ja NAs uU drin stehen, und die muessen erst weg. und die gehen weg durch die normalisierun
    
    #anders bei den sachen aus ner datei, wo fuer das datei-datenset durchaus die cpfts berechnet werden koennen wie oben
    
    if (typadd == 1 || typadd == 2) {
    
      norm <- sum(VolMat)
      VolMat <- 100*VolMat/norm
    
    }

    
    
    #hier weiter mit umrechnungen, zu... retnetion on sole sieve/component (retention density curve), wie in database gespeichert
    #umrechnung fuer cpft -> retention density schon in verify.R
    
    #typadd : 1 = Retention Density-curve; 2 = Passthrough Density-curve, 3 = Passthrough Sum-curve (CPFT); 4 = Retention sum-curve
    
    VolMatMod <- vector(length=dnum)    

    if (typadd == 1) {
    
      VolMatMod <- VolMat
      
    } else if (typadd == 2) {
    
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatMod[x] <- VolMat[x+1]
      }
      VolMatMod[dnum] <- 0
    
    } else if (typadd == 3) {
      
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatMod[x] <- VolMat[x+1]-VolMat[x]
      }
      VolMatMod[dnum] <- 0
    
    } else {
    
      
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatMod[x] <- VolMat[x]-VolMat[x+1]
      }
      VolMatMod[dnum] <- 0
      
    }
    
    
    
    
    
    
} else { #end if input manually; start else import from csv

    matfile <- tk_choose.files(caption="Select material data file", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))   
    if (length(matfile) == 0) { next }
    
     message("  Import particle size data from raw amterial file: ",matfile)
   
    #requirements on file: two columns 'Diameter' and 'Percent'; no NAs, words, ... only numbers 
   
    matdata <- read.csv2(file=matfile,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

    #check for every dsize, if it is included in matdata$Diameter. If not, add row (Percent=NA).
    
    
    
    
    
    
    #cpft berechnen
    
matcpft <- vector(length=length(matdata$Diameter))    

    if (typadd == 1) {
    
      matcpft[1] <- 0
    for(x in seq(from=2, to=length(matdata$Diameter), by=1)) {
      matcpft[x] <- matdata$Percent[x-1]+matcpft[x-1]
    }
      
    } else if (typadd == 2) {
    
      matcpft[1] <- matdata$Percent[1]
    for(x in seq(from=2, to=length(matdata$Diameter), by=1)) {
      matcpft[x] <- matdata$Percent[x]+matcpft[x-1]
    }
    
    } else if (typadd == 3) {
      
      matcpft <- matdata$Percent
    
    } else {
    
      matcpft <- 100-matdata$Percent
      
    }    
    
   
    
    sizemat <- data.frame(cbind(matdata$Diameter,matcpft))
   colnames(sizemat) <- c("Diameter","Percent")
   #print(sizemat)
   
   
    
    
    
    
    sizeinput <- data.frame(cbind(dsizes,rep(NA, dnum)))
    colnames(sizeinput) <- c("Diameter","Percent")
    
    #print(matdata)
    #print(sizeinput)
    
    #OLD: matmerge <- merge(matdata,sizeinput,all=TRUE)
    matmerge <- merge(sizemat,sizeinput,all=TRUE)
    
    #print(matmerge)
    
    matmerge <- matmerge[!duplicated(matmerge$Diameter), ]
    
    #print(matmerge)
    
    #put first and last percent if necessary
    
    if (is.na(matmerge$Percent[1])) {
      for (m in seq(from=1, to=length(matmerge$Diameter), by=1)) {
        if (!is.na(matmerge$Percent[m])) { 
          matmerge$Percent[1] <- matmerge$Percent[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    if (is.na(matmerge$Percent[length(matmerge$Diameter)])) {
      for (m in seq(from=length(matmerge$Diameter), to=1, by=-1)) {
        if (!is.na(matmerge$Percent[m])) { 
          matmerge$Percent[length(matmerge$Diameter)] <- matmerge$Percent[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    
    #lineares fuellen
    
    i=1
    while (i < length(matmerge$Diameter)) {#<=
    
      if (is.na(matmerge$Percent[i])) {#beginn NA ist element VolMat[i] falls TRUE; VolMat[i-1] unterer Wert fuer Lienarisierung
        for (j in seq(from=i+1, to=length(matmerge$Diameter), by=1)) {
          if (!is.na(matmerge$Percent[j])) {#erstes element nach i, was kein NA mehr ist, ist j; damit VolMat[j] oberer Wert fuer Linearisierung
            
            #Linearisierung fuer alle elemente zwischen i-1 und j
            for (k in seq(from=i, to=j-1, by=1)) {
              matmerge$Percent[k] <- matmerge$Percent[i-1] + (matmerge$Diameter[k]-matmerge$Diameter[i-1])*(matmerge$Percent[j]-matmerge$Percent[i-1])/(matmerge$Diameter[j]-matmerge$Diameter[i-1])
            }
            
            i=j+1#oder ohne +1?
            break #out of for j in seq-loop
            
          }
        }#end for j in seq
        
      } else {
       i=i+1
      }
      
    }#end while i < length(matmerge$Diameter)
    
    #print(matmerge)
    
    
    #next: kep only dsizes resp sizeinput
    matcrop <- matmerge[matmerge$Diameter %in% dsizes,]
    
    #print(matcrop)
    
    #VolMat <- matcrop$Percent
    
    #print(VolMat)
    
    VolMatMod <- vector(length=dnum)    
      
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatMod[x] <- matcrop$Percent[x+1]-matcrop$Percent[x]
      }
      VolMatMod[dnum] <- 0 

      
}#end else import from csv




    



    #calculate d10, d25, d50, d75, d90 from cpft
    MatCPFT <- vector(length=dnum)
    MatCPFT[1] <- 0
    for(x in seq(from=2, to=dnum, by=1)) {
      MatCPFT[x] <- VolMatMod[x-1]+MatCPFT[x-1]
    }
    
    d10 <- 0
d25 <- 0
d50 <- 0
d75 <- 0
d90 <- 0

for (v in seq(from=1, to=dnum, by=1)) {
 
 if (d10 == 0 && MatCPFT[v] > 10) { #interpolate with value before for d10
  d10 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (10 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d25 == 0 && MatCPFT[v] > 25) { #single if's because it s also possible that multiple values have to be interpolated between the same pair of data
  d25 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (25 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d50 == 0 && MatCPFT[v] > 50) { 
  d50 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (50 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d75 == 0 && MatCPFT[v] > 75) { 
  d75 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (75 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d90 == 0 && MatCPFT[v] > 90) { 
  d90 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (90 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
}
    
    #weiter zeile 612 bzw besser
    #show material information dialog (siehe ab zeile 786 in verify.R)

  #get infos
mathead <- c('Identifier','Name','Last modified','Origin/Supplier','Date supplied','Price per MT','True density','Date measured','Measurement method','Specific surface area','Date measured','Measurement method','d10','d25','d50','d75','d90','Date measured','Measurement method')

matvars <- c('ID','Name','Last','Origin','Date','Price','Density','DenDate','DenMeas','SSA','SSADate','SSAMeas','d10','d25','d50','d75','d90','dDate','dMeas')

matlabs <- c('Unique identifier (Required!):','Material name:','Last modified:','Origin/Supplier:','Date supplied:','Price per MT:','True density:','Date measured (Density):','Measurement method (Density):','Specific surface area (SSA):','Date measured (SSA):','Measurement method (SSA):',paste('d(10%) in',tclvalue(dunit),':'),paste('d(25%) in',tclvalue(dunit),':'),paste('d(50%) in',tclvalue(dunit),':'),paste('d(75%) in',tclvalue(dunit),':'),paste('d(90%) in',tclvalue(dunit),':'),'Date measured (Particle sizes):','Measurement method (Particle sizes):')

matpre <- c(paste('Material ',paste(Sys.time())),'',paste(Sys.Date()),'','','','','','','','','',round(d10, digits=accuracy),round(d25, digits=accuracy),round(d50, digits=accuracy),round(d75, digits=accuracy),round(d90, digits=accuracy),'','')

matinfo <- varEntryDialog(vars=matvars, labels=matlabs, title="Material information",prompt="Adjust material information:",preset=matpre,cancellab="Back to How to add material's particle size data")
    if(is.null(matinfo)) { next }#if input ok, go on, if not return ?!???

    
    id <- as.character(matinfo[1])
    nam <- as.character(matinfo[2])
    last <- as.character(matinfo[3])
    orig <- as.character(matinfo[4])
    date <- as.character(matinfo[5])
    price <- format(as.numeric(matinfo[6]), decimal.mark=tclvalue(decpoint), na.encode=F)
    td <- format(as.numeric(matinfo[7]), decimal.mark=tclvalue(decpoint), na.encode=F)
    tddate <- as.character(matinfo[8])
    tdmeas <- as.character(matinfo[9])
    ssa <- format(as.numeric(matinfo[10]), decimal.mark=tclvalue(decpoint), na.encode=F)
    ssadate <- as.character(matinfo[11])
    ssameas <- as.character(matinfo[12])
    d10s <- format(as.numeric(matinfo[13]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d25s <- format(as.numeric(matinfo[14]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d50s <- format(as.numeric(matinfo[15]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d75s <- format(as.numeric(matinfo[16]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d90s <- format(as.numeric(matinfo[17]), decimal.mark=tclvalue(decpoint), na.encode=F)
    ddate <- as.character(matinfo[18])
    dmeas <- as.character(matinfo[19])
    #dmax <- as.numeric(vals[1])
    #n <- as.numeric(vals[2])
    
    materinfo <- c(id,nam,last,orig,date,price,td,tddate,tdmeas,ssa,ssadate,ssameas,d10s,d25s,d50s,d75s,d90s,ddate,dmeas)
    test2 <- gsub("NA","",materinfo)
    
    
    #make database; save material
    
    col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
col2 <- c(test2[2:length(mathead)],format(round(VolMatMod, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo

#print(MatList)

#print(dbinfo[,MatList])






#NEU:
}#end else input or import particle data separately







savematdb <- data.frame(cbind(col1,col2,as.data.frame(dbinfo[,MatList])), row.names=1:length(col1))
#savematdb <- data.frame(cbind(col1,col2,dbinfo[,MatList], drop=F), row.names=1:length(col1))
#print(savematdb)

colnames(savematdb) <- c("Identifier",id,MatList)
#print(savematdb)

savematdb <- savematdb[,colnames(savematdb)[2:length(colnames(savematdb))], drop=F]
#print(savematdb)

#save dataframe with new first line (=col.names)
write.table(savematdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",id,sep=""),MatList), quote=F)
  message("  Material added to database.")
  
  addmore <- tkmessageBox(title = "Material added", message="Material was added to the database. Do you wish to add more materials to this database now?", icon = "info", type = "yesno")
 message("  Add more materials to new database? Decision: ",addmore,"\n")
    if (tclvalue(addmore) == "no") { 
      
      break
      
    } 

  } #while addmaterial

}#end addmaterial function









deletematerial <- function(dbfile) {

while (TRUE) {

 message("BEGIN delete material function\n")

dbinfo <- read.csv2(file=dbfile,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

    #read mass% values separately
    dbdata <- read.csv2(file=dbfile,skip=19,header=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
    colnames(dbdata) <- colnames(dbinfo)
    
    #print(dbinfo)
     
    MatList <- colnames(dbinfo)[2:length(colnames(dbinfo))]
    
    #print(MatList)
    
    dnum <- length(dbdata$Identifier)
    dsizes <- dbdata$Identifier

    MatSelection <- tk_select.list(choices=MatList, preselect=NULL, multiple=TRUE, title="Select materials to delete:")
    if (length(MatSelection) == 0) { break }
    
    message("  Number of selected materials to delete: ",length(MatSelection))

    #print(MatSelection)
    
    #im prinzip direkt hier so... WEITER HIER
    #matcrop <- matmerge[matmerge$Diameter %in% dsizes,]
    
    #MatDB <- MatList[-MatSelection]
    
    MatDB <- NULL
    MatTF <- MatList %in% MatSelection
    #print(MatTF)
    for (x in seq(from=1, to=length(MatList), by=1)) {
      if (!MatTF[x]) {
        MatDB <- c(MatDB, MatList[x])
      }
    }
    
    #print (MatDB)
    
    if (length(MatDB) == 0) {
      
      delall <- tkmessageBox(title = "Delete database?", message="All materials marked for removal. This deletes the database. Do you want to delete all materials and the database?", icon = "info", type = "yesno")

      message("  Delete database? Decision: ",delall)
      
      if (tclvalue(delall) == "no") { 
      
        next
      
      } else {
        file.remove(dbfile)
        message("  Database deleted.\n")
        break
      }
    
    }
    
    
    
    #abfrage zu length(MatDB) == 0, dann DB loeschen fragen
    
    mathead <- c('Identifier','Name','Last modified','Origin/Supplier','Date supplied','Price per MT','True density','Date measured','Measurement method','Specific surface area','Date measured','Measurement method','d10','d25','d50','d75','d90','Date measured','Measurement method')

    col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
    
    prepDB <- data.frame(col1, row.names=1:length(col1))
    
    #prepDB <- dbinfo[,MatDB[1],drop=F]
    
    #print (prepDB)
    
    #if (length(MatDB > 1)) {
      for (m in seq(from=1, to=length(MatDB), by=1)) {
        prepDB <- cbind(prepDB, dbinfo[,MatDB[m],drop=F])
      }
    #}
    
    #print (prepDB)
    
    colnames(prepDB) <- c("Identifier",MatDB)
#print(prepDB)

prepDB <- prepDB[,colnames(prepDB)[2:length(colnames(prepDB))], drop=F]
#print(prepDB)
    

#save dataframe with new first line (=col.names)

if (length(MatDB) == 1) {

write.table(prepDB,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep="")), quote=F)

} else {

write.table(prepDB,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep=""),MatDB[2:length(MatDB)]), quote=F)
}
message("  Materials were deleted from database.")
  
  delmore <- tkmessageBox(title = "Materials deleted", message="Selected materials were deleted from the database. Do you wish to delete more materials from this database now?", icon = "info", type = "yesno")
message("  Delete more materials? Decision: ",delmore,"\n")
    if (tclvalue(delmore) == "no") { 
      
      break
      
    } 







} #end while deletematerial

} #end deletematerial function















































































editmaterial <- function(dbfile) {

while (TRUE) {

message("BEGIN edit material function")

dbmat <- read.csv2(file=dbfile,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

    #read mass% values separately
    
    
    #print(dbinfo)
     
    MatList <- colnames(dbmat)[2:length(colnames(dbmat))]
    
    #print(MatList)
    
    

    MatSel <- tk_select.list(choices=MatList, preselect=NULL, multiple=FALSE, title="Select material to edit:")
    if (MatSel == "") { break }

   #print(MatSel)
   
   if (exists("id")) {
       rm(id)
    }
    
    
    
    
    while (TRUE) {
    
    if (exists("id")) {
       MatSelection <- test2[1]
    } else {
      MatSelection <- MatSel
    }
    
    message("\n  Loading window for selected material ",MatSelection)
    
    #print(MatSelection)
    
    dbinfo <- read.csv2(file=dbfile,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

    
      dbdata <- read.csv2(file=dbfile,skip=19,header=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
    colnames(dbdata) <- colnames(dbinfo)
    
    dnum <- length(dbdata$Identifier)
    dsizes <- dbdata$Identifier
    
   message("  Number of component sizes in database: ",dnum)
    
    #print ("noch ok")
    
    #print(MatSelection)
    
      VolMat <- dbdata[,MatSelection]
      
      #print ("danach")
    
      mathead <- c('Identifier','Name','Last modified','Origin/Supplier','Date supplied','Price per MT','True density','Date measured','Measurement method','Specific surface area','Date measured','Measurement method','d10','d25','d50','d75','d90','Date measured','Measurement method')

      matvars <- c('ID','Name','Last','Origin','Date','Price','Density','DenDate','DenMeas','SSA','SSADate','SSAMeas','d10','d25','d50','d75','d90','dDate','dMeas')

      matlabs <- c('Unique identifier:','Material name:','Last modified:','Origin/Supplier:','Date supplied:','Price per MT:','True density:','Date measured (Density):','Measurement method (Density):','Specific surface area (SSA):','Date measured (SSA):','Measurement method (SSA):',paste('d(10%) in',tclvalue(dunit),':'),paste('d(25%) in',tclvalue(dunit),':'),paste('d(50%) in',tclvalue(dunit),':'),paste('d(75%) in',tclvalue(dunit),':'),paste('d(90%) in',tclvalue(dunit),':'),'Date measured (Particle sizes):','Measurement method (Particle sizes):')
    
      matpre <- vector(length=19)
      matpre[1] <- MatSelection
      for (o in c(2,3,4,5,8,9,11,12,18,19)) {
        if (length(dbinfo[o-1,MatSelection]) == 0) {
          matpre[o] <- ''
        } else {
          matpre[o] <- as.character(dbinfo[o-1,MatSelection])
        }
      }
      for (q in c(6,7,10,13,14,15,16,17)) {
        if (length(dbinfo[q-1,MatSelection]) == 0) {
          matpre[q] <- ''
        } else {
          matpre[q] <- gsub(",",".",dbinfo[q-1,MatSelection]) #...mit as.numeric( gsub ) werdens zahlen statt umformatierten textes
        }
      }

      #print(matpre)

      windb <- tktoplevel(bg=hintergrund)
  tkwm.title(windb,"Edit material")
  
  tkraise(windb)

  db <- tclVar(0)

  tkbind(windb,"<Destroy>",function() tclvalue(db)<-1) 
  
  Menu <- tkmenu(windb, bg=menue)           
  tkconfigure(windb, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","05editmat","05.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())
  
  #QuMenu <- tkmenu(Menu, tearoff = FALSE)
  #tkadd(Menu, "command", label = "Back to Main Menu", command = function() tclvalue(db)<-1)
  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(db)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(db)<-1)
  
  #window:
  #tkgrid(tklabel(windb, font=tkfont.create(size=1), text=""),pady=10, padx=10, columnspan=3)
  tkgrid(tklabel(windb, bg=hintergrund, text="Material information"), pady=5, padx=10, columnspan=2)
  #tkgrid(tklabel(windb, font=tkfont.create(size=1), text=""),pady=10, padx=10, columnspan=3)
  
  for(p in seq(from=1, to=19, by=1)) {
    tkgrid(tklabel(windb, bg=hintergrund, text=matlabs[p]), tklabel(windb, bg=hintergrund, text=matpre[p]), pady=1, padx=10, columnspan=2)
  }
  
  tkgrid(tkbutton(windb, text='Save as model (as .CSV)', bg=knoepfe, command=function() tclvalue(db)<-3), tkbutton(windb, text='Edit material information', bg=knoepfe, command=function() tclvalue(db)<-4), pady=5, padx=10, columnspan=2)
  #tkgrid(tklabel(windb, font=tkfont.create(size=1), text=""),pady=10, padx=10, columnspan=3)
  tkgrid(ttkseparator(windb), columnspan=18, pady=5, padx=10, sticky="we")
  #tkgrid(tklabel(windb, font=tkfont.create(size=1), text=""),pady=10, padx=10, columnspan=3)
  tkgrid(tklabel(windb, bg=hintergrund, text="Do you want to access the particle size data?"), pady=5, padx=10, columnspan=2)
  
  rb1 <- tkradiobutton(windb)
  rb2 <- tkradiobutton(windb)
  rb3 <- tkradiobutton(windb)
  rb4 <- tkradiobutton(windb)
  
  rbValue <- tclVar(1)
  
  tkconfigure(rb1,variable=rbValue,value=1)
  tkconfigure(rb2,variable=rbValue,value=2)
  tkconfigure(rb3,variable=rbValue,value=3)
  tkconfigure(rb4,variable=rbValue,value=4)
  
  tkgrid(tklabel(windb,text="Retention Density-curve"), rb1, pady=1, padx=10, columnspan=2)
  tkgrid(tklabel(windb,text="Passthrough Density-curve"), rb2, pady=1, padx=10, columnspan=2)
  tkgrid(tklabel(windb,text="Passthrough Sum-curve (CPFT)"), rb3, pady=1, padx=10, columnspan=2)
  tkgrid(tklabel(windb,text="Retention sum-curve"), rb4, pady=1, padx=10, columnspan=2)
  
  tkgrid(tkbutton(windb, text='View/edit manually', bg=knoepfe, command=function() tclvalue(db)<-5), tkbutton(windb, text='Overwrite by CSV-import', bg=knoepfe, command=function() tclvalue(db)<-6), pady=5, padx=10, columnspan=2)
  
  
  #warum alle 4 varianten als import, aber nicht als manually... eher vlt erst variante waehlen und dann entscheiden ob import oder manually. und eignetlich bei beiden interpolieren...
  
  tkfocus(windb)

  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(db)

  var <- tclvalue(db)
  typadd <- tclvalue(rbValue)
  
  tkdestroy(windb)
  
  message("  Chosen action (1 Main menu, 2 Material selection, 3 Save as model, 4 Edit info, 5 View/edit particle size data, 6 Import particle size data): ",var)
  message("  Chosen particle size data type (1 Retention-density, 2 Passthrough-density, 3 Passthrough-Sum, 4 Retention-Sum): ",typadd,"\n")
  
  if (var < 3) {
    break
  } else if (var == 3) { #siehe in design-funktion -> cpft/retention-bla-bla-umrehcnugnen beachten!!!

  #dsizes
   #   VolMat #ist retention density 
  
  
  
  
  VolMatMod <- vector(length=dnum)    
  
  VolMatMod[1] <- 0
      for(x in seq(from=2, to=dnum, by=1)) {
        VolMatMod[x] <- VolMatMod[x-1]+VolMat[x-1]
      }
      

      

  modelinfo <- paste(MatSelection,"-model",sep="")
  
  
  FileMod = tclvalue(tcl("tk_getSaveFile"))
        if (FileMod != "") { 
        
        modeldf <- data.frame(cbind(dsizes, VolMatMod))
        colnames(modeldf) <- c("Diameter",modelinfo)
        
        write.table(modeldf, file=FileMod, row.names=FALSE,col.names=TRUE,dec=tclvalue(decpoint),sep=tclvalue(csvtype)) 
        
        }
  
  
  #vals <- varEntryDialog(vars=c('name'), labels=c('Name of the model:'), title="Other/free model",prompt="Name the model:",preset=c("Other/free model"),cancellab='Cancel')
    #if(is.null(vals)) { next } 
    
    #modelinfo <- as.character(vals[1])
  
  
  #FreeMod <- tkmessageBox(title =paste("Save",modelinfo), message="Do you want to save the model? If yes, save as *.csv-file, please.", icon = "question", type = "yesno")

      #if (tclvalue(FreeMod) == "yes") {
      
        
    
      #} #if save
  
    
  
  
  message("  Saved as model in file: ",FileMod)
  
  
  

  } else if (var == 4) {
  
  message("  Edit material info")
  
    matinfo <- varEntryDialog(vars=matvars, labels=matlabs, title="Edit material information",prompt="Adjust material information:",preset=matpre)
    if(is.null(matinfo)) { next } #cancel->back?
    
    #print("test")
    
    id <- as.character(matinfo[1])
    nam <- as.character(matinfo[2])
    last <- as.character(matinfo[3])
    orig <- as.character(matinfo[4])
    date <- as.character(matinfo[5])
    price <- format(as.numeric(matinfo[6]), decimal.mark=tclvalue(decpoint), na.encode=F)
    td <- format(as.numeric(matinfo[7]), decimal.mark=tclvalue(decpoint), na.encode=F)
    tddate <- as.character(matinfo[8])
    tdmeas <- as.character(matinfo[9])
    ssa <- format(as.numeric(matinfo[10]), decimal.mark=tclvalue(decpoint), na.encode=F)
    ssadate <- as.character(matinfo[11])
    ssameas <- as.character(matinfo[12])
    d10s <- format(as.numeric(matinfo[13]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d25s <- format(as.numeric(matinfo[14]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d50s <- format(as.numeric(matinfo[15]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d75s <- format(as.numeric(matinfo[16]), decimal.mark=tclvalue(decpoint), na.encode=F)
    d90s <- format(as.numeric(matinfo[17]), decimal.mark=tclvalue(decpoint), na.encode=F)
    ddate <- as.character(matinfo[18])
    dmeas <- as.character(matinfo[19])
    #dmax <- as.numeric(vals[1])
    #n <- as.numeric(vals[2])
    
    materinfo <- c(id,nam,last,orig,date,price,td,tddate,tdmeas,ssa,ssadate,ssameas,d10s,d25s,d50s,d75s,d90s,ddate,dmeas)
    test2 <- gsub("NA","",materinfo)
    
    #abfrage, ob database 1 oder mehr amterialien hatte..?
    
    if (length(MatList) == 1) {#database ueberschreiben; MatSelection updaten; next
    
    message("  Database contains just one material")
    
      col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
      col2 <- c(test2[2:length(mathead)],format(round(VolMat, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo
      
      newmatdb <- data.frame(cbind(col1,col2), row.names=1:length(col1))
      newmatdb <- newmatdb[,2, drop=F]

      write.table(newmatdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", col.names=paste("Identifier;",id,sep=""), row.names=col1, quote=F)
      
      #print(test2[1])
      
      #MatSelection <<- test2[1] #oder id
      
      #print(MatSelection)
      
      #next #notwendig?!? wo kommt das if/else sonst raus - vielleciht am ende von while?
    
    } else {#aus alter datenbank alten eintrag löschen; neuen hinzufuegen; MatSelection updaten; next (um material-info-dialog mit enuen eingaben aufzurufen)
    
    
    message("  Database contains more than one material")
    
    MatDB <- NULL
    MatTF <- MatList %in% MatSelection
    #print(MatTF)
    for (x in seq(from=1, to=length(MatList), by=1)) {
      if (!MatTF[x]) {
        MatDB <- c(MatDB, MatList[x])
      }
    }
    
    #print (MatDB)
    
    col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
    
    
    prepDB <- data.frame(col1, row.names=1:length(col1))
    
    #prepDB <- dbinfo[,MatDB[1],drop=F]
    
    #print (prepDB)
    
    #if (length(MatDB > 1)) {
      for (m in seq(from=1, to=length(MatDB), by=1)) {
        prepDB <- cbind(prepDB, dbinfo[,MatDB[m],drop=F])
      }
    #}
    
    #print (prepDB)
    
    #colnames(prepDB) <- c("Identifier",MatDB)
#print(prepDB)

#prepDB <- prepDB[,colnames(prepDB)[2:length(colnames(prepDB))], drop=F]
#print(prepDB)


#neu hinzufuegen
col2 <- c(test2[2:length(mathead)],format(round(VolMat, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo
      
savematdb <- data.frame(cbind(prepDB,col2), row.names=1:length(col1))

#print(savematdb)

colnames(savematdb) <- c("Identifier",MatDB,id)

#print(savematdb)

savematdb <- savematdb[,colnames(savematdb)[2:length(colnames(savematdb))], drop=F]

#print(savematdb)

    
    #write.table(prepDB,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep=""),MatDB[2:length(MatDB)]), quote=F)
    
    
    
    
    
    #col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))


#print(MatList)

#print(dbinfo[,MatList])


#savematdb <- data.frame(cbind(col1,col2,dbinfo[,MatList], drop=F), row.names=1:length(col1))
#print(savematdb)

#colnames(savematdb) <- c("Identifier",id,MatList)
#print(savematdb)

#savematdb <- savematdb[,colnames(savematdb)[2:length(colnames(savematdb))], drop=F]
#print(savematdb)

#save dataframe with new first line (=col.names)
#write.table(savematdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",id,sep=""),MatDB), quote=F)

if (length(MatDB) == 1) {

write.table(savematdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep=""),id), quote=F)

} else {

write.table(savematdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep=""),MatDB[2:length(MatDB)],id), quote=F)
}


    
    }#end else MatList == 1
    
    message("  Material info edited by updating database.")
    
  } else if (var == 5) { #end var==4; beginn var=5 view edit particle sizes manually
  
  #umrechnen zu gewuenschten typ von retention density
  
  #dsizes
   #   VolMat #ist retention density 
  
  
  
  VolMatMod <- vector(length=dnum)    
  
  if (typadd == 1) {
addtitle <- "Retention density-curve"
addtext <- "Retention on sole sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 0% to 0%!):"

VolMatMod <- VolMat
} else if (typadd == 2) {
addtitle <- "Passthrough density-curve"
addtext <- "Pass through sole sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 0% to 0%!):"

VolMatMod[1] <- 0
      for(x in seq(from=2, to=dnum, by=1)) {
        VolMatMod[x] <- VolMat[x-1]
      }


} else if (typadd == 3) {
addtitle <- "Passthrough sum-curve (CPFT)"
addtext <- "Cumulative pass through (CPFT) sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 0% to 100%):"

VolMatMod[1] <- 0
      for(x in seq(from=2, to=dnum, by=1)) {
        VolMatMod[x] <- VolMatMod[x-1]+VolMat[x-1]
      }


} else {
addtitle <- "Retention sum-curve"
addtext <- "Cumulative retention on sieve/component "
addpromt <- "Input the particle size distribution data (Has to run from 100% to 0%):"

#VolMatMod[1] <- VolMat[1]
#      for(x in seq(from=2, to=dnum, by=1)) {
#        VolMatMod[x] <- VolMatMod[x-1]+VolMat[x]
#      }

VolMatMod[1] <- 0
      for(x in seq(from=2, to=dnum, by=1)) {
        VolMatMod[x] <- VolMatMod[x-1]+VolMat[x-1]
      }
      
      VolMatMod <- 100-VolMatMod

}
  
  
  
  
  
  
#weiter mit dialog, preset ist VolMatMod  
  
  VolMatModRev <- vector(length=dnum)
    winnr <- ceiling(dnum/10)
    for (i in seq(from=0, to=winnr-1, by=1)) { #bis -1 weil von 0; 0 necessary due to condition-types with i*10+INPUTlineNUMBER
      
      if (i*10+10 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9','d10'), labels=c(
        paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+8], tclvalue(dunit), ":",sep=""),
        paste(addtext, dsizes[i*10+9], tclvalue(dunit), ":",sep=""), 
        paste(addtext, dsizes[i*10+10], tclvalue(dunit), ":",sep="")), 
        preset=c(VolMatMod[i*10+1],VolMatMod[i*10+2],VolMatMod[i*10+3],VolMatMod[i*10+4],VolMatMod[i*10+5],VolMatMod[i*10+6],VolMatMod[i*10+7],VolMatMod[i*10+8],VolMatMod[i*10+9],VolMatMod[i*10+10]),
        title=addtitle,
        prompt=addpromt,
         cancellab="Back to Edit material dialog")
        if(is.null(vals)) { break }
        
        VolMatModRev[i*10+1] <- as.numeric(vals[1])
        VolMatModRev[i*10+2] <- as.numeric(vals[2])
        VolMatModRev[i*10+3] <- as.numeric(vals[3])
        VolMatModRev[i*10+4] <- as.numeric(vals[4])
        VolMatModRev[i*10+5] <- as.numeric(vals[5])
        VolMatModRev[i*10+6] <- as.numeric(vals[6])
        VolMatModRev[i*10+7] <- as.numeric(vals[7])
        VolMatModRev[i*10+8] <- as.numeric(vals[8])
        VolMatModRev[i*10+9] <- as.numeric(vals[9])
        VolMatModRev[i*10+10] <- as.numeric(vals[10])
      
      } else if (i*10+9 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+8], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+9], tclvalue(dunit), ":",sep="")), 
        preset=c(VolMatMod[i*10+1],VolMatMod[i*10+2],VolMatMod[i*10+3],VolMatMod[i*10+4],VolMatMod[i*10+5],VolMatMod[i*10+6],VolMatMod[i*10+7],VolMatMod[i*10+8],VolMatMod[i*10+9]), title=addtitle,prompt=addpromt, cancellab="Back to Edit material dialog")
        if(is.null(vals)) { break }
        
        VolMatModRev[i*10+1] <- as.numeric(vals[1])
        VolMatModRev[i*10+2] <- as.numeric(vals[2])
        VolMatModRev[i*10+3] <- as.numeric(vals[3])
        VolMatModRev[i*10+4] <- as.numeric(vals[4])
        VolMatModRev[i*10+5] <- as.numeric(vals[5])
        VolMatModRev[i*10+6] <- as.numeric(vals[6])
        VolMatModRev[i*10+7] <- as.numeric(vals[7])
        VolMatModRev[i*10+8] <- as.numeric(vals[8])
        VolMatModRev[i*10+9] <- as.numeric(vals[9])
        
      } else if (i*10+8 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+8], tclvalue(dunit), ":",sep="")),preset=c(VolMatMod[i*10+1],VolMatMod[i*10+2],VolMatMod[i*10+3],VolMatMod[i*10+4],VolMatMod[i*10+5],VolMatMod[i*10+6],VolMatMod[i*10+7],VolMatMod[i*10+8]), title=addtitle,prompt=addpromt, cancellab="Back to Edit material dialog")
        if(is.null(vals)) { break }
        
        VolMatModRev[i*10+1] <- as.numeric(vals[1])
        VolMatModRev[i*10+2] <- as.numeric(vals[2])
        VolMatModRev[i*10+3] <- as.numeric(vals[3])
        VolMatModRev[i*10+4] <- as.numeric(vals[4])
        VolMatModRev[i*10+5] <- as.numeric(vals[5])
        VolMatModRev[i*10+6] <- as.numeric(vals[6])
        VolMatModRev[i*10+7] <- as.numeric(vals[7])
        VolMatModRev[i*10+8] <- as.numeric(vals[8])
        
      } else if (i*10+7 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+7], tclvalue(dunit), ":",sep="")),preset=c(VolMatMod[i*10+1],VolMatMod[i*10+2],VolMatMod[i*10+3],VolMatMod[i*10+4],VolMatMod[i*10+5],VolMatMod[i*10+6],VolMatMod[i*10+7]), title=addtitle,prompt=addpromt, cancellab="Back to Edit material dialog")
        if(is.null(vals)) { break }
        
        VolMatModRev[i*10+1] <- as.numeric(vals[1])
        VolMatModRev[i*10+2] <- as.numeric(vals[2])
        VolMatModRev[i*10+3] <- as.numeric(vals[3])
        VolMatModRev[i*10+4] <- as.numeric(vals[4])
        VolMatModRev[i*10+5] <- as.numeric(vals[5])
        VolMatModRev[i*10+6] <- as.numeric(vals[6])
        VolMatModRev[i*10+7] <- as.numeric(vals[7])
        
      } else if (i*10+6 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+6], tclvalue(dunit), ":",sep="")),preset=c(VolMatMod[i*10+1],VolMatMod[i*10+2],VolMatMod[i*10+3],VolMatMod[i*10+4],VolMatMod[i*10+5],VolMatMod[i*10+6]), title=addtitle,prompt=addpromt, cancellab="Back to Edit material dialog")
        if(is.null(vals)) { break }
        
        VolMatModRev[i*10+1] <- as.numeric(vals[1])
        VolMatModRev[i*10+2] <- as.numeric(vals[2])
        VolMatModRev[i*10+3] <- as.numeric(vals[3])
        VolMatModRev[i*10+4] <- as.numeric(vals[4])
        VolMatModRev[i*10+5] <- as.numeric(vals[5])
        VolMatModRev[i*10+6] <- as.numeric(vals[6])
        
      } else if (i*10+5 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+5], tclvalue(dunit), ":",sep="")),preset=c(VolMatMod[i*10+1],VolMatMod[i*10+2],VolMatMod[i*10+3],VolMatMod[i*10+4],VolMatMod[i*10+5]), title=addtitle,prompt=addpromt, cancellab="Back to Edit material dialog")
        if(is.null(vals)) { break }
        
        VolMatModRev[i*10+1] <- as.numeric(vals[1])
        VolMatModRev[i*10+2] <- as.numeric(vals[2])
        VolMatModRev[i*10+3] <- as.numeric(vals[3])
        VolMatModRev[i*10+4] <- as.numeric(vals[4])
        VolMatModRev[i*10+5] <- as.numeric(vals[5])
        
      } else if (i*10+4 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+4], tclvalue(dunit), ":",sep="")),preset=c(VolMatMod[i*10+1],VolMatMod[i*10+2],VolMatMod[i*10+3],VolMatMod[i*10+4]), title=addtitle,prompt=addpromt, cancellab="Back to Edit material dialog")
        if(is.null(vals)) { break }
        
        VolMatModRev[i*10+1] <- as.numeric(vals[1])
        VolMatModRev[i*10+2] <- as.numeric(vals[2])
        VolMatModRev[i*10+3] <- as.numeric(vals[3])
        VolMatModRev[i*10+4] <- as.numeric(vals[4])
        
      } else if (i*10+3 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+3], tclvalue(dunit), ":",sep="")),preset=c(VolMatMod[i*10+1],VolMatMod[i*10+2],VolMatMod[i*10+3]), title=addtitle,prompt=addpromt, cancellab="Back to Edit material dialog")
        if(is.null(vals)) { break }
        
        VolMatModRev[i*10+1] <- as.numeric(vals[1])
        VolMatModRev[i*10+2] <- as.numeric(vals[2])
        VolMatModRev[i*10+3] <- as.numeric(vals[3])
        
      } else if (i*10+2 < dnum+1) {
        vals <- varEntryDialog(vars=c('d1','2'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep=""),paste(addtext, dsizes[i*10+2], tclvalue(dunit), ":",sep="")),preset=c(VolMatMod[i*10+1],VolMatMod[i*10+2]), title=addtitle,prompt=addpromt, cancellab="Back to Edit material dialog")
        if(is.null(vals)) { break }
        
        VolMatModRev[i*10+1] <- as.numeric(vals[1])
        VolMatModRev[i*10+2] <- as.numeric(vals[2])
        
      } else {
        vals <- varEntryDialog(vars=c('d1'), labels=c(paste(addtext, dsizes[i*10+1], tclvalue(dunit), ":",sep="")),preset=c(VolMatMod[i*10+1]), title=addtitle,prompt=addpromt, cancellab="Back to Edit material dialog")
        if(is.null(vals)) { break }
        
        VolMatModRev[i*10+1] <- as.numeric(vals[1])
      }
      
    } #for
    if(is.null(vals)) { next } #if cancelled, back to amount input type selection
    
    
    #put first and last amount if necessary
    
    #if (is.na(VolMat[1]) || is.na(VolMat[dnum])) {
    #  tkmessageBox(title ="Missing values", message="First or last amount were not given, but are mandatory. Returning to How to add material's particle size data.", icon = "error", type = "ok")
    #  return (4) #back to amount input type selection
    #}
    
    if (is.na(VolMatModRev[1])) {
      for (m in seq(from=1, to=length(VolMatModRev), by=1)) {
        if (!is.na(VolMatModRev[m])) { 
          VolMatModRev[1] <- VolMatModRev[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    if (is.na(VolMatModRev[length(VolMatModRev)])) {
      for (m in seq(from=length(VolMatModRev), to=1, by=-1)) {
        if (!is.na(VolMatModRev[m])) { 
          VolMatModRev[length(VolMatModRev)] <- VolMatModRev[m]
          break
        }
      }
    }
    
    
    
    
    
    
    #NAs linear ausgleichen?
    
    if (!all(!is.na(VolMatModRev))) {#wenn min 1 NA, dann: linearisierende fuellung
      tkmessageBox(title ="Missing values", message="At least one missing value was detected. A linear interpolation follows. If this is not wanted but you want to input all values correctly, go 'Back to How to add material's particle size data' in the next dialog.", icon = "warning", type = "ok")
    #  return (4) #back to amount input type selection
    }
    
    #print(VolMat)
    i=1
    while (i < dnum) {#<=
    
      if (is.na(VolMatModRev[i])) {#beginn NA ist element VolMatModRev[i] falls TRUE; VolMatModRev[i-1] unterer Wert fuer Lienarisierung
        for (j in seq(from=i+1, to=dnum, by=1)) {
          if (!is.na(VolMatModRev[j])) {#erstes element nach i, was kein NA mehr ist, ist j; damit VolMatModRev[j] oberer Wert fuer Linearisierung
            
            #Linearisierung fuer alle elemente zwischen i-1 und j
            for (k in seq(from=i, to=j-1, by=1)) {
              VolMatModRev[k] <- VolMatModRev[i-1] + (dsizes[k]-dsizes[i-1])*(VolMatModRev[j]-VolMatModRev[i-1])/(dsizes[j]-dsizes[i-1])
            }
            
            i=j+1#oder ohne +1?
            break #out of for j in seq-loop
            
          }
        }#end for j in seq
        
      } else {
       i=i+1
      }
      
    }#end while i < dnum
    #print(VolMat)
    
    
    
    #neu hinzugefuegt
    if (typadd == 1 || typadd == 2) {
    
      norm <- sum(VolMatModRev)
      VolMatModRev <- 100*VolMatModRev/norm
    
    }
    
    
    
    #rest von umrechnung und speichern und so noch
    
    #hier weiter mit umrechnungen, zu... retnetion on sole sieve/component (retention density curve), wie in database gespeichert
    #umrechnung fuer cpft -> retention density schon in verify.R
    
    #typadd : 1 = Retention Density-curve; 2 = Passthrough Density-curve, 3 = Passthrough Sum-curve (CPFT); 4 = Retention sum-curve
    
    VolMatModNew <- vector(length=dnum)    

    if (typadd == 1) {
    
      VolMatModNew <- VolMatModRev
      
    } else if (typadd == 2) {
    
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatModNew[x] <- VolMatModRev[x+1]
      }
      VolMatModNew[dnum] <- 0
    
    } else if (typadd == 3) {
      
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatModNew[x] <- VolMatModRev[x+1]-VolMatModRev[x]
      }
      VolMatModNew[dnum] <- 0
    
    } else {
    
    for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatModNew[x] <- VolMatModRev[x]-VolMatModRev[x+1]
      }
      VolMatModNew[dnum] <- 0
      
    }
    



    #calculate d10, d25, d50, d75, d90 from cpft
    MatCPFT <- vector(length=dnum)
    MatCPFT[1] <- 0
    for(x in seq(from=2, to=dnum, by=1)) {
      MatCPFT[x] <- VolMatModNew[x-1]+MatCPFT[x-1]
    }
    
    d10 <- 0
d25 <- 0
d50 <- 0
d75 <- 0
d90 <- 0

for (v in seq(from=1, to=dnum, by=1)) {
 
 if (d10 == 0 && MatCPFT[v] > 10) { #interpolate with value before for d10
  d10 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (10 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d25 == 0 && MatCPFT[v] > 25) { #single if's because it s also possible that multiple values have to be interpolated between the same pair of data
  d25 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (25 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d50 == 0 && MatCPFT[v] > 50) { 
  d50 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (50 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d75 == 0 && MatCPFT[v] > 75) { 
  d75 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (75 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d90 == 0 && MatCPFT[v] > 90) { 
  d90 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (90 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
}




d10old <- as.numeric(matpre[13])
d25old <- as.numeric(matpre[14])
d50old <- as.numeric(matpre[15])
d75old <- as.numeric(matpre[16])
d90old <- as.numeric(matpre[17])

#print(d10old)

#should d10, d25 etc be updated?

d10new <<- d10old
     d25new <<- d25old
     d50new <<- d50old
     d75new <<- d75old
     d90new <<- d90old

if (!all(!is.na(c(d10old,d25old,d50old,d75old,d90old)))) { # !is.na=mit wert; !all()=nicht alle mit wert

updateD <- tkmessageBox(title = "Update d(CPFT)", message=paste("Within the saved values of d10, d25, d50, d75 and d90, at least one contains no number.\n\nWould you like to update the saved values to the values calculable from the particle size distribution?\n\nThe calculable d10, d25, d50, d75 and d90 values are in ",tclvalue(dunit),": ",round(d10, digits=accuracy),", ",round(d25, digits=accuracy),", ",round(d50, digits=accuracy),", ",round(d75, digits=accuracy)," and ",round(d90, digits=accuracy), ".",sep=""), icon = "question", type = "yesno")
message("  Update d(CPFT)? Decision: ",updateD)
    if (tclvalue(updateD) == "yes") { 
      #call to other function
      
     d10new <<- round(d10, digits=accuracy)
     d25new <<- round(d25, digits=accuracy)
     d50new <<- round(d50, digits=accuracy)
     d75new <<- round(d75 , digits=accuracy)
     d90new <<- round(d90 , digits=accuracy)
     
    } 

} else if (round(d10, digits=accuracy) != d10old || round(d25, digits=accuracy) != d25old || round(d50, digits=accuracy) != d50old || round(d75, digits=accuracy) != d75old || round(d90, digits=accuracy) != d90old) {

updateD <- tkmessageBox(title = "Update d(CPFT)", message=paste("The d10, d25, d50, d75 and d90 values calculable from the particle size distribution are in ",tclvalue(dunit),": ",round(d10, digits=accuracy),", ",round(d25, digits=accuracy),", ",round(d50, digits=accuracy),", ",round(d75, digits=accuracy)," and ",round(d90, digits=accuracy), ".\n\nIn this sequence, the values saved in the material information are in ",tclvalue(dunit),": ", d10old,", ",d25old,", ",d50old,", ",d75old," and ",d90old,".\n\nWould you like to update the saved values to the calculable values?",sep=""), icon = "question", type = "yesno")
message("  Update d(CPFT)? Decision: ",updateD)
    if (tclvalue(updateD) == "yes") { 
      #call to other function
      
     d10new <<- round(d10, digits=accuracy)
     d25new <<- round(d25, digits=accuracy)
     d50new <<- round(d50, digits=accuracy)
     d75new <<- round(d75 , digits=accuracy)
     d90new <<- round(d90 , digits=accuracy)
     
    } 

}

#print (d10new)
#print (d25new)
#print (d50new)
#print (d75new)
#print (d90new)



#muss noch gespeichert werden (mit dXXnew werten)


    id <- as.character(matpre[1])
    nam <- as.character(matpre[2])
    last <- as.character(matpre[3])
    orig <- as.character(matpre[4])
    date <- as.character(matpre[5])
    price <- format(as.numeric(matpre[6]), decimal.mark=tclvalue(decpoint), na.encode=F)
    td <- format(as.numeric(matpre[7]), decimal.mark=tclvalue(decpoint), na.encode=F)
    tddate <- as.character(matpre[8])
    tdmeas <- as.character(matpre[9])
    ssa <- format(as.numeric(matpre[10]), decimal.mark=tclvalue(decpoint), na.encode=F)
    ssadate <- as.character(matpre[11])
    ssameas <- as.character(matpre[12])
    d10s <- format(d10new, decimal.mark=tclvalue(decpoint), na.encode=F)
    d25s <- format(d25new, decimal.mark=tclvalue(decpoint), na.encode=F)
    d50s <- format(d50new, decimal.mark=tclvalue(decpoint), na.encode=F)
    d75s <- format(d75new, decimal.mark=tclvalue(decpoint), na.encode=F)
    d90s <- format(d90new, decimal.mark=tclvalue(decpoint), na.encode=F)
    ddate <- as.character(matpre[18])
    dmeas <- as.character(matpre[19])
    #dmax <- as.numeric(vals[1])
    #n <- as.numeric(vals[2])
    
    materinfo <- c(id,nam,last,orig,date,price,td,tddate,tdmeas,ssa,ssadate,ssameas,d10s,d25s,d50s,d75s,d90s,ddate,dmeas)
    test2 <- gsub("NA","",materinfo)
    
    #abfrage, ob database 1 oder mehr amterialien hatte..?
    
    if (length(MatList) == 1) {#database ueberschreiben; MatSelection updaten; next
    
    message("  Database contains only one material.")
    
      col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
      col2 <- c(test2[2:length(mathead)],format(round(VolMatModNew, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo
      
      newmatdb <- data.frame(cbind(col1,col2), row.names=1:length(col1))
      newmatdb <- newmatdb[,2, drop=F]

      write.table(newmatdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", col.names=paste("Identifier;",id,sep=""), row.names=col1, quote=F)
      
      #print(test2[1])
      
      #MatSelection <<- test2[1] #oder id
      
      #print(MatSelection)
      
      #next #notwendig?!? wo kommt das if/else sonst raus - vielleciht am ende von while?
    
    } else {#aus alter datenbank alten eintrag löschen; neuen hinzufuegen; MatSelection updaten; next (um material-info-dialog mit enuen eingaben aufzurufen)
    
    message("  Database contains more than one material.")
    
    MatDB <- NULL
    MatTF <- MatList %in% MatSelection
    #print(MatTF)
    for (x in seq(from=1, to=length(MatList), by=1)) {
      if (!MatTF[x]) {
        MatDB <- c(MatDB, MatList[x])
      }
    }
    
    #print (MatDB)
    
    col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
    
    
    prepDB <- data.frame(col1, row.names=1:length(col1))
    
    #prepDB <- dbinfo[,MatDB[1],drop=F]
    
    #print (prepDB)
    
    #if (length(MatDB > 1)) {
      for (m in seq(from=1, to=length(MatDB), by=1)) {
        prepDB <- cbind(prepDB, dbinfo[,MatDB[m],drop=F])
      }
    #}
    
    #print (prepDB)
    
    #colnames(prepDB) <- c("Identifier",MatDB)
#print(prepDB)

#prepDB <- prepDB[,colnames(prepDB)[2:length(colnames(prepDB))], drop=F]
#print(prepDB)


#neu hinzufuegen
col2 <- c(test2[2:length(mathead)],format(round(VolMatModNew, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo
      
savematdb <- data.frame(cbind(prepDB,col2), row.names=1:length(col1))

#print(savematdb)

colnames(savematdb) <- c("Identifier",MatDB,id)

#print(savematdb)

savematdb <- savematdb[,colnames(savematdb)[2:length(colnames(savematdb))], drop=F]

#print(savematdb)

    
    #write.table(prepDB,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep=""),MatDB[2:length(MatDB)]), quote=F)
    
    
    
    
    
    #col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))


#print(MatList)

#print(dbinfo[,MatList])


#savematdb <- data.frame(cbind(col1,col2,dbinfo[,MatList], drop=F), row.names=1:length(col1))
#print(savematdb)

#colnames(savematdb) <- c("Identifier",id,MatList)
#print(savematdb)

#savematdb <- savematdb[,colnames(savematdb)[2:length(colnames(savematdb))], drop=F]
#print(savematdb)

#save dataframe with new first line (=col.names)
#write.table(savematdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",id,sep=""),MatDB), quote=F)

if (length(MatDB) == 1) {

write.table(savematdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep=""),id), quote=F)

} else {

write.table(savematdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep=""),MatDB[2:length(MatDB)],id), quote=F)
}


    
    }#end else MatList == 1

  
  message("  Material particle size data edited by updating database.")
  
  
  
  
  
  } else { #end var ==5; beginn var ==6 import particle sizes
  
  
 
  
  matfile <- tk_choose.files(caption="Select material data file", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))   
    if (length(matfile) == 0) { next }
    
    message("  Import particle size data from raw material file ",matfile)
   
    #requirements on file: two columns 'Diameter' and 'Percent'; no NAs, words, ... only numbers 
   
    matdata <- read.csv2(file=matfile,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

    #check for every dsize, if it is included in matdata$Diameter. If not, add row (Percent=NA).
    
    
    
       #cpft berechnen
    
matcpft <- vector(length=length(matdata$Diameter))    

    if (typadd == 1) {
    
      matcpft[1] <- 0
    for(x in seq(from=2, to=length(matdata$Diameter), by=1)) {
      matcpft[x] <- matdata$Percent[x-1]+matcpft[x-1]
    }
      
    } else if (typadd == 2) {
    
      matcpft[1] <- matdata$Percent[1]
    for(x in seq(from=2, to=length(matdata$Diameter), by=1)) {
      matcpft[x] <- matdata$Percent[x]+matcpft[x-1]
    }
    
    } else if (typadd == 3) {
      
      matcpft <- matdata$Percent
    
    } else {
    
      matcpft <- 100-matdata$Percent
      
    }    
    
   
    
    sizemat <- data.frame(cbind(matdata$Diameter,matcpft))
   colnames(sizemat) <- c("Diameter","Percent")
   #print(sizemat)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    sizeinput <- data.frame(cbind(dsizes,rep(NA, dnum)))
    colnames(sizeinput) <- c("Diameter","Percent")
    
    #print("import")
    #print(matdata)
    #print(sizeinput)
    
    #matmerge <- merge(matdata,sizeinput,all=TRUE)
    matmerge <- merge(sizemat,sizeinput,all=TRUE)
    
    #print(matmerge)
    
    matmerge <- matmerge[!duplicated(matmerge$Diameter), ]
    
    #print(matmerge)
    
    #put first and last percent if necessary
    
    if (is.na(matmerge$Percent[1])) {
      for (m in seq(from=1, to=length(matmerge$Diameter), by=1)) {
        if (!is.na(matmerge$Percent[m])) { 
          matmerge$Percent[1] <- matmerge$Percent[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    if (is.na(matmerge$Percent[length(matmerge$Diameter)])) {
      for (m in seq(from=length(matmerge$Diameter), to=1, by=-1)) {
        if (!is.na(matmerge$Percent[m])) { 
          matmerge$Percent[length(matmerge$Diameter)] <- matmerge$Percent[m]
          break
        }
      }
    }
    
    #print(matmerge)
    
    
    #lineares fuellen
    
    i=1
    while (i < length(matmerge$Diameter)) {#<=
    
      if (is.na(matmerge$Percent[i])) {#beginn NA ist element VolMat[i] falls TRUE; VolMat[i-1] unterer Wert fuer Lienarisierung
        for (j in seq(from=i+1, to=length(matmerge$Diameter), by=1)) {
          if (!is.na(matmerge$Percent[j])) {#erstes element nach i, was kein NA mehr ist, ist j; damit VolMat[j] oberer Wert fuer Linearisierung
            
            #Linearisierung fuer alle elemente zwischen i-1 und j
            for (k in seq(from=i, to=j-1, by=1)) {
              matmerge$Percent[k] <- matmerge$Percent[i-1] + (matmerge$Diameter[k]-matmerge$Diameter[i-1])*(matmerge$Percent[j]-matmerge$Percent[i-1])/(matmerge$Diameter[j]-matmerge$Diameter[i-1])
            }
            
            i=j+1#oder ohne +1?
            break #out of for j in seq-loop
            
          }
        }#end for j in seq
        
      } else {
       i=i+1
      }
      
    }#end while i < length(matmerge$Diameter)
    
    #print(matmerge)
    
    
    #next: kep only dsizes resp sizeinput
    matcrop <- matmerge[matmerge$Diameter %in% dsizes,]
    
    #print(matcrop)
    
    VolMatModRev <- matcrop$Percent
    
    #print(VolMat)
    
    
    VolMatModNew <- vector(length=dnum)    
      
      for(x in seq(from=1, to=dnum-1, by=1)) {
        VolMatModNew[x] <- matcrop$Percent[x+1]-matcrop$Percent[x]
      }
      VolMatModNew[dnum] <- 0 
    
    
    
    



    #calculate d10, d25, d50, d75, d90 from cpft
    MatCPFT <- vector(length=dnum)
    MatCPFT[1] <- 0
    for(x in seq(from=2, to=dnum, by=1)) {
      MatCPFT[x] <- VolMatModNew[x-1]+MatCPFT[x-1]
    }
    
    d10 <- 0
d25 <- 0
d50 <- 0
d75 <- 0
d90 <- 0

for (v in seq(from=1, to=dnum, by=1)) {
 
 if (d10 == 0 && MatCPFT[v] > 10) { #interpolate with value before for d10
  d10 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (10 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d25 == 0 && MatCPFT[v] > 25) { #single if's because it s also possible that multiple values have to be interpolated between the same pair of data
  d25 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (25 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d50 == 0 && MatCPFT[v] > 50) { 
  d50 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (50 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d75 == 0 && MatCPFT[v] > 75) { 
  d75 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (75 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
 if (d90 == 0 && MatCPFT[v] > 90) { 
  d90 <- dsizes[v-1] + (dsizes[v] - dsizes[v-1]) * (90 - MatCPFT[v-1]) / (MatCPFT[v] - MatCPFT[v-1])
 }
}




d10old <- as.numeric(matpre[13])
d25old <- as.numeric(matpre[14])
d50old <- as.numeric(matpre[15])
d75old <- as.numeric(matpre[16])
d90old <- as.numeric(matpre[17])

#should d10, d25 etc be updated?

d10new <<- d10old
     d25new <<- d25old
     d50new <<- d50old
     d75new <<- d75old
     d90new <<- d90old

if (!all(!is.na(c(d10old,d25old,d50old,d75old,d90old)))) { # !is.na=mit wert; !all()=nicht alle mit wert

updateD <- tkmessageBox(title = "Update d(CPFT)", message=paste("Within the saved values of d10, d25, d50, d75 and d90, at least one contains no number.\n\nWould you like to update the saved values to the values calculable from the particle size distribution?\n\nThe calculable d10, d25, d50, d75 and d90 values are in ",tclvalue(dunit),": ",round(d10, digits=accuracy),", ",round(d25, digits=accuracy),", ",round(d50, digits=accuracy),", ",round(d75, digits=accuracy)," and ",round(d90, digits=accuracy), ".",sep=""), icon = "question", type = "yesno")
message("  Update d(CPFT)? Decision: ",updateD)
    if (tclvalue(updateD) == "yes") { 
      #call to other function
      
     d10new <<- round(d10, digits=accuracy)
     d25new <<- round(d25, digits=accuracy)
     d50new <<- round(d50, digits=accuracy)
     d75new <<- round(d75 , digits=accuracy)
     d90new <<- round(d90 , digits=accuracy)
     
    } 

} else if (round(d10, digits=accuracy) != d10old || round(d25, digits=accuracy) != d25old || round(d50, digits=accuracy) != d50old || round(d75, digits=accuracy) != d75old || round(d90, digits=accuracy) != d90old) {

updateD <- tkmessageBox(title = "Update d(CPFT)", message=paste("The d10, d25, d50, d75 and d90 values calculable from the particle size distribution are in ",tclvalue(dunit),": ",round(d10, digits=accuracy),", ",round(d25, digits=accuracy),", ",round(d50, digits=accuracy),", ",round(d75, digits=accuracy)," and ",round(d90, digits=accuracy), ".\n\nIn this sequence, the values saved in the material information are in ",tclvalue(dunit),": ", d10old,", ",d25old,", ",d50old,", ",d75old," and ",d90old,".\n\nWould you like to update the saved values to the calculable values?",sep=""), icon = "question", type = "yesno")
message("  Update d(CPFT)? Decision: ",updateD)
    if (tclvalue(updateD) == "yes") { 
      #call to other function
      
     d10new <<- round(d10, digits=accuracy)
     d25new <<- round(d25, digits=accuracy)
     d50new <<- round(d50, digits=accuracy)
     d75new <<- round(d75 , digits=accuracy)
     d90new <<- round(d90 , digits=accuracy)
     
    } 

}



#print (d10new)
#print (d25new)
#print (d50new)
#print (d75new)
#print (d90new)


#muss noch gespeichert werden (mit dXXnew werten)


    id <- as.character(matpre[1])
    nam <- as.character(matpre[2])
    last <- as.character(matpre[3])
    orig <- as.character(matpre[4])
    date <- as.character(matpre[5])
    price <- format(as.numeric(matpre[6]), decimal.mark=tclvalue(decpoint), na.encode=F)
    td <- format(as.numeric(matpre[7]), decimal.mark=tclvalue(decpoint), na.encode=F)
    tddate <- as.character(matpre[8])
    tdmeas <- as.character(matpre[9])
    ssa <- format(as.numeric(matpre[10]), decimal.mark=tclvalue(decpoint), na.encode=F)
    ssadate <- as.character(matpre[11])
    ssameas <- as.character(matpre[12])
    d10s <- format(d10new, decimal.mark=tclvalue(decpoint), na.encode=F)
    d25s <- format(d25new, decimal.mark=tclvalue(decpoint), na.encode=F)
    d50s <- format(d50new, decimal.mark=tclvalue(decpoint), na.encode=F)
    d75s <- format(d75new, decimal.mark=tclvalue(decpoint), na.encode=F)
    d90s <- format(d90new, decimal.mark=tclvalue(decpoint), na.encode=F)
    ddate <- as.character(matpre[18])
    dmeas <- as.character(matpre[19])
    #dmax <- as.numeric(vals[1])
    #n <- as.numeric(vals[2])
    
    materinfo <- c(id,nam,last,orig,date,price,td,tddate,tdmeas,ssa,ssadate,ssameas,d10s,d25s,d50s,d75s,d90s,ddate,dmeas)
    test2 <- gsub("NA","",materinfo)
    
    #abfrage, ob database 1 oder mehr amterialien hatte..?
    
    if (length(MatList) == 1) {#database ueberschreiben; MatSelection updaten; next
    message("  Database contains one material.")
    
      col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
      col2 <- c(test2[2:length(mathead)],format(round(VolMatModNew, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo
      
      newmatdb <- data.frame(cbind(col1,col2), row.names=1:length(col1))
      newmatdb <- newmatdb[,2, drop=F]

      write.table(newmatdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", col.names=paste("Identifier;",id,sep=""), row.names=col1, quote=F)
      
      message("  Material edited by updating database.")
    
    } else {#aus alter datenbank alten eintrag löschen; neuen hinzufuegen; MatSelection updaten; next (um material-info-dialog mit enuen eingaben aufzurufen)
    
    message("  Database contains more than one material.")
    
    MatDB <- NULL
    MatTF <- MatList %in% MatSelection
    #print(MatTF)
    for (x in seq(from=1, to=length(MatList), by=1)) {
      if (!MatTF[x]) {
        MatDB <- c(MatDB, MatList[x])
      }
    }
    
    #print (MatDB)
    
    col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))
    
    
    prepDB <- data.frame(col1, row.names=1:length(col1))
    
    #prepDB <- dbinfo[,MatDB[1],drop=F]
    
    #print (prepDB)
    
    #if (length(MatDB > 1)) {
      for (m in seq(from=1, to=length(MatDB), by=1)) {
        prepDB <- cbind(prepDB, dbinfo[,MatDB[m],drop=F])
      }
    #}
    
    #print (prepDB)
    
    #colnames(prepDB) <- c("Identifier",MatDB)
#print(prepDB)

#prepDB <- prepDB[,colnames(prepDB)[2:length(colnames(prepDB))], drop=F]
#print(prepDB)


#neu hinzufuegen
col2 <- c(test2[2:length(mathead)],format(round(VolMatModNew, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo
      
savematdb <- data.frame(cbind(prepDB,col2), row.names=1:length(col1))

#print(savematdb)

colnames(savematdb) <- c("Identifier",MatDB,id)

#print(savematdb)

savematdb <- savematdb[,colnames(savematdb)[2:length(colnames(savematdb))], drop=F]

#print(savematdb)

    
    #write.table(prepDB,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep=""),MatDB[2:length(MatDB)]), quote=F)
    
    
    
    
    
    #col1 <- c(mathead[2:length(mathead)],format(dsizes, decimal.mark=tclvalue(decpoint), na.encode=F))


#print(MatList)

#print(dbinfo[,MatList])


#savematdb <- data.frame(cbind(col1,col2,dbinfo[,MatList], drop=F), row.names=1:length(col1))
#print(savematdb)

#colnames(savematdb) <- c("Identifier",id,MatList)
#print(savematdb)

#savematdb <- savematdb[,colnames(savematdb)[2:length(colnames(savematdb))], drop=F]
#print(savematdb)

#save dataframe with new first line (=col.names)
#write.table(savematdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",id,sep=""),MatDB), quote=F)

if (length(MatDB) == 1) {

write.table(savematdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep=""),id), quote=F)

} else {

write.table(savematdb,file=dbfile, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",MatDB[1],sep=""),MatDB[2:length(MatDB)],id), quote=F)
}


    
    }#end else MatList == 1
  
  
  
  
  message("  Material edited by updating database.")
  
  
  
  
  
  } #end var==6 import particle sizes
  
  
  
  

    } #end while update material-edit-window

    if (var < 2) {
      break
    }
    
} #end while editmaterial

} #end editmaterial function
