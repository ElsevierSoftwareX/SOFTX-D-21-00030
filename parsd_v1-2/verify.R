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





modelver <- function(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol) {

#return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 for back to open recipe or database
  #return 3 for back to bounds
 
  matdmax <- 0
  
  for (i in seq(from=1, to=length(MatSelection),by=1)) {
    for (j in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (preparation[j,MatSelection[i]] > 100-overgraining) {
        if (preparation$Diameter[j] > matdmax) {
          matdmax <- preparation$Diameter[j]
        }
        break #raus aus j-schleife, sobald erstmals 100% auftauchen

      }
    }
  }
  
   while (TRUE) {
   
   message("BEGIN verify-model function\n")
  
  winmod <- tktoplevel(bg=hintergrund)
  tkwm.title(winmod,"Choose model for verification")
  tkraise(winmod)

  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod, bg=menue)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","10model","10.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())
  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text='\nChoose model for verification:', bg=hintergrund), pady=10, padx=10)

  tkgrid(tkbutton(winmod, text='Andreasen model', bg=knoepfe, command=function() tclvalue(cm)<-4), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Psi model', bg=knoepfe, command=function() tclvalue(cm)<-6), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Kawamura model', bg=knoepfe, command=function() tclvalue(cm)<-7), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Dinger/Funk model', bg=knoepfe, command=function() tclvalue(cm)<-5), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Modified Psi model', bg=knoepfe, command=function() tclvalue(cm)<-8), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Modified Kawamura model', bg=knoepfe, command=function() tclvalue(cm)<-9), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Other/free model', bg=knoepfe, command=function() tclvalue(cm)<-10), pady=5, padx=10, sticky="ew")

  tkfocus(winmod)

  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  message("  Choose model (1 Main menu, 2 Material selection, 3 Batch, 4 Andreasen model, 5 Dinger/Funk model, 6 Psi model, 7 Kawamura model, 8 Modified Psi model, 9 Modified Kawamura model, 10 Free model): ",var)
  
  if (var < 4) { return(var) } 
  
  #ModSelection <- tk_select.list(choices=c("Andreasen-Model", "Dinger/Funk-Model", "Psi-Model", "Kawamura-Model", "Modified Psi-Model", "Modified Kawamura-Model", "Other/free model"), preselect=NULL, multiple=FALSE, title="Choose model for fitting:")
  
  
VolModel <<- vector(length=length(preparation$Diameter))

  #if (ModSelection=="") { 
  #  return(0)
  #} else 
  if (var==4) { 
    ModSelection <<- "Andreasen model"

    vals <- varEntryDialog(vars=c('dmax', 'n'), labels=c(paste('Maximum particle size in',tclvalue(dunit),':'), 'Distribution modulus n:'), title=ModSelection,prompt=paste("Adjust ",ModSelection,":",sep=""),preset=c(matdmax, stdandn),cancellab='Back to Model Selection')
    if(is.null(vals)) { next }#if input ok, go on, if not return to model selection

    dmax <- as.numeric(vals[1])
    n <- as.numeric(vals[2])
 
    if (all(!is.na(c(dmax, n))) == FALSE) {
      tkmessageBox(title = "Model definition error", message="At least one input value was not given or registrated as number.", icon = "error", type = "ok")
      next
    }

    for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax < preparation$Diameter[i]) {
        VolModel[i] <<- 100 }
      else {
        VolModel[i] <<- 100*(preparation$Diameter[i]/dmax)^n 

      }
    }
 
  modelparam <<- vector(length=3)
  modelparam[1] <<- ModSelection
  modelparam[2] <<- paste("d(max) in",tclvalue(dunit),":")
  modelparam[3] <<- "n:"
  
  modelvalue <<- vector(length=3)
  modelvalue[1] <<- NA
  modelvalue[2] <<- dmax
  modelvalue[3] <<- n
 
   modelinfo <<- vector(length=3)
 
   modelinfo[1] <<- ModSelection
 
   modelinfo[2] <<- paste("d(max) =",dmax,tclvalue(dunit))
 
   modelinfo[3] <<- paste("n =",n)
 
   modelsummary <<- paste(modelinfo[1],modelinfo[2],modelinfo[3],sep="\n")
   #break } 
   
  } else if (var==5) { 
    ModSelection <<- "Dinger/Funk model"
    
    if (is.na(stddfdmin)) { stddfdmin <- preparation$Diameter[1] }
    
    vals <- varEntryDialog(vars=c('dmax', 'dmin', 'n'), labels=c(paste('Maximum particle size in',tclvalue(dunit),':'), paste('Minimum particle size in',tclvalue(dunit),':'), 'Distribution modulus n:'), title=ModSelection,prompt=paste("Adjust ",ModSelection,":",sep=""),preset=c(matdmax, stddfdmin, stddfn),cancellab='Back to Model Selection')
    if(is.null(vals)) { next } #if input ok, go on, if not return to model selection
    
    dmax <- as.numeric(vals[1])
    dmin <- as.numeric(vals[2])
    n <- as.numeric(vals[3])
    
    if (all(!is.na(c(dmax, dmin, n))) == FALSE) {
      tkmessageBox(title = "Model definition error", message="At least one input value was not given or registrated as number.", icon = "error", type = "ok")
      next
    }
    
    for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax < preparation$Diameter[i]) {
        VolModel[i] <<- 100
      }
 else if (dmin < preparation$Diameter[i]) {
        VolModel[i] <<- 100*(preparation$Diameter[i]^n - dmin^n)/(dmax^n - dmin^n)
      }
 else {
        VolModel[i] <<- 0
      }
    }

    
    modelparam <<- vector(length=4)
  modelparam[1] <<- ModSelection
  modelparam[2] <<- paste("d(max) in",tclvalue(dunit),":")
  modelparam[3] <<- paste("d(min) in",tclvalue(dunit),":")
  modelparam[4] <<- "n:"
  
  modelvalue <- vector(length=4)
  modelvalue[1] <<- NA
  modelvalue[2] <<- dmax
  modelvalue[3] <<- dmin
  modelvalue[4] <<- n
    
    
    
    modelinfo <<- vector(length=4)
 
   modelinfo[1] <<- ModSelection
 
   modelinfo[2] <<- paste("d(max) =",dmax,tclvalue(dunit))
 
   modelinfo[3] <<- paste("d(min) =",dmin,tclvalue(dunit))
 
   modelinfo[4] <<- paste("n =",n)
 
   
 
   modelsummary <<- paste(modelinfo[1],modelinfo[2],modelinfo[3],modelinfo[4],sep="\n")
   
   #break }
   
  } else if (var==6) { 
    ModSelection <<- "Psi model"
  
    vals <- varEntryDialog(vars=c('dmax', 'nmin', 'nmax'), labels=c(paste('Maximum particle size in',tclvalue(dunit),':'), 'Minimum distribution modulus n(min):', 'Maximum distribution modulus n(max):'), title=ModSelection,prompt=paste("Adjust ",ModSelection,":",sep=""),preset=c(matdmax, stdpsinmin, stdpsinmax),cancellab='Back to Model Selection')
    if(is.null(vals)) { next } #if input ok, go on, if not return to model selection
    
    dmax <- as.numeric(vals[1])
    nmin <- as.numeric(vals[2])
    nmax <- as.numeric(vals[3])
    
    if (all(!is.na(c(dmax, nmin, nmax))) == FALSE) {
      tkmessageBox(title = "Model definition error", message="At least one input value was not given or registrated as number.", icon = "error", type = "ok")
      next
    }
    
    for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax < preparation$Diameter[i]) {
        VolModel[i] <<- 100 
      } else {
        VolModel[i] <<- 100*(preparation$Diameter[i]/dmax)^(nmin + preparation$Diameter[i]*(nmax-nmin)/dmax)
      }
    }
    
    
    modelparam <<- vector(length=4)
  modelparam[1] <<- ModSelection
  modelparam[2] <<- paste("d(max) in",tclvalue(dunit),":")
  modelparam[3] <<- "n(min):"
  modelparam[4] <<- "n(max):"
  
  modelvalue <<- vector(length=4)
  modelvalue[1] <<- NA
  modelvalue[2] <<- dmax
  modelvalue[3] <<- nmin
  modelvalue[4] <<- nmax
    
    
    
    modelinfo <<- vector(length=4)
 
   modelinfo[1] <<- ModSelection
 
   modelinfo[2] <<- paste("d(max) =",dmax,tclvalue(dunit))
 
   modelinfo[3] <<- paste("n(min) =",nmin)
 
   modelinfo[4] <<- paste("n(max) =",nmax)
 
   
 
   modelsummary <<- paste(modelinfo[1],modelinfo[2],modelinfo[3],modelinfo[4],sep="\n")
   
   #break }
  
  } else if (var==7) { 
    ModSelection <<- "Kawamura model"
  
    vals <- varEntryDialog(vars=c('dmax', 'dgap', 'nand', 'ndf'), labels=c(paste('Maximum particle size in',tclvalue(dunit),':'), paste('Gap particle size in',tclvalue(dunit),':'), 'Distribution modulus of fines (Andreasen-part):', 'Distribution modulus of coarses (Furnas-part):'), title=ModSelection,prompt=paste("Adjust ",ModSelection,":",sep=""),preset=c(matdmax, stdkawadgap, stdkawanand, stdkawanfur),cancellab='Back to Model Selection')
    if(is.null(vals)) { next } #if input ok, go on, if not return to model selection
    
    dmax <- as.numeric(vals[1])
    dgap <- as.numeric(vals[2])
    nand <- as.numeric(vals[3])
    ndf <- as.numeric(vals[4])
    
    if (all(!is.na(c(dmax, dgap, nand, ndf))) == FALSE) {
      tkmessageBox(title = "Model definition error", message="At least one input value was not given or registrated as number.", icon = "error", type = "ok")
      next
    }
    
    for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax < preparation$Diameter[i]) {
        VolModel[i] <<- 100 }

      else if (dgap < preparation$Diameter[i]) { #Furnas-part

        VolModel[i] <<- 100*( (dgap/dmax)^nand + (1 - (dgap/dmax)^nand)*(preparation$Diameter[i]^ndf - dgap^ndf)/(dmax^ndf - dgap^ndf) )
      }
 else { #Andreasen-part
        VolModel[i] <<- 100*(preparation$Diameter[i]/dmax)^nand
      }
    }
    
    
    modelparam <<- vector(length=5)
  modelparam[1] <<- ModSelection
  modelparam[2] <<- paste("d(max) in",tclvalue(dunit),":")
  modelparam[3] <<- paste("d(gap) in",tclvalue(dunit),":")
  modelparam[4] <<- "n(Andreasen-part):"
  modelparam[5] <<- "n(Furnas-part):"
  
  modelvalue <<- vector(length=5)
  modelvalue[1] <<- NA
  modelvalue[2] <<- dmax
  modelvalue[3] <<- dgap
  modelvalue[4] <<- nand
  modelvalue[5] <<- ndf
    
    
    
    
    modelinfo <<- vector(length=5)
 
   modelinfo[1] <<- ModSelection
 
   modelinfo[2] <<- paste("d(max) =",dmax,tclvalue(dunit))
 
   modelinfo[3] <<- paste("d(gap) =",dgap,tclvalue(dunit))
 
   modelinfo[4] <<- paste("n(Andreasen-part) =",nand)
 
   modelinfo[5] <<- paste("n(Furnas-part) =",ndf)
 
   
 
   modelsummary <<- paste(modelinfo[1],modelinfo[2],modelinfo[3],modelinfo[4],modelinfo[5],sep="\n")
  
  #break }
  
  } else if (var==8) { 
    ModSelection <<- "Modified Psi model"
    
    if (is.na(stdmpsidmin)) { stdmpsidmin <- preparation$Diameter[1] }
  
    vals <- varEntryDialog(vars=c('dmax', 'dmin', 'nmin', 'nmax'), labels=c(paste('Maximum particle size in',tclvalue(dunit),':'), paste('Minimum particle size in',tclvalue(dunit),':'), 'Minimum distribution modulus n(min):', 'Maximum distribution modulus n(max):'), title=ModSelection,prompt=paste("Adjust ",ModSelection,":",sep=""),preset=c(matdmax, stdmpsidmin, stdmpsinmin, stdmpsinmax),cancellab='Back to Model Selection')
    if(is.null(vals)) { next } #if input ok, go on, if not return to model selection
    
    dmax <- as.numeric(vals[1])
    dmin <- as.numeric(vals[2])
    nmin <- as.numeric(vals[3])
    nmax <- as.numeric(vals[4])
    
    if (all(!is.na(c(dmax, dmin, nmin, nmax))) == FALSE) {
      tkmessageBox(title = "Model definition error", message="At least one input value was not given or registrated as number.", icon = "error", type = "ok")
      next
    }
    
    for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax < preparation$Diameter[i]) {
        VolModel[i] <<- 100 
      }
 else if (dmin < preparation$Diameter[i]) {
        VolModel[i] <<- 100*(preparation$Diameter[i]^(nmin+preparation$Diameter[i]*(nmax-nmin)/dmax) - dmin^(nmin+preparation$Diameter[i]*(nmax-nmin)/dmax))/(dmax^(nmin+preparation$Diameter[i]*(nmax-nmin)/dmax) - dmin^(nmin+preparation$Diameter[i]*(nmax-nmin)/dmax)) 

      }
 else {
        VolModel[i] <<- 0
      }
    }
    
    
    
    modelparam <<- vector(length=5)
  modelparam[1] <<- ModSelection
  modelparam[2] <<- paste("d(max) in",tclvalue(dunit),":")
  modelparam[3] <<- paste("d(min) in",tclvalue(dunit),":")
  modelparam[4] <<- "n(min):"
  modelparam[5] <<- "n(max):"
  
  modelvalue <<- vector(length=5)
  modelvalue[1] <<- NA
  modelvalue[2] <<- dmax
  modelvalue[3] <<- dmin
  modelvalue[4] <<- nmin
  modelvalue[5] <<- nmax
    
    
    
    modelinfo <<- vector(length=5)
 
   modelinfo[1] <<- ModSelection
 
   modelinfo[2] <<- paste("d(max) =",dmax,tclvalue(dunit))
 
   modelinfo[3] <<- paste("d(min) =",dmin,tclvalue(dunit))
 
   modelinfo[4] <<- paste("n(min) =",nmin)
 
   modelinfo[5] <<- paste("n(max) =",nmax)
 
   
 
   modelsummary <<- paste(modelinfo[1],modelinfo[2],modelinfo[3],modelinfo[4],modelinfo[5],sep="\n")
  
  #break }
  
  } else if (var==9) { 
    ModSelection <<- "Modified Kawamura model"
    
    if (is.na(stdmkawadmin)) { stdmkawadmin <- preparation$Diameter[1] }
  
    vals <- varEntryDialog(vars=c('dmax', 'dmin', 'dgap', 'nand', 'ndf'), labels=c(paste('Maximum particle size in',tclvalue(dunit),':'), paste('Minimum particle size in',tclvalue(dunit),':'), paste('Gap particle size in',tclvalue(dunit),':'), 'Distribution modulus of fines (Dinger/Funk-part):', 'Distribution modulus of coarses (Furnas-part):'), title=ModSelection,prompt=paste("Adjust ",ModSelection,":",sep=""),preset=c(matdmax, stdmkawadmin, stdmkawadgap, stdmkawanand, stdmkawanfur),cancellab='Back to Model Selection')
    if(is.null(vals)) { next } #if input ok, go on, if not return to model selection
    
    dmax <- as.numeric(vals[1])
    dmin <- as.numeric(vals[2])
    dgap <- as.numeric(vals[3])
    nand <- as.numeric(vals[4])
    ndf <- as.numeric(vals[5])
    
    if (all(!is.na(c(dmax, dmin, dgap, nand, ndf))) == FALSE) {
      tkmessageBox(title = "Model definition error", message="At least one input value was not given or registrated as number.", icon = "error", type = "ok")
      next
    }
    
    for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax < preparation$Diameter[i]) {
        VolModel[i] <<- 100 
      } else if (dgap < preparation$Diameter[i]) { #Furnas-part
        VolModel[i] <<- 100*( (dgap^nand - dmin^nand)/(dmax^nand - dmin^nand) + (1 - (dgap^nand - dmin^nand)/(dmax^nand - dmin^nand)) * (preparation$Diameter[i]^ndf - dgap^ndf)/(dmax^ndf - dgap^ndf) )
      } else if (dmin < preparation$Diameter[i]) { #DF-part
        VolModel[i] <<- 100*(preparation$Diameter[i]^nand - dmin^nand)/(dmax^nand - dmin^nand) 

      }
 else {
        VolModel[i] <<- 0
      }
    }
    
    
    modelparam <<- vector(length=5)
  modelparam[1] <<- ModSelection
  modelparam[2] <<- paste("d(max) in",tclvalue(dunit),":")
  modelparam[3] <<- paste("d(min) in",tclvalue(dunit),":")
  modelparam[4] <<- paste("d(gap) in",tclvalue(dunit),":")
  modelparam[5] <<- "n(Dinger/Funk-part):"
  modelparam[6] <<- "n(Furnas-part):"
  
  modelvalue <<- vector(length=5)
  modelvalue[1] <<- NA
  modelvalue[2] <<- dmax
  modelvalue[3] <<- dmin
  modelvalue[4] <<- dgap
  modelvalue[5] <<- nand
  modelvalue[6] <<- ndf
    
    
    modelinfo <<- vector(length=6)
 
   modelinfo[1] <<- ModSelection
 
   modelinfo[2] <<- paste("d(max) =",dmax,tclvalue(dunit))
 
   modelinfo[3] <<- paste("d(min) =",dmin,tclvalue(dunit))
 
   modelinfo[4] <<- paste("d(gap) =",dgap,tclvalue(dunit))
 
   modelinfo[5] <<- paste("n(Dinger/Funk-part) =",nand)
 
   modelinfo[6] <<- paste("n(Furnas-part) =",ndf)
 
   
 
   modelsummary <<- paste(modelinfo[1],modelinfo[2],modelinfo[3],modelinfo[4],modelinfo[5],modelinfo[6],sep="\n")
  
  #break }
  
  } else {
  
    #there should be asked if a model can be opened from file
    FreeMod <- tkmessageBox(title = "Load Other/free model", message="Do you have the model saved?", icon = "question", type = "yesno")
message("  Free model saved? ",FreeMod)

    if (tclvalue(FreeMod) == "yes") {
    
      FileMod <- tk_choose.files(caption="Select a saved model", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))
    

      if (length(FileMod) == 0) { next } #back to model selection if cancelled
      message("  Model saved: ",FileMod)
      
        modeldf <- read.csv2(file=FileMod,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
        
        modelinfo <- vector(length=1)
        modelinfo[1] <- colnames(modeldf)[2]
        modelsummary <- paste(modelinfo[1])
        
        modelparam <- modelinfo
        modelvalue <- vector(length=1)
        modelvalue[1] <- NA
        
        
        VolModel <- modeldf[,2]
        
        #break
      
      #} #else {      
      #}
    
    } else {
  
    vals <- varEntryDialog(vars=c('name'), labels=c('Name of the model:'), title="Other/free model",prompt="Name the model:",preset=c("Other/free model"),cancellab='Back to Model Selection')
    if(is.null(vals)) { next } #if input ok, go on, if not return to model selection
    
    modelinfo <- vector(length=1)
    modelinfo[1] <- as.character(vals[1])
    modelsummary <- paste(modelinfo[1])
    
    message("  New model: ",modelinfo[1])
    
    modelparam <- modelinfo
    modelvalue <- vector(length=1)
    modelvalue[1] <- NA
    
    winnr <- ceiling(length(preparation$Diameter)/10)
    for (i in seq(from=0, to=winnr-1, by=1)) {
      
      if (i*10+10 < length(preparation$Diameter)+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9','d10'), labels=c(paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+2],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+3],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+4],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+5],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+6],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+7],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+8],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+9],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+10],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
        if(is.null(vals)) { break }
        
        VolModel[i*10+1] <- as.numeric(vals[1])
        VolModel[i*10+2] <- as.numeric(vals[2])
        VolModel[i*10+3] <- as.numeric(vals[3])
        VolModel[i*10+4] <- as.numeric(vals[4])
        VolModel[i*10+5] <- as.numeric(vals[5])
        VolModel[i*10+6] <- as.numeric(vals[6])
        VolModel[i*10+7] <- as.numeric(vals[7])
        VolModel[i*10+8] <- as.numeric(vals[8])
        VolModel[i*10+9] <- as.numeric(vals[9])
        VolModel[i*10+10] <- as.numeric(vals[10])
      
      } else if (i*10+9 < length(preparation$Diameter)+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9'), labels=c(paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+2],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+3],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+4],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+5],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+6],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+7],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+8],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+9],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
        if(is.null(vals)) { break }
        
        VolModel[i*10+1] <- as.numeric(vals[1])
        VolModel[i*10+2] <- as.numeric(vals[2])
        VolModel[i*10+3] <- as.numeric(vals[3])
        VolModel[i*10+4] <- as.numeric(vals[4])
        VolModel[i*10+5] <- as.numeric(vals[5])
        VolModel[i*10+6] <- as.numeric(vals[6])
        VolModel[i*10+7] <- as.numeric(vals[7])
        VolModel[i*10+8] <- as.numeric(vals[8])
        VolModel[i*10+9] <- as.numeric(vals[9])
        
      } else if (i*10+8 < length(preparation$Diameter)+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8'), labels=c(paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+2],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+3],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+4],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+5],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+6],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+7],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+8],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
        if(is.null(vals)) { break }
        
        VolModel[i*10+1] <- as.numeric(vals[1])
        VolModel[i*10+2] <- as.numeric(vals[2])
        VolModel[i*10+3] <- as.numeric(vals[3])
        VolModel[i*10+4] <- as.numeric(vals[4])
        VolModel[i*10+5] <- as.numeric(vals[5])
        VolModel[i*10+6] <- as.numeric(vals[6])
        VolModel[i*10+7] <- as.numeric(vals[7])
        VolModel[i*10+8] <- as.numeric(vals[8])
        
      } else if (i*10+7 < length(preparation$Diameter)+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7'), labels=c(paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+2],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+3],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+4],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+5],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+6],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+7],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
        if(is.null(vals)) { break }
        
        VolModel[i*10+1] <- as.numeric(vals[1])
        VolModel[i*10+2] <- as.numeric(vals[2])
        VolModel[i*10+3] <- as.numeric(vals[3])
        VolModel[i*10+4] <- as.numeric(vals[4])
        VolModel[i*10+5] <- as.numeric(vals[5])
        VolModel[i*10+6] <- as.numeric(vals[6])
        VolModel[i*10+7] <- as.numeric(vals[7])
        
      } else if (i*10+6 < length(preparation$Diameter)+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6'), labels=c(paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+2],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+3],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+4],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+5],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+6],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
        if(is.null(vals)) { break }
        
        VolModel[i*10+1] <- as.numeric(vals[1])
        VolModel[i*10+2] <- as.numeric(vals[2])
        VolModel[i*10+3] <- as.numeric(vals[3])
        VolModel[i*10+4] <- as.numeric(vals[4])
        VolModel[i*10+5] <- as.numeric(vals[5])
        VolModel[i*10+6] <- as.numeric(vals[6])
        
      } else if (i*10+5 < length(preparation$Diameter)+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5'), labels=c(paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+2],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+3],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+4],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+5],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
        if(is.null(vals)) { break }
        
        VolModel[i*10+1] <- as.numeric(vals[1])
        VolModel[i*10+2] <- as.numeric(vals[2])
        VolModel[i*10+3] <- as.numeric(vals[3])
        VolModel[i*10+4] <- as.numeric(vals[4])
        VolModel[i*10+5] <- as.numeric(vals[5])
        
      } else if (i*10+4 < length(preparation$Diameter)+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4'), labels=c(paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+2],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+3],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+4],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
        if(is.null(vals)) { break }
        
        VolModel[i*10+1] <- as.numeric(vals[1])
        VolModel[i*10+2] <- as.numeric(vals[2])
        VolModel[i*10+3] <- as.numeric(vals[3])
        VolModel[i*10+4] <- as.numeric(vals[4])
        
      } else if (i*10+3 < length(preparation$Diameter)+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3'), labels=c(paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+2],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+3],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
        if(is.null(vals)) { break }
        
        VolModel[i*10+1] <- as.numeric(vals[1])
        VolModel[i*10+2] <- as.numeric(vals[2])
        VolModel[i*10+3] <- as.numeric(vals[3])
        
      } else if (i*10+2 < length(preparation$Diameter)+1) {
        vals <- varEntryDialog(vars=c('d1','2'), labels=c(paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+2],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
        if(is.null(vals)) { break }
        
        VolModel[i*10+1] <- as.numeric(vals[1])
        VolModel[i*10+2] <- as.numeric(vals[2])
        
      } else {
        vals <- varEntryDialog(vars=c('d1'), labels=c(paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
        if(is.null(vals)) { break }
        
        VolModel[i*10+1] <- as.numeric(vals[1])
      }
      
    } #for
    if(is.null(vals)) { next } #if cancelled, back to model selection
    
    #if(!is.null(vals)) { 
    
      FreeMod <- tkmessageBox(title =paste("Save",modelinfo[1]), message="Do you want to save the model? If yes, save as *.csv-file, please.", icon = "question", type = "yesno")

      message("  Save model? ",FreeMod)
      
      if (tclvalue(FreeMod) == "yes") {
      
        FileMod = tclvalue(tcl("tk_getSaveFile"))
        if (FileMod != "") { 
        
        modeldf <- data.frame(cbind(preparation$Diameter, VolModel))
        colnames(modeldf) <- c("Diameter",modelinfo[1])
        
        write.table(modeldf, file=FileMod, row.names=FALSE,col.names=TRUE,dec=tclvalue(decpoint),sep=tclvalue(csvtype)) 
        message("  Save model: ",FileMod)
        }
    
      } #if save
    
    
    #break 
    
    #} #after for loop has to be tested again
    
    
    
    
    #} #if !is.null
    
    
    
    
    
    } #else free mode saved
  
  } #else choose free model
  
  message("  Model fully defined.\n")
  
  valmod <- verify(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol)
  message("END verify function, RETURN value: ",valmod,"\n")
  
  #return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 for back to open recipe or database
  #return 3 for back to batch
  #return 4 for back to model selection

if (valmod < 4) { return(valmod) }
  
  } #while loop model selection 
  

}
#end modelver()











verify <- function (MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol) {

#return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 for back to open recipe or database
  #return 3 for back to bounds
  #return 4 for back to model selection
  
  message("BEGIN verify function\n")

  preparation <- cbind(preparation, VolModel)
  colnames(preparation)[22] <- "VolModel"
  
  tuning <- preparation #save preparation in tuning
  
  #print(preparation$VolModel)
  
  #put colnames in temp from preparation and exchange with defined names for calculation

  SaveColNames <- colnames(preparation)
  colnames(preparation) <- c("Diameter","Mat1","Mat2","Mat3","Mat4","Mat5","Mat6","Mat7","Mat8","Mat9","Mat10","Mat11","Mat12","Mat13","Mat14","Mat15","Mat16","Mat17","Mat18","Mat19","Mat20","VolModel")

#hier war die curve fit optimierung, um optimale vol/mass % zu finden. die sind jetzt ja gegeben

Den <- 0
for (n in seq(from=1, to=length(MatSelection), by=1)) {
 Den <- Den+boundsVol[n]*densities[n]
}
#print(paste("Versatzdichte:",Den))



#get momentaneous mass percents
Sssa <- 0
price <- 0
for (o in seq(from=1, to=length(MatSelection), by=1)) {
 Sssa <- Sssa + boundsMass[o]*ssa[o]
 price <- price + boundsMass[o]*prices[o]
}



d10 <- 0
d25 <- 0
d50 <- 0
d75 <- 0
d90 <- 0
CurveFit <- vector(length=length(tuning$Diameter))
errsq <- vector(length=length(tuning$Diameter))
sumerrsq <- 0
for (v in seq(from=1, to=length(tuning$Diameter), by=1)) {
 
 dummy <- 0
 for (i in seq(from=1, to=length(MatSelection), by=1)) {
  #print(tuning[v,i+1])
  dummy <- dummy+boundsVol[i]*round(tuning[v,i+1],digits=accuracy)
  }
 CurveFit[v] <- dummy
 errsq[v] <- ((tuning$VolModel[v]-CurveFit[v])*(tuning$VolModel[v]-CurveFit[v]))
 sumerrsq <- sumerrsq+errsq[v]
 
 if (d10 == 0 && CurveFit[v] > 10) { #interpolate with value before for d10
  d10 <- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (10 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
 }
 if (d25 == 0 && CurveFit[v] > 25) { #single if's because it s also possible that multiple values have to be interpolated between the same pair of data
  d25 <- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (25 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
 }
 if (d50 == 0 && CurveFit[v] > 50) { 
  d50 <- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (50 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
 }
 if (d75 == 0 && CurveFit[v] > 75) { 
  d75 <- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (75 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
 }
 if (d90 == 0 && CurveFit[v] > 90) { 
  d90 <- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (90 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
 }
}

xb <- c(tuning$Diameter[1],tuning$Diameter[length(tuning$Diameter)])
yb <- c(0,100)

lc <- "x"

mag=1.2

while (TRUE) {

message("  Loading results window")

#show results and decide for finetuning 
reswin <- tktoplevel()
tkwm.title(reswin, "Results of verification")	
 tkraise(reswin)

done <- tclVar(0)

tkbind(reswin,"<Destroy>",function() tclvalue(done)<-1)

#cancel2 <- function() {		
#    tclvalue(done)<-4
#    if ( !is.null(dev.list()) ) { 
#
#        dev.off(length(dev.list())+1) #close last opened interactive R window
#
#    }
#}

#cancel <- function() {		
#    tclvalue(done)<-1
#    if ( !is.null(dev.list()) ) { 
#
#        dev.off(length(dev.list())+1) #close last opened interactive R window
#
#    }
#}
#cancel.but <- tkbutton(reswin, text='Back to\nMain menu', command=cancel)
#cancel.but <- tkmenubutton(reswin, text="Back to", relief="raised")
#CMen <- tkmenu(cancel.but, tearoff = FALSE)
#tkconfigure(cancel.but, menu=CMen)
#tkadd(CMen, "command", label = "Main Menu", command = cancel)
#tkadd(CMen, "command", label = "Material Selection", command = cancel2)


savemat <- function() {

dbsave <- tk_choose.files(caption="Select database with raw materials", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))   
if (length(dbsave) != 0) {  
dbsaveinfo <- read.csv2(file=dbsave,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
dbsaveMatList <- colnames(dbsaveinfo)[2:length(colnames(dbsaveinfo))]

#FileNewMat = tclvalue(tcl("tk_getSaveFile"))
#if (FileNewMat != "") {

#get infos
mathead <- c('Identifier','Name','Last modified','Origin/Supplier','Date supplied','Price per MT','True density','Date measured','Measurement method','Specific surface area','Date measured','Measurement method','d10','d25','d50','d75','d90','Date measured','Measurement method')

matvars <- c('ID','Name','Last','Origin','Date','Price','Density','DenDate','DenMeas','SSA','SSADate','SSAMeas','d10','d25','d50','d75','d90','dDate','dMeas')

matlabs <- c('Unique identifier (Required!):','Material name:','Last modified:','Origin/Supplier:','Date supplied:','Price per MT:','True density:','Date measured (Density):','Measurement method (Density):','Specific surface area (SSA):','Date measured (SSA):','Measurement method (SSA):',paste('d(10%) in',tclvalue(dunit),':'),paste('d(25%) in',tclvalue(dunit),':'),paste('d(50%) in',tclvalue(dunit),':'),paste('d(75%) in',tclvalue(dunit),':'),paste('d(90%) in',tclvalue(dunit),':'),'Date measured (Particle sizes):','Measurement method (Particle sizes):')

matpre <- c(paste('Batch from',paste(Sys.time())),'Calculated batch',paste(Sys.Date()),'Calculated',paste(Sys.Date()),round(price, digits=2),round(Den, digits=4),paste(Sys.Date()),'Calculated',round(Sssa, digits=4),paste(Sys.Date()),'Calculated',round(d10, digits=accuracy),round(d25, digits=accuracy),round(d50, digits=accuracy),round(d75, digits=accuracy),round(d90, digits=accuracy),paste(Sys.Date()),'Calculated')

matinfo <- varEntryDialog(vars=matvars, labels=matlabs, title="Material (batch) information",prompt="Adjust material/batch information:",preset=matpre,cancellab='Back to Verification Results')
    #if(is.null(matinfo)) { return() }#if input ok, go on, if not return ?!???

    #prices[m] <- as.numeric(gsub(",",".",dbinfo[5,MatSelection[m]]))
    
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
    #prices[m] <- as.numeric(gsub(",",".",dbinfo[5,MatSelection[m]]))

#construct dataframe without first line and first column..?

#modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(errsq, digits=accuracy),rep(NA, 4+length(modelvalue)))))

retentiondensity <- vector(length=length(tuning$Diameter))
for(x in seq(from=1, to=length(tuning$Diameter)-1, by=1)) {
 retentiondensity[x] <- CurveFit[x+1]-CurveFit[x]
}
retentiondensity[length(tuning$Diameter)] <- 0

col1 <- c(mathead[2:length(mathead)],format(tuning$Diameter, decimal.mark=tclvalue(decpoint), na.encode=F))
col2 <- c(test2[2:length(mathead)],format(round(retentiondensity, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo

#dbsaveinfo <- read.csv2(file=dbsave,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
#dbsaveMatList <- colnames(dbsaveinfo)[2:length(colnames(dbinfo))]

#print(dbsaveMatList)

savematdb <- data.frame(cbind(col1,col2,as.data.frame(dbsaveinfo[,dbsaveMatList])), row.names=1:length(col1))
#savematdb <- data.frame(cbind(col1,col2,dbsaveinfo[,dbsaveMatList],drop=F), row.names=1:length(col1))
#print(savematdb)

colnames(savematdb) <- c("Identifier",id,dbsaveMatList)
#print(savematdb)

savematdb <- savematdb[,colnames(savematdb)[2:length(colnames(savematdb))], drop=F]
#print(savematdb)

#save dataframe with new first line (=col.names)
write.table(savematdb,file=dbsave, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", row.names=col1, col.names=c(paste("Identifier;",id,sep=""),dbsaveMatList), quote=F)

message("  Saved material as raw material in database: ",dbsave)

#}#von save-file-dialog
}#if dbsave != 0
}
savemat.but <- tkbutton(reswin, text='Material database/recipe', command=savemat)

#saverecxxx <- function() {#nur hier fuer zusammenshcnippseln der savemat funktion
#
#        matdb <- data.frame(dbinfo$Identifier)
#
#        colnames(matdb) <- "Identifier"
#
#        for (i in seq(from=1, to=length(MatSelection), by=1)) {
#
#            matdb <- cbind(matdb,dbinfo[,MatSelection[i]])
#
#           colnames(matdb)[i+1] <- MatSelection[i]
#
#        }	 
#
#        write.table(matdb, file=FileRec, row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 
#        
#}












savenewmat <- function() {

FileNewMat = tclvalue(tcl("tk_getSaveFile"))
if (FileNewMat != "") {

#get infos
mathead <- c('Identifier','Name','Last modified','Origin/Supplier','Date supplied','Price per MT','True density','Date measured','Measurement method','Specific surface area','Date measured','Measurement method','d10','d25','d50','d75','d90','Date measured','Measurement method')

matvars <- c('ID','Name','Last','Origin','Date','Price','Density','DenDate','DenMeas','SSA','SSADate','SSAMeas','d10','d25','d50','d75','d90','dDate','dMeas')

matlabs <- c('Unique identifier (Required!):','Material name:','Last modified:','Origin/Supplier:','Date supplied:','Price per MT:','True density:','Date measured (Density):','Measurement method (Density):','Specific surface area (SSA):','Date measured (SSA):','Measurement method (SSA):',paste('d(10%) in',tclvalue(dunit),':'),paste('d(25%) in',tclvalue(dunit),':'),paste('d(50%) in',tclvalue(dunit),':'),paste('d(75%) in',tclvalue(dunit),':'),paste('d(90%) in',tclvalue(dunit),':'),'Date measured (Particle sizes):','Measurement method (Particle sizes):')

matpre <- c(paste('Batch from',paste(Sys.time())),'Calculated batch',paste(Sys.Date()),'Calculated',paste(Sys.Date()),round(price, digits=2),round(Den, digits=4),paste(Sys.Date()),'Calculated',round(Sssa, digits=4),paste(Sys.Date()),'Calculated',round(d10, digits=accuracy),round(d25, digits=accuracy),round(d50, digits=accuracy),round(d75, digits=accuracy),round(d90, digits=accuracy),paste(Sys.Date()),'Calculated')

matinfo <- varEntryDialog(vars=matvars, labels=matlabs, title="Material (batch) information",prompt="Adjust material/batch information:",preset=matpre,cancellab='Back to Verification Results')
    #if(is.null(matinfo)) { return() }#if input ok, go on, if not return ?!???

    #prices[m] <- as.numeric(gsub(",",".",dbinfo[5,MatSelection[m]]))
    
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
    #prices[m] <- as.numeric(gsub(",",".",dbinfo[5,MatSelection[m]]))

#construct dataframe without first line and first column..?

#modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(errsq, digits=accuracy),rep(NA, 4+length(modelvalue)))))

retentiondensity <- vector(length=length(tuning$Diameter))
for(x in seq(from=1, to=length(tuning$Diameter)-1, by=1)) {
 retentiondensity[x] <- CurveFit[x+1]-CurveFit[x]
}
retentiondensity[length(tuning$Diameter)] <- 0

col1 <- c(mathead[2:length(mathead)],format(tuning$Diameter, decimal.mark=tclvalue(decpoint), na.encode=F))
col2 <- c(test2[2:length(mathead)],format(round(retentiondensity, digits=accuracy), decimal.mark=tclvalue(decpoint), na.encode=F))#materinfo

#col1 <- c(mathead,tuning$Diameter)
#col2 <- c(materinfo,round(CurveFit, digits=accuracy))


newmatdb <- data.frame(cbind(col1,col2), row.names=1:length(col1))
newmatdb <- newmatdb[,2, drop=F]
#print(newmatdb)

#save dataframe with new first line (=col.names)
write.table(newmatdb,file=FileNewMat, dec=tclvalue(decpoint), sep=tclvalue(csvtype), na="", col.names=paste("Identifier;",id,sep=""), row.names=col1, quote=F)

message("  Saved material as raw material in new material file: ",FileNewMat)

}
}
savenewmat.but <- tkbutton(reswin, text='New material file', command=savenewmat)


#finevol <- function() {		
#    tclvalue(done)<-5
#    #if ( !is.null(dev.list()) ) { 
#
#     #   dev.off(length(dev.list())+1) #close last opened interactive R window
#
#    #}
#}
#finevol.but <- tkbutton(reswin, text='Fine-Tuning in Vol%', command=finevol)


#finemass <- function() {		
#    tclvalue(done)<-6
#    #if ( !is.null(dev.list()) ) { 
#
#     #   dev.off(length(dev.list())+1) #close last opened interactive R window
#
#    #}
#}
#finemass.but <- tkbutton(reswin, text='Fine-Tuning in Ma%', command=finemass)




modplot <- function() {

message("  Plot adjustment")

ap <- varEntryDialog(vars=c('xb1', 'xb2', 'yb1', 'yb2', 'lc', 'mag'), labels=c('Lower limit of x-axis:', 'Upper limit of x-axis:', 'Lower limit of y-axis:', 'Upper limit of y-axis:', 'Logarithmic axis? (type x, y, xy, or leave empty):', 'Magnification of axis-labels etc.:'), title='Adjust plot',prompt='Adjust plot properties:',preset=c(xb[1],xb[2],yb[1],yb[2],lc,mag))

 

 xb[1] <<- as.numeric(ap[1])

 xb[2] <<- as.numeric(ap[2])

 yb[1] <<- as.numeric(ap[3])

 yb[2] <<- as.numeric(ap[4])

 lc <<- as.character(ap[5])

 mag <<- as.numeric(ap[6])

 replot(renew="yes")
}
modplot.but <- tkbutton(reswin, text='Adjust plot', bg=knoepfe, command=modplot)


replot <- function(renew="no") {

message("  Open/Close/Refresh plot")

	if (is.null(dev.list())) {

        if (os=="linux") { X11(title="ParSD: Batch verification") }
        else if (os=="osx") { quartz(title="ParSD: Batch verification") }
        else { windows(title="ParSD: Batch verification") }
        
		par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

        plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

        lines(x=tuning$Diameter,y=CurveFit,lty=2)

        legend("bottomright", c("Model","Batch"), lty=c(1,2), inset=0.01)

    } else if (renew=="yes") {
        par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

        plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

        lines(x=tuning$Diameter,y=CurveFit,lty=2)

        legend("bottomright", c("Model","Batch"), lty=c(1,2), inset=0.01)
    
    } else { 

        dev.off(length(dev.list())+1) 

    }   

}
replot.but <- tkbutton(reswin, text='Open/close plot', bg=knoepfe, command=replot)




infom <- function() {		
        tkmessageBox(title = "Model summary", message=modelsummary, icon = "info", type = "ok")
}
infom.but <- tkbutton(reswin, text='Model info', command=infom)











saverec <- function() {

FileRec = tclvalue(tcl("tk_getSaveFile"))
if (FileRec != "") {

if (is.null(dbinfo2)) {
        matdb <- data.frame(dbinfo$Identifier)

        colnames(matdb) <- "Identifier"

        for (i in seq(from=1, to=length(MatSelection), by=1)) {

            matdb <- cbind(matdb,dbinfo[,MatSelection[i]])

           colnames(matdb)[i+1] <- MatSelection[i]

        }	 

        write.table(matdb, file=FileRec, row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 
        } else {

         matdb <- data.frame(dbinfo$Identifier)
         colnames(matdb) <- "Identifier"
         
         for (i in seq(from=1, to=length(MatSelection1), by=1)) {

            matdb <- cbind(matdb,dbinfo[,MatSelection1[i]])

           colnames(matdb)[i+1] <- MatSelection[i]

        }	 
        for (i in seq(from=1, to=length(MatSelection2), by=1)) {

            matdb <- cbind(matdb,dbinfo2[,MatSelection2[i]])

           colnames(matdb)[i+length(MatSelection1)+1] <- MatSelection2[i]

        }	
         
         write.table(matdb, file=FileRec, row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 

        } #else -> 2 files

       message("  Saved recipe: ",FileRec)
}
}


saveplot <- function() {

FilePlot = tclvalue(tcl("tk_getSaveFile"))
if (FilePlot != "") {

png(FilePlot)

par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

lines(x=tuning$Diameter,y=CurveFit,lty=2)

legend("bottomright", c("Model","Batch"), lty=c(1,2), inset=0.01)

dev.off(length(dev.list())+1)
message("  Saved plot: ",FilePlot)
}

}



savebatch <- function() {

FileBatch = tclvalue(tcl("tk_getSaveFile"))
if (FileBatch != "") {

batchdb <- data.frame(cbind(c(round(100*boundsVol, digits=accuracy),round(100*sum(boundsVol), digits=accuracy)),c(round(100*boundsMass, digits=accuracy),round(100*sum(boundsMass), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2))))
	

	 write.table(batchdb,file=FileBatch,row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%","Mass%","Density","SSA","Price per MT"),quote=FALSE)
message("  Saved batch: ",FileBatch)
}
}


savefit <- function() {

FileFit = tclvalue(tcl("tk_getSaveFile"))
if (FileFit != "") {

modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(errsq, digits=accuracy),rep(NA, 4+length(modelvalue)))))
        
        
        write.table(modeldb,file=FileFit,row.names=c(rep(" ",length(tuning$Diameter)),"-----",modelparam,"-----","Corr. coeff.:","Sum. sq. dev.:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c(paste("Model and batch;Diameter in",tclvalue(dunit)),"Q(Model) in %","Q(Batch) in %","Squared deviation"),quote=FALSE)
message("  Saved fit: ",FileFit)
}
}


saveres <- function() {

        parent <- tk_choose.dir(getwd(), caption = "Select parent directory to save a folder with the results in")
        
        if (!is.na(parent)) {

        while (TRUE) {
            savename <- varEntryDialog(vars=c('saven'), labels=c('Name for folder to save results in:'), title='Name for saving',prompt='Files with results will be put into a newly created folder.')

            if (is.null(savename)) { break }

            if (file.exists(paste(parent,savename,sep=pathsep))) {

                overwrite <- tkmessageBox(title = "Naming conflict", message="A folder with the chosen name exists already. Overwrite contents?", icon = "info", type = "yesno")

                if (tclvalue(overwrite) == "yes") { break }

            } else { 
            dir.create(paste(parent,savename,sep=pathsep))
            break 
            }

        }

        
        if (!is.null(savename)) {
	

        #save raw material data

        if (is.null(dbinfo2)) {
        matdb <- data.frame(dbinfo$Identifier)

        colnames(matdb) <- "Identifier"

        for (i in seq(from=1, to=length(MatSelection), by=1)) {

            matdb <- cbind(matdb,dbinfo[,MatSelection[i]])

           colnames(matdb)[i+1] <- MatSelection[i]

        }	 

        write.table(matdb, file=paste(parent,pathsep,savename,pathsep,savename,"-recipe.csv",sep=""), row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 
        } else {

         matdb <- data.frame(dbinfo$Identifier)
         colnames(matdb) <- "Identifier"
         
         for (i in seq(from=1, to=length(MatSelection1), by=1)) {

            matdb <- cbind(matdb,dbinfo[,MatSelection1[i]])

           colnames(matdb)[i+1] <- MatSelection[i]

        }	 
        for (i in seq(from=1, to=length(MatSelection2), by=1)) {

            matdb <- cbind(matdb,dbinfo2[,MatSelection2[i]])

           colnames(matdb)[i+length(MatSelection1)+1] <- MatSelection2[i]

        }	
         
         write.table(matdb, file=paste(parent,pathsep,savename,pathsep,savename,"-recipe.csv",sep=""), row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 

        } #else -> 2 files
        
        
        #write plot or whatever...

	png(paste(parent,pathsep,savename,pathsep,savename,"-plot.png",sep=""))

par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

lines(x=tuning$Diameter,y=CurveFit,lty=2)

legend("bottomright", c("Model","Batch"), lty=c(1,2), inset=0.01)

dev.off(length(dev.list())+1)
        
        
        
        
        #save batch 

#      if (BoundsType=="Yes, in mass percent") { #with Mass%-bounds
      
#      batchdb <- data.frame(cbind(c(round(100*boundsVol, digits=accuracy),round(100*sum(boundsVol), digits=accuracy)),c(round(100*boundsMass, digits=accuracy),round(100*sum(boundsMass), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2)),c(boundsLowMass,NA),c(boundsUpMass,NA)))
	
#	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
#     #   rownames(batchdb) <- c(MatSelection,"Batch:")

#	 write.table(batchdb,file=paste(parent,"/",savename,"/",savename,"-batch.csv",sep=""),row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT","Set lower mass% bounds","Set upper mass% bounds"),quote=FALSE)

#} else if (BoundsType=="Yes, in volume percent") { #VOl%-FT 

#batchdb <- data.frame(cbind(c(round(100*boundsVol, digits=accuracy),round(100*sum(boundsVol), digits=accuracy)),c(round(100*boundsMass, digits=accuracy),round(100*sum(boundsMass), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2)),c(boundsLowIn,NA),c(boundsUpIn,NA)))
	
#	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
#     #   rownames(batchdb) <- c(MatSelection,"Batch:")

#	 write.table(batchdb,file=paste(parent,"/",savename,"/",savename,"-batch.csv",sep=""),row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT","Set lower vol% bounds","Set upper vol% bounds"),quote=FALSE)

#} else {

batchdb <- data.frame(cbind(c(round(100*boundsVol, digits=accuracy),round(100*sum(boundsVol), digits=accuracy)),c(round(100*boundsMass, digits=accuracy),round(100*sum(boundsMass), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2))))
	
	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
     #   rownames(batchdb) <- c(MatSelection,"Batch:")

	 write.table(batchdb,file=paste(parent,"/",savename,"/",savename,"-batch.csv",sep=""),row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%","Mass%","Density","SSA","Price per MT"),quote=FALSE)

        #}
        #save model and fit
        
        modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(errsq, digits=accuracy),rep(NA, 4+length(modelvalue)))))
        
        
        write.table(modeldb,file=paste(parent,"/",savename,"/",savename,"-model.csv",sep=""),row.names=c(rep(" ",length(tuning$Diameter)),"-----",modelparam,"-----","Corr. coeff.:","Sum. sq. dev.:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c(paste("Model and batch;Diameter in",tclvalue(dunit)),"Q(Model) in %","Q(Batch) in %","Squared deviation"),quote=FALSE)
        } #!is.null savename
        } #!is.na
        
        message("  Results saved in folder: ",paste(parent,savename,sep=pathsep))
}
#saveres.but <- tkbutton(reswin, text='Save all', command=saveres)








setdm <- function(...) {

     tkyview(t.d,...)

     tkyview(t.m,...)

     tkyview(t.f,...)

     tkyview(t.e,...)

    }

	


	scr.d <- tkscrollbar(reswin, repeatinterval=4,command=setdm)

	

	t.d <- tklistbox(reswin, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	t.m <- tklistbox(reswin, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	t.f <- tklistbox(reswin, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	t.e <- tklistbox(reswin, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	

	

	for (i in seq(from=1, to=length(tuning$Diameter), by=1)) {

	

  tkinsert(t.d, "end",as.character(tuning$Diameter[i]))

   tkinsert(t.m, "end",as.character(round(tuning$VolModel[i],digits=accuracy)))

   tkinsert(t.f, "end",as.character(round(CurveFit[i],digits=accuracy)))

   tkinsert(t.e, "end",as.character(round((tuning$VolModel[i]-CurveFit[i])*(tuning$VolModel[i]-CurveFit[i]),digits=accuracy)))

  

}


#menu

 Menu <- tkmenu(reswin)           
  tkconfigure(reswin, menu = Menu) 
  
  SaveMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Save", menu = SaveMenu)
  tkadd(SaveMenu, "command", label = "Recipe (Save as CSV-file!)", command =function() saverec())
  tkadd(SaveMenu, "command", label = "Batch (Save as CSV-file!)", command =function() savebatch())
  tkadd(SaveMenu, "command", label = "Model/Comparison (Save as CSV-file!)", command =function() savefit())
  tkadd(SaveMenu, "command", label = "Graph (Save as PNG-file!)", command =function() saveplot())
  tkadd(SaveMenu, "separator")
  tkadd(SaveMenu, "command", label = "All (in an extra folder)", command =function() saveres())
  
  
  OptWin <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Window options", menu = OptWin)
  
  #sbheight <- 3
  #ygap <- 1
  tkadd(OptWin, "radiobutton", variable=sbheight, value='3', label="Small listbox height")
  tkadd(OptWin, "radiobutton", variable=sbheight, value='7', label="Medium listbox height")
  tkadd(OptWin, "radiobutton", variable=sbheight, value='11', label="Large listbox height")
  tkadd(OptWin, "separator")
  tkadd(OptWin, "radiobutton", variable=ygap, value='0', label="Small vertical distance between window elements")
  tkadd(OptWin, "radiobutton", variable=ygap, value='1', label="Medium vertical distance between window elements")
  tkadd(OptWin, "radiobutton", variable=ygap, value='2', label="Large vertical distance between window elements")
  
tkadd(Menu, "command", label = "Apply window options", command =function() tclvalue(done)<-5)
  
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","14resverify","14.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(done)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(done)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(done)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(done)<-1)
  tkadd(QuMenu, "separator")
  tkadd(QuMenu, "command", label = "Quit ParSD app", command = function() tclvalue(done)<-0)

tkgrid(tklabel(reswin, font=tkfont.create(size=1), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, text="Material"), tklabel(reswin, text="Vol%"), tklabel(reswin, text="Mass%"),tklabel(reswin, text="Density"), tklabel(reswin, text="SSA"), tklabel(reswin, text="Price per MT"), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), columnspan=6, pady=tclvalue(ygap), padx=10, sticky="we")
#tkgrid(tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"),tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), pady=tclvalue(ygap), padx=10, columnspan=6)

for (i in seq(from=1, to=length(MatSelection), by=1)) {
    tkgrid(tklabel(reswin, text=MatSelection[i]), tklabel(reswin, text=as.character(round(100*boundsVol[i], digits=accuracy))), tklabel(reswin, text=as.character(round(100*boundsMass[i], digits=accuracy))), tklabel(reswin,text=as.character(round(densities[i], digits=4))), tklabel(reswin, text=as.character(round(ssa[i], digits=4))), tklabel(reswin, text=as.character(round(prices[i], digits=2))), pady=tclvalue(ygap), padx=10, columnspan=6)
}

tkgrid(ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), columnspan=6, pady=tclvalue(ygap), padx=10, sticky="we")
#tkgrid(tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"),tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, text="Batch:"), tklabel(reswin, text=as.character(round(100*sum(boundsVol), digits=accuracy))), tklabel(reswin, text=as.character(round(100*sum(boundsMass), digits=accuracy))), tklabel(reswin, text=as.character(round(Den, digits=4))), tklabel(reswin, text=as.character(round(Sssa, digits=4))), tklabel(reswin, text=as.character(round(price, digits=2))), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, font=tkfont.create(size=1), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, text="d(CPFT) of batch:"), tklabel(reswin, text=paste("d(10%) =",round(d10, digits=accuracy),tclvalue(dunit))), tklabel(reswin, text=paste("d(25%) =",round(d25, digits=accuracy),tclvalue(dunit))), tklabel(reswin, text=paste("d(50%) =",round(d50, digits=accuracy),tclvalue(dunit))), tklabel(reswin, text=paste("d(75%) =",round(d75, digits=accuracy),tclvalue(dunit))), tklabel(reswin, text=paste("d(90%) =",round(d90, digits=accuracy),tclvalue(dunit))), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, font=tkfont.create(size=1), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, text=""), tklabel(reswin, text="Save batch as material to"), savenewmat.but, tklabel(reswin, text="(Save as CSV-file!)"), tklabel(reswin, text="or add it to existent"), savemat.but, pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)


#tkgrid(tklabel(reswin, text=""),pady=tclvalue(ygap), padx=10, columnspan=6)
tkgrid(ttkseparator(reswin), columnspan=36, pady=tclvalue(ygap), padx=10, sticky="we")
tkgrid(tklabel(reswin, font=tkfont.create(size=1), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

#tkgrid(tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"),tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), pady=tclvalue(ygap), padx=10, columnspan=6)





tkgrid(tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)



	



#Vol and MAss%-FTs if No-bounds and Den not NA
#tkgrid(tklabel(reswin, text="Fit quality:"), tklabel(reswin, text="Cor. coeff. ="), tklabel(reswin, text=as.character(round(cor(tuning$VolModel,CurveFit), digits=4))),tklabel(reswin, text=""),tklabel(reswin, text=""), tklabel(reswin, text=""), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, text="Quality:", bg=hintergrund), replot.but, modplot.but, tklabel(reswin, text="Corr. coeff.:", bg=hintergrund), tklabel(reswin, text=as.character(round(cor(tuning$VolModel,CurveFit), digits=4))), tklabel(reswin,text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)


#tkgrid(tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), columnspan=6, pady=0, padx=10, sticky="we")

tkgrid(tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin,text="", bg=hintergrund), tklabel(reswin, text="Diameter", bg=hintergrund), tklabel(reswin, text="Model", bg=hintergrund), tklabel(reswin, text="Batch", bg=hintergrund), tklabel(reswin, text="Squared deviation", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin,text="(use scrollbar\nto move lists\nsimulaneously)", bg=hintergrund),t.d, t.m, t.f, t.e, scr.d, pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid.configure(scr.d,rowspan=tclvalue(sbheight),sticky="nsw")

tkgrid(tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

#tkgrid(tklabel(reswin, text=""), tklabel(reswin, text="Sum sq. dev. ="), tklabel(reswin, text=as.character(round(sumerrsq, digits=accuracy))),replot.but,infom.but,modplot.but,  pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, text="", bg=hintergrund), tklabel(reswin, text="", bg=hintergrund), infom.but, tklabel(reswin, text="Sum sq. dev.:", bg=hintergrund), tklabel(reswin, text=as.character(round(sumerrsq, digits=accuracy))), tklabel(reswin, text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)
	
tkgrid(tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

	
tkfocus(reswin)
# Do not proceed with the following code until the variable done is non-zero.
#   (But other processes can still run, i.e. the system is not frozen.)
tkwait.variable(done)

don <- tclvalue(done)
tkdestroy(reswin) 

message("  Chosen action (0 Quit, 1 Main menu, 2 Material Selection, 3 Batch definition, 4 Model selection, 5 Apply window options): ",don,"\n")

if (don < 5) { 
if ( !is.null(dev.list()) ) { 

        dev.off(length(dev.list())+1) #close last opened interactive R window

    }
return(don)
} 

} #while loop reswin

} 
#end verify()







