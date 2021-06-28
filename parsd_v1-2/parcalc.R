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



modelcalc <- function(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol) {

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
  
   
  nacount <<- 0
  
   while (TRUE) {
   
   message("BEGIN calculate-model-parameters-model function\n")
  
  winmod <- tktoplevel(bg=hintergrund)
  tkwm.title(winmod,"Choose model for paramter determination")
  tkraise(winmod)

  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod, bg=menue)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","11params","11.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())
  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text='\nChoose model for parameter determination:', bg=hintergrund), pady=10, padx=10)

  tkgrid(tkbutton(winmod, text='Andreasen model', bg=knoepfe, command=function() tclvalue(cm)<-4), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Psi model', bg=knoepfe, command=function() tclvalue(cm)<-6), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Kawamura model', bg=knoepfe, command=function() tclvalue(cm)<-7), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Dinger/Funk model', bg=knoepfe, command=function() tclvalue(cm)<-5), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Modified Psi model', bg=knoepfe, command=function() tclvalue(cm)<-8), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='Modified Kawamura model', bg=knoepfe, command=function() tclvalue(cm)<-9), pady=5, padx=10, sticky="ew")
  
  #tkgrid(tkbutton(winmod, text='Other/free model', bg=knoepfe, command=function() tclvalue(cm)<-10), pady=5, padx=10, sticky="ew")

  tkfocus(winmod)

  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  #VolModel <<- vector(length=length(preparation$Diameter)) #BRAUCHE iCH DAS?!?!?!?
message("  Choose model (1 Main menu, 2 Material selection, 3 Batch, 4 Andreasen model, 5 Dinger/Funk model, 6 Psi model, 7 Kawamura model, 8 Modified Psi model, 9 Modified Kawamura model): ",var)
  
  if (var < 4) { return(var) 
  } else if (var==4) { 
    ModSelection <<- "Andreasen model"
    
    while (TRUE) {
    boundsVMod <- varEntryDialog(vars=c('dmaxlow', 'dmaxup', 'nlow', 'nup'), labels=c(paste('Lower bound of maximum particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of maximum particle size in ',tclvalue(dunit),':', sep=""),'Lower bound of distribution modulus:','Upper bound of distribution modulus:'), title='Bounds of model parameters',prompt=paste('Do you want to adjust bounds for the parameters of the ',ModSelection,'?',sep=""),preset=c(matdmax, matdmax,"",""),cancellab='Back to Model Selection') 
    if(!is.null(boundsVMod)) { 
        
      boundsLMod <- as.numeric(boundsVMod[1]) #dmax low
      if (is.na(boundsLMod)) {boundsLMod <- preparation$Diameter[3]} 
      #print(boundsLMod)
    
      boundsUMod <- as.numeric(boundsVMod[2]) #dmax up;as.numeric gives NA if string was given
      if (is.na(boundsUMod)) {boundsUMod <- preparation$Diameter[length(preparation$Diameter)]} 
      #print(boundsUMod)
 
      boundsNl <- as.numeric(boundsVMod[3])
      if (is.na(boundsNl)) {boundsNl <- -Inf} 
      
      boundsNu <- as.numeric(boundsVMod[4])
      if (is.na(boundsNu)) {boundsNu <- Inf} 
 
 
      #print(boundsLMod < preparation$Diameter[3])
      #print(boundsUMod > preparation$Diameter[length(preparation$Diameter)])
      #print(boundsLMod > boundsUMod)
    
      if (boundsNl > boundsNu || boundsLMod < preparation$Diameter[3] || boundsUMod > preparation$Diameter[length(preparation$Diameter)] || boundsLMod > boundsUMod) {
        tkmessageBox(title = "Input error", message=paste("The lower bounds have to be smaller than the corresponding upper bounds.\n\nTaken from the database, the minimum lower bound for the maximum particle size is ",preparation$Diameter[3],tclvalue(dunit),' and the maximum value for the upper bound is ',preparation$Diameter[length(preparation$Diameter)],tclvalue(dunit),'. The reason is that at least three Cumulative Percent Finer Than d- (CPFT(d)-) values for particle sizes d are required to calculate a curve fit.',sep=""), icon = "info", type = "ok")
      
        next
      } else if (boundsLMod == boundsUMod) { 
        break 
      } else if (boundsLMod > matdmax || boundsUMod < matdmax) {
        strangedmax <- tkmessageBox(title = "Maximum particle size", message=paste('The chosen range for the maximum particle size (from ',boundsLMod,tclvalue(dunit),' to ',boundsUMod,tclvalue(dunit),') does not include the preset value (',matdmax,tclvalue(dunit),').\n\nThe preset value of the maximum particle size is calculated analytically from the batch. The optimal maximum particle size might be near to it.\n\nDo you want to change the chosen range?',sep=""), icon = "question", type = "yesno", default="yes")      
        if (tclvalue(strangedmax) == "yes") { next }
      }
      
      break
    } else { break }#if !is.null boundsVMod
    }#while input dmax bounds
    if(is.null(boundsVMod)) { next } #if input ok, go on, if not return model selection (oder mit return(4))
    
   #print("weiter")
    
    
    
#    for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
 #     if (dmax < preparation$Diameter[i]) {
  #      VolModel[i] <<- 100 }
   #   else {
    #    VolModel[i] <<- 100*(preparation$Diameter[i]/dmax)^n 

     # }
    #}
 
#  modelparam <<- vector(length=3)
 # modelparam[1] <<- ModSelection
  #modelparam[2] <<- paste("d(max) in",tclvalue(dunit),":")
#  modelparam[3] <<- "n:"
  
 # modelvalue <<- vector(length=3)
  #modelvalue[1] <<- NA
#  modelvalue[2] <<- dmax
 # modelvalue[3] <<- n
 
  # modelinfo <<- vector(length=3)
 
   #modelinfo[1] <<- ModSelection
 
#   modelinfo[2] <<- paste("d(max) =",dmax,tclvalue(dunit))
 
 #  modelinfo[3] <<- paste("n =",n)
 
  # modelsummary <<- paste(modelinfo[1],modelinfo[2],modelinfo[3],sep="\n")
   #break }
   
   
   
   
  } else if (var==5) { 
  
  ModSelection <<- "Dinger/Funk model"
    
   while (TRUE) {   
   
    boundsVMod <- varEntryDialog(vars=c('dminlow','dminup','dmaxlow', 'dmaxup', 'nlow', 'nup'), labels=c(paste('Lower bound of minimum particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of minimum particle size in ',tclvalue(dunit),':', sep=""),paste('Lower bound of maximum particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of maximum particle size in ',tclvalue(dunit),':', sep=""),'Lower bound of distribution modulus:','Upper bound of distribution modulus:'), title='Bounds of model parameters',prompt=paste('Do you want to adjust bounds for the parameters of the ',ModSelection,'?',sep=""),preset=c('','',matdmax, matdmax,"",""),cancellab='Back to Model Selection') 
    if(!is.null(boundsVMod)) { 
        
      boundsLMod <- vector(length=2)
      boundsUMod <- vector(length=2)
      
      boundsLMod[1] <- as.numeric(boundsVMod[1]) #dmin low
      if (is.na(boundsLMod[1])) {boundsLMod[1] <- preparation$Diameter[1]} #????
      #print(boundsLMod[1])
    
      boundsUMod[1] <- as.numeric(boundsVMod[2]) #dmin up;as.numeric gives NA if string was given
      if (is.na(boundsUMod[1])) {boundsUMod[1] <- preparation$Diameter[length(preparation$Diameter)-3]} #funktioniert auhc mit -2
      #print(boundsUMod[1])
          
      boundsLMod[2] <- as.numeric(boundsVMod[3]) #dmax low
      if (is.na(boundsLMod[2])) {boundsLMod[2] <- preparation$Diameter[3]} 
      #print(boundsLMod[2])
          
      boundsUMod[2] <- as.numeric(boundsVMod[4]) #dmax up;as.numeric gives NA if string was given
      if (is.na(boundsUMod[2])) {boundsUMod[2] <- preparation$Diameter[length(preparation$Diameter)]}
      #print(boundsUMod[2])
          
        boundsNl <- as.numeric(boundsVMod[5])
      if (is.na(boundsNl)) {boundsNl <- -Inf} 
      
      boundsNu <- as.numeric(boundsVMod[6])
      if (is.na(boundsNu)) {boundsNu <- Inf}   
          
        
      if (boundsNl > boundsNu || boundsLMod[2] < preparation$Diameter[3] || boundsUMod[2] > preparation$Diameter[length(preparation$Diameter)] || boundsLMod[2] > boundsUMod[2] || boundsUMod[1] > preparation$Diameter[length(preparation$Diameter-3)] || boundsLMod[1] < preparation$Diameter[1] || boundsLMod[1] > boundsUMod[1] || boundsUMod[2] < boundsLMod[1]) {
        tkmessageBox(title = "Input error", message=paste("The lower bounds have to be smaller than the corresponding upper bounds.\n\nTaken from the database, the minimum lower bounds are ",preparation$Diameter[1],tclvalue(dunit),' and ',preparation$Diameter[3],tclvalue(dunit),' for the minimum and maximum particle size, respectively. The maximum values for the upper bounds are ',preparation$Diameter[length(preparation$Diameter)-3],tclvalue(dunit),' and ',preparation$Diameter[length(preparation$Diameter)],tclvalue(dunit),' for the minimum and maximum particle size, respectively. The reason is that at least three Cumulative Percent Finer Than d- (CPFT(d)-) values for particle sizes d are required to calculate a curve fit. Furthermore, the upper bound of the maximum particle size cannot be smaller than the lower bound of the minimum particle size.',sep=""), icon = "info", type = "ok")
      
        next
      } else if (boundsLMod[2] == boundsUMod[2]) { 
        break 
      } else if (boundsLMod[2] > matdmax || boundsUMod[2] < matdmax) {
        strangedmax <- tkmessageBox(title = "Maximum particle size", message=paste('The chosen range for the maximum particle size (from ',boundsLMod[2],tclvalue(dunit),' to ',boundsUMod[2],tclvalue(dunit),') does not include the preset value (',matdmax,tclvalue(dunit),').\n\nThe preset value of the maximum particle size is calculated analytically from the batch. The optimal maximum particle size might be near to it.\n\nDo you want to change the chosen range?',sep=""), icon = "question", type = "yesno", default="yes")      
        if (tclvalue(strangedmax) == "yes") { next }
      }
      
      break
    } else { break }#if !is.null boundsVMod
    }#while input dmax bounds
    if(is.null(boundsVMod)) { next } #if input ok, go on, if not return model selection (oder mit return(4))
   
  } else if (var==6) { 
    ModSelection <<- "Psi model"
    
    #braucht es mehr als 3 zeilen, um das zu fitten?
    
   
    while (TRUE) {
    boundsVMod <- varEntryDialog(vars=c('dmaxlow', 'dmaxup', 'nminlow', 'nminup', 'nmaxlow', 'nmaxup'), labels=c(paste('Lower bound of maximum particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of maximum particle size in ',tclvalue(dunit),':', sep="") ,'Lower bound of minimum distribution modulus:','Upper bound of minimum distribution modulus:' ,'Lower bound of maximum distribution modulus:','Upper bound of maximum distribution modulus:'), title='Bounds of model parameters',prompt=paste('Do you want to adjust bounds for the parameters of the ',ModSelection,'?',sep=""),preset=c(matdmax, matdmax,"","","",""),cancellab='Back to Model Selection') 
    if(!is.null(boundsVMod)) { 
        
      boundsLMod <- as.numeric(boundsVMod[1]) #dmax low
      if (is.na(boundsLMod)) {boundsLMod <- preparation$Diameter[3]} 
      #print(boundsLMod)
    
      boundsUMod <- as.numeric(boundsVMod[2]) #dmax up;as.numeric gives NA if string was given
      if (is.na(boundsUMod)) {boundsUMod <- preparation$Diameter[length(preparation$Diameter)]} 
      #print(boundsUMod)
      
      boundsNl <- vector(length=2)
      boundsNu <- vector(length=2)
      
      boundsNl[1] <- as.numeric(boundsVMod[3])
      if (is.na(boundsNl[1])) {boundsNl[1] <- -Inf} 
      
      boundsNu[1] <- as.numeric(boundsVMod[4])
      if (is.na(boundsNu[1])) {boundsNu[1] <- Inf} 
      
      boundsNl[2] <- as.numeric(boundsVMod[5])
      if (is.na(boundsNl[2])) {boundsNl[2] <- -Inf} 
      
      boundsNu[2] <- as.numeric(boundsVMod[6])
      if (is.na(boundsNu[2])) {boundsNu[2] <- Inf} 
    
      #print(boundsLMod < preparation$Diameter[3])
      #print(boundsUMod > preparation$Diameter[length(preparation$Diameter)])
      #print(boundsLMod > boundsUMod)
    
      if (boundsNl[1] > boundsNu[1] || boundsNl[2] > boundsNu[2] || boundsLMod < preparation$Diameter[3] || boundsUMod > preparation$Diameter[length(preparation$Diameter)] || boundsLMod > boundsUMod) {
        tkmessageBox(title = "Input error", message=paste("The lower bounds have to be smaller than the corresponding upper bounds.\n\nTaken from the database, the minimum lower bound for the maximum particle size is ",preparation$Diameter[3],tclvalue(dunit),' and the maximum value for the upper bound is ',preparation$Diameter[length(preparation$Diameter)],tclvalue(dunit),'. The reason is that at least three Cumulative Percent Finer Than d- (CPFT(d)-) values for particle sizes d are required to calculate a curve fit.',sep=""), icon = "info", type = "ok")
      
        next
      } else if (boundsLMod == boundsUMod) { 
        break 
      } else if (boundsLMod > matdmax || boundsUMod < matdmax) {
        strangedmax <- tkmessageBox(title = "Maximum particle size", message=paste('The chosen range for the maximum particle size (from ',boundsLMod,tclvalue(dunit),' to ',boundsUMod,tclvalue(dunit),') does not include the preset value (',matdmax,tclvalue(dunit),').\n\nThe preset value of the maximum particle size is calculated analytically from the batch. The optimal maximum particle size might be near to it.\n\nDo you want to change the chosen range?',sep=""), icon = "question", type = "yesno", default="yes")      
        if (tclvalue(strangedmax) == "yes") { next }
      }
      
      break
    } else { break }#if !is.null boundsVMod
    }#while input dmax bounds
    if(is.null(boundsVMod)) { next }
  
  } else if (var==7) { 
    ModSelection <<- "Kawamura model"
  
  while (TRUE) {
  
  
  
  
    boundsVMod <- varEntryDialog(vars=c('dgaplow','dgapup','dmaxlow', 'dmaxup' , 'nandlow', 'nandup' , 'nfurlow', 'nfurup'), labels=c(paste('Lower bound of gap particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of gap particle size in ',tclvalue(dunit),':', sep=""),paste('Lower bound of maximum particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of maximum particle size in ',tclvalue(dunit),':', sep=""),'Lower bound of distribution modulus (Andreasen-part):','Upper bound of distribution modulus (Andreasen-part):','Lower bound of distribution modulus (Furnas-part):','Upper bound of distribution modulus (Furnas-part):'), title='Bounds of model parameters',prompt=paste('Do you want to adjust bounds for the parameters of the ',ModSelection,'?',sep=""),preset=c('','',matdmax, matdmax,"","","",""),cancellab='Back to Model Selection') 
    if(!is.null(boundsVMod)) { 
        
      boundsLMod <- vector(length=2)
      boundsUMod <- vector(length=2)
      
      boundsLMod[1] <- as.numeric(boundsVMod[1]) #dgap low
      if (is.na(boundsLMod[1])) {boundsLMod[1] <- preparation$Diameter[3]} #????
      #print(boundsLMod[1])
    
      boundsUMod[1] <- as.numeric(boundsVMod[2]) #dgap up;as.numeric gives NA if string was given
      if (is.na(boundsUMod[1])) {boundsUMod[1] <- preparation$Diameter[length(preparation$Diameter)-3]} #funktioniert NICHT mit -2
      #print(boundsUMod[1])
          
      boundsLMod[2] <- as.numeric(boundsVMod[3]) #dmax low
      if (is.na(boundsLMod[2])) {boundsLMod[2] <- preparation$Diameter[6]} 
      #print(boundsLMod[2])
          
      boundsUMod[2] <- as.numeric(boundsVMod[4]) #dmax up;as.numeric gives NA if string was given
      if (is.na(boundsUMod[2])) {boundsUMod[2] <- preparation$Diameter[length(preparation$Diameter)]}
      #print(boundsUMod[2])
      
      boundsNl <- vector(length=2)
      boundsNu <- vector(length=2)
      
      boundsNl[1] <- as.numeric(boundsVMod[5])
      if (is.na(boundsNl[1])) {boundsNl[1] <- -Inf} 
      
      boundsNu[1] <- as.numeric(boundsVMod[6])
      if (is.na(boundsNu[1])) {boundsNu[1] <- Inf} 
      
      boundsNl[2] <- as.numeric(boundsVMod[7])
      if (is.na(boundsNl[2])) {boundsNl[2] <- -Inf} 
      
      boundsNu[2] <- as.numeric(boundsVMod[8])
      if (is.na(boundsNu[2])) {boundsNu[2] <- Inf} 
          
      if (boundsNl[1] > boundsNu[1] || boundsNl[2] > boundsNu[2] || boundsLMod[2] < preparation$Diameter[6] || boundsUMod[2] > preparation$Diameter[length(preparation$Diameter)] || boundsLMod[2] > boundsUMod[2] || boundsUMod[1] > preparation$Diameter[length(preparation$Diameter-3)] || boundsLMod[1] < preparation$Diameter[3] || boundsLMod[1] > boundsUMod[1] || boundsUMod[2] < boundsLMod[1]) {
        tkmessageBox(title = "Input error", message=paste("The lower bounds have to be smaller than the corresponding upper bounds.\n\nTaken from the database, the minimum lower bounds are ",preparation$Diameter[3],tclvalue(dunit),' and ',preparation$Diameter[6],tclvalue(dunit),' for the gap and maximum particle size, respectively. The maximum values for the upper bounds are ',preparation$Diameter[length(preparation$Diameter)-3],tclvalue(dunit),' and ',preparation$Diameter[length(preparation$Diameter)],tclvalue(dunit),' for the gap and maximum particle size, respectively. The reason is that at least three Cumulative Percent Finer Than d- (CPFT(d)-) values for particle sizes d are required to calculate a curve fit and the Kawamura model consists of two parts which have to be fitted. Furthermore, the upper bound of the maximum particle size cannot be smaller than the lower bound of the minimum particle size.',sep=""), icon = "info", type = "ok")
      
        next
      } else if (boundsLMod[2] == boundsUMod[2]) { 
        break 
      } else if (boundsLMod[2] > matdmax || boundsUMod[2] < matdmax) {
        strangedmax <- tkmessageBox(title = "Maximum particle size", message=paste('The chosen range for the maximum particle size (from ',boundsLMod[2],tclvalue(dunit),' to ',boundsUMod[2],tclvalue(dunit),') does not include the preset value (',matdmax,tclvalue(dunit),').\n\nThe preset value of the maximum particle size is calculated analytically from the batch. The optimal maximum particle size might be near to it.\n\nDo you want to change the chosen range?',sep=""), icon = "question", type = "yesno", default="yes")      
        if (tclvalue(strangedmax) == "yes") { next }
      }
      
      break
    } else { break }#if !is.null boundsVMod
    }#while input dmax bounds
    if(is.null(boundsVMod)) { next } #if input ok, go on, if not return model selection (oder mit return(4))
   
  
  } else if (var==8) { 
    ModSelection <<- "Modified Psi model"
    
    #braucht es mehr als 3 zeilen, um das zu fitten?
  
     while (TRUE) {
     
 
     
    boundsVMod <- varEntryDialog(vars=c('dminlow','dminup','dmaxlow', 'dmaxup', 'nminlow', 'nminup', 'nmaxlow', 'nmaxup'), labels=c(paste('Lower bound of minimum particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of minimum particle size in ',tclvalue(dunit),':', sep=""),paste('Lower bound of maximum particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of maximum particle size in ',tclvalue(dunit),':', sep=""),'Lower bound of minimum distribution modulus:','Upper bound of minimum distribution modulus:','Lower bound of maximum distribution modulus:','Upper bound of maximum distribution modulus:'), title='Bounds of model parameters',prompt=paste('Do you want to adjust bounds for the parameters of the ',ModSelection,'?',sep=""),preset=c('','',matdmax, matdmax,"","","",""),cancellab='Back to Model Selection') 
    if(!is.null(boundsVMod)) { 
        
      boundsLMod <- vector(length=2)
      boundsUMod <- vector(length=2)
      
      boundsLMod[1] <- as.numeric(boundsVMod[1]) #dmin low
      if (is.na(boundsLMod[1])) {boundsLMod[1] <- preparation$Diameter[1]} #????
      #print(boundsLMod[1])
    
      boundsUMod[1] <- as.numeric(boundsVMod[2]) #dmin up;as.numeric gives NA if string was given
      if (is.na(boundsUMod[1])) {boundsUMod[1] <- preparation$Diameter[length(preparation$Diameter)-3]} #funktioniert auhc mit -2 iwie
      #print(boundsUMod[1])
          
      boundsLMod[2] <- as.numeric(boundsVMod[3]) #dmax low
      if (is.na(boundsLMod[2])) {boundsLMod[2] <- preparation$Diameter[3]} 
      #print(boundsLMod[2])
          
      boundsUMod[2] <- as.numeric(boundsVMod[4]) #dmax up;as.numeric gives NA if string was given
      if (is.na(boundsUMod[2])) {boundsUMod[2] <- preparation$Diameter[length(preparation$Diameter)]}
      #print(boundsUMod[2])
      
      boundsNl <- vector(length=2)
      boundsNu <- vector(length=2)
      
      boundsNl[1] <- as.numeric(boundsVMod[5])
      if (is.na(boundsNl[1])) {boundsNl[1] <- -Inf} 
      
      boundsNu[1] <- as.numeric(boundsVMod[6])
      if (is.na(boundsNu[1])) {boundsNu[1] <- Inf} 
      
      boundsNl[2] <- as.numeric(boundsVMod[7])
      if (is.na(boundsNl[2])) {boundsNl[2] <- -Inf} 
      
      boundsNu[2] <- as.numeric(boundsVMod[8])
      if (is.na(boundsNu[2])) {boundsNu[2] <- Inf} 
      
      
          
      if (boundsNl[1] > boundsNu[1] || boundsNl[2] > boundsNu[2] || boundsLMod[2] < preparation$Diameter[3] || boundsUMod[2] > preparation$Diameter[length(preparation$Diameter)] || boundsLMod[2] > boundsUMod[2] || boundsUMod[1] > preparation$Diameter[length(preparation$Diameter-3)] || boundsLMod[1] < preparation$Diameter[1] || boundsLMod[1] > boundsUMod[1] || boundsUMod[2] < boundsLMod[1]) {
        tkmessageBox(title = "Input error", message=paste("The lower bounds have to be smaller than the corresponding upper bounds.\n\nTaken from the database, the minimum lower bounds are ",preparation$Diameter[1],tclvalue(dunit),' and ',preparation$Diameter[3],tclvalue(dunit),' for the minimum and maximum particle size, respectively. The maximum values for the upper bounds are ',preparation$Diameter[length(preparation$Diameter)-3],tclvalue(dunit),' and ',preparation$Diameter[length(preparation$Diameter)],tclvalue(dunit),' for the minimum and maximum particle size, respectively. The reason is that at least three Cumulative Percent Finer Than d- (CPFT(d)-) values for particle sizes d are required to calculate a curve fit. Furthermore, the upper bound of the maximum particle size cannot be smaller than the lower bound of the minimum particle size.',sep=""), icon = "info", type = "ok")
      
        next
      } else if (boundsLMod[2] == boundsUMod[2]) { 
        break 
      } else if (boundsLMod[2] > matdmax || boundsUMod[2] < matdmax) {
        strangedmax <- tkmessageBox(title = "Maximum particle size", message=paste('The chosen range for the maximum particle size (from ',boundsLMod[2],tclvalue(dunit),' to ',boundsUMod[2],tclvalue(dunit),') does not include the preset value (',matdmax,tclvalue(dunit),').\n\nThe preset value of the maximum particle size is calculated analytically from the batch. The optimal maximum particle size might be near to it.\n\nDo you want to change the chosen range?',sep=""), icon = "question", type = "yesno", default="yes")      
        if (tclvalue(strangedmax) == "yes") { next }
      }
      
      break
    } else { break }#if !is.null boundsVMod
    }#while input dmax bounds
    if(is.null(boundsVMod)) { next } #if input ok, go on, if not return model selection (oder mit return(4))
  
  } else { #else if (var==9)
    ModSelection <<- "Modified Kawamura model"
    
    while (TRUE) {
    
    boundsVMod <- varEntryDialog(vars=c('dminlow','dminup','dgaplow','dgapup','dmaxlow', 'dmaxup', 'nandlow', 'nandup', 'nfurlow', 'nfurup'), labels=c(paste('Lower bound of minimum particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of minimum particle size in ',tclvalue(dunit),':', sep=""),paste('Lower bound of gap particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of gap particle size in ',tclvalue(dunit),':', sep=""),paste('Lower bound of maximum particle size in ',tclvalue(dunit),':',sep=""), paste('Upper bound of maximum particle size in ',tclvalue(dunit),':', sep=""),'Lower bound of distribution modulus (Andreasen-part):','Upper bound of distribution modulus (Andreasen-part):','Lower bound of distribution modulus (Furnas-part):','Upper bound of distribution modulus (Furnas-part):'), title='Bounds of model parameters',prompt=paste('Do you want to adjust bounds for the parameters of the ',ModSelection,'?',sep=""),preset=c('','','','',matdmax, matdmax,'','','',''),cancellab='Back to Model Selection') 
    if(!is.null(boundsVMod)) { 
        
      boundsLMod <- vector(length=3)
      boundsUMod <- vector(length=3)      
      
      boundsLMod[1] <- as.numeric(boundsVMod[1]) #dmin low
      if (is.na(boundsLMod[1])) {boundsLMod[1] <- preparation$Diameter[1]} #????
      #print(boundsLMod[1])
    
      boundsUMod[1] <- as.numeric(boundsVMod[2]) #dmin up;as.numeric gives NA if string was given
      if (is.na(boundsUMod[1])) {boundsUMod[1] <- preparation$Diameter[length(preparation$Diameter)-6]} 
      #print(boundsUMod[1])
      
      boundsLMod[2] <- as.numeric(boundsVMod[3]) #dgap low
      if (is.na(boundsLMod[2])) {boundsLMod[2] <- preparation$Diameter[4]} #????
      #print(boundsLMod[1])
    
      boundsUMod[2] <- as.numeric(boundsVMod[4]) #dgap up;as.numeric gives NA if string was given
      if (is.na(boundsUMod[2])) {boundsUMod[2] <- preparation$Diameter[length(preparation$Diameter)-3]} 
      #print(boundsUMod[1])
          
      boundsLMod[3] <- as.numeric(boundsVMod[5]) #dmax low
      if (is.na(boundsLMod[3])) {boundsLMod[3] <- preparation$Diameter[7]} 
      #print(boundsLMod[2])
          
      boundsUMod[3] <- as.numeric(boundsVMod[6]) #dmax up;as.numeric gives NA if string was given
      if (is.na(boundsUMod[3])) {boundsUMod[3] <- preparation$Diameter[length(preparation$Diameter)]}
      #print(boundsUMod[2])
          
          #print("iO")
      boundsNl <- vector(length=2)
      boundsNu <- vector(length=2)
      
      #print("iO2")
      
      boundsNl[1] <- as.numeric(boundsVMod[7])
      if (is.na(boundsNl[1])) {boundsNl[1] <- -Inf} 
      
      #print("iO3")
      
      boundsNu[1] <- as.numeric(boundsVMod[8])
      if (is.na(boundsNu[1])) {boundsNu[1] <- Inf} 
      
      #print(boundsVMod)
      
      boundsNl[2] <- as.numeric(boundsVMod[9])
      if (is.na(boundsNl[2])) {boundsNl[2] <- -Inf} 
      
      #print("iO5")
      
      boundsNu[2] <- as.numeric(boundsVMod[10])
      if (is.na(boundsNu[2])) {boundsNu[2] <- Inf} 
          
      #print("ok")
        
      if (boundsNl[1] > boundsNu[1] || boundsNl[2] > boundsNu[2] || boundsUMod[1] > preparation$Diameter[length(preparation$Diameter-6)] || boundsLMod[1] < preparation$Diameter[1] || boundsLMod[1] > boundsUMod[1] || boundsLMod[3] < preparation$Diameter[7] || boundsUMod[3] > preparation$Diameter[length(preparation$Diameter)] || boundsLMod[3] > boundsUMod[3] || boundsUMod[2] > preparation$Diameter[length(preparation$Diameter-3)] || boundsLMod[2] < preparation$Diameter[4] || boundsLMod[2] > boundsUMod[2] || boundsUMod[3] < boundsLMod[1] || boundsUMod[3] < boundsLMod[2] || boundsUMod[2] < boundsLMod[1]) {
        tkmessageBox(title = "Input error", message=paste("The lower bounds have to be smaller than the corresponding upper bounds.\n\nTaken from the database, the minimum lower bounds are ",preparation$Diameter[1],tclvalue(dunit),', ',preparation$Diameter[4],tclvalue(dunit),' and ',preparation$Diameter[7],tclvalue(dunit),' for the minimum, gap and maximum particle size, respectively. The maximum values for the upper bounds are ',preparation$Diameter[length(preparation$Diameter)-6],tclvalue(dunit),', ',preparation$Diameter[length(preparation$Diameter)-3],tclvalue(dunit),' and ',preparation$Diameter[length(preparation$Diameter)],tclvalue(dunit),' for the minimum, gap and maximum particle size, respectively. The reason is that at least three Cumulative Percent Finer Than d- (CPFT(d)-) values for particle sizes d are required to calculate a curve fit and the Modified Kawamura model consists of two parts which have to be fitted. Furthermore, the upper bounds of the parameters of larger particle sizes cannot be smaller than the lower bound of in relation lower ones.',sep=""), icon = "info", type = "ok")
      
        next
      } else if (boundsLMod[3] == boundsUMod[3]) { 
        break 
      } else if (boundsLMod[3] > matdmax || boundsUMod[3] < matdmax) {
        strangedmax <- tkmessageBox(title = "Maximum particle size", message=paste('The chosen range for the maximum particle size (from ',boundsLMod[3],tclvalue(dunit),' to ',boundsUMod[3],tclvalue(dunit),') does not include the preset value (',matdmax,tclvalue(dunit),').\n\nThe preset value of the maximum particle size is calculated analytically from the batch. The optimal maximum particle size might be near to it.\n\nDo you want to change the chosen range?',sep=""), icon = "question", type = "yesno", default="yes")      
        if (tclvalue(strangedmax) == "yes") { next }
      }
      
      break
    } else { break }#if !is.null boundsVMod
    }#while input dmax bounds
    if(is.null(boundsVMod)) { next } #if input ok, go on, if not return model selection (oder mit return(4))
   
  
  } #else (if) Mod Kawamura model
  
 
  
  if (all(boundsNl==boundsNu) && all(boundsLMod==boundsUMod)) {
tkmessageBox(title = "Definition error", message="All specified lower and upper bounds are equal, giving a fully defined model and nothing to optimize or fit. You may choose other bounds or one of the other application functions: 'Verify a batch' or 'Design a batch'.", icon = "error", type = "ok")
 message("  All specified lower and upper model parameter bounds are equal.\n")
next
}
  
   message("  Model parameter bounds specified.\n")
  
  valmod <- calcparams(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol,ModSelection,boundsLMod,boundsUMod,boundsNl,boundsNu)
  message("END calculate model parameters function, RETURN value: ",valmod,"\n")
  
  #return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 for back to open recipe or database
  #return 3 for back to batch
  #return 4 for back to model selection

if (valmod < 4) { return(valmod) }
  
  } #while loop model selection 
  

}
#end modelcalc()




































calcparams <- function (MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol,ModSelection,boundsLMod,boundsUMod,boundsNl,boundsNu) {

#return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 for back to open recipe or database
  #return 3 for back to bounds
  #return 4 for back to model selection
  
  message("BEGIN calculate model parameters function\n")
  
  #batch berechnen:
  d10 <- 0
d25 <- 0
d50 <- 0
d75 <- 0
d90 <- 0
CurveFit <- vector(length=length(preparation$Diameter))
#errsq <- vector(length=length(preparation$Diameter))
#sumerrsq <- 0
for (v in seq(from=1, to=length(preparation$Diameter), by=1)) {
 
 dummy <- 0
 for (i in seq(from=1, to=length(MatSelection), by=1)) {
  #print(preparation[v,i+1])
  dummy <- dummy+boundsVol[i]*round(preparation[v,i+1],digits=accuracy)
  }
 CurveFit[v] <- dummy
 #errsq[v] <- ((preparation$VolModel[v]-CurveFit[v])*(preparation$VolModel[v]-CurveFit[v]))
 #sumerrsq <- sumerrsq+errsq[v]
 
 if (d10 == 0 && CurveFit[v] > 10) { #interpolate with value before for d10
  d10 <- preparation$Diameter[v-1] + (preparation$Diameter[v] - preparation$Diameter[v-1]) * (10 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
 }
 if (d25 == 0 && CurveFit[v] > 25) { #single if's because it s also possible that multiple values have to be interpolated between the same pair of data
  d25 <- preparation$Diameter[v-1] + (preparation$Diameter[v] - preparation$Diameter[v-1]) * (25 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
 }
 if (d50 == 0 && CurveFit[v] > 50) { 
  d50 <- preparation$Diameter[v-1] + (preparation$Diameter[v] - preparation$Diameter[v-1]) * (50 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
 }
 if (d75 == 0 && CurveFit[v] > 75) { 
  d75 <- preparation$Diameter[v-1] + (preparation$Diameter[v] - preparation$Diameter[v-1]) * (75 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
 }
 if (d90 == 0 && CurveFit[v] > 90) { 
  d90 <- preparation$Diameter[v-1] + (preparation$Diameter[v] - preparation$Diameter[v-1]) * (90 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
 }
}
  
#In CurveFit ist jetzt der Versatz gespeichert, an den angefittet werden soll  
  
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
 
 
 #praeparation des data frame in analogie zu design-funktion
  preparation <- cbind(preparation, CurveFit)
  colnames(preparation)[22] <- "CurveFit"
  
  tuning <- preparation #save preparation in tuning
  
  #put colnames in temp from preparation and exchange with defined names for calculation

  SaveColNames <- colnames(preparation)
  colnames(preparation) <- c("Diameter","Mat1","Mat2","Mat3","Mat4","Mat5","Mat6","Mat7","Mat8","Mat9","Mat10","Mat11","Mat12","Mat13","Mat14","Mat15","Mat16","Mat17","Mat18","Mat19","Mat20","CurveFit")

#hier muss optimierung folgen. beachten, dass fuer mehrere funktionen itaration notwendig ist.

VolModel <<- vector(length=length(preparation$Diameter)) #in VolModel soll modell gespeichert werden koennen spaeter...


run <- 0
step <- 0
 #digits with leading numbers other than zero for accuracy - behind zeros follow. example: for 3: 0.1230 to 3150; for 2 the same: 0.12 to 3200... common sieve size accuracy is 3 so to say.

pbm <- tkProgressBar(title = "Optimization progress", min = 0, max = 0, width = 350)
setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

if (ModSelection == "Andreasen model") {

#boundsLMod[1] <- as.numeric(boundsVLMod[1]) #dmax
#boundsLMod[2] <- as.numeric(boundsVLMod[2]) #n; as.numeric gives NA if string was given
    
#boundsUMod[1] <- as.numeric(boundsVUMod[1]) #dmax
#boundsUMod[2] <- as.numeric(boundsVUMod[2]) #n; as.numeric gives NA if string was given

dmaxl <<- boundsLMod
dmaxu <<- boundsUMod
dmaxmin <<- boundsLMod


while (TRUE) {

if (run > limparcalc) {

message("  Limit of iterations reached: ",limparcalc,"\n")

close(pbm) 
  
  winmod <- tktoplevel()
  tkwm.title(winmod,"Iteration limit")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste("Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings. Setting\nor changing bounds might help. To get an impression in what dir-\nection this might go, here the values of the last iteration step:\n\nMaximum particle size:",signif(dmaxmin, digits=accsizes),tclvalue(dunit),"\nDistribution modulus:",coef(fit))), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}


run <- run + 1
step <- 1

#print(paste("sig3 dmaxl:",signif(dmaxl, digits=accsizes), " und sig3 dmaxu:", signif(dmaxu, digits=accsizes)))

if (signif(dmaxl, digits=accsizes+1) == signif(dmaxu, digits=accsizes+1)) {

message("  Particle size iterations finished after iteration number: ",run)

dmax <- dmaxmin

modelsub <- subset(preparation, preparation$Diameter <= dmax)

if (boundsNl==-Inf && boundsNu==Inf) {
starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl==-Inf) { #-Inf bis Grenze boundsNu
starterr <- c(boundsNu, boundsNu-0.2*boundsNu, boundsNu-0.5*boundsNu, boundsNu-0.9*boundsNu, boundsNu-1.3*boundsNu, boundsNu-2.0*boundsNu)
} else if (boundsNu==Inf) { #von grenze boundsNl bis info
starterr <- c(boundsNl, boundsNl+0.2*boundsNl, boundsNl+0.5*boundsNl, boundsNl+0.9*boundsNl, boundsNl+1.3*boundsNl, boundsNl+2.0*boundsNl)
} else { #both given
starterr <- c(boundsNl, boundsNl+(boundsNu-boundsNl)/6, boundsNl+(boundsNu-boundsNl)/3, boundsNu-(boundsNu-boundsNl)/3, boundsNu-(boundsNu-boundsNl)/6 ,boundsNu)
}

for (ierr in seq(from=1, to=length(starterr), by=1)) {
fit <- NULL
   try(fit <- nls(CurveFit ~ 100*(Diameter/dmax)^Vn , data=modelsub, start=list(Vn=starterr[ierr]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#ohne bounds fuer n; oben vlt nur bounds fuer d abfragen...; vlt auch gauss-newton statt port (waere default)
   if(!is.null(fit)) { break }
   
}

if (is.null(fit)) { #happens, if nls did not work

message("  Optimization error. Last error message: ",paste(geterrmessage()))

  close(pbm) 
  
  winmod <- tktoplevel()
  tkwm.title(winmod,"Optimization error")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 
  
  
   #break
   }
   
#create VolModel-column and calculate error:
errsq <- vector(length=length(preparation$Diameter))
sumerrsq <- 0
for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax < preparation$Diameter[i]) {
        VolModel[i] <<- 100 }
      else {
        VolModel[i] <<- 100*(preparation$Diameter[i]/dmax)^coef(fit)[1]
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
      sumerrsq <- sumerrsq+errsq[i]
    }
    
    #print(paste("\n Total error:",sumerrsq,"\n"))
    
    preparation <- cbind(preparation, VolModel)
  colnames(preparation)[23] <- "VolModel"
    
#print(preparation)
setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

break #out of while loop

} else {

#> l <- 0.05
#> u <- 500
#> logl <- log10(l)
#> logu <- log10(u)
#> 10^logl
#[1] 0.05
#> 10^logu
#[1] 500
#> mean(c(logl,logu))
#[1] 0.69897
#> 10^mean(c(logl,logu))
#[1] 5
#> 


dmax <- vector(length=4)
dmax[1] <- dmaxl
dmax[2] <- 10^(log10(dmaxl)+(log10(dmaxu)-log10(dmaxl))/3)
dmax[3] <- 10^(log10(dmaxu)-(log10(dmaxu)-log10(dmaxl))/3)
dmax[4] <- dmaxu

#fuer jedes dmax fehler berechnen:
sumerrsq <- rep(0, length(dmax))
for(j in seq(from=1, to=length(dmax), by=1)) {
step <- j

modelsub <- subset(preparation, preparation$Diameter <= dmax[j])

if (boundsNl==-Inf && boundsNu==Inf) {
starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl==-Inf) { #-Inf bis Grenze boundsNu
starterr <- c(boundsNu, boundsNu-0.2*boundsNu, boundsNu-0.5*boundsNu, boundsNu-0.9*boundsNu, boundsNu-1.3*boundsNu, boundsNu-2.0*boundsNu)
} else if (boundsNu==Inf) { #von grenze boundsNl bis info
starterr <- c(boundsNl, boundsNl+0.2*boundsNl, boundsNl+0.5*boundsNl, boundsNl+0.9*boundsNl, boundsNl+1.3*boundsNl, boundsNl+2.0*boundsNl)
} else { #both given
starterr <- c(boundsNl, boundsNl+(boundsNu-boundsNl)/6, boundsNl+(boundsNu-boundsNl)/3, boundsNu-(boundsNu-boundsNl)/3, boundsNu-(boundsNu-boundsNl)/6 ,boundsNu)
}

for (ierr in seq(from=1, to=length(starterr), by=1)) {
fit <- NULL
   try(fit <- nls(CurveFit ~ 100*(Diameter/dmax[j])^Vn , data=modelsub, start=list(Vn=starterr[ierr]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#ohne bounds fuer n; oben vlt nur bounds fuer d abfragen...; vlt auch gauss-newton statt port (waere default)
   if(!is.null(fit)) { break }
   
}

if (is.null(fit)) { #happens, if nls did not work
  #close(pbm) 
  
  sumerrsq[j] <- NA
  nacount <<- nacount+1
  next
   #break
   }
      
#create VolModel-column and calculate error:
errsq <- vector(length=length(preparation$Diameter))
for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax[j] < preparation$Diameter[i]) {
        VolModel[i] <<- 100 }
      else {
        VolModel[i] <<- 100*(preparation$Diameter[i]/dmax[j])^coef(fit)[1]
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
      sumerrsq[j] <- sumerrsq[j]+errsq[i]
    }
    
setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))


} #for j in dmax

#print(paste('Run ',run,': Err(dmax1=',dmax[1],')=',sumerrsq[1],', Err(dmax2=',dmax[2],')=',sumerrsq[2],', Err(dmax3=',dmax[3],')=',sumerrsq[3],', Err(dmax4=',dmax[4],')=',sumerrsq[4],'', sep=""))

#entscheidung wleches min und was neue dmaxl und dmaxu:

  if (sum(is.na(sumerrsq)) == 4) {
         #dann kein verlgeichswert vorhanden -> abbruch und neue bounds oder neues modell fordern
         close(pbm)
         
         message("  Optimization errors for all particle size steps. Last error message: ",paste(geterrmessage()))
         
         winmod <- tktoplevel()
  tkwm.title(winmod,"Optimization error")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 
      }


if (which.min(sumerrsq) == 1) {
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
} else if (which.min(sumerrsq) == 2) {
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
} else if (which.min(sumerrsq) == 3) {
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
} else {
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
}

dmaxmin <<- dmax[which.min(sumerrsq)]

#print(paste('Minimum bei dmax',which.min(sumerrsq),sep=""))

}#else von if round == round

setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

}#end while loop Andreasen

close(pbm) 

message("  Optimization finished after iteration number: ",run)

#for calling a results-function together with ModSelection:
optd <- dmax
optn <- coef(fit)

namd <- "Maximum particle size"
namn <- "Distribution modulus"

resultsmsg <- paste(ModSelection,"\n\nMaximum particle size:",signif(optd[1], digits=accsizes),tclvalue(dunit),"\nDistribution modulus:",optn[1],"\n\nSum of squared deviations:",sumerrsq)



















} else if (ModSelection == "Dinger/Funk model") {#end if ModSelection == Andreasen

dminl <<- boundsLMod[1]
dminu <<- boundsUMod[1]
dmaxl <<- boundsLMod[2]
dmaxu <<- boundsUMod[2]
dminmin <<- boundsLMod[1]
dmaxmin <<- boundsLMod[2] #davor dmaxmin

while (TRUE) {

if (run > limparcalc) {

message("  Limit of iterations reached: ",limparcalc,"\n")

close(pbm) 
  
  winmod <- tktoplevel()
  tkwm.title(winmod,"Iteration limit")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste("Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings. Setting\nor changing bounds might help. To get an impression in what dir-\nection this might go, here the values of the last iteration step:\n\nMinimum particle size:",signif(dminmin, digits=accsizes),tclvalue(dunit),"Maximum particle size:",signif(dmaxmin, digits=accsizes),tclvalue(dunit),"\nDistribution modulus:",coef(fit))), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}

run <- run + 1
step <- 1

#print(" ")
#print(" ")
#print(paste("Run",run,":"))
#print(paste("dminl:",signif(dminl, digits=accsizes), " und dminu:", signif(dminu, digits=accsizes)))
#print(paste("dmaxl:",signif(dmaxl, digits=accsizes), " und dmaxu:", signif(dmaxu, digits=accsizes)))
#print(" ")

if (signif(dmaxl, digits=accsizes+1) == signif(dmaxu, digits=accsizes+1) && signif(dminl, digits=accsizes+1) == signif(dminu, digits=accsizes+1)) {

message("  Particle size iterations finished after iteration number: ",run)

#print("Start IF-part")
#print(" ")

#dmin <- signif(dminmin, digits=accsizes+1)
#dmax <- signif(dmaxmin, digits=accsizes+1) #means, that there has to be a number; if NA, something has to be constructed..!
dmin <- dminmin
dmax <- dmaxmin


modelsub2 <- subset(preparation, preparation$Diameter >= dmin)
modelsub <- subset(modelsub2, modelsub2$Diameter <= dmax)

if (boundsNl==-Inf && boundsNu==Inf) {
starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl==-Inf) { #-Inf bis Grenze boundsNu
starterr <- c(boundsNu, boundsNu-0.2*boundsNu, boundsNu-0.5*boundsNu, boundsNu-0.9*boundsNu, boundsNu-1.3*boundsNu, boundsNu-2.0*boundsNu)
} else if (boundsNu==Inf) { #von grenze boundsNl bis info
starterr <- c(boundsNl, boundsNl+0.2*boundsNl, boundsNl+0.5*boundsNl, boundsNl+0.9*boundsNl, boundsNl+1.3*boundsNl, boundsNl+2.0*boundsNl)
} else { #both given
starterr <- c(boundsNl, boundsNl+(boundsNu-boundsNl)/6, boundsNl+(boundsNu-boundsNl)/3, boundsNu-(boundsNu-boundsNl)/3, boundsNu-(boundsNu-boundsNl)/6 ,boundsNu)
}

for (ierr in seq(from=1, to=length(starterr), by=1)) {
fit <- NULL
   try(fit <- nls(CurveFit ~ 100*(Diameter^Vn - dmin^Vn)/(dmax^Vn - dmin^Vn) , data=modelsub, start=list(Vn=starterr[ierr]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#ohne bounds fuer n; oben vlt nur bounds fuer d abfragen...; vlt auch gauss-newton statt port (waere default)
   if(!is.null(fit)) { break }
   
}

if (is.null(fit)) { #happens, if nls did not work

message("  Optimization error. Last error message: ",paste(geterrmessage()))

  close(pbm)
         
         winmod <- tktoplevel()
  tkwm.title(winmod,"Optimization error")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 
   #break
   }
   
#create VolModel-column and calculate error:
errsq <- vector(length=length(preparation$Diameter))
sumerrsq <- 0
for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax < preparation$Diameter[i]) {
        VolModel[i] <<- 100 }
      else if (dmin < preparation$Diameter[i]) {
        VolModel[i] <<- 100*(preparation$Diameter[i]^coef(fit)[1] - dmin^coef(fit)[1])/(dmax^coef(fit)[1] - dmin^coef(fit)[1])
      }
      else {
        VolModel[i] <<- 0
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
      sumerrsq <- sumerrsq+errsq[i]
    }
    
    #print(paste("Total error:",sumerrsq))
    
    preparation <- cbind(preparation, VolModel)
  colnames(preparation)[23] <- "VolModel"
    
#print(preparation)

setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

break #out of while loop

} else {

#print("ELSE-part")
#print(" ")

dmin <- vector(length=4)
#dmin[1] <- signif(dminl, digits=accsizes+1) 
#dmin[2] <- signif(dminl+(dminu-dminl)/3, digits=accsizes+1) 
#dmin[3] <- signif(dminu-(dminu-dminl)/3, digits=accsizes+1) 
#dmin[4] <- signif(dminu, digits=accsizes+1) 
dmin[1] <- dminl
dmin[2] <- 10^(log10(dminl)+(log10(dminu)-log10(dminl))/3)
dmin[3] <- 10^(log10(dminu)-(log10(dminu)-log10(dminl))/3)
dmin[4] <- dminu
#print(paste("dmin:",dmin))

dmax <- vector(length=4)
#dmax[1] <- signif(dmaxl, digits=accsizes+1) 
#dmax[2] <- signif(dmaxl+(dmaxu-dmaxl)/3, digits=accsizes+1) 
#dmax[3] <- signif(dmaxu-(dmaxu-dmaxl)/3, digits=accsizes+1) 
#dmax[4] <- signif(dmaxu, digits=accsizes+1) 
dmax[1] <- dmaxl
dmax[2] <- 10^(log10(dmaxl)+(log10(dmaxu)-log10(dmaxl))/3)
dmax[3] <- 10^(log10(dmaxu)-(log10(dmaxu)-log10(dmaxl))/3)
dmax[4] <- dmaxu
#print(paste("dmax:",dmax))

#fuer jedes dmax fehler berechnen:
sumerrsq <- rep(0, length(dmax)*length(dmin))
sumerrsqcount <- 0
for (h in seq(from=1, to=length(dmin), by=1)) {
for(j in seq(from=1, to=length(dmax), by=1)) {

sumerrsqcount <- sumerrsqcount + 1
step <- sumerrsqcount


if (dmin[h] >= dmax[j] || length(intersect(which(dmin[h] <= preparation$Diameter), which(dmax[j] >= preparation$Diameter))) < 3) {
sumerrsq[sumerrsqcount] <- NA #geht, da NA von which.min nicht beahctet werden

#print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dmin=",dmin[h],"and dmax=",dmax[j]))
next
}

modelsub2 <- subset(preparation, preparation$Diameter >= dmin[h])
modelsub <- subset(modelsub2, modelsub2$Diameter <= dmax[j])

if (boundsNl==-Inf && boundsNu==Inf) {
starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl==-Inf) { #-Inf bis Grenze boundsNu
starterr <- c(boundsNu, boundsNu-0.2*boundsNu, boundsNu-0.5*boundsNu, boundsNu-0.9*boundsNu, boundsNu-1.3*boundsNu, boundsNu-2.0*boundsNu)
} else if (boundsNu==Inf) { #von grenze boundsNl bis info
starterr <- c(boundsNl, boundsNl+0.2*boundsNl, boundsNl+0.5*boundsNl, boundsNl+0.9*boundsNl, boundsNl+1.3*boundsNl, boundsNl+2.0*boundsNl)
} else { #both given
starterr <- c(boundsNl, boundsNl+(boundsNu-boundsNl)/6, boundsNl+(boundsNu-boundsNl)/3, boundsNu-(boundsNu-boundsNl)/3, boundsNu-(boundsNu-boundsNl)/6 ,boundsNu)
}

for (ierr in seq(from=1, to=length(starterr), by=1)) {
fit <- NULL
   try(fit <- nls(CurveFit ~ 100*(Diameter^Vn - dmin[h]^Vn)/(dmax[j]^Vn - dmin[h]^Vn) , data=modelsub, start=list(Vn=starterr[ierr]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
   if(!is.null(fit)) { break }   
}

if (is.null(fit)) { #happens, if nls did not work
  #close(pbm) 
  
  sumerrsq[sumerrsqcount] <- NA
  nacount <<- nacount+1
  next
   #break
   }

   
#create VolModel-column and calculate error:
errsq <- vector(length=length(preparation$Diameter))
for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax[j] < preparation$Diameter[i]) {
        VolModel[i] <<- 100 }
      else if (dmin[h] < preparation$Diameter[i]) {
        VolModel[i] <<- 100*(preparation$Diameter[i]^coef(fit)[1] - dmin[h]^coef(fit)[1])/(dmax[j]^coef(fit)[1] - dmin[h]^coef(fit)[1])
      }
      else {
        VolModel[i] <<- 0
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
      sumerrsq[sumerrsqcount] <- sumerrsq[sumerrsqcount]+errsq[i]
    }

#print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dmin=",dmin[h],"and dmax=",dmax[j]))

setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

} #for j in dmax
} #for h in dmin

#entscheidung wleches min und was neue dmaxl und dmaxu:

   
      if (sum(is.na(sumerrsq)) == 16) {
         #dann kein verlgeichswert vorhanden -> abbruch und neue bounds oder neues modell fordern
         close(pbm)
         
         message("  Optimization errors for all particle size steps. Last error message: ",paste(geterrmessage()))
         
         winmod <- tktoplevel()
  tkwm.title(winmod,"Optimization error")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 
      }

#hier weiter

if (which.min(sumerrsq) == 1) {
dminl <<- dmin[1]
dminu <<- dmin[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dmincount <- 1
dmaxcount <- 1
} else if (which.min(sumerrsq) == 2) {
dminl <<- dmin[1]
dminu <<- dmin[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dmincount <- 1
dmaxcount <- 2
} else if (which.min(sumerrsq) == 3) {
dminl <<- dmin[1]
dminu <<- dmin[2]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dmincount <- 1
dmaxcount <- 3
} else if (which.min(sumerrsq) == 4) {
dminl <<- dmin[1]
dminu <<- dmin[2]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dmincount <- 1
dmaxcount <- 4
} else if (which.min(sumerrsq) == 5) {
dminl <<- dmin[1]
dminu <<- dmin[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dmincount <- 2
dmaxcount <- 1
} else if (which.min(sumerrsq) == 6) {
dminl <<- dmin[1]
dminu <<- dmin[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dmincount <- 2
dmaxcount <- 2
} else if (which.min(sumerrsq) == 7) {
dminl <<- dmin[1]
dminu <<- dmin[3]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dmincount <- 2
dmaxcount <- 3
} else if (which.min(sumerrsq) == 8) {
dminl <<- dmin[1]
dminu <<- dmin[3]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dmincount <- 2
dmaxcount <- 4
} else if (which.min(sumerrsq) == 9) {
dminl <<- dmin[2]
dminu <<- dmin[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dmincount <- 3
dmaxcount <- 1
} else if (which.min(sumerrsq) == 10) {
dminl <<- dmin[2]
dminu <<- dmin[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dmincount <- 3
dmaxcount <- 2
} else if (which.min(sumerrsq) == 11) {
dminl <<- dmin[2]
dminu <<- dmin[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dmincount <- 3
dmaxcount <- 3
} else if (which.min(sumerrsq) == 12) {
dminl <<- dmin[2]
dminu <<- dmin[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dmincount <- 3
dmaxcount <- 4
} else if (which.min(sumerrsq) == 13) {
dminl <<- dmin[3]
dminu <<- dmin[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dmincount <- 4
dmaxcount <- 1
} else if (which.min(sumerrsq) == 14) {
dminl <<- dmin[3]
dminu <<- dmin[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dmincount <- 4
dmaxcount <- 2
} else if (which.min(sumerrsq) == 15) {
dminl <<- dmin[3]
dminu <<- dmin[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dmincount <- 4
dmaxcount <- 3
} else { #if (which.min(sumerrsq) == 16) 
dminl <<- dmin[3]
dminu <<- dmin[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dmincount <- 4
dmaxcount <- 4
}

#dminmin <<- dmin[which.min(dmincount)]
#dmaxmin <<- dmax[which.min(dmaxcount)]
dminmin <<- dmin[dmincount]
dmaxmin <<- dmax[dmaxcount]

#print(paste('Minimum bei sumerrsqcount',which.min(sumerrsq)))

}#else von if round == round

setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

}#end while loop Dinger Funk

close(pbm) 

message("  Optimization finished after iteration number: ",run)

#for calling a results-function together with ModSelection:
optd <- c(dmin, dmax)
optn <- coef(fit)

namd <- c("Minimum particle size","Maximum particle size")
namn <- "Distribution modulus"

resultsmsg <- paste(ModSelection,"\n\nMinimum particle size:",signif(optd[1], digits=accsizes),tclvalue(dunit),"\nMaximum particle size:",signif(optd[2], digits=accsizes),tclvalue(dunit),"\nDistribution modulus:",optn,"\n\nSum of squared deviations:",sumerrsq)



























} else if (ModSelection == "Psi model") {#end else if ModSelection == DIngerFunk

dmaxl <<- boundsLMod
dmaxu <<- boundsUMod
dmaxmin <<- boundsLMod



while (TRUE) {

if (run > limparcalc) {

message("  Limit of iterations reached: ",limparcalc,"\n")

close(pbm) 
  
  winmod <- tktoplevel()
  tkwm.title(winmod,"Iteration limit")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste("Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings. Setting\nor changing bounds might help. To get an impression in what dir-\nection this might go, here the values of the last iteration step:\n\nMaximum particle size:",signif(dmaxmin, digits=accsizes),tclvalue(dunit),"\nMinimum distribution modulus:",coef(fit)[1],"\nMaximum distribution modulus:",coef(fit)[2])), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}

run <- run + 1
step <- 1

#print(paste("sig3 dmaxl:",signif(dmaxl, digits=accsizes), " und sig3 dmaxu:", signif(dmaxu, digits=accsizes)))

if (signif(dmaxl, digits=accsizes+1) == signif(dmaxu, digits=accsizes+1)) {

message("  Particle size iterations finished after iteration number: ",run)

dmax <- dmaxmin

modelsub <- subset(preparation, preparation$Diameter <= dmax)


if (boundsNl[1]==-Inf && boundsNu[1]==Inf) {
starterr1 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[1]==-Inf) { #-Inf bis Grenze boundsNu
starterr1 <- c(boundsNu[1], boundsNu[1]-0.2*boundsNu[1], boundsNu[1]-0.5*boundsNu[1], boundsNu[1]-0.9*boundsNu[1], boundsNu[1]-1.3*boundsNu[1], boundsNu[1]-2.0*boundsNu[1])
} else if (boundsNu[1]==Inf) { #von grenze boundsNl bis info
starterr1 <- c(boundsNl[1], boundsNl[1]+0.2*boundsNl[1], boundsNl[1]+0.5*boundsNl[1], boundsNl[1]+0.9*boundsNl[1], boundsNl[1]+1.3*boundsNl[1], boundsNl[1]+2.0*boundsNl[1])
} else { #both given
starterr1 <- c(boundsNl[1], boundsNl[1]+(boundsNu[1]-boundsNl[1])/6, boundsNl[1]+(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/6 ,boundsNu[1])
}
#, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}


for (ierr1 in seq(from=1, to=length(starterr1), by=1)) {
for (ierr2 in seq(from=1, to=length(starterr2), by=1)) {
fit <- NULL
   try(fit <- nls(CurveFit ~ 100*(Diameter/dmax)^(Vnmin + Diameter*(Vnmax - Vnmin)/dmax) , data=modelsub, start=list(Vnmin=starterr1[ierr1], Vnmax=starterr2[ierr2]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#ohne bounds fuer n; oben vlt nur bounds fuer d abfragen...; vlt auch gauss-newton statt port (waere default)
   if(!is.null(fit)) { break }
   
}}

if (is.null(fit)) { #happens, if nls did not work

message("  Optimization error. Last error message: ",paste(geterrmessage()))

  close(pbm) 
  
  winmod <- tktoplevel()
  tkwm.title(winmod,"Optimization error")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
 tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
  tkdestroy(winmod)
  return(var) 
   #break
   }
   
#create VolModel-column and calculate error:
errsq <- vector(length=length(preparation$Diameter))
sumerrsq <- 0
for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax < preparation$Diameter[i]) {
        VolModel[i] <<- 100 }
      else {
        VolModel[i] <<- 100*(preparation$Diameter[i]/dmax)^(coef(fit)[1] + preparation$Diameter[i]*(coef(fit)[2]-coef(fit)[1])/dmax)
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
      sumerrsq <- sumerrsq+errsq[i]
    }
    
    #print(paste("\n Total error:",sumerrsq,"\n"))
    
    preparation <- cbind(preparation, VolModel)
  colnames(preparation)[23] <- "VolModel"
    
#print(preparation)
setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))



break #out of while loop

} else {

dmax <- vector(length=4)
#dmax[1] <- signif(dmaxl, digits=accsizes+1) 
#dmax[2] <- signif(dmaxl+(dmaxu-dmaxl)/3, digits=accsizes+1) 
#dmax[3] <- signif(dmaxu-(dmaxu-dmaxl)/3, digits=accsizes+1) 
#dmax[4] <- signif(dmaxu, digits=accsizes+1) 
dmax[1] <- dmaxl
dmax[2] <- 10^(log10(dmaxl)+(log10(dmaxu)-log10(dmaxl))/3)
dmax[3] <- 10^(log10(dmaxu)-(log10(dmaxu)-log10(dmaxl))/3)
dmax[4] <- dmaxu
#print(paste("dmax:",dmax))

#fuer jedes dmax fehler berechnen:
sumerrsq <- rep(0, length(dmax))
for(j in seq(from=1, to=length(dmax), by=1)) {
step <- j

modelsub <- subset(preparation, preparation$Diameter <= dmax[j])

if (boundsNl[1]==-Inf && boundsNu[1]==Inf) {
starterr1 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[1]==-Inf) { #-Inf bis Grenze boundsNu
starterr1 <- c(boundsNu[1], boundsNu[1]-0.2*boundsNu[1], boundsNu[1]-0.5*boundsNu[1], boundsNu[1]-0.9*boundsNu[1], boundsNu[1]-1.3*boundsNu[1], boundsNu[1]-2.0*boundsNu[1])
} else if (boundsNu[1]==Inf) { #von grenze boundsNl bis info
starterr1 <- c(boundsNl[1], boundsNl[1]+0.2*boundsNl[1], boundsNl[1]+0.5*boundsNl[1], boundsNl[1]+0.9*boundsNl[1], boundsNl[1]+1.3*boundsNl[1], boundsNl[1]+2.0*boundsNl[1])
} else { #both given
starterr1 <- c(boundsNl[1], boundsNl[1]+(boundsNu[1]-boundsNl[1])/6, boundsNl[1]+(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/6 ,boundsNu[1])
}
#, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

for (ierr1 in seq(from=1, to=length(starterr1), by=1)) {
for (ierr2 in seq(from=1, to=length(starterr2), by=1)) {
fit <- NULL
   try(fit <- nls(CurveFit ~ 100*(Diameter/dmax[j])^(Vnmin + Diameter*(Vnmax - Vnmin)/dmax[j]) , data=modelsub, start=list(Vnmin=starterr1[ierr1], Vnmax=starterr2[ierr2]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#ohne bounds fuer n; oben vlt nur bounds fuer d abfragen...; vlt auch gauss-newton statt port (waere default)
   if(!is.null(fit)) { break }
   
}}

if (is.null(fit)) { #happens, if nls did not work
  #close(pbm) 

  

  sumerrsq[j] <- NA

  nacount <<- nacount+1

  next
   #break
   }
      
#create VolModel-column and calculate error:
errsq <- vector(length=length(preparation$Diameter))
for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax[j] < preparation$Diameter[i]) {
        VolModel[i] <<- 100 }
      else {
        VolModel[i] <<- 100*(preparation$Diameter[i]/dmax[j])^(coef(fit)[1] + preparation$Diameter[i]*(coef(fit)[2]-coef(fit)[1])/dmax[j])
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
      sumerrsq[j] <- sumerrsq[j]+errsq[i]
    }
    
    setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))



} #for j in dmax

#print(paste('Run ',run,': Err(dmax1=',dmax[1],')=',sumerrsq[1],', Err(dmax2=',dmax[2],')=',sumerrsq[2],', Err(dmax3=',dmax[3],')=',sumerrsq[3],', Err(dmax4=',dmax[4],')=',sumerrsq[4],'', sep=""))

#entscheidung wleches min und was neue dmaxl und dmaxu:

if (sum(is.na(sumerrsq)) == 4) {

         #dann kein verlgeichswert vorhanden -> abbruch und neue bounds oder neues modell fordern

         close(pbm)

         message("  Optimization errors for all particle size steps. Last error message: ",paste(geterrmessage()))

         winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)



  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

 tkdestroy(winmod)

  return(var) 

      }

if (which.min(sumerrsq) == 1) {
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
} else if (which.min(sumerrsq) == 2) {
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
} else if (which.min(sumerrsq) == 3) {
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
} else {
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
}

dmaxmin <<- dmax[which.min(sumerrsq)]

#print(paste('Minimum bei dmax',which.min(sumerrsq),sep=""))

}#else von if round == round

setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))



}#end while loop 

close(pbm) 

message("  Optimization finished after iteration number: ",run)

#for calling a results-function together with ModSelection:
optd <- dmax
optn <- coef(fit) #[1] nmin, [2] nmax

namd <- "Maximum particle size"
namn <- c("Min. dist. modulus","Max. dist. modulus")

#print(coef(fit))
resultsmsg <- paste(ModSelection,"\n\nMaximum particle size:",signif(optd, digits=accsizes),tclvalue(dunit),"\nDistribution modulus nmin:",optn[1],"\nDistribution modulus nmax:",optn[2],"\n\nSum of squared deviations:",sumerrsq)

























} else if (ModSelection == "Modified Psi model") { #end else if ModSelection == Psi

dminl <<- boundsLMod[1]
dminu <<- boundsUMod[1]
dmaxl <<- boundsLMod[2]
dmaxu <<- boundsUMod[2]
dminmin <<- boundsLMod[1]
dmaxmin <<- boundsLMod[2] #davor dmaxmin

while (TRUE) {

if (run > limparcalc) {

message("  Limit of iterations reached: ",limparcalc,"\n")

close(pbm) 
  
  winmod <- tktoplevel()
  tkwm.title(winmod,"Iteration limit")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste("Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings. Setting\nor changing bounds might help. To get an impression in what dir-\nection this might go, here the values of the last iteration step:\n\nMinimum particle size:",signif(dminmin, digits=accsizes),tclvalue(dunit),"Maximum particle size:",signif(dmaxmin, digits=accsizes),tclvalue(dunit),"\nMinimum distribution modulus:",coef(fit)[1],"\nMaximum distribution modulus:",coef(fit)[2])), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}

run <- run + 1
step <- 1

#print(" ")
#print(" ")
#print(paste("Run",run,":"))
#print(paste("dminl:",signif(dminl, digits=accsizes), " und dminu:", signif(dminu, digits=accsizes)))
#print(paste("dmaxl:",signif(dmaxl, digits=accsizes), " und dmaxu:", signif(dmaxu, digits=accsizes)))
#print(" ")

if (signif(dmaxl, digits=accsizes+1) == signif(dmaxu, digits=accsizes+1) && signif(dminl, digits=accsizes+1) == signif(dminu, digits=accsizes+1)) {

message("  Particle size iterations finished after iteration number: ",run)

#print("Start IF-part")
#print(" ")

dmin <- dminmin
dmax <- dmaxmin #means, that there has to be a number; if NA, something has to be constructed..!

modelsub2 <- subset(preparation, preparation$Diameter >= dmin)
modelsub <- subset(modelsub2, modelsub2$Diameter <= dmax)

if (boundsNl[1]==-Inf && boundsNu[1]==Inf) {
starterr1 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[1]==-Inf) { #-Inf bis Grenze boundsNu
starterr1 <- c(boundsNu[1], boundsNu[1]-0.2*boundsNu[1], boundsNu[1]-0.5*boundsNu[1], boundsNu[1]-0.9*boundsNu[1], boundsNu[1]-1.3*boundsNu[1], boundsNu[1]-2.0*boundsNu[1])
} else if (boundsNu[1]==Inf) { #von grenze boundsNl bis info
starterr1 <- c(boundsNl[1], boundsNl[1]+0.2*boundsNl[1], boundsNl[1]+0.5*boundsNl[1], boundsNl[1]+0.9*boundsNl[1], boundsNl[1]+1.3*boundsNl[1], boundsNl[1]+2.0*boundsNl[1])
} else { #both given
starterr1 <- c(boundsNl[1], boundsNl[1]+(boundsNu[1]-boundsNl[1])/6, boundsNl[1]+(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/6 ,boundsNu[1])
}
#, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

for (ierr1 in seq(from=1, to=length(starterr1), by=1)) {
for (ierr2 in seq(from=1, to=length(starterr2), by=1)) {

fit <- NULL
   try(fit <- nls(CurveFit ~ 100*(Diameter^(Vnmin+Diameter*(Vnmax-Vnmin)/dmax) - dmin^(Vnmin+Diameter*(Vnmax-Vnmin)/dmax))/(dmax^(Vnmin+Diameter*(Vnmax-Vnmin)/dmax) - dmin^(Vnmin+Diameter*(Vnmax-Vnmin)/dmax)) , data=modelsub, start=list(Vnmin=starterr1[ierr1], Vnmax=starterr2[ierr2]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#ohne bounds fuer n; oben vlt nur bounds fuer d abfragen...; vlt auch gauss-newton statt port (waere default)
   if(!is.null(fit)) { break }
   
}}

if (is.null(fit)) { #happens, if nls did not work

message("  Optimization error. Last error message: ",paste(geterrmessage()))

  close(pbm) 
  
  winmod <- tktoplevel()
  tkwm.title(winmod,"Optimization error")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)



  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
  tkdestroy(winmod)
  return(var) 
   #break
   }
   
#create VolModel-column and calculate error:
errsq <- vector(length=length(preparation$Diameter))
sumerrsq <- 0
for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax < preparation$Diameter[i]) {
        VolModel[i] <<- 100 }
      else if (dmin < preparation$Diameter[i]) {
        VolModel[i] <<- 100*(preparation$Diameter[i]^(coef(fit)[1]+preparation$Diameter[i]*(coef(fit)[2]-coef(fit)[1])/dmax) - dmin^(coef(fit)[1]+preparation$Diameter[i]*(coef(fit)[2]-coef(fit)[1])/dmax))/(dmax^(coef(fit)[1]+preparation$Diameter[i]*(coef(fit)[2]-coef(fit)[1])/dmax) - dmin^(coef(fit)[1]+preparation$Diameter[i]*(coef(fit)[2]-coef(fit)[1])/dmax))
      }
      else {
        VolModel[i] <<- 0
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
      sumerrsq <- sumerrsq+errsq[i]
    }
    
    #print(paste("Total error:",sumerrsq))
    
    preparation <- cbind(preparation, VolModel)
  colnames(preparation)[23] <- "VolModel"
    
#print(preparation)
setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))


break #out of while loop

} else {

#print("ELSE-part")
#print(" ")

dmin <- vector(length=4)
#dmin[1] <- signif(dminl, digits=accsizes+1) 
#dmin[2] <- signif(dminl+(dminu-dminl)/3, digits=accsizes+1) 
#dmin[3] <- signif(dminu-(dminu-dminl)/3, digits=accsizes+1) 
#dmin[4] <- signif(dminu, digits=accsizes+1) 
dmin[1] <- dminl
dmin[2] <- 10^(log10(dminl)+(log10(dminu)-log10(dminl))/3)
dmin[3] <- 10^(log10(dminu)-(log10(dminu)-log10(dminl))/3)
dmin[4] <- dminu
#print(paste("dmin:",dmin))

dmax <- vector(length=4)
#dmax[1] <- signif(dmaxl, digits=accsizes+1) 
#dmax[2] <- signif(dmaxl+(dmaxu-dmaxl)/3, digits=accsizes+1) 
#dmax[3] <- signif(dmaxu-(dmaxu-dmaxl)/3, digits=accsizes+1) 
#dmax[4] <- signif(dmaxu, digits=accsizes+1) 
dmax[1] <- dmaxl
dmax[2] <- 10^(log10(dmaxl)+(log10(dmaxu)-log10(dmaxl))/3)
dmax[3] <- 10^(log10(dmaxu)-(log10(dmaxu)-log10(dmaxl))/3)
dmax[4] <- dmaxu
#print(paste("dmax:",dmax))

#fuer jedes dmax fehler berechnen:
sumerrsq <- rep(0, length(dmax)*length(dmin))
sumerrsqcount <- 0
for (h in seq(from=1, to=length(dmin), by=1)) {
for(j in seq(from=1, to=length(dmax), by=1)) {

sumerrsqcount <- sumerrsqcount + 1
step <- sumerrsqcount

if (dmin[h] >= dmax[j] || length(intersect(which(dmin[h] <= preparation$Diameter), which(dmax[j] >= preparation$Diameter))) < 3) {
sumerrsq[sumerrsqcount] <- NA #geht, da NA von which.min nicht beahctet werden

#print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dmin=",dmin[h],"and dmax=",dmax[j]))
next
}

modelsub2 <- subset(preparation, preparation$Diameter >= dmin[h])
modelsub <- subset(modelsub2, modelsub2$Diameter <= dmax[j])


if (boundsNl[1]==-Inf && boundsNu[1]==Inf) {
starterr1 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[1]==-Inf) { #-Inf bis Grenze boundsNu
starterr1 <- c(boundsNu[1], boundsNu[1]-0.2*boundsNu[1], boundsNu[1]-0.5*boundsNu[1], boundsNu[1]-0.9*boundsNu[1], boundsNu[1]-1.3*boundsNu[1], boundsNu[1]-2.0*boundsNu[1])
} else if (boundsNu[1]==Inf) { #von grenze boundsNl bis info
starterr1 <- c(boundsNl[1], boundsNl[1]+0.2*boundsNl[1], boundsNl[1]+0.5*boundsNl[1], boundsNl[1]+0.9*boundsNl[1], boundsNl[1]+1.3*boundsNl[1], boundsNl[1]+2.0*boundsNl[1])
} else { #both given
starterr1 <- c(boundsNl[1], boundsNl[1]+(boundsNu[1]-boundsNl[1])/6, boundsNl[1]+(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/6 ,boundsNu[1])
}
#, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

for (ierr1 in seq(from=1, to=length(starterr1), by=1)) {
for (ierr2 in seq(from=1, to=length(starterr2), by=1)) {

fit <- NULL
   try(fit <- nls(CurveFit ~ 100*(Diameter^(Vnmin+Diameter*(Vnmax-Vnmin)/dmax[j]) - dmin[h]^(Vnmin+Diameter*(Vnmax-Vnmin)/dmax[j]))/(dmax[j]^(Vnmin+Diameter*(Vnmax-Vnmin)/dmax[j]) - dmin[h]^(Vnmin+Diameter*(Vnmax-Vnmin)/dmax[j])) , data=modelsub, start=list(Vnmin=starterr1[ierr1], Vnmax=starterr2[ierr2]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#ohne bounds fuer n; oben vlt nur bounds fuer d abfragen...; vlt auch gauss-newton statt port (waere default)
   if(!is.null(fit)) { break }
   
}}

if (is.null(fit)) { #happens, if nls did not work
  #close(pbm) 

  

  sumerrsq[sumerrsqcount] <- NA

  nacount <<- nacount+1

  next

   }

   
#create VolModel-column and calculate error:
errsq <- vector(length=length(preparation$Diameter))
for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
      if (dmax[j] < preparation$Diameter[i]) {
        VolModel[i] <<- 100 }
      else if (dmin[h] < preparation$Diameter[i]) {
        VolModel[i] <<- 100*(preparation$Diameter[i]^(coef(fit)[1]+preparation$Diameter[i]*(coef(fit)[2]-coef(fit)[1])/dmax[j]) - dmin[h]^(coef(fit)[1]+preparation$Diameter[i]*(coef(fit)[2]-coef(fit)[1])/dmax[j]))/(dmax[j]^(coef(fit)[1]+preparation$Diameter[i]*(coef(fit)[2]-coef(fit)[1])/dmax[j]) - dmin[h]^(coef(fit)[1]+preparation$Diameter[i]*(coef(fit)[2]-coef(fit)[1])/dmax[j]))
      }
      else {
        VolModel[i] <<- 0
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
      sumerrsq[sumerrsqcount] <- sumerrsq[sumerrsqcount]+errsq[i]
    }

#print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dmin=",dmin[h],"and dmax=",dmax[j]))

setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

} #for j in dmax
} #for h in dmin

#entscheidung wleches min und was neue dmaxl und dmaxu:

     if (sum(is.na(sumerrsq)) == 16) {

         #dann kein verlgeichswert vorhanden -> abbruch und neue bounds oder neues modell fordern
         
         message("  Optimization errors for all particle size steps. Last error message: ",paste(geterrmessage()))

         close(pbm)

         

         winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)



  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

 tkdestroy(winmod)

  return(var) 

      }

#hier weiter

if (which.min(sumerrsq) == 1) {
dminl <<- dmin[1]
dminu <<- dmin[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dmincount <- 1
dmaxcount <- 1
} else if (which.min(sumerrsq) == 2) {
dminl <<- dmin[1]
dminu <<- dmin[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dmincount <- 1
dmaxcount <- 2
} else if (which.min(sumerrsq) == 3) {
dminl <<- dmin[1]
dminu <<- dmin[2]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dmincount <- 1
dmaxcount <- 3
} else if (which.min(sumerrsq) == 4) {
dminl <<- dmin[1]
dminu <<- dmin[2]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dmincount <- 1
dmaxcount <- 4
} else if (which.min(sumerrsq) == 5) {
dminl <<- dmin[1]
dminu <<- dmin[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dmincount <- 2
dmaxcount <- 1
} else if (which.min(sumerrsq) == 6) {
dminl <<- dmin[1]
dminu <<- dmin[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dmincount <- 2
dmaxcount <- 2
} else if (which.min(sumerrsq) == 7) {
dminl <<- dmin[1]
dminu <<- dmin[3]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dmincount <- 2
dmaxcount <- 3
} else if (which.min(sumerrsq) == 8) {
dminl <<- dmin[1]
dminu <<- dmin[3]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dmincount <- 2
dmaxcount <- 4
} else if (which.min(sumerrsq) == 9) {
dminl <<- dmin[2]
dminu <<- dmin[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dmincount <- 3
dmaxcount <- 1
} else if (which.min(sumerrsq) == 10) {
dminl <<- dmin[2]
dminu <<- dmin[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dmincount <- 3
dmaxcount <- 2
} else if (which.min(sumerrsq) == 11) {
dminl <<- dmin[2]
dminu <<- dmin[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dmincount <- 3
dmaxcount <- 3
} else if (which.min(sumerrsq) == 12) {
dminl <<- dmin[2]
dminu <<- dmin[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dmincount <- 3
dmaxcount <- 4
} else if (which.min(sumerrsq) == 13) {
dminl <<- dmin[3]
dminu <<- dmin[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dmincount <- 4
dmaxcount <- 1
} else if (which.min(sumerrsq) == 14) {
dminl <<- dmin[3]
dminu <<- dmin[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dmincount <- 4
dmaxcount <- 2
} else if (which.min(sumerrsq) == 15) {
dminl <<- dmin[3]
dminu <<- dmin[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dmincount <- 4
dmaxcount <- 3
} else { #if (which.min(sumerrsq) == 16) 
dminl <<- dmin[3]
dminu <<- dmin[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dmincount <- 4
dmaxcount <- 4
}

dminmin <<- dmin[dmincount]
dmaxmin <<- dmax[dmaxcount]

#print(paste('Minimum bei sumerrsqcount',which.min(sumerrsq)))

}#else von if round == round

setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

}#end while loop Dinger Funk

close(pbm) 

message("  Optimization finished after iteration number: ",run)

#for calling a results-function together with ModSelection:
optd <- c(dmin, dmax)
optn <- coef(fit)

namd <- c("Minimum particle size","Maximum particle size")
namn <- c("Min. dist. modulus","Max. dist. modulus")

resultsmsg <- paste(ModSelection,"\n\nMinimum particle size:",signif(optd[1], digits=accsizes),tclvalue(dunit),"\nMaximum particle size:",signif(optd[2], digits=accsizes),tclvalue(dunit),"\nDistribution modulus nmin:",optn[1],"\nDistribution modulus nmax:",optn[2],"\n\nSum of squared deviations:",sumerrsq)























} else if (ModSelection == "Kawamura model") { #end else if ModSelection == Mod Psi


dgapl <<- boundsLMod[1]
dgapu <<- boundsUMod[1]
dmaxl <<- boundsLMod[2]
dmaxu <<- boundsUMod[2]
dgapmin <<- boundsLMod[1]
dmaxmin <<- boundsLMod[2] #davor dmaxmin

while (TRUE) {

if (run > limparcalc) {

message("  Limit of iterations reached: ",limparcalc,"\n")

close(pbm) 
  
  winmod <- tktoplevel()
  tkwm.title(winmod,"Iteration limit")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste("Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings. Setting\nor changing bounds might help. To get an impression in what dir-\nection this might go, here the values of the last iteration step:\n\nGap particle size:",signif(dgapmin, digits=accsizes),tclvalue(dunit),"Maximum particle size:",signif(dmaxmin, digits=accsizes),tclvalue(dunit),"\nDistribution modulus (Andreasen-part):",coef(fit)[1],"\nDistribution modulus (Furnas-part):",coef(fit)[2])), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}

   run <- run + 1
   step <- 1

   #print(" ")
   #print(" ")
   #print(paste("Run",run,":"))
   #print(paste("dgapl:",signif(dgapl, digits=accsizes), " und dgapu:", signif(dgapu, digits=accsizes)))
   #print(paste("dmaxl:",signif(dmaxl, digits=accsizes), " und dmaxu:", signif(dmaxu, digits=accsizes)))
   #print(" ")

   if (signif(dmaxl, digits=accsizes+1) == signif(dmaxu, digits=accsizes+1) && signif(dgapl, digits=accsizes+1) == signif(dgapu, digits=accsizes+1)) {
   
   message("  Particle size iterations finished after iteration number: ",run)

      #print("Start IF-part")
      #print(" ")

      dgap <- dgapmin
      dmax <- dmaxmin #means, that there has to be a number; if NA, something has to be constructed..!

      modelsub <- subset(preparation, preparation$Diameter <= dmax)
      modelsubAnd <- subset(modelsub, modelsub$Diameter <= dgap)
      modelsubDF <- subset(modelsub, modelsub$Diameter >= dgap)

      #print("")
            #print("Andreasen-part-fit1")
            if (boundsNl[1]==-Inf && boundsNu[1]==Inf) {
starterr1 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[1]==-Inf) { #-Inf bis Grenze boundsNu
starterr1 <- c(boundsNu[1], boundsNu[1]-0.2*boundsNu[1], boundsNu[1]-0.5*boundsNu[1], boundsNu[1]-0.9*boundsNu[1], boundsNu[1]-1.3*boundsNu[1], boundsNu[1]-2.0*boundsNu[1])
} else if (boundsNu[1]==Inf) { #von grenze boundsNl bis info
starterr1 <- c(boundsNl[1], boundsNl[1]+0.2*boundsNl[1], boundsNl[1]+0.5*boundsNl[1], boundsNl[1]+0.9*boundsNl[1], boundsNl[1]+1.3*boundsNl[1], boundsNl[1]+2.0*boundsNl[1])
} else { #both given
starterr1 <- c(boundsNl[1], boundsNl[1]+(boundsNu[1]-boundsNl[1])/6, boundsNl[1]+(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/6 ,boundsNu[1])
}
#, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

            #starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
            for (ierr in seq(from=1, to=length(starterr1), by=1)) {
               fit <- NULL
               try(fit <- nls(CurveFit ~ 100*(Diameter/dmax)^Vnand , data=modelsubAnd, start=list(Vnand=starterr1[ierr]), algorithm="port", lower=boundsNl[1], upper=boundsNu[1]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
               if(!is.null(fit)) { break }   
            }
            if(is.null(fit)) { 
            
            message("  Optimization error. Last error message: ",paste(geterrmessage()))
            
            close(pbm) 

  

  winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)






  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

  tkdestroy(winmod)

  return(var) 
            }
            nand1 <<- coef(fit)[1]
            #print(paste("n(and) 1:",nand1))

            #starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
            for (ierr in seq(from=1, to=length(starterr2), by=1)) {
               fit <- NULL
               try(fit <- nls(CurveFit ~ 100*( (dgap/dmax)^nand1 + (1 - (dgap/dmax)^nand1)*(Diameter^Vndf - dgap^Vndf)/(dmax^Vndf - dgap^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
               if(!is.null(fit)) { break }   
            }
            if(is.null(fit)) { 
            
            message("  Optimization error. Last error message: ",paste(geterrmessage()))
            
            close(pbm) 

  

  winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)






  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

  tkdestroy(winmod)

  return(var) 
            }
            ndf1 <<- coef(fit)[1]
            #print(paste("n(df) 1:",ndf1))

            #print("DF-part-fit2 incl And-part nand2")
            #starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
            for (ierr1 in seq(from=1, to=length(starterr1), by=1)) {
               for (ierr2 in seq(from=1, to=length(starterr2), by=1)) {
                  fit <- NULL
                  try(fit <- nls(CurveFit ~ 100*( (dgap/dmax)^Vnand + (1 - (dgap/dmax)^Vnand)*(Diameter^Vndf - dgap^Vndf)/(dmax^Vndf - dgap^Vndf) ) , data=modelsubDF, start=list(Vnand=starterr1[ierr1], Vndf=starterr2[ierr2]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                  if(!is.null(fit)) { break }   
               }
            }
            if(is.null(fit)) {  
            
            message("  Optimization error. Last error message: ",paste(geterrmessage()))
            
               close(pbm) 

  

  winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)






  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

  tkdestroy(winmod)

  return(var)  
            
            }
            nand2 <<- coef(fit)[1]
            ndf2 <<- coef(fit)[2]

            #print(paste("n(and) 2:",nand2))
            #print(paste("n(df) 2:",ndf2))

            nandmin <<- nand1
            ndfmin <<- ndf1

            subrun <- 0

            while (TRUE) {

               subrun <- subrun + 1
               step <- subrun
               #print("")
               #print(paste("Subrun",subrun,"with nand1",nand1,"and nand2",nand2))

               if (round(nand1, digits=accuracy+1) == round(nand2, digits=accuracy+1)) {
               
               #message("  Distribution modulus iterations finished after subrun: ",subrun)

                  nand <- nandmin
           
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}
                  
                  #find corresponding ndf
                  #starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
                  for (ierr in seq(from=1, to=length(starterr2), by=1)) {
                     fit <- NULL
                     try(fit <- nls(CurveFit ~ 100*( (dgap/dmax)^nand + (1 - (dgap/dmax)^nand)*(Diameter^Vndf - dgap^Vndf)/(dmax^Vndf - dgap^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                     if(!is.null(fit)) { break }   
                  }
                  if(is.null(fit)) { 
                  
                  message("  Optimization error. Last error message: ",paste(geterrmessage()))
                  
                  close(pbm) 

  

  winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)






  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

  tkdestroy(winmod)

  return(var) 
                  }
                  ndfmin <<- coef(fit)[1]
                  
                  ndf <- ndfmin
                  #print(paste("to nand =",nand,"ndf =",ndf,"is corresponding in subrun",subrun))

                  #create VolModel-column and calculate error:
                  errsq <- vector(length=length(preparation$Diameter))
                  sumerrsq <- 0
                  for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
                     if (dmax < preparation$Diameter[i]) {
                        VolModel[i] <<- 100 
                     } else if (dgap < preparation$Diameter[i]) {
                        VolModel[i] <<- 100*( (dgap/dmax)^nand + (1 - (dgap/dmax)^nand)*(preparation$Diameter[i]^ndf - dgap^ndf)/(dmax^ndf - dgap^ndf) )
                     } else {
                        VolModel[i] <<- 100*(preparation$Diameter[i]/dmax)^nand
                     }
                     #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
                     errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
                     sumerrsq <- sumerrsq+errsq[i]
                  }
                  
                  setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

                  break
               } else {

                  nand <- vector(length=4)

                  nand[1] <- nand1 
                  nand[2] <- nand1+(nand2-nand1)/3
                  nand[3] <- nand2-(nand2-nand1)/3
                  nand[4] <- nand2
                  #print(paste("nand:",nand))

                  sumerrsqnand <- rep(0, length(nand))

                  for(k in seq(from=1, to=length(nand), by=1)) {

                     #find corresponding ndf

if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

                     for (ierr in seq(from=1, to=length(starterr2), by=1)) {
                        fit <- NULL
                        try(fit <- nls(CurveFit ~ 100*( (dgap/dmax)^nand[k] + (1 - (dgap/dmax)^nand[k])*(Diameter^Vndf - dgap^Vndf)/(dmax^Vndf - dgap^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                        if(!is.null(fit)) { break }   
                     }
                     if(is.null(fit)) { 
                        sumerrsqnand[k] <- NA
                        nacount <<- nacount+1
                        next
                     }
                     ndfsub <- coef(fit)[1]
                     #print(paste("to nand",k," = ",nand[k],"ndf =",ndfsub,"is corresponding in subrun",subrun))

                     errsq <- vector(length=length(preparation$Diameter))
                     for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
                        if (dmax < preparation$Diameter[i]) {
                           VolModel[i] <<- 100 
                        } else if (dgap < preparation$Diameter[i]) {
                           VolModel[i] <<- 100*( (dgap/dmax)^nand[k] + (1 - (dgap/dmax)^nand[k])*(preparation$Diameter[i]^ndfsub - dgap^ndfsub)/(dmax^ndfsub - dgap^ndfsub) )
                        } else {
                           VolModel[i] <<- 100*(preparation$Diameter[i]/dmax)^nand[k]
                        }
                        #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
                        errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
                        sumerrsqnand[k] <- sumerrsqnand[k]+errsq[i]
                     }
    
                     #print(paste("Sumerrsqnand",k,"=",sumerrsqnand[k],"for dgap=",dgap[h],"and dmax=",dmax[j]))
                     
                     setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))
                     
                  } #for k nand

                      if (sum(is.na(sumerrsqnand)) == 4) {


         #dann kein verlgeichswert vorhanden -> abbruch und neue bounds oder neues modell fordern


         close(pbm)


         


         winmod <- tktoplevel()


  tkwm.title(winmod,"Optimization error")


  tkraise(winmod)





  cm <- tclVar(0)


  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 


  


  Menu <- tkmenu(winmod)           


  tkconfigure(winmod, menu = Menu) 


  


  AbMenu <- tkmenu(Menu, tearoff = FALSE)


  tkadd(Menu, "cascade", label = "About", menu = AbMenu)


  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))


  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  


  QuMenu <- tkmenu(Menu, tearoff = FALSE)


  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)


  


  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)


  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)


  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)


  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)


  


  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)





  tkfocus(winmod)


  tkwait.variable(cm)


  var <- tclvalue(cm)


 tkdestroy(winmod)


  return(var) 


      }

                  if (which.min(sumerrsqnand) == 1) {

                     nand1 <<- nand[1]

                     nand2 <<- nand[2]

                  } else if (which.min(sumerrsqnand) == 2) {

                     nand1 <<- nand[1]

                     nand2 <<- nand[3]

                  } else if (which.min(sumerrsqnand) == 3) {

                     nand1 <<- nand[2]

                     nand2 <<- nand[4]

                  } else {

                     nand1 <<- nand[3]

                     nand2 <<- nand[4]

                  }

                  #print(paste("nand",nand,"and sumerrsqnand",sumerrsqnand))
                  #print(paste("min",which.min(sumerrsqnand),"and nand-min",nand[which.min(sumerrsqnand)]))
                  

                  nandmin <<- nand[which.min(sumerrsqnand)]
                  #print(paste("nandmin =",nandmin))
    
                  #HIER WEITER?

               } # else nand-opt
               
               setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))
    
            } #while nand-opt

            #sumerrsq[sumerrsqcount] <- sumerrsq[sumerrsqcount]
            #print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dmin=",dmin[h],"and dmax=",dmax[j]))
      
      
      
      #print(paste("Total error:",sumerrsq))
    
      preparation <- cbind(preparation, VolModel)
      colnames(preparation)[23] <- "VolModel"
      #print(preparation)
      
      setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))


      break #out of while loop

   } else {

      #print("ELSE-part")
      #print(" ")

      dgap <- vector(length=4)
#dmin[1] <- signif(dminl, digits=accsizes+1) 
#dmin[2] <- signif(dminl+(dminu-dminl)/3, digits=accsizes+1) 
#dmin[3] <- signif(dminu-(dminu-dminl)/3, digits=accsizes+1) 
#dmin[4] <- signif(dminu, digits=accsizes+1) 
dgap[1] <- dgapl
dgap[2] <- 10^(log10(dgapl)+(log10(dgapu)-log10(dgapl))/3)
dgap[3] <- 10^(log10(dgapu)-(log10(dgapu)-log10(dgapl))/3)
dgap[4] <- dgapu
#print(paste("dmin:",dmin))

dmax <- vector(length=4)
#dmax[1] <- signif(dmaxl, digits=accsizes+1) 
#dmax[2] <- signif(dmaxl+(dmaxu-dmaxl)/3, digits=accsizes+1) 
#dmax[3] <- signif(dmaxu-(dmaxu-dmaxl)/3, digits=accsizes+1) 
#dmax[4] <- signif(dmaxu, digits=accsizes+1) 
dmax[1] <- dmaxl
dmax[2] <- 10^(log10(dmaxl)+(log10(dmaxu)-log10(dmaxl))/3)
dmax[3] <- 10^(log10(dmaxu)-(log10(dmaxu)-log10(dmaxl))/3)
dmax[4] <- dmaxu
#print(paste("dmax:",dmax))

      #fuer jedes dmax fehler berechnen:
      sumerrsq <- rep(0, length(dmax)*length(dgap))
      sumerrsqcount <- 0

      for (h in seq(from=1, to=length(dgap), by=1)) {
         for(j in seq(from=1, to=length(dmax), by=1)) {

            sumerrsqcount <- sumerrsqcount + 1
            step <- sumerrsqcount

            if (dgap[h] >= dmax[j] || length(intersect(which(dgap[h] <= preparation$Diameter), which(dmax[j] >= preparation$Diameter))) < 3) {#dgap zu nah an prep-Diam[1] eigentlich vorher shcon ausgeshclossen

               sumerrsq[sumerrsqcount] <- NA #geht, da NA von which.min nicht beahctet werden
               #print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dmin=",dgap[h],"and dmax=",dmax[j]))
               next
            }

            modelsub <- subset(preparation, preparation$Diameter <= dmax[j])
            modelsubAnd <- subset(modelsub, modelsub$Diameter <= dgap[h])
            modelsubDF <- subset(modelsub, modelsub$Diameter >= dgap[h])

            #print("")
            #print("Andreasen-part-fit1")
            if (boundsNl[1]==-Inf && boundsNu[1]==Inf) {
starterr1 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[1]==-Inf) { #-Inf bis Grenze boundsNu
starterr1 <- c(boundsNu[1], boundsNu[1]-0.2*boundsNu[1], boundsNu[1]-0.5*boundsNu[1], boundsNu[1]-0.9*boundsNu[1], boundsNu[1]-1.3*boundsNu[1], boundsNu[1]-2.0*boundsNu[1])
} else if (boundsNu[1]==Inf) { #von grenze boundsNl bis info
starterr1 <- c(boundsNl[1], boundsNl[1]+0.2*boundsNl[1], boundsNl[1]+0.5*boundsNl[1], boundsNl[1]+0.9*boundsNl[1], boundsNl[1]+1.3*boundsNl[1], boundsNl[1]+2.0*boundsNl[1])
} else { #both given
starterr1 <- c(boundsNl[1], boundsNl[1]+(boundsNu[1]-boundsNl[1])/6, boundsNl[1]+(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/6 ,boundsNu[1])
}
#, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

            for (ierr in seq(from=1, to=length(starterr1), by=1)) {
               fit <- NULL
               try(fit <- nls(CurveFit ~ 100*(Diameter/dmax[j])^Vnand , data=modelsubAnd, start=list(Vnand=starterr1[ierr]), algorithm="port", lower=boundsNl[1], upper=boundsNu[1]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
               if(!is.null(fit)) { break }   
            }
            if(is.null(fit)) { 
            #print("ptimization error; else-part; And-part 1")
            sumerrsq[sumerrsqcount] <- NA
            nacount <<- nacount+1
             #print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dgap=",dgap[h],"and dmax=",dmax[j]))
                next
                }
            nand1 <<- coef(fit)[1]
            #print(paste("n(and) 1:",nand1))

            #starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
            for (ierr in seq(from=1, to=length(starterr2), by=1)) {
               fit <- NULL
               try(fit <- nls(CurveFit ~ 100*( (dgap[h]/dmax[j])^nand1 + (1 - (dgap[h]/dmax[j])^nand1)*(Diameter^Vndf - dgap[h]^Vndf)/(dmax[j]^Vndf - dgap[h]^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
               if(!is.null(fit)) { break }   
            }
            if(is.null(fit)) { 
            #print("ptimization error; else-part; DF-part 1")
            sumerrsq[sumerrsqcount] <- NA
            nacount <<- nacount+1
                   #print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dgap=",dgap[h],"and dmax=",dmax[j]))
                next
                }
            ndf1 <<- coef(fit)[1]
            #print(paste("n(df) 1:",ndf1))

            #print("DF-part-fit2 incl And-part nand2")
            #starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
            for (ierr1 in seq(from=1, to=length(starterr1), by=1)) {
               for (ierr2 in seq(from=1, to=length(starterr2), by=1)) {
                  fit <- NULL
                  try(fit <- nls(CurveFit ~ 100*( (dgap[h]/dmax[j])^Vnand + (1 - (dgap[h]/dmax[j])^Vnand)*(Diameter^Vndf - dgap[h]^Vndf)/(dmax[j]^Vndf - dgap[h]^Vndf) ) , data=modelsubDF, start=list(Vnand=starterr1[ierr1], Vndf=starterr2[ierr2]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                  if(!is.null(fit)) { break }   
               }
            }
            if(is.null(fit)) {         
               #print("ptimization error; else-part; AndDF-part 2") 
            
                   sumerrsq[sumerrsqcount] <- NA
                   nacount <<- nacount+1
                   #print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dgap=",dgap[h],"and dmax=",dmax[j]))
                next
            }
            nand2 <<- coef(fit)[1]
            ndf2 <<- coef(fit)[2]

            #print(paste("n(and) 2:",nand2))
            #print(paste("n(df) 2:",ndf2))

            nandmin <<- nand1
            ndfmin <<- ndf1

            subrun <- 0

            while (TRUE) {

               subrun <- subrun + 1
               #print("")
               #print(paste("Subrun",subrun,"with nand1",nand1,"and nand2",nand2))

               if (round(nand1, digits=accuracy+1) == round(nand2, digits=accuracy+1)) {
               
               #message("  Distribution modulus iterations finished after subrun: ",subrun)

                  nand <- nandmin
                  
                  #find corresponding ndf
                  
#, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

                  for (ierr in seq(from=1, to=length(starterr2), by=1)) {
                     fit <- NULL
                     try(fit <- nls(CurveFit ~ 100*( (dgap[h]/dmax[j])^nand + (1 - (dgap[h]/dmax[j])^nand)*(Diameter^Vndf - dgap[h]^Vndf)/(dmax[j]^Vndf - dgap[h]^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                     if(!is.null(fit)) { break }   
                  }
                  if(is.null(fit)) { 
                  
                  message("  Optimization error. Last error message: ",paste(geterrmessage()))
                  
                    close(pbm) 

  

  winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)






  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

  tkdestroy(winmod)

  return(var) 
                  
                  }
                  ndfmin <<- coef(fit)[1]
                  
                  ndf <- ndfmin
                  #print(paste("to nand =",nand,"ndf =",ndf,"is corresponding in subrun",subrun))

                  #create VolModel-column and calculate error:
                  errsq <- vector(length=length(preparation$Diameter))
                  for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
                     if (dmax[j] < preparation$Diameter[i]) {
                        VolModel[i] <<- 100 
                     } else if (dgap[h] < preparation$Diameter[i]) {
                        VolModel[i] <<- 100*( (dgap[h]/dmax[j])^nand + (1 - (dgap[h]/dmax[j])^nand)*(preparation$Diameter[i]^ndf - dgap[h]^ndf)/(dmax[j]^ndf - dgap[h]^ndf) )
                     } else {
                        VolModel[i] <<- 100*(preparation$Diameter[i]/dmax[j])^nand
                     }
                     #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
                     errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
                     sumerrsq[sumerrsqcount] <- sumerrsq[sumerrsqcount]+errsq[i]
                  }
                  #print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dgap=",dgap[h],"and dmax=",dmax[j]))

                  setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))
                  
                  break
               } else {

                  nand <- vector(length=4)

                  nand[1] <- nand1 

                  nand[2] <- nand1+(nand2-nand1)/3
                  nand[3] <- nand2-(nand2-nand1)/3
                  nand[4] <- nand2
                  #print(paste("nand:",nand))

                  sumerrsqnand <- rep(0, length(nand))

                  for(k in seq(from=1, to=length(nand), by=1)) {

                     #find corresponding ndf
                     #, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

                     for (ierr in seq(from=1, to=length(starterr2), by=1)) {
                        fit <- NULL
                        try(fit <- nls(CurveFit ~ 100*( (dgap[h]/dmax[j])^nand[k] + (1 - (dgap[h]/dmax[j])^nand[k])*(Diameter^Vndf - dgap[h]^Vndf)/(dmax[j]^Vndf - dgap[h]^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                        if(!is.null(fit)) { break }   
                     }
                     if(is.null(fit)) { 
                        #print(paste("Optimization err in subrun",subrun,"and nand",k)) 
                        
                        sumerrsqnand[k] <- NA
                        nacount <<- nacount+1
                        next
                     }
                     ndfsub <- coef(fit)[1]
                     #print(paste("to nand",k," = ",nand[k],"ndf =",ndfsub,"is corresponding in subrun",subrun))

                     errsq <- vector(length=length(preparation$Diameter))
                     for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
                        if (dmax[j] < preparation$Diameter[i]) {
                           VolModel[i] <<- 100 
                        } else if (dgap[h] < preparation$Diameter[i]) {
                           VolModel[i] <<- 100*( (dgap[h]/dmax[j])^nand[k] + (1 - (dgap[h]/dmax[j])^nand[k])*(preparation$Diameter[i]^ndfsub - dgap[h]^ndfsub)/(dmax[j]^ndfsub - dgap[h]^ndfsub) )
                        } else {
                           VolModel[i] <<- 100*(preparation$Diameter[i]/dmax[j])^nand[k]
                        }
                        #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
                        errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
                        sumerrsqnand[k] <- sumerrsqnand[k]+errsq[i]
                     }
    
                     #print(paste("Sumerrsqnand",k,"=",sumerrsqnand[k],"for dgap=",dgap[h],"and dmax=",dmax[j]))
                     
                     setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))
                     
                  } #for k nand
                  
                  
                  #ist die folgende abfage notwendig??????
                  if (sum(is.na(sumerrsqnand)) == 4) {


         #dann kein verlgeichswert vorhanden -> abbruch und neue bounds oder neues modell fordern


         close(pbm)


         


         winmod <- tktoplevel()


  tkwm.title(winmod,"Optimization error")


  tkraise(winmod)





  cm <- tclVar(0)


  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 


  


  Menu <- tkmenu(winmod)           


  tkconfigure(winmod, menu = Menu) 


  


  AbMenu <- tkmenu(Menu, tearoff = FALSE)


  tkadd(Menu, "cascade", label = "About", menu = AbMenu)


  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))


  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  


  QuMenu <- tkmenu(Menu, tearoff = FALSE)


  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)


  


  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)


  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)


  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)


  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)


  


  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)





  tkfocus(winmod)


  tkwait.variable(cm)


  var <- tclvalue(cm)


 tkdestroy(winmod)


  return(var) 


      }

                  if (which.min(sumerrsqnand) == 1) {

                     nand1 <<- nand[1]

                     nand2 <<- nand[2]

                  } else if (which.min(sumerrsqnand) == 2) {

                     nand1 <<- nand[1]

                     nand2 <<- nand[3]

                  } else if (which.min(sumerrsqnand) == 3) {

                     nand1 <<- nand[2]

                     nand2 <<- nand[4]

                  } else {

                     nand1 <<- nand[3]

                     nand2 <<- nand[4]

                  }

                  #print(paste("nand",nand,"and sumerrsqnand",sumerrsqnand))
                  #print(paste("min",which.min(sumerrsqnand),"and nand-min",nand[which.min(sumerrsqnand)]))
                  

                  nandmin <<- nand[which.min(sumerrsqnand)]
                  #print(paste("nandmin =",nandmin))
    
                  #HIER WEITER?

               } # else nand-opt
               
               setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))
    
            } #while nand-opt

            #sumerrsq[sumerrsqcount] <- sumerrsq[sumerrsqcount]
            #print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dmin=",dmin[h],"and dmax=",dmax[j]))

            #setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size interval calculation:",run,"\nRefining step of the current iteration:",step))
            
         } #for j in dmax
      } #for h in dgap


      #entscheidung wleches min und was neue dmaxl und dmaxu:

      #hier weiter
      
      if (sum(is.na(sumerrsq)) == 16) {
         #dann kein verlgeichswert vorhanden -> abbruch und neue bounds oder neues modell fordern
         close(pbm)
         
         message("  Optimization errors for all particle size steps. Last error message: ",paste(geterrmessage()))
         
         winmod <- tktoplevel()
  tkwm.title(winmod,"Optimization error")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 
      }

if (which.min(sumerrsq) == 1) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dgapcount <- 1
dmaxcount <- 1
} else if (which.min(sumerrsq) == 2) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dgapcount <- 1
dmaxcount <- 2
} else if (which.min(sumerrsq) == 3) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dgapcount <- 1
dmaxcount <- 3
} else if (which.min(sumerrsq) == 4) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dgapcount <- 1
dmaxcount <- 4
} else if (which.min(sumerrsq) == 5) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dgapcount <- 2
dmaxcount <- 1
} else if (which.min(sumerrsq) == 6) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dgapcount <- 2
dmaxcount <- 2
} else if (which.min(sumerrsq) == 7) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dgapcount <- 2
dmaxcount <- 3
} else if (which.min(sumerrsq) == 8) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dgapcount <- 2
dmaxcount <- 4
} else if (which.min(sumerrsq) == 9) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dgapcount <- 3
dmaxcount <- 1
} else if (which.min(sumerrsq) == 10) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dgapcount <- 3
dmaxcount <- 2
} else if (which.min(sumerrsq) == 11) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dgapcount <- 3
dmaxcount <- 3
} else if (which.min(sumerrsq) == 12) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dgapcount <- 3
dmaxcount <- 4
} else if (which.min(sumerrsq) == 13) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dgapcount <- 4
dmaxcount <- 1
} else if (which.min(sumerrsq) == 14) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dgapcount <- 4
dmaxcount <- 2
} else if (which.min(sumerrsq) == 15) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dgapcount <- 4
dmaxcount <- 3
} else { #if (which.min(sumerrsq) == 16) 
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dgapcount <- 4
dmaxcount <- 4
}

dgapmin <<- dgap[dgapcount]
dmaxmin <<- dmax[dmaxcount]

#print(paste('Minimum bei sumerrsqcount',which.min(sumerrsq)))

   }#else von if round == round

   setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

}#end while loop mod kawa

close(pbm) 

message("  Optimization finished after iteration number: ",run)

#for calling a results-function together with ModSelection:
optd <- c(dgap, dmax)
optn <- c(nand, ndf)

namd <- c("Gap particle size","Maximum particle size")
namn <- c("Dist. mod. (Andreasen-part)","Dist. modulus (Furnas-part)")


resultsmsg <- paste(ModSelection,"\n\nGap particle size:",signif(optd[1], digits=accsizes),tclvalue(dunit),"\nMaximum particle size:",signif(optd[2], digits=accsizes),tclvalue(dunit),"\nDistribution modulus n(And):",optn[1],"\nDistribution modulus n(DF):",optn[2],"\n\nSum of squared deviations:",sumerrsq)
















} else { # end else if kawa UND else modified kawamura

dminl <<- boundsLMod[1]
dminu <<- boundsUMod[1]
dgapl <<- boundsLMod[2]
dgapu <<- boundsUMod[2]
dmaxl <<- boundsLMod[3]
dmaxu <<- boundsUMod[3]
dminmin <<- boundsLMod[1]
dgapmin <<- boundsLMod[2]
dmaxmin <<- boundsLMod[3] #davor dmaxmin

while (TRUE) {

if (run > limparcalc) {

message("  Limit of iterations reached: ",limparcalc,"\n")

close(pbm) 
  
  winmod <- tktoplevel()
  tkwm.title(winmod,"Iteration limit")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste("Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings. Setting\nor changing bounds might help. To get an impression in what dir-\nection this might go, here the values of the last iteration step:\n\nMinimum particle size:",signif(dminmin, digits=accsizes),tclvalue(dunit),"\nGap particle size:",signif(dgapmin, digits=accsizes),tclvalue(dunit),"Maximum particle size:",signif(dmaxmin, digits=accsizes),tclvalue(dunit),"\nDistribution modulus (Andreasen-part):",coef(fit)[1],"\nDistribution modulus (Furnas-part):",coef(fit)[2])), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}

run <- run + 1
step <- 1

if (signif(dmaxl, digits=accsizes+1) == signif(dmaxu, digits=accsizes+1) && signif(dminl, digits=accsizes+1) == signif(dminu, digits=accsizes+1) && signif(dgapl, digits=accsizes+1) == signif(dgapu, digits=accsizes+1)) {

message("  Particle size iterations finished after iteration number: ",run)

#print("if dmaxl==dmaxu + dminl==dminu + dgapl==dgapu")

dmin <- dminmin
dgap <- dgapmin
dmax <- dmaxmin

modelsub2 <- subset(preparation, preparation$Diameter >= dmin)
modelsub <- subset(modelsub2, modelsub2$Diameter <= dmax)
modelsubAnd <- subset(modelsub, modelsub$Diameter <= dgap)
modelsubDF <- subset(modelsub, modelsub$Diameter >= dgap)

#print("Andreasen-part-fit1")
            if (boundsNl[1]==-Inf && boundsNu[1]==Inf) {
starterr1 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[1]==-Inf) { #-Inf bis Grenze boundsNu
starterr1 <- c(boundsNu[1], boundsNu[1]-0.2*boundsNu[1], boundsNu[1]-0.5*boundsNu[1], boundsNu[1]-0.9*boundsNu[1], boundsNu[1]-1.3*boundsNu[1], boundsNu[1]-2.0*boundsNu[1])
} else if (boundsNu[1]==Inf) { #von grenze boundsNl bis info
starterr1 <- c(boundsNl[1], boundsNl[1]+0.2*boundsNl[1], boundsNl[1]+0.5*boundsNl[1], boundsNl[1]+0.9*boundsNl[1], boundsNl[1]+1.3*boundsNl[1], boundsNl[1]+2.0*boundsNl[1])
} else { #both given
starterr1 <- c(boundsNl[1], boundsNl[1]+(boundsNu[1]-boundsNl[1])/6, boundsNl[1]+(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/6 ,boundsNu[1])
}
#, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

            for (ierr in seq(from=1, to=length(starterr1), by=1)) {
               fit <- NULL
               try(fit <- nls(CurveFit ~ 100*(Diameter^Vnand - dmin^Vnand)/(dmax^Vnand - dmin^Vnand) , data=modelsubAnd, start=list(Vnand=starterr1[ierr]), algorithm="port", lower=boundsNl[1], upper=boundsNu[1]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
               if(!is.null(fit)) { break }   
            }
            if(is.null(fit)) { 
            
            message("  Optimization error. Last error message: ",paste(geterrmessage()))
            
            close(pbm) 

  

  winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)






  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

  tkdestroy(winmod)

  return(var) 
            }
            nand1 <<- coef(fit)[1]
            #print(paste("n(and) 1:",nand1))

            #starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
            for (ierr in seq(from=1, to=length(starterr2), by=1)) {
               fit <- NULL
               try(fit <- nls(CurveFit ~ 100*( (dgap^nand1 - dmin^nand1)/(dmax^nand1 - dmin^nand1) + (1 - (dgap^nand1 - dmin^nand1)/(dmax^nand1 - dmin^nand1))*(Diameter^Vndf - dgap^Vndf)/(dmax^Vndf - dgap^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
               if(!is.null(fit)) { break }   
            }
            if(is.null(fit)) { 
            
            message("  Optimization error. Last error message: ",paste(geterrmessage()))
            
            close(pbm) 

  

  winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)






  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

  tkdestroy(winmod)

  return(var) 
            }
            ndf1 <<- coef(fit)[1]
            #print(paste("n(df) 1:",ndf1))

            #print("DF-part-fit2 incl And-part nand2")
            #starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
            for (ierr1 in seq(from=1, to=length(starterr1), by=1)) {
               for (ierr2 in seq(from=1, to=length(starterr2), by=1)) {
                  fit <- NULL
                  try(fit <- nls(CurveFit ~ 100*( (dgap^Vnand - dmin^Vnand)/(dmax^Vnand - dmin^Vnand) + (1 - (dgap^Vnand - dmin^Vnand)/(dmax^Vnand - dmin^Vnand))*(Diameter^Vndf - dgap^Vndf)/(dmax^Vndf - dgap^Vndf) ) , data=modelsubDF, start=list(Vnand=starterr1[ierr1], Vndf=starterr2[ierr2]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                  if(!is.null(fit)) { break }   
               }
            }
            if(is.null(fit)) {        
            
            message("  Optimization error. Last error message: ",paste(geterrmessage()))
            
               close(pbm) 

  

  winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)






  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

  tkdestroy(winmod)

  return(var)  
            
            }
            nand2 <<- coef(fit)[1]
            ndf2 <<- coef(fit)[2]

            
            nandmin <<- nand1
            ndfmin <<- ndf1

            subrun <- 0

            while (TRUE) {

               subrun <- subrun + 1
               step <- subrun
            
            if (round(nand1, digits=accuracy+1) == round(nand2, digits=accuracy+1)) {
            
            #message("  Distribution modulus iterations finished after subrun: ",subrun)

                  nand <- nandmin
                  
                  #find corresponding ndf

if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

                  for (ierr in seq(from=1, to=length(starterr2), by=1)) {
                     fit <- NULL
                     try(fit <- nls(CurveFit ~ 100*( (dgap^nand - dmin^nand)/(dmax^nand - dmin^nand) + (1 - (dgap^nand - dmin^nand)/(dmax^nand - dmin^nand))*(Diameter^Vndf - dgap^Vndf)/(dmax^Vndf - dgap^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                     if(!is.null(fit)) { break }   
                  }
                  if(is.null(fit)) { 
                  
                  message("  Optimization error. Last error message: ",paste(geterrmessage()))
                  
                  close(pbm) 

  

  winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)






  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

  tkdestroy(winmod)

  return(var) 
                  }
                  ndfmin <<- coef(fit)[1]
                  
                  ndf <- ndfmin
                  
                                   
            #create VolModel-column and calculate error:
                  errsq <- vector(length=length(preparation$Diameter))
                  sumerrsq <- 0
                  for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
                     if (dmax < preparation$Diameter[i]) {
                        VolModel[i] <<- 100 
                     } else if (dgap < preparation$Diameter[i]) {
                        VolModel[i] <<- 100*( (dgap^nand - dmin^nand)/(dmax^nand - dmin^nand) + (1 - (dgap^nand - dmin^nand)/(dmax^nand - dmin^nand))*(preparation$Diameter[i]^ndf - dgap^ndf)/(dmax^ndf - dgap^ndf) )
                     } else if (dmin < preparation$Diameter[i]) {
                        VolModel[i] <<- 100*(preparation$Diameter[i]^nand - dmin^nand)/(dmax^nand - dmin^nand) 
                     } else {
                        VolModel[i] <<- 0
                     }
                     #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
                     errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
                     sumerrsq <- sumerrsq+errsq[i]
                  }
                  
                  setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))

                  break
                  
            } else {#end if nand1==nand2
            
            nand <- vector(length=4)

                  nand[1] <- nand1 
                  nand[2] <- nand1+(nand2-nand1)/3
                  nand[3] <- nand2-(nand2-nand1)/3
                  nand[4] <- nand2
                  #print(paste("nand:",nand))

                  sumerrsqnand <- rep(0, length(nand))

                  for(k in seq(from=1, to=length(nand), by=1)) {
            
            #find corresponding ndf
  
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

                     for (ierr in seq(from=1, to=length(starterr2), by=1)) {
                        fit <- NULL
                        
                        try(fit <- nls(CurveFit ~ 100*( (dgap^nand[k] - dmin^nand[k])/(dmax^nand[k] - dmin^nand[k]) + (1 - (dgap^nand[k] - dmin^nand[k])/(dmax^nand[k] - dmin^nand[k]))*(Diameter^Vndf - dgap^Vndf)/(dmax^Vndf - dgap^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                        if(!is.null(fit)) { break }   
                     }
                     if(is.null(fit)) { 
                        sumerrsqnand[k] <- NA
                        nacount <<- nacount+1
                        next
                     }
                     ndfsub <- coef(fit)[1]                     
                     
                     
                     errsq <- vector(length=length(preparation$Diameter))
                     for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
                        if (dmax < preparation$Diameter[i]) {
                           VolModel[i] <<- 100 
                        } else if (dgap < preparation$Diameter[i]) {
                           VolModel[i] <<- 100*( (dgap^nand[k] - dmin^nand[k])/(dmax^nand[k] - dmin^nand[k]) + (1 - (dgap^nand[k] - dmin^nand[k])/(dmax^nand[k] - dmin^nand[k]))*(preparation$Diameter[i]^ndfsub - dgap^ndfsub)/(dmax^ndfsub - dgap^ndfsub) )
                        } else if (dmin < preparation$Diameter[i]) {
                           VolModel[i] <<- 100*(preparation$Diameter[i]^nand[k] - dmin^nand[k])/(dmax^nand[k] - dmin^nand[k]) 
                        } else {
        VolModel[i] <<- 0
      }
                        #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
                        errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
                        sumerrsqnand[k] <- sumerrsqnand[k]+errsq[i]
                     }
                     
                     setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))
                
                } #end for k in nand
                
                
                if (sum(is.na(sumerrsqnand)) == 4) {


         #dann kein verlgeichswert vorhanden -> abbruch und neue bounds oder neues modell fordern


         close(pbm)


         


         winmod <- tktoplevel()


  tkwm.title(winmod,"Optimization error")


  tkraise(winmod)





  cm <- tclVar(0)


  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 


  


  Menu <- tkmenu(winmod)           


  tkconfigure(winmod, menu = Menu) 


  


  AbMenu <- tkmenu(Menu, tearoff = FALSE)


  tkadd(Menu, "cascade", label = "About", menu = AbMenu)


  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))


  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  


  QuMenu <- tkmenu(Menu, tearoff = FALSE)


  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)


  


  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)


  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)


  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)


  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)


  


  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)





  tkfocus(winmod)


  tkwait.variable(cm)


  var <- tclvalue(cm)


 tkdestroy(winmod)


  return(var) 


      }
                
       
       
         if (which.min(sumerrsqnand) == 1) {

                     nand1 <<- nand[1]

                     nand2 <<- nand[2]

                  } else if (which.min(sumerrsqnand) == 2) {

                     nand1 <<- nand[1]

                     nand2 <<- nand[3]

                  } else if (which.min(sumerrsqnand) == 3) {

                     nand1 <<- nand[2]

                     nand2 <<- nand[4]

                  } else {

                     nand1 <<- nand[3]

                     nand2 <<- nand[4]

                  }

                  #print(paste("nand",nand,"and sumerrsqnand",sumerrsqnand))
                  #print(paste("min",which.min(sumerrsqnand),"and nand-min",nand[which.min(sumerrsqnand)]))
                  

                  nandmin <<- nand[which.min(sumerrsqnand)]
       
                
            
            } #end else (nand1 != nand2)
            
            
            setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))
            
            
            
            
            } #end while nand/subrun
            
            preparation <- cbind(preparation, VolModel)
      colnames(preparation)[23] <- "VolModel"
      #print(preparation)
      
      setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))


      break #out of while loop
            

} else { #end if (dmaxl==dmaxu + dminl==dminu + dgapl==dgapu)

#print("else von if dmaxl==dmaxu + dgapl==dgapu + dminl==dminu")

dmin <- vector(length=4)
dmin[1] <- dminl
dmin[2] <- 10^(log10(dminl)+(log10(dminu)-log10(dminl))/3)
dmin[3] <- 10^(log10(dminu)-(log10(dminu)-log10(dminl))/3)
dmin[4] <- dminu

dgap <- vector(length=4)
dgap[1] <- dgapl
dgap[2] <- 10^(log10(dgapl)+(log10(dgapu)-log10(dgapl))/3)
dgap[3] <- 10^(log10(dgapu)-(log10(dgapu)-log10(dgapl))/3)
dgap[4] <- dgapu

dmax <- vector(length=4)
dmax[1] <- dmaxl
dmax[2] <- 10^(log10(dmaxl)+(log10(dmaxu)-log10(dmaxl))/3)
dmax[3] <- 10^(log10(dmaxu)-(log10(dmaxu)-log10(dmaxl))/3)
dmax[4] <- dmaxu

#fuer jedes dmax fehler berechnen:
      sumerrsq <- rep(0, length(dmax)*length(dgap)*length(dmin))
      sumerrsqcount <- 0

      for (m in seq(from=1, to=length(dmin), by=1)) {
      for (h in seq(from=1, to=length(dgap), by=1)) {
         for(j in seq(from=1, to=length(dmax), by=1)) {

            sumerrsqcount <- sumerrsqcount + 1
            step <- sumerrsqcount

if (dgap[h] >= dmax[j] || length(intersect(which(dgap[h] <= preparation$Diameter), which(dmax[j] >= preparation$Diameter))) < 3 || dmin[m] >= dgap[h] || length(intersect(which(dmin[m] <= preparation$Diameter), which(dgap[h] >= preparation$Diameter))) < 3) {#dgap zu nah an prep-Diam[1] eigentlich vorher shcon ausgeshclossen

               sumerrsq[sumerrsqcount] <- NA #geht, da NA von which.min nicht beahctet werden
               #print(paste("Sumerrsq (if):",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dmin=",dmin[m],", dgap=",dgap[h],"and dmax=",dmax[j]))
               next
            }
            
            modelsub2 <- subset(preparation, preparation$Diameter >= dmin[m])
            modelsub <- subset(modelsub2, modelsub2$Diameter <= dmax[j])
            modelsubAnd <- subset(modelsub, modelsub$Diameter <= dgap[h])
            modelsubDF <- subset(modelsub, modelsub$Diameter >= dgap[h])
            
            #print("Andreasen-part-fit1")
            if (boundsNl[1]==-Inf && boundsNu[1]==Inf) {
starterr1 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[1]==-Inf) { #-Inf bis Grenze boundsNu
starterr1 <- c(boundsNu[1], boundsNu[1]-0.2*boundsNu[1], boundsNu[1]-0.5*boundsNu[1], boundsNu[1]-0.9*boundsNu[1], boundsNu[1]-1.3*boundsNu[1], boundsNu[1]-2.0*boundsNu[1])
} else if (boundsNu[1]==Inf) { #von grenze boundsNl bis info
starterr1 <- c(boundsNl[1], boundsNl[1]+0.2*boundsNl[1], boundsNl[1]+0.5*boundsNl[1], boundsNl[1]+0.9*boundsNl[1], boundsNl[1]+1.3*boundsNl[1], boundsNl[1]+2.0*boundsNl[1])
} else { #both given
starterr1 <- c(boundsNl[1], boundsNl[1]+(boundsNu[1]-boundsNl[1])/6, boundsNl[1]+(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/3, boundsNu[1]-(boundsNu[1]-boundsNl[1])/6 ,boundsNu[1])
}
#, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

            for (ierr in seq(from=1, to=length(starterr1), by=1)) {
               fit <- NULL
               try(fit <- nls(CurveFit ~ 100*(Diameter^Vnand - dmin[m]^Vnand)/(dmax[j]^Vnand - dmin[m]^Vnand)  , data=modelsubAnd, start=list(Vnand=starterr1[ierr]), algorithm="port", lower=boundsNl[1], upper=boundsNu[1]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
               if(!is.null(fit)) { break }   
            }
            if(is.null(fit)) { 
            #print("Optimization error 1-1")
            sumerrsq[sumerrsqcount] <- NA
            nacount <<- nacount+1
             #print(paste("Sumerrsq (Opt. err. 1-1)",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dgap=",dgap[h],"and dmax=",dmax[j]))
                next
                }
            nand1 <<- coef(fit)[1]
            
            #starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
            for (ierr in seq(from=1, to=length(starterr2), by=1)) {
               fit <- NULL
               try(fit <- nls(CurveFit ~ 100*( (dgap[h]^nand1 - dmin[m]^nand1)/(dmax[j]^nand1 - dmin[m]^nand1) + (1 - (dgap[h]^nand1 - dmin[m]^nand1)/(dmax[j]^nand1 - dmin[m]^nand1)) * (Diameter^Vndf - dgap[h]^Vndf)/(dmax[j]^Vndf - dgap[h]^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
               if(!is.null(fit)) { break }   
            }
            if(is.null(fit)) { 
            #print("ptimization error; else-part; DF-part 1")
            sumerrsq[sumerrsqcount] <- NA
            nacount <<- nacount+1
            #print(paste("Sumerrsq (Opt. err. 1-2)",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dgap=",dgap[h],"and dmax=",dmax[j]))
             
                   #print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dgap=",dgap[h],"and dmax=",dmax[j]))
                next
                }
            ndf1 <<- coef(fit)[1]
            
            
            #print("DF-part-fit2 incl And-part nand2")
            #starterr <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3, 0.1, 2.0)
            for (ierr1 in seq(from=1, to=length(starterr1), by=1)) {
               for (ierr2 in seq(from=1, to=length(starterr2), by=1)) {
                  fit <- NULL
                  try(fit <- nls(CurveFit ~ 100*( (dgap[h]^Vnand - dmin[m]^Vnand)/(dmax[j]^Vnand - dmin[m]^Vnand) + (1 - (dgap[h]^Vnand - dmin[m]^Vnand)/(dmax[j]^Vnand - dmin[m]^Vnand)) * (Diameter^Vndf - dgap[h]^Vndf)/(dmax[j]^Vndf - dgap[h]^Vndf) ) , data=modelsubDF, start=list(Vnand=starterr1[ierr1], Vndf=starterr2[ierr2]), algorithm="port", lower=boundsNl, upper=boundsNu),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                  if(!is.null(fit)) { break }   
               }
            }
            if(is.null(fit)) {         
               #print("ptimization error; else-part; AndDF-part 2") 
            
                   sumerrsq[sumerrsqcount] <- NA
                   nacount <<- nacount+1
                   #print(paste("Sumerrsq (Opt. err. 2)",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dmin=",dmin[m],", dgap=",dgap[h],"and dmax=",dmax[j]))
                   #print(paste("Error message:",geterrmessage()))
                   #print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dgap=",dgap[h],"and dmax=",dmax[j]))
                next
            }
            nand2 <<- coef(fit)[1]
            ndf2 <<- coef(fit)[2]

            
            nandmin <<- nand1
            ndfmin <<- ndf1

            subrun <- 0

            while (TRUE) {

               subrun <- subrun + 1
            
            
            if (round(nand1, digits=accuracy+1) == round(nand2, digits=accuracy+1)) {
            
            #message("  Distribution modulus iterations finished after subrun: ",subrun)

            #print("if nand1==nand2")
                  nand <- nandmin
                  
                  #find corresponding ndf
                  #, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

                  for (ierr in seq(from=1, to=length(starterr2), by=1)) {
                     fit <- NULL
                     try(fit <- nls(CurveFit ~ 100*( (dgap[h]^nand - dmin[m]^nand)/(dmax[j]^nand - dmin[m]^nand) + (1 - (dgap[h]^nand - dmin[m]^nand)/(dmax[j]^nand - dmin[m]^nand)) * (Diameter^Vndf - dgap[h]^Vndf)/(dmax[j]^Vndf - dgap[h]^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                     if(!is.null(fit)) { break }   
                  }
                  if(is.null(fit)) { 
                  
                  message("  Optimization error. Last error message: ",paste(geterrmessage()))
                  
                    close(pbm) 

  #print("opt error in if nand1==nand2")

  winmod <- tktoplevel()

  tkwm.title(winmod,"Optimization error")

  tkraise(winmod)



  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 

  

  Menu <- tkmenu(winmod)           

  tkconfigure(winmod, menu = Menu) 

  

  AbMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))

  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

  QuMenu <- tkmenu(Menu, tearoff = FALSE)

  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)

  

  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)

  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)

  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)

  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)

  

  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)






  tkfocus(winmod)

  tkwait.variable(cm)

  var <- tclvalue(cm)

  tkdestroy(winmod)

  return(var) 
                  
                  }
                  ndfmin <<- coef(fit)[1]
                  
                  ndf <- ndfmin
                             
                               
                  #create VolModel-column and calculate error:
                  errsq <- vector(length=length(preparation$Diameter))
                  for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
                     if (dmax[j] < preparation$Diameter[i]) {
                        VolModel[i] <<- 100 
                     } else if (dgap[h] < preparation$Diameter[i]) {
                        VolModel[i] <<- 100*( (dgap[h]^nand - dmin[m]^nand)/(dmax[j]^nand - dmin[m]^nand) + (1 - (dgap[h]^nand - dmin[m]^nand)/(dmax[j]^nand - dmin[m]^nand)) * (preparation$Diameter[i]^ndf - dgap[h]^ndf)/(dmax[j]^ndf - dgap[h]^ndf) )
                     } else if (dmin[m] < preparation$Diameter[i]) {
                        VolModel[i] <<- 100*(preparation$Diameter[i]^nand - dmin[m]^nand)/(dmax[j]^nand - dmin[m]^nand) 
                     } else {
        VolModel[i] <<- 0
      }
                     #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
                     errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
                     sumerrsq[sumerrsqcount] <- sumerrsq[sumerrsqcount]+errsq[i]
                  }
                  #print(paste("Sumerrsq",sumerrsqcount,"=",sumerrsq[sumerrsqcount],"for dgap=",dgap[h],"and dmax=",dmax[j]))

                  setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))
                  
                  break
                  
                  
             } else { #end if nand1==nand2     
            
            #print("else von if nand1==nand2")
            
            nand <- vector(length=4)

                  nand[1] <- nand1 
                  nand[2] <- nand1+(nand2-nand1)/3
                  nand[3] <- nand2-(nand2-nand1)/3
                  nand[4] <- nand2
                  #print(paste("nand:",nand))

                  sumerrsqnand <- rep(0, length(nand))

                  for(k in seq(from=1, to=length(nand), by=1)) {
                  
                  #find corresponding ndf
                     #, algorithm="port", lower=boundsNl, upper=boundsNu
if (boundsNl[2]==-Inf && boundsNu[2]==Inf) {
starterr2 <- c(0.53, 0.37, 0.7, 0.22, 0.85, 1.3)
} else if (boundsNl[2]==-Inf) { #-Inf bis Grenze boundsNu
starterr2 <- c(boundsNu[2], boundsNu[2]-0.2*boundsNu[2], boundsNu[2]-0.5*boundsNu[2], boundsNu[2]-0.9*boundsNu[2], boundsNu[2]-1.3*boundsNu[2], boundsNu[2]-2.0*boundsNu[2])
} else if (boundsNu[2]==Inf) { #von grenze boundsNl bis info
starterr2 <- c(boundsNl[2], boundsNl[2]+0.2*boundsNl[2], boundsNl[2]+0.5*boundsNl[2], boundsNl[2]+0.9*boundsNl[2], boundsNl[2]+1.3*boundsNl[2], boundsNl[2]+2.0*boundsNl[2])
} else { #both given
starterr2 <- c(boundsNl[2], boundsNl[2]+(boundsNu[2]-boundsNl[2])/6, boundsNl[2]+(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/3, boundsNu[2]-(boundsNu[2]-boundsNl[2])/6 ,boundsNu[2])
}

                     for (ierr in seq(from=1, to=length(starterr2), by=1)) {
                        fit <- NULL
                        try(fit <- nls(CurveFit ~ 100*( (dgap[h]^nand[k] - dmin[m]^nand[k])/(dmax[j]^nand[k] - dmin[m]^nand[k]) + (1 - (dgap[h]^nand[k] - dmin[m]^nand[k])/(dmax[j]^nand[k] - dmin[m]^nand[k])) * (Diameter^Vndf - dgap[h]^Vndf)/(dmax[j]^Vndf - dgap[h]^Vndf) ) , data=modelsubDF, start=list(Vndf=starterr2[ierr]), algorithm="port", lower=boundsNl[2], upper=boundsNu[2]),silent=TRUE)#in nls:   , control=list(warnOnly=TRUE)
                        if(!is.null(fit)) { break }   
                     }
                     if(is.null(fit)) { 
                        #print(paste("Optimization err in subrun",subrun,"and nand",k)) 
                        
                        sumerrsqnand[k] <- NA
                        nacount <<- nacount+1
                        next
                     }
                     ndfsub <- coef(fit)[1]
                    
            
            errsq <- vector(length=length(preparation$Diameter))
                     for (i in seq(from=1,to=length(preparation$Diameter),by=1)) {
                        if (dmax[j] < preparation$Diameter[i]) {
                           VolModel[i] <<- 100 
                        } else if (dgap[h] < preparation$Diameter[i]) {
                          VolModel[i] <<- 100*( (dgap[h]^nand[k] - dmin[m]^nand[k])/(dmax[j]^nand[k] - dmin[m]^nand[k]) + (1 - (dgap[h]^nand[k] - dmin[m]^nand[k])/(dmax[j]^nand[k] - dmin[m]^nand[k])) * (preparation$Diameter[i]^ndfsub - dgap[h]^ndfsub)/(dmax[j]^ndfsub - dgap[h]^ndfsub) )
                        } else if (dmin[m] < preparation$Diameter[i]) {
                        
                           VolModel[i] <<- 100*(preparation$Diameter[i]^nand[k] - dmin[m]^nand[k])/(dmax[j]^nand[k] - dmin[m]^nand[k]) 
                        } else {
        VolModel[i] <<- 0
      }
                        #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
                        errsq[i] <- ((VolModel[i]-CurveFit[i])*(VolModel[i]-CurveFit[i]))
                        sumerrsqnand[k] <- sumerrsqnand[k]+errsq[i]
                     } 
    
                     #print(paste("Sumerrsqnand",k,"=",sumerrsqnand[k],"for dgap=",dgap[h],"and dmax=",dmax[j]))
                     
                     setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))
            
            } #end for k in nand
            
            
            if (sum(is.na(sumerrsqnand)) == 4) {


         #dann kein verlgeichswert vorhanden -> abbruch und neue bounds oder neues modell fordern


         close(pbm)


         


         winmod <- tktoplevel()


  tkwm.title(winmod,"Optimization error")


  tkraise(winmod)





  cm <- tclVar(0)


  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 


  


  Menu <- tkmenu(winmod)           


  tkconfigure(winmod, menu = Menu) 


  


  AbMenu <- tkmenu(Menu, tearoff = FALSE)


  tkadd(Menu, "cascade", label = "About", menu = AbMenu)


  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))


  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  


  QuMenu <- tkmenu(Menu, tearoff = FALSE)


  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)


  


  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)


  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)


  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)


  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)


  


  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)





  tkfocus(winmod)


  tkwait.variable(cm)


  var <- tclvalue(cm)


 tkdestroy(winmod)


  return(var) 


      }

                  if (which.min(sumerrsqnand) == 1) {

                     nand1 <<- nand[1]

                     nand2 <<- nand[2]

                  } else if (which.min(sumerrsqnand) == 2) {

                     nand1 <<- nand[1]

                     nand2 <<- nand[3]

                  } else if (which.min(sumerrsqnand) == 3) {

                     nand1 <<- nand[2]

                     nand2 <<- nand[4]

                  } else {

                     nand1 <<- nand[3]

                     nand2 <<- nand[4]

                  }

                  #print(paste("nand",nand,"and sumerrsqnand",sumerrsqnand))
                  #print(paste("min",which.min(sumerrsqnand),"and nand-min",nand[which.min(sumerrsqnand)]))
                  

                  nandmin <<- nand[which.min(sumerrsqnand)]
            
            } #end else von if nand1 == nand2
            
            
            
            
            setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))
            
            } #end while nand / subrun
            
            
            
            } #end for j (dmax)
            } #end for h (dgap)
            } #end for m (dmin)

                  
      if (sum(is.na(sumerrsq)) == 64) {
         #dann kein verlgeichswert vorhanden -> abbruch und neue bounds oder neues modell fordern
         close(pbm)
         
         message("  Optimization errors for all particle size steps. Last error message: ",paste(geterrmessage()))
         
         winmod <- tktoplevel()
  tkwm.title(winmod,"Optimization error")
  tkraise(winmod)

  cm <- tclVar(0)
  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Calculate model params-function)", command =function() manual(man=paste("Help","13optparams","13.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=errormsgopt), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 
      }
            
            
#dmin[m]=1
if (which.min(sumerrsq) == 1) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 1
dmaxcount <- 1
} else if (which.min(sumerrsq) == 2) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 1
dmaxcount <- 2
} else if (which.min(sumerrsq) == 3) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 1
dmaxcount <- 3
} else if (which.min(sumerrsq) == 4) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 1
dmaxcount <- 4
} else if (which.min(sumerrsq) == 5) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 2
dmaxcount <- 1
} else if (which.min(sumerrsq) == 6) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 2
dmaxcount <- 2
} else if (which.min(sumerrsq) == 7) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 2
dmaxcount <- 3
} else if (which.min(sumerrsq) == 8) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 2
dmaxcount <- 4
} else if (which.min(sumerrsq) == 9) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 3
dmaxcount <- 1
} else if (which.min(sumerrsq) == 10) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 3
dmaxcount <- 2
} else if (which.min(sumerrsq) == 11) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 3
dmaxcount <- 3
} else if (which.min(sumerrsq) == 12) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 3
dmaxcount <- 4
} else if (which.min(sumerrsq) == 13) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 4
dmaxcount <- 1
} else if (which.min(sumerrsq) == 14) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 4
dmaxcount <- 2
} else if (which.min(sumerrsq) == 15) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 4
dmaxcount <- 3
} else if (which.min(sumerrsq) == 16) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[2]
dmincount <- 1
dgapcount <- 4
dmaxcount <- 4
} else if (which.min(sumerrsq) == 17) { #dmin[m] == 2
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 1
dmaxcount <- 1
} else if (which.min(sumerrsq) == 18) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 1
dmaxcount <- 2
} else if (which.min(sumerrsq) == 19) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 1
dmaxcount <- 3
} else if (which.min(sumerrsq) == 20) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 1
dmaxcount <- 4
} else if (which.min(sumerrsq) == 21) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 2
dmaxcount <- 1
} else if (which.min(sumerrsq) == 22) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 2
dmaxcount <- 2
} else if (which.min(sumerrsq) == 23) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 2
dmaxcount <- 3
} else if (which.min(sumerrsq) == 24) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 2
dmaxcount <- 4
} else if (which.min(sumerrsq) == 25) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 3
dmaxcount <- 1
} else if (which.min(sumerrsq) == 26) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 3
dmaxcount <- 2
} else if (which.min(sumerrsq) == 27) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 3
dmaxcount <- 3
} else if (which.min(sumerrsq) == 28) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 3
dmaxcount <- 4
} else if (which.min(sumerrsq) == 29) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 4
dmaxcount <- 1
} else if (which.min(sumerrsq) == 30) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 4
dmaxcount <- 2
} else if (which.min(sumerrsq) == 31) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 4
dmaxcount <- 3
} else if (which.min(sumerrsq) == 32) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[1]
dminu <<- dmin[3]
dmincount <- 2
dgapcount <- 4
dmaxcount <- 4
} else if (which.min(sumerrsq) == 33) { #dmin[m] == 3
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 1
dmaxcount <- 1
} else if (which.min(sumerrsq) == 34) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 1
dmaxcount <- 2
} else if (which.min(sumerrsq) == 35) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 1
dmaxcount <- 3
} else if (which.min(sumerrsq) == 36) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 1
dmaxcount <- 4
} else if (which.min(sumerrsq) == 37) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 2
dmaxcount <- 1
} else if (which.min(sumerrsq) == 38) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 2
dmaxcount <- 2
} else if (which.min(sumerrsq) == 39) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 2
dmaxcount <- 3
} else if (which.min(sumerrsq) == 40) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 2
dmaxcount <- 4
} else if (which.min(sumerrsq) == 41) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 3
dmaxcount <- 1
} else if (which.min(sumerrsq) == 42) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 3
dmaxcount <- 2
} else if (which.min(sumerrsq) == 43) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 3
dmaxcount <- 3
} else if (which.min(sumerrsq) == 44) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 3
dmaxcount <- 4
} else if (which.min(sumerrsq) == 45) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 4
dmaxcount <- 1
} else if (which.min(sumerrsq) == 46) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 4
dmaxcount <- 2
} else if (which.min(sumerrsq) == 47) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 4
dmaxcount <- 3
} else if (which.min(sumerrsq) == 48) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[2]
dminu <<- dmin[4]
dmincount <- 3
dgapcount <- 4
dmaxcount <- 4
} else if (which.min(sumerrsq) == 49) { #dmin[m] == 4
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 1
dmaxcount <- 1
} else if (which.min(sumerrsq) == 50) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 1
dmaxcount <- 2
} else if (which.min(sumerrsq) == 51) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 1
dmaxcount <- 3
} else if (which.min(sumerrsq) == 52) {
dgapl <<- dgap[1]
dgapu <<- dgap[2]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 1
dmaxcount <- 4
} else if (which.min(sumerrsq) == 53) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 2
dmaxcount <- 1
} else if (which.min(sumerrsq) == 54) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 2
dmaxcount <- 2
} else if (which.min(sumerrsq) == 55) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 2
dmaxcount <- 3
} else if (which.min(sumerrsq) == 56) {
dgapl <<- dgap[1]
dgapu <<- dgap[3]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 2
dmaxcount <- 4
} else if (which.min(sumerrsq) == 57) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 3
dmaxcount <- 1
} else if (which.min(sumerrsq) == 58) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 3
dmaxcount <- 2
} else if (which.min(sumerrsq) == 59) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 3
dmaxcount <- 3
} else if (which.min(sumerrsq) == 60) {
dgapl <<- dgap[2]
dgapu <<- dgap[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 3
dmaxcount <- 4
} else if (which.min(sumerrsq) == 61) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[2]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 4
dmaxcount <- 1
} else if (which.min(sumerrsq) == 62) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[1]
dmaxu <<- dmax[3]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 4
dmaxcount <- 2
} else if (which.min(sumerrsq) == 63) {
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[2]
dmaxu <<- dmax[4]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 4
dmaxcount <- 3
} else { #if (which.min(sumerrsq) == 64) 
dgapl <<- dgap[3]
dgapu <<- dgap[4]
dmaxl <<- dmax[3]
dmaxu <<- dmax[4]
dminl <<- dmin[3]
dminu <<- dmin[4]
dmincount <- 4
dgapcount <- 4
dmaxcount <- 4
}
          
dminmin <<- dmin[dmincount]
dgapmin <<- dgap[dgapcount]
dmaxmin <<- dmax[dmaxcount]

} #end else von if (dmaxl==dmaxu + dminl==dminu + dgapl==dgapu)

setTkProgressBar(pbm, 0, label=paste("Iteration step of particle size calculation:",run,"\nRefining step of the current iteration:",step))


} #end while mod kawa

close(pbm) 

message("  Optimization finished after iteration number: ",run)

optd <- c(dmin, dgap, dmax)
optn <- c(nand, ndf)

namd <- c("Minimum particle size","Gap particle size","Maximum particle size")
namn <- c("Dist. mod. (Dinger/Funk-part)","Dist. modulus (Furnas-part)")


resultsmsg <- paste(ModSelection,"\n\nMinimum particle size:",signif(optd[1], digits=accsizes),tclvalue(dunit),"\nGap particle size:",signif(optd[2], digits=accsizes),tclvalue(dunit),"\nMaximum particle size:",signif(optd[3], digits=accsizes),tclvalue(dunit),"\nDistribution modulus n(And):",optn[1],"\nDistribution modulus n(DF):",optn[2],"\n\nSum of squared deviations:",sumerrsq)

} #end else (modified kawamura)

















if (nacount > 0) {
        #variable muss erst noch global definiert werden; hierin sollen curve-fit-error-NAs aufszummiert werden, damit gewarnt werdne kann, das snicht alles glatt lief
        
        tkmessageBox(title = "Fitting errors occurred", message="During the calculation, fitting errors occurred. Usually they occur if the model itself does not fit the data well or if the bounds were not well specified. However, the found final values might be correct because typically the fitting errors occur if the data and model with bounds do not fit well together.\n\nTo check the results, you could run the calculation again with bounds near around the calculated results which will be shown after this message.", icon = "warning", type = "ok")
        
        
        message("  Fitting errors occurred. Last error message was:\n  ",paste(geterrmessage()))
      
      } else { message("") }

      
#----------------------
#
#hier CALL TO results-function mit optd und optn und ModSelection
#
#----------------------




resetd <- optd
resetn <- optn


valparam <- paramsft(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol,ModSelection,optd,optn,resetd,resetn,namd,namn,d10,d25,d50,d75,d90,Den,Sssa,price,sumerrsq)
message("END model parameter finetuning function, RETURN value: ",valparam,"\n")

return(valparam)  

  #return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 for back to open recipe or database
  #return 3 for back to batch
  #return 4 for back to model selection

#tkmessageBox(title = "Model fit results", message=resultsmsg, icon = "info", type = "ok")
#return(4)

} 
#end calcparams()







