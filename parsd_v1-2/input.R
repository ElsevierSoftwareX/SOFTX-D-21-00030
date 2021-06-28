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


openmat <- function(fun, rec) {
  #fun = 2 design a batch
  #fun = 3 verify a batch
  #fun = 4 model parameters
 
  #rec = 2 open database
  #rec = 3 open recipe
 
  #return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 (or else) for back to open recipe or database
 
 message("BEGIN Select materials function\n")
 
  if (rec == 2) {
    dbfile <- tk_choose.files(caption="Select database with raw materials", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))   
    if (length(dbfile) == 0) { return(2) }
   
    dbinfo <- read.csv2(file=dbfile,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

    #read mass% values separately
    dbdata <- read.csv2(file=dbfile,skip=19,header=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
    colnames(dbdata) <- colnames(dbinfo)
 
    
    MatList <- colnames(dbinfo)[2:length(colnames(dbinfo))]

    while (TRUE) {
      MatSelection <- tk_select.list(choices=MatList, preselect=NULL, multiple=TRUE, title="Select up to 20 raw materials\n to use for the optimization:")
      if (length(MatSelection) == 0) { return(2) #going back to open database or recipe
      } else if (length(MatSelection) > 20) {
        tkmessageBox(title = "Too many materials chosen", message="Maximum number of materials to select is 20.", icon = "info", type = "ok")
      } else { break }
    } #while TRUE mat selection
    
    message("  Number of selected materials: ",length(MatSelection),"\n")

      #Get prices
      prices <- vector(length=length(MatSelection))
      for (m in seq(from=1, to=length(MatSelection), by=1)) {
        prices[m] <- as.numeric(gsub(",",".",dbinfo[5,MatSelection[m]]))
      }
      
      #Get densities of selected materials
      densities <- vector(length=length(MatSelection))
      for (m in seq(from=1, to=length(MatSelection), by=1)) {
        densities[m] <- as.numeric(gsub(",",".",dbinfo[6,MatSelection[m]]))
      }
      
      #Get SSA
      ssa <- vector(length=length(MatSelection))
      for (m in seq(from=1, to=length(MatSelection), by=1)) {
        ssa[m] <- as.numeric(gsub(",",".",dbinfo[9,MatSelection[m]]))
      }
    
      #starting preparation of calculation data frame with diameters 
      preparation <- data.frame(dbdata$Identifier)
      colnames(preparation) <- c("Diameter")

      #calculate Q-throughput from q-retention for selected raw materials and add columns to preparation data frame
      for (j in seq(from=1,to=length(MatSelection),by=1)) {
        MatQ <- vector(length=length(dbdata$Identifier))
        MatQ[1] <- 0 #MatQ[1] <- dbdata[1,MatSelection[j]]
        for (k in seq(from=2,to=length(dbdata$Identifier),by=1)) {
          MatQ[k] <- MatQ[k-1] + dbdata[k-1,MatSelection[j]]
        }
        preparation <- cbind(preparation, MatQ)
        colnames(preparation)[1+j] <- MatSelection[j]
      }
      #tuning <- preparation

      #add 100-columns for rest of 20 materials if not 20 were chosen
      if (length(MatSelection) < 20) {
        for (l in seq(from=1, to=20-length(MatSelection), by=1)) {
          preparation <- cbind(preparation, Z=rep(100, length(dbdata$Identifier)))
        }
      }
      
      #dbfile2 <- character() #no second file used, only one database. for later-on functions important
      dbinfo2 <- NULL
      MatSelection1 <- NULL
      MatSelection2 <- NULL
      
  } else {
  
    dbfile <- tk_choose.files(caption="Select recipe database with raw materials", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))
    
    if (length(dbfile) == 0) { return(2) }

    dbinfo <- read.csv2(file=dbfile,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

    #read mass% values separately
    dbdata <- read.csv2(file=dbfile,skip=19,header=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
    colnames(dbdata) <- colnames(dbinfo)
    
    #select raw materials for optimization
    MatList1 <- colnames(dbinfo)[2:length(colnames(dbinfo))]

    while (TRUE) {
      MatSelection1 <- tk_select.list(choices=MatList1, preselect=MatList1, multiple=TRUE, title="You can deselect materials from the recipe.\n In the next step you can also\n add materials from another database:")
      if (length(MatSelection1) == 0) { return(2) 
      } else if (length(MatSelection1) > 20) {
        tkmessageBox(title = "Too many materials chosen", message="Maximum number of materials to select is 20.", icon = "info", type = "ok")
        #going back to main menu if more than 20 materials were chosen
      } else { break }
    } #while
    
    #starting preparation of calculation data frame with diameters 
    preparation <- data.frame(dbdata$Identifier)
    colnames(preparation) <- c("Diameter")

    #calculate Q-throughput from q-retention for selected raw materials and add columns to preparation data frame
    for (j in seq(from=1,to=length(MatSelection1),by=1)) {
      MatQ <- vector(length=length(dbdata$Identifier))
      MatQ[1] <- 0 #MatQ[1] <- dbdata[1,MatSelection[j]]
      for (k in seq(from=2,to=length(dbdata$Identifier),by=1)) {
        MatQ[k] <- MatQ[k-1] + dbdata[k-1,MatSelection1[j]]
      }
      preparation <- cbind(preparation, MatQ)
      colnames(preparation)[1+j] <- MatSelection1[j]
    }
    #print(preparation)
    
    while (TRUE) {
      addmat <- tkmessageBox(title = "Add materials to recipe", message="Do you want to add materials from another database or recipe?", icon = "question", type = "yesno")
      if (tclvalue(addmat) == "yes") {
        
        dbfile2 <- tk_choose.files(caption="Select database to add raw materials from", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))    
        if (length(dbfile2) == 0) { next }

        dbinfo2 <- read.csv2(file=dbfile2,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))

        #read mass% values separately
        dbdata2 <- read.csv2(file=dbfile2,skip=19,header=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
        colnames(dbdata2) <- colnames(dbinfo2)
          
        #select raw materials for optimization, but only those who are not in recipe
        MatDiff <- colnames(dbinfo2)[2:length(colnames(dbinfo2))]
        MatList2 <- MatDiff[!MatDiff %in% MatSelection1]
          
        MatSelection2 <- tk_select.list(choices=MatList2, preselect=FALSE, multiple=TRUE, title="You can select materials to add to the\n recipe. The maximum number\n of materials in total is 20:")
        if (length(MatSelection2) == 0) { next
        } else if (length(MatSelection1)+length(MatSelection2) > 20) {
            tkmessageBox(title = "Too many materials added", message="Maximum number of materials to select in total is 20.", icon = "info", type = "ok")
            next #going back to main menu if more than 20 materials were chosen
        } else { #everything okay and materials should be added
          
            #calculate Q-throughput from q-retention for added raw materials and add columns to preparation data frame
            for (j in seq(from=1,to=length(MatSelection2),by=1)) {
              MatQ <- vector(length=length(dbdata2$Identifier))
              MatQ[1] <- 0 #MatQ[1] <- dbdata[1,MatSelection[j]]
              for (k in seq(from=2,to=length(dbdata2$Identifier),by=1)) {
                MatQ[k] <- MatQ[k-1] + dbdata2[k-1,MatSelection2[j]]
            }
            preparation <- cbind(preparation, MatQ)
            colnames(preparation)[1+length(MatSelection1)+j] <- MatSelection2[j]
            }
            #print(preparation)
            
            MatSelection <- c(MatSelection1,MatSelection2) #update MatSelection; later checking if one or two files by if (e.g. length(dbfile2) != 0)
          }
      
      } else {
          dbfile2 <- character() #if no was chosen, it can be went on, but for later-on checking, length(dbfile2) != 0
                MatSelection <- MatSelection1
                dbinfo2 <- NULL
                MatSelection2 <- NULL
      }

      break
} #while add another source
    message("  Total number of selected materials: ",length(MatSelection1)+length(MatSelection2),"\n")
    
    #Get densities of selected materials
        densities <- vector(length=length(MatSelection))
        if (!is.null(dbinfo2)) {
          for (m in seq(from=1, to=length(MatSelection1), by=1)) {
            densities[m] <- as.numeric(gsub(",",".",dbinfo[6,MatSelection1[m]]))
          }
          for (m in seq(from=1, to=length(MatSelection2), by=1)) {
            densities[m+length(MatSelection1)] <- as.numeric(gsub(",",".",dbinfo2[6,MatSelection2[m]]))
          }
          #for (m in seq(from=length(MatSelection1)+1, to=length(MatSelection), by=1)) {
          #  densities[m] <- as.numeric(gsub(",",".",dbinfo2[6,MatSelection2[m]]))
          #}
        } else {
          for (m in seq(from=1, to=length(MatSelection), by=1)) {
            densities[m] <- as.numeric(gsub(",",".",dbinfo[6,MatSelection[m]]))
          }
        }
        #densities
        
        
        
        #Get prices
        prices <- vector(length=length(MatSelection))
        if (length(dbfile2) != 0) {
          for (m in seq(from=1, to=length(MatSelection1), by=1)) {
            prices[m] <- as.numeric(gsub(",",".",dbinfo[5,MatSelection1[m]]))
          }
          for (m in seq(from=1, to=length(MatSelection2), by=1)) {
            prices[m+length(MatSelection1)] <- as.numeric(gsub(",",".",dbinfo2[5,MatSelection2[m]]))
          }
        } else {
          for (m in seq(from=1, to=length(MatSelection), by=1)) {
            prices[m] <- as.numeric(gsub(",",".",dbinfo[5,MatSelection[m]]))
          }
        }
        
        
        #Get SSA
        ssa <- vector(length=length(MatSelection))
        if (length(dbfile2) != 0) {
          for (m in seq(from=1, to=length(MatSelection1), by=1)) {
            ssa[m] <- as.numeric(gsub(",",".",dbinfo[9,MatSelection1[m]]))
          }
          for (m in seq(from=1, to=length(MatSelection2), by=1)) {
            ssa[m+length(MatSelection1)] <- as.numeric(gsub(",",".",dbinfo2[9,MatSelection2[m]]))
          }
        } else {
          for (m in seq(from=1, to=length(MatSelection), by=1)) {
            ssa[m] <- as.numeric(gsub(",",".",dbinfo[9,MatSelection[m]]))
          }
        }
    
        
         #add 100-columns for rest of 20 materials if not 20 were chosen
        if (length(MatSelection) < 20) {
          for (l in seq(from=1, to=20-length(MatSelection), by=1)) {
            preparation <- cbind(preparation, Z=rep(100, length(dbdata$Identifier)))
          }
        }    
    
    
  } #else rec == 3

  
  if (fun==2) { #design batch
        error <- bounds(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2)
        message("END bounds function, RETURN value: ",error,"\n")
      } else if (fun==3) { #Verify recipe
        error <- batch(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,fun)
        message("END batch function, RETURN value: ",error,"\n")
      } else if (fun==4) { #Model parameters
        error <- batch(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,fun) #<-0
        message("END batch function, RETURN value: ",error,"\n")
      }
      
      return(error)
      #error=0 quit
      #error=1 main menu
      #error=2 open recipe/databas

}


#The only slightly modified code of the varEntryDialog function was taken from the sourc:
#J. Bryer (2012): 'User input using tcl/tk'. https://www.r-bloggers.com/2012/08/user-input-using-tcltk/. Accessed 04 Dec 2020.
#As stated in this blog entry (https://www.r-bloggers.com/2015/09/license/), the code of this function is under MIT license (https://directory.fsf.org/wiki/License:Expat):
#
#MIT License
#Copyright (C) 2012 J. Bryer
#
#Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
#The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
varEntryDialog <- function(vars, 
						   labels = vars,
						   preset = rep("", length(vars)),
						   fun = rep(list(as.character), length(vars)),
						   title = 'Variable Entry',
						   prompt = NULL,
						   cancellab = 'Cancel') {
		
	stopifnot(length(vars) == length(labels), length(labels) == length(fun))

	# Create a variable to keep track of the state of the dialog window:
	# done = 0; If the window is active
	# done = 1; If the window has been closed using the OK button
	# done = 2; If the window has been closed using the Cancel button or destroyed
	done <- tclVar(0)

	tt <- tktoplevel(bg=hintergrund)
	tkwm.title(tt, title)	

    tkraise(tt)

	entries <- list()
	tclvars <- list()

	# Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens, 
	# assign 2 to done.
	tkbind(tt,"<Destroy>",function() tclvalue(done)<-2)
	
	for(i in seq_along(vars)) {
		tclvars[[i]] <- tclVar(preset[i])
		entries[[i]] <- tkentry(tt, textvariable=tclvars[[i]])
	}
	
	doneVal <- as.integer(tclvalue(done))
	results <- list()

	reset <- function() {
		for(i in seq_along(entries)) {
			tclvalue(tclvars[[i]]) <<- preset[i]
		}
	}
	reset.but <- tkbutton(tt, text="Reset", bg=knoepfe, command=reset)
	
	#cancel <- function() {
#		tclvalue(done) <- 2
#	}
#	cancel.but <- tkbutton(tt, text=cancellab, bg=knoepfe, command=cancel)
	
	submit <- function() {
		for(i in seq_along(vars)) {
			tryCatch( {
				results[[vars[[i]]]] <<- fun[[i]](tclvalue(tclvars[[i]]))
				tclvalue(done) <- 1
				},
				error = function(e) { tkmessageBox(message=geterrmessage()) },
				finally = { }
			)
		}
	}
	submit.but <- tkbutton(tt, text="OK", bg=knoepfe, command=submit)
	
	
	
	MenuIn <- tkmenu(tt, bg=menue)           
   tkconfigure(tt, menu = MenuIn)   
  
   tkadd(MenuIn, "command", label = cancellab, command = function() tclvalue(done)<-2)
	
	
	if(!is.null(prompt)) {
		tkgrid(tklabel(tt,text=prompt, bg=hintergrund), columnspan=1, pady=tclvalue(ygap))
	}
	
	for(i in seq_along(vars)) {
		tkgrid(tklabel(tt, text=labels[i], bg=hintergrund), entries[[i]], pady=tclvalue(ygap), padx=10, columnspan=2)
	}
	
	#tkgrid(submit.but, cancel.but, reset.but, pady=tclvalue(ygap), padx=10, columnspan=3)
	tkgrid(submit.but, reset.but, pady=tclvalue(ygap), padx=10, columnspan=2)
	tkfocus(tt)

	# Do not proceed with the following code until the variable done is non-zero.
	#   (But other processes can still run, i.e. the system is not frozen.)
	tkwait.variable(done)
	
	if(tclvalue(done) != 1) {
		results <- NULL
	}
	
	tkdestroy(tt)
	return(results)
}




bounds <- function (MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2) {

#return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 (or else) for back to open recipe or database

  
  while (TRUE) {
  
  message("BEGIN bounds function\n")
  
   winmod <- tktoplevel(bg=hintergrund)
  tkwm.title(winmod,"Choose bounds type")
  
  tkraise(winmod)

  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod, bg=menue)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","09bounds","09.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())
  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text='\nDo you want to specify bounds for\nthe amounts of the raw materials?', bg=hintergrund), pady=10, padx=10)

  if(!is.na(sum(densities))) { 
  tkgrid(tkbutton(winmod, text='Yes, in mass%', bg=knoepfe, command=function() tclvalue(cm)<-3), pady=5, padx=10, sticky="ew")
  } else {
  tkgrid(tklabel(winmod, text='At least for one selected material,\nthe density is not given. Thus only\na volume optimization is possible.', bg=hintergrund), pady=10, padx=10, sticky="ew")
  }
  
  tkgrid(tkbutton(winmod, text='Yes, in volume%', bg=knoepfe, command=function() tclvalue(cm)<-4), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(winmod, text='No', bg=knoepfe, command=function() tclvalue(cm)<-5), pady=5, padx=10, sticky="ew")

  tkfocus(winmod)

  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  message("  Specify bounds in 3 wt%, 4 vol%, 5 not? (1 Main menu, 2 Material selection) Decision: ",var)
  
  if (var < 3) { return(var) 
  } else if (var==3) { 

    boundsLowMass <- rep(0,length(MatSelection)) 

    boundsVarL <- varEntryDialog(vars=MatSelection, title='Lower bounds',prompt='Adjust lower bounds in %:',preset=boundsLowMass,cancellab='Back to Bounds-Type Selection') 
    if(is.null(boundsVarL)) { next }

      for (t in seq(from=1, to=length(MatSelection), by=1)) {

        boundsLowMass[t] <- as.numeric(boundsVarL[t])/100

      }

      #print(boundsLowMass)
      boundsUpMass <- rep(100,length(MatSelection)) 

      boundsVarU <- varEntryDialog(vars=MatSelection, title='Upper bounds',prompt='Adjust upper bounds in %:',preset=boundsUpMass,cancellab='Back to Bounds-Type Selection') 
      if(is.null(boundsVarU)) { next }

        for (t in seq(from=1, to=length(MatSelection), by=1)) {

          boundsUpMass[t] <- as.numeric(boundsVarU[t])/100

        }


        boundsLowIn <- boundsLowMass

        boundsUpIn <- boundsUpMass

    
        #break #get out of bounds loop, if input ok
      #}
 
    #}#if !isnull boundsVarL
 
  } else if (var==4) {

    boundsLowIn <- rep(0,length(MatSelection)) 

    boundsVarL <- varEntryDialog(vars=MatSelection, title='Lower bounds',prompt='Adjust lower bounds in %:',preset=boundsLowIn,cancellab='Back to Bounds-Type Selection')
    if(is.null(boundsVarL)) { next }
 

    for (t in seq(from=1, to=length(MatSelection), by=1)) {

      boundsLowIn[t] <- as.numeric(boundsVarL[t])/100

    }

  
    boundsUpIn <- rep(100,length(MatSelection)) 

    boundsVarU <- varEntryDialog(vars=MatSelection, title='Upper bounds',prompt='Adjust upper bounds in %:',preset=boundsUpIn,cancellab='Back to Bounds-Type Selection') 
    if(is.null(boundsVarU)) { next }
 

      for (t in seq(from=1, to=length(MatSelection), by=1)) {

        boundsUpIn[t] <- as.numeric(boundsVarU[t])/100

      }

      #for masses can be fixed

      boundsLowMass <- rep(0,length(MatSelection))

      boundsUpMass <- rep(1,length(MatSelection)) 

 
      #break #get out of bounds loop, if input ok
    #}
 #}#if !isnull boundsVarL

 

} else {
 #if (BoundsType=="No")
 #can be fixed, no input required

 boundsLowMass <- rep(0,length(MatSelection))

 boundsUpMass <- rep(1,length(MatSelection)) 

 

 boundsLowIn <- rep(0,length(MatSelection))

 boundsUpIn <- rep(1,length(MatSelection)) 

 
 #break #get out of bounds loop, no input required
}


if (all(boundsLowMass==boundsUpMass) || all(boundsLowIn==boundsUpIn)) {
tkmessageBox(title = "Definition error", message="All specified lower and upper bounds are equal, giving a fully defined batch and nothing to optimize or fit. You may choose other bounds or one of the other application functions: 'Verify a batch' or 'Calculate model parameters'.", icon = "error", type = "ok")
message("  All specified lower and upper bounds are equal.")
next
}




if (!is.null(modelsummary)) {

  #ask if to keep last model
  KeepMod <- tkmessageBox(title = "Keep model?", message="Do you want to keep the model from the last run?", icon = "question", type = "yesno")
  
  message("  Keep model? Decision: ",KeepMod,"\n")

    if (tclvalue(KeepMod) == "yes") {
     value <- design(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsLowMass,boundsUpMass,boundsLowIn,boundsUpIn)
     message("END design function, RETURN value: ",value,"\n")
    }
    else {
     value <- model(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsLowMass,boundsUpMass,boundsLowIn,boundsUpIn)
     message("END model function, RETURN value: ",value,"\n")
    }

} else {
message("")
value <- model(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsLowMass,boundsUpMass,boundsLowIn,boundsUpIn)
message("END model function, RETURN value: ",value,"\n")
}

#return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 for back to open recipe or database
  #return 3 for back to bounds

if (value < 3) { return(value) }

} #while loop bounds 

}
#end bounds()





model <- function(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsLowMass,boundsUpMass,boundsLowIn,boundsUpIn) {

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
   
   message("BEGIN model function\n")
  
  winmod <- tktoplevel(bg=hintergrund)
  tkwm.title(winmod,"Choose model for fitting")
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
  
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text='\nChoose model for fitting:', bg=hintergrund), pady=10, padx=10)

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

  var <- as.numeric(tclvalue(cm))
  
  #print(var)
  
  tkdestroy(winmod)
  
  message("  Choose model (1 Main menu, 2 Material selection, 3 Bounds, 4 Andreasen model, 5 Dinger/Funk model, 6 Psi model, 7 Kawamura model, 8 Modified Psi model, 9 Modified Kawamura model, 10 Free model): ",var)
  
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
  
  #print(var)
  
    #there should be asked if a model can be opened from file
    FreeMod <- tkmessageBox(title = "Load Other/free model", message="Do you have the model saved?", icon = "question", type = "yesno")
    message("  Free model saved? ",FreeMod)

    if (tclvalue(FreeMod) == "yes") {
    
      FileMod <- tk_choose.files(caption="Select a saved model", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))
      

      if (length(FileMod) == 0) { next } #back to model selection if cancelled
      message("  Model saved: ",FileMod)
      
        modeldf <- read.csv2(file=FileMod,header=TRUE,check.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype))
        
        modelinfo <<- vector(length=1)
        modelinfo[1] <<- colnames(modeldf)[2]
        modelsummary <<- paste(modelinfo[1])
        
        modelparam <<- modelinfo
        modelvalue <<- vector(length=1)
        modelvalue[1] <<- NA
        
        
        VolModel <- modeldf[,2]
        
        #break
      
      #} #else {      
      #}
    
    } else {
    
    
  
    vals <- varEntryDialog(vars=c('name'), labels=c('Name of the model:'), title="Other/free model",prompt="Name the model:",preset=c("Other/free model"),cancellab='Back to Model Selection')
    if(is.null(vals)) { next } #if input ok, go on, if not return to model selection
    
    modelinfo <<- vector(length=1)
    modelinfo[1] <<- as.character(vals[1])
    modelsummary <<- paste(modelinfo[1])
    
    message("  New model: ",modelinfo[1])
    
    modelparam <<- modelinfo
    modelvalue <<- vector(length=1)
    modelvalue[1] <<- NA
    
    winnr <- ceiling(length(preparation$Diameter)/10)
    for (i in seq(from=0, to=winnr-1, by=1)) {
      
      if (i*10+10 < length(preparation$Diameter)+1) {
        vals <- varEntryDialog(vars=c('d1','2','d3','d4','d5','d6','d7','d8','d9','d10'), labels=c(
        paste("CPFT(d=",preparation$Diameter[i*10+1],tclvalue(dunit),"):",sep=""),
        paste("CPFT(d=",preparation$Diameter[i*10+2],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+3],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+4],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+5],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+6],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+7],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+8],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+9],tclvalue(dunit),"):",sep=""),paste("CPFT(d=",preparation$Diameter[i*10+10],tclvalue(dunit),"):",sep="")), title=modelinfo[1],prompt=paste("Adjust ",modelinfo[1],":\nInput all \"Cumulative Percent \n Finer Than d\"- (CPFT-) values:",sep=""),cancellab='Back to Model Selection')
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
  
  valmod <- design(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsLowMass,boundsUpMass,boundsLowIn,boundsUpIn)
  message("END design function, RETURN value: ",valmod,"\n")
  
  #return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 for back to open recipe or database
  #return 3 for back to bounds
  #return 4 for back to model selection

if (valmod < 4) { return(valmod) }
  
  } #while loop model selection 
  

}
#end model()










batch <- function (MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,fun) {

#return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 (or else) for back to open recipe or database

  #hier weiter
  while (TRUE) {
  
  message("BEGIN batch function\n")
  
   winmod <- tktoplevel(bg=hintergrund)
  tkwm.title(winmod,"Choose batch type")
  
  tkraise(winmod)

  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod, bg=menue)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","08batch","08.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())
  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text='\nIs the batch given\n in mass% or volume%?', bg=hintergrund), pady=10, padx=10)

  if(!is.na(sum(densities))) { 
  tkgrid(tkbutton(winmod, text='In mass%', bg=knoepfe, command=function() tclvalue(cm)<-3), pady=5, padx=10, sticky="ew")
  } else {
  tkgrid(tklabel(winmod, text='At least for one selected material,\nthe density is not given. Thus only\n volume% is possible.', bg=hintergrund), pady=10, padx=10, sticky="ew")
  }
  
  tkgrid(tkbutton(winmod, text='In volume%', bg=knoepfe, command=function() tclvalue(cm)<-4), pady=5, padx=10, sticky="ew")
  
    tkfocus(winmod)

  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  message("  Define batch in (1, Main menu, 2 Material selection, 3 wt%, 4 vol%): ",var)
  
  if (var < 3) { return(var) 
  } else if (var==3) { #in mass%

    boundsMass <- rep(0,length(MatSelection)) 
    boundsVol <- vector(length=length(MatSelection))

    boundsMassV <- varEntryDialog(vars=MatSelection, title='Batch',prompt='Adjust batch in %:',preset=boundsMass,cancellab='Back to Batch-Type Selection') 
    if(is.null(boundsMassV)) { next }
    
    
    boundsVolS <- 0

      for (t in seq(from=1, to=length(MatSelection), by=1)) {

        boundsMass[t] <- as.numeric(boundsMassV[t])/100
        boundsVol[t] <- boundsMass[t]/densities[t]
        boundsVolS <- boundsVolS + boundsVol[t]
        
#print(paste("Material ",t," Ma%/100%: ",boundsMass[t],", Volume: ",boundsVol[t],", VolGes: ",boundsVolS))
      }

      
      for (m in seq(from=1, to=length(MatSelection), by=1)) {
        boundsVol[m] <- boundsVol[m]/boundsVolS
        #print(paste("Material ",m," Ma%/100%: ",boundsMass[m],", Vol%/100%: ",boundsVol[m],", VolGes: ",boundsVolS))
      }

 
 
  } else if (var==4) {#in vol%

    boundsVol <- rep(0,length(MatSelection)) 
    boundsMass <- vector(length=length(MatSelection))

    boundsVolV <- varEntryDialog(vars=MatSelection, title='Batch',prompt='Adjust batch in %:',preset=boundsVol,cancellab='Back to Batch-Type Selection')
    if(is.null(boundsVolV)) { next }
 
 boundsMassS <- 0

    for (t in seq(from=1, to=length(MatSelection), by=1)) {

      boundsVol[t] <- as.numeric(boundsVolV[t])/100
      boundsMass[t] <- boundsVol[t]*densities[t]
        boundsMassS <- boundsMassS + boundsMass[t]
        #print(paste("Material ",t," Vol%/100%: ",boundsVol[t],", Mass: ",boundsMass[t],", MassGes: ",boundsMassS))

    }

   for (m in seq(from=1, to=length(MatSelection), by=1)) {
        boundsMass[m] <- boundsMass[m]/boundsMassS
        #print(paste("Material ",m," Vol%/100%: ",boundsVol[m],", Ma%/100%: ",boundsMass[m],", MassGes: ",boundsMassS))
      }

 #tkmessageBox(title = "Given vol bounds", message=paste("Given vol bounds:\n",boundsVol), icon = "info", type = "ok")

}





if (round(100*sum(boundsVol),digits=accuracy)!=100 || round(100*sum(boundsMass),digits=accuracy)!=100) {

tkmessageBox(title = "Batch definition error", message="The sum of the batch component amounts is not 100%.", icon = "error", type = "ok")
message("  The sum of the batch component amounts is not 100%.")
next

}

message("  Batch defined.")

if (!is.null(modelsummary)) {

  #ask if to keep last model
  KeepMod <- tkmessageBox(title = "Keep model?", message="Do you want to keep the model from the last run?", icon = "question", type = "yesno")
  
  message("  Keep model? ",KeepMod,"\n")

    if (tclvalue(KeepMod) == "yes") {
    
     if (fun==3) {
     value <- verify(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol)
     message("END verify function, RETURN value: ",value,"\n")
     } else if (fun == 4) {
     value <- calcparams(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol,ModSelection,boundsLMod,boundsUMod,startopt)
     message("END calculate model parameters function, RETURN value: ",value,"\n")
     }

    }
    else {
     if (fun==3) {
     value <- modelver(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol)
     message("END verify-model function, RETURN value: ",value,"\n")
     } else if (fun == 4) {
     value <- modelcalc(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol)
     message("END calculate-model-parameters-model function, RETURN value: ",value,"\n")
     }

    }

} else {

message("")

if (fun==3) {
     value <- modelver(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol)
     message("END verify-model function, RETURN value: ",value,"\n")
     } else if (fun == 4) {
     value <- modelcalc(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol)
     message("END calculate-model-parameters-model function, RETURN value: ",value,"\n")
     }



}

#return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 for back to open recipe or database
  #return 3 for back to bounds

if (value < 3) { return(value) }

} #while loop batch

}
#end batch()
