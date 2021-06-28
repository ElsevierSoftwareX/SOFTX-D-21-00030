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


paramsft <- function(MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsMass,boundsVol,ModSelection,optd,optn,resetd,resetn,namd,namn,d10,d25,d50,d75,d90,Den,Sssa,price,sumerrsq) {

message("BEGIN model parameter finetuning function\n")

tuning <- preparation

xb <- c(tuning$Diameter[1],tuning$Diameter[length(tuning$Diameter)])
yb <- c(0,100)
lc <- "x"
mag <- 1.2

while (TRUE) {

message("  Loading model parameter finetuning window")

parft <- tktoplevel(bg=hintergrund)
tkwm.title(parft, "Parameter fine-tuning")	
 tkraise(parft)

done <- tclVar(0)

tkbind(parft,"<Destroy>",function() tclvalue(done)<-1)

#init values

entriesD <- list()
entriesN <- list()
tclvarsD <- list()
tclvarsN <- list()

for(i in seq(from=1, to=length(optd), by=1)) {
        tclvarsD[[i]] <- tclVar(as.character(signif(optd[i], digits=accsizes)))
		entriesD[[i]] <- tkentry(parft, textvariable=tclvarsD[[i]])
}

for(i in seq(from=1, to=length(optn), by=1)) {
        tclvarsN[[i]] <- tclVar(as.character(round(optn[i], digits=accuracy+2)))
		entriesN[[i]] <- tkentry(parft, textvariable=tclvarsN[[i]])
}

tclvarcor <- tclVar(round(cor(tuning$VolModel,tuning$CurveFit),digits=4))
labelcor <- tklabel(parft, textvariable=tclvarcor, bg=hintergrund)

tclvarerr <- tclVar(round(sumerrsq, digits=accuracy))
labelerr <- tklabel(parft, textvariable=tclvarerr, bg=hintergrund)

#functions of buttons

#hier weiter mit ueberarbeitung
saveres <- function() {

        parent <- tk_choose.dir(getwd(), caption = "Select parent directory to save a folder with the results in")
        
        if (!is.na(parent)) {

        while (TRUE) {
            savename <- varEntryDialog(vars=c('saven'), labels=c('Name for folder to save results in:'), title='Name for saving',prompt='Files with results will be put into a newly created folder.')

            if (is.null(savename)) { break }

            if (file.exists(paste(parent,savename,sep="/"))) {

                overwrite <- tkmessageBox(title = "Naming conflict", message="A folder with the chosen name exists already. Overwrite contents?", icon = "info", type = "yesno")

                if (tclvalue(overwrite) == "yes") { break }

            } else { 
            dir.create(paste(parent,savename,sep="/"))
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

        write.table(matdb, file=paste(parent,"/",savename,"/",savename,"-recipe.csv",sep=""), row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 
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
         
         write.table(matdb, file=paste(parent,"/",savename,"/",savename,"-recipe.csv",sep=""), row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 

        } #else -> 2 files
        
        
        #write plot or whatever...

	png(paste(parent,"/",savename,"/",savename,"-plot.png",sep=""))
	
	par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

lines(x=tuning$Diameter,y=tuning$CurveFit,lty=2)

legend("bottomright", c("Model-Fit","Batch"), lty=c(1,2), inset=0.01)

dev.off(length(dev.list())+1)
        
        
        
        
        #save batch 

batchdb <- data.frame(cbind(c(round(100*boundsVol, digits=accuracy),round(100*sum(boundsVol), digits=accuracy)),c(round(100*boundsMass, digits=accuracy),round(100*sum(boundsMass), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2))))
	
	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
     #   rownames(batchdb) <- c(MatSelection,"Batch:")

	 write.table(batchdb,file=paste(parent,"/",savename,"/",savename,"-batch.csv",sep=""),row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%","Mass%","Density","SSA","Price per MT"),quote=FALSE)

        
        #save model and fit
        
        if (ModSelection == "Andreasen model") {
 modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), "n:")
 modelvalue <- c(NA, optd[1], optn[1])
} else if (ModSelection == "Dinger/Funk model") {
modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), paste("d(min) in",tclvalue(dunit),":"), "n:")
modelvalue <- c(NA, optd[2], optd[1], optn[1])
} else if (ModSelection == "Psi model") {
modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), "n(min):", "n(max):")
modelvalue <- c(NA, optd[1], optn[1], optn[2])
} else if (ModSelection == "Kawamura model") {
modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), paste("d(gap) in",tclvalue(dunit),":"), "n(Andreasen-part):", "n(Furnas-part):")
modelvalue <- c(NA, optd[2], optd[1], optn[1], optn[2])
} else if (ModSelection == "Modified Psi model") {
modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), paste("d(min) in",tclvalue(dunit),":"), "n(min):", "n(max):")
modelvalue <- c(NA, optd[2], optd[1], optn[1], optn[2])
} else {
modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), paste("d(min) in",tclvalue(dunit),":"), paste("d(gap) in",tclvalue(dunit),":"), "n(Dinger/Funk-part):", "n(Furnas-part):")
modelvalue <- c(NA, optd[3], optd[1], optd[2], optn[1], optn[2])
}

modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,tuning$CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(tuning$CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round((tuning$VolModel-tuning$CurveFit)*(tuning$VolModel-tuning$CurveFit), digits=accuracy),rep(NA, 4+length(modelvalue)))))
        
        
        write.table(modeldb,file=paste(parent,"/",savename,"/",savename,"-model.csv",sep=""),row.names=c(rep(" ",length(tuning$Diameter)),"-----",modelparam,"-----","Corr. coeff.:","Sum. sq. dev.:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c(paste("Model-Fit and Batch;Diameter in",tclvalue(dunit)),"Q(Model-Fit) in %","Q(Batch) in %","Squared deviation"),quote=FALSE)
        } #!is.null savename
        } #!is.na
        
        message("  Results saved in folder: ",paste(parent,savename,sep=pathsep))
}









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








savebatch <- function() {

FileBatch = tclvalue(tcl("tk_getSaveFile"))
if (FileBatch != "") {

batchdb <- data.frame(cbind(c(round(100*boundsVol, digits=accuracy),round(100*sum(boundsVol), digits=accuracy)),c(round(100*boundsMass, digits=accuracy),round(100*sum(boundsMass), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2))))
	
	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
     #   rownames(batchdb) <- c(MatSelection,"Batch:")

	 write.table(batchdb,file=FileBatch,row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%","Mass%","Density","SSA","Price per MT"),quote=FALSE)
message("  Saved batch: ",FileBatch)
}
}



savefit <- function() {

FileFit = tclvalue(tcl("tk_getSaveFile"))
if (FileFit != "") {

#aus input.R:
if (ModSelection == "Andreasen model") {
 modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), "n:")
 modelvalue <- c(NA, optd[1], optn[1])
} else if (ModSelection == "Dinger/Funk model") {
modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), paste("d(min) in",tclvalue(dunit),":"), "n:")
modelvalue <- c(NA, optd[2], optd[1], optn[1])
} else if (ModSelection == "Psi model") {
modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), "n(min):", "n(max):")
modelvalue <- c(NA, optd[1], optn[1], optn[2])
} else if (ModSelection == "Kawamura model") {
modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), paste("d(gap) in",tclvalue(dunit),":"), "n(Andreasen-part):", "n(Furnas-part):")
modelvalue <- c(NA, optd[2], optd[1], optn[1], optn[2])
} else if (ModSelection == "Modified Psi model") {
modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), paste("d(min) in",tclvalue(dunit),":"), "n(min):", "n(max):")
modelvalue <- c(NA, optd[2], optd[1], optn[1], optn[2])
} else {
modelparam <- c(ModSelection, paste("d(max) in",tclvalue(dunit),":"), paste("d(min) in",tclvalue(dunit),":"), paste("d(gap) in",tclvalue(dunit),":"), "n(Dinger/Funk-part):", "n(Furnas-part):")
modelvalue <- c(NA, optd[3], optd[1], optd[2], optn[1], optn[2])
}

modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,tuning$CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(tuning$CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round((tuning$VolModel-tuning$CurveFit)*(tuning$VolModel-tuning$CurveFit), digits=accuracy),rep(NA, 4+length(modelvalue)))))
        
        
        write.table(modeldb,file=FileFit,row.names=c(rep(" ",length(tuning$Diameter)),"-----",modelparam,"-----","Corr. coeff.:","Sum. sq. dev.:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c(paste("Model-Fit and Batch;Diameter in",tclvalue(dunit)),"Q(Model-Fit) in %","Q(Batch) in %","Squared deviation"),quote=FALSE)
message("  Saved fit: ",FileFit)
}
}




saveplot <- function() {

FilePlot = tclvalue(tcl("tk_getSaveFile"))
if (FilePlot != "") {

png(FilePlot)

par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

lines(x=tuning$Diameter,y=tuning$CurveFit,lty=2)

legend("bottomright", c("Model-Fit","Batch"), lty=c(1,2), inset=0.01)

dev.off(length(dev.list())+1)
message("  Saved plot: ",FilePlot)
}

}

#ende zu ueberarbeiten

reset <- function() {

for(i in seq(from=1, to=length(optd), by=1)) {
        tclvalue(tclvarsD[[i]]) <<- resetd[i]
}

for(i in seq(from=1, to=length(optn), by=1)) {
        tclvalue(tclvarsN[[i]]) <<- resetn[i]
}
		submit()
	}
	reset.but <- tkbutton(parft, text="Reset", command=reset, bg=knoepfe)

	
	
	
	
	
submit <- function() {
message("  Recalculating...")

for(i in seq(from=1, to=length(optd), by=1)) {
        optd[i] <<- as.numeric(tclvalue(tclvarsD[[i]]))
}

for(i in seq(from=1, to=length(optn), by=1)) {
        optn[i] <<- as.numeric(tclvalue(tclvarsN[[i]]))
}

if (ModSelection == "Andreasen model") {

errsq <<- vector(length=length(tuning$Diameter))
sumerrsq <<- 0
for (i in seq(from=1,to=length(tuning$Diameter),by=1)) {
      if (optd[1] < tuning$Diameter[i]) {
        tuning$VolModel[i] <<- 100 }
      else {
        tuning$VolModel[i] <<- 100*(tuning$Diameter[i]/optd[1])^optn[1]
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <<- ((tuning$VolModel[i]-tuning$CurveFit[i])*(tuning$VolModel[i]-tuning$CurveFit[i]))
      sumerrsq <<- sumerrsq+errsq[i]
    }

} else if (ModSelection == "Dinger/Funk model") {

errsq <<- vector(length=length(tuning$Diameter))
sumerrsq <<- 0
for (i in seq(from=1,to=length(tuning$Diameter),by=1)) {
      if (optd[2] < tuning$Diameter[i]) {
        tuning$VolModel[i] <<- 100 }
      else if (optd[1] < tuning$Diameter[i]) {
        tuning$VolModel[i] <<- 100*(tuning$Diameter[i]^optn[1] - optd[1]^optn[1])/(optd[2]^optn[1] - optd[1]^optn[1])
      }
      else {
        tuning$VolModel[i] <<- 0
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <<- ((tuning$VolModel[i]-tuning$CurveFit[i])*(tuning$VolModel[i]-tuning$CurveFit[i]))
      sumerrsq <<- sumerrsq+errsq[i]
    }

} else if (ModSelection == "Psi model") {

errsq <<- vector(length=length(tuning$Diameter))
sumerrsq <<- 0
for (i in seq(from=1,to=length(tuning$Diameter),by=1)) {
      if (optd[1] < tuning$Diameter[i]) {
        tuning$VolModel[i] <<- 100 }
      else {
        tuning$VolModel[i] <<- 100*(tuning$Diameter[i]/optd[1])^(optn[1] + tuning$Diameter[i]*(optn[2]-optn[1])/optd[1])
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <<- ((tuning$VolModel[i]-tuning$CurveFit[i])*(tuning$VolModel[i]-tuning$CurveFit[i]))
      sumerrsq <<- sumerrsq+errsq[i]
    }
    
} else if (ModSelection == "Modified Psi model") {

errsq <<- vector(length=length(tuning$Diameter))
sumerrsq <<- 0
for (i in seq(from=1,to=length(tuning$Diameter),by=1)) {
      if (optd[2] < tuning$Diameter[i]) {
        tuning$VolModel[i] <<- 100 }
      else if (optd[1] < tuning$Diameter[i]) {
        tuning$VolModel[i] <<- 100*(tuning$Diameter[i]^(optn[1]+tuning$Diameter[i]*(optn[2]-optn[1])/optd[2]) - optd[1]^(optn[1]+tuning$Diameter[i]*(optn[2]-optn[1])/optd[2]))/(optd[2]^(optn[1]+tuning$Diameter[i]*(optn[2]-optn[1])/optd[2]) - optd[1]^(optn[1]+tuning$Diameter[i]*(optn[2]-optn[1])/optd[2]))
      }
      else {
        tuning$VolModel[i] <<- 0
      }
      #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
      errsq[i] <<- ((tuning$VolModel[i]-tuning$CurveFit[i])*(tuning$VolModel[i]-tuning$CurveFit[i]))
      sumerrsq <<- sumerrsq+errsq[i]
    }

} else if (ModSelection == "Kawamura model") {

errsq <<- vector(length=length(tuning$Diameter))
                  sumerrsq <<- 0
                  for (i in seq(from=1,to=length(tuning$Diameter),by=1)) {
                     if (optd[2] < tuning$Diameter[i]) {
                        tuning$VolModel[i] <<- 100 
                     } else if (optd[1] < tuning$Diameter[i]) {
                        tuning$VolModel[i] <<- 100*( (optd[1]/optd[2])^optn[1] + (1 - (optd[1]/optd[2])^optn[1])*(tuning$Diameter[i]^optn[2] - optd[1]^optn[2])/(optd[2]^optn[2] - optd[1]^optn[2]) )
                     } else {
                        tuning$VolModel[i] <<- 100*(tuning$Diameter[i]/optd[2])^optn[1]
                     }
                     #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
                     errsq[i] <<- ((tuning$VolModel[i]-tuning$CurveFit[i])*(tuning$VolModel[i]-tuning$CurveFit[i]))
                     sumerrsq <<- sumerrsq+errsq[i]
                  }

} else { #Mod Kawamura

errsq <<- vector(length=length(tuning$Diameter))
                  sumerrsq <<- 0
                  for (i in seq(from=1,to=length(tuning$Diameter),by=1)) {
                     if (optd[3] < tuning$Diameter[i]) {
                        tuning$VolModel[i] <<- 100 
                     } else if (optd[2] < tuning$Diameter[i]) {
                        tuning$VolModel[i] <<- 100*( (optd[2]^optn[1] - optd[1]^optn[1])/(optd[3]^optn[1] - optd[1]^optn[1]) + (1 - (optd[2]^optn[1] - optd[1]^optn[1])/(optd[3]^optn[1] - optd[1]^optn[1]))*(tuning$Diameter[i]^optn[2] - optd[2]^optn[2])/(optd[3]^optn[2] - optd[2]^optn[2]) )
                     } else if (optd[1] < tuning$Diameter[i]) {
                        tuning$VolModel[i] <<- 100*(tuning$Diameter[i]^optn[1] - optd[1]^optn[1])/(optd[3]^optn[1] - optd[1]^optn[1]) 
                     } else {
                        tuning$VolModel[i] <<- 0
                     }
                     #print(paste("VolModel:",VolModel[i],", CurveFit:",CurveFit[i]", Diff:",VolModel[i]-CurveFit[i]))
      
                     errsq[i] <<- ((tuning$VolModel[i]-tuning$CurveFit[i])*(tuning$VolModel[i]-tuning$CurveFit[i]))
                     sumerrsq <<- sumerrsq+errsq[i]
                  }

}
     
        
        #felder neu labeln
        
tclvalue(tclvarcor) <<- round(cor(tuning$VolModel,tuning$CurveFit),digits=4)
labelcor <<- tklabel(parft, textvariable=tclvarcor, bg=hintergrund)

tclvalue(tclvarerr) <<- round(sumerrsq, digits=accuracy)
labelerr <<- tklabel(parft, textvariable=tclvarerr, bg=hintergrund)


tkdelete(t.d,0,'end')
	tkdelete(t.m,0,'end')
	tkdelete(t.f,0,'end')
	tkdelete(t.e,0,'end')

 
for (i in seq(from=1, to=length(tuning$Diameter), by=1)) {	

  tkinsert(t.d, "end",as.character(tuning$Diameter[i]))
   tkinsert(t.m, "end",as.character(round(tuning$VolModel[i],digits=accuracy)))
   tkinsert(t.f, "end",as.character(round(tuning$CurveFit[i],digits=accuracy)))
   tkinsert(t.e, "end",as.character(round((tuning$VolModel[i]-tuning$CurveFit[i])*(tuning$VolModel[i]-tuning$CurveFit[i]),digits=accuracy)))
}

if (!is.null(dev.list())) { 
 #dev.off(length(dev.list())+1) #close last opened interactive R window
 replot(renew="yes")
}
 
	}
submit.but <- tkbutton(parft, text="Calculate", bg=knoepfe, command=submit)







infob <- function() {
message("  Batch info.")

        batchwin <- tktoplevel()
tkwm.title(batchwin, "Batch information")	
 tkraise(batchwin)

ok <- tclVar(0)

tkbind(batchwin,"<Destroy>",function() tclvalue(ok)<-1)

#menu

 MenuIn <- tkmenu(batchwin, bg=menue)           
   tkconfigure(batchwin, menu = MenuIn)   
   tkadd(MenuIn, "command", label = "Back to Parameter Fine-tuning", command = function() tclvalue(ok)<-1)
   
#window

tkgrid(tklabel(batchwin, font=tkfont.create(size=5), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(batchwin, text="Material"), tklabel(batchwin, text="Vol%"), tklabel(batchwin, text="Mass%"),tklabel(batchwin, text="Density"), tklabel(batchwin, text="SSA"), tklabel(batchwin, text="Price per MT"), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(ttkseparator(batchwin), ttkseparator(batchwin), ttkseparator(batchwin), ttkseparator(batchwin), ttkseparator(batchwin), ttkseparator(batchwin), columnspan=6, pady=tclvalue(ygap), padx=10, sticky="we")

for (i in seq(from=1, to=length(MatSelection), by=1)) {
    tkgrid(tklabel(batchwin, text=MatSelection[i]), tklabel(batchwin, text=as.character(round(100*boundsVol[i], digits=accuracy))), tklabel(batchwin, text=as.character(round(100*boundsMass[i], digits=accuracy))), tklabel(batchwin,text=as.character(round(densities[i], digits=4))), tklabel(batchwin, text=as.character(round(ssa[i], digits=4))), tklabel(batchwin, text=as.character(round(prices[i], digits=2))), pady=tclvalue(ygap), padx=10, columnspan=6)
}

tkgrid(ttkseparator(batchwin), ttkseparator(batchwin), ttkseparator(batchwin), ttkseparator(batchwin), ttkseparator(batchwin), ttkseparator(batchwin), columnspan=6, pady=tclvalue(ygap), padx=10, sticky="we")

tkgrid(tklabel(batchwin, text="Batch:"), tklabel(batchwin, text=as.character(round(100*sum(boundsVol), digits=accuracy))), tklabel(batchwin, text=as.character(round(100*sum(boundsMass), digits=accuracy))), tklabel(batchwin, text=as.character(round(Den, digits=4))), tklabel(batchwin, text=as.character(round(Sssa, digits=4))), tklabel(batchwin, text=as.character(round(price, digits=2))), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(batchwin, font=tkfont.create(size=5), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(batchwin, text="d(CPFT) of batch:"), tklabel(batchwin, text=paste("d(10%) =",round(d10, digits=accuracy),tclvalue(dunit))), tklabel(batchwin, text=paste("d(25%) =",round(d25, digits=accuracy),tclvalue(dunit))), tklabel(batchwin, text=paste("d(50%) =",round(d50, digits=accuracy),tclvalue(dunit))), tklabel(batchwin, text=paste("d(75%) =",round(d75, digits=accuracy),tclvalue(dunit))), tklabel(batchwin, text=paste("d(90%) =",round(d90, digits=accuracy),tclvalue(dunit))), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(batchwin, font=tkfont.create(size=5), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

tkfocus(batchwin)

tkwait.variable(ok)

tkdestroy(batchwin) 
}
info.but <- tkbutton(parft, text='Batch info', bg=knoepfe, command=infob)



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
modplot.but <- tkbutton(parft, text='Adjust plot', bg=knoepfe, command=modplot)


replot <- function(renew="no") {
message("  Open/Close/Refresh plot")
	if (is.null(dev.list())) {

        if (os=="linux") { X11(title="ParSD: Fit model parameters") }
        else if (os=="osx") { quartz(title="ParSD: Fit model parameters") }
        else { windows(title="ParSD: Fit model parameters") }
        
		par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

        plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

        lines(x=tuning$Diameter,y=tuning$CurveFit,lty=2)

        legend("bottomright", c("Model-Fit","Batch"), lty=c(1,2), inset=0.01)

    } else if (renew=="yes") {
        par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

        plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

        lines(x=tuning$Diameter,y=tuning$CurveFit,lty=2)

        legend("bottomright", c("Model-Fit","Batch"), lty=c(1,2), inset=0.01)
    
    } else { 

        dev.off(length(dev.list())+1) 

    }   

}
replot.but <- tkbutton(parft, text='Open/close plot', bg=knoepfe, command=replot)

#end functions of buttons

setdm <- function(...) {
     tkyview(t.d,...)
     tkyview(t.m,...)
     tkyview(t.f,...)
     tkyview(t.e,...)
}

	scr.d <- tkscrollbar(parft, repeatinterval=4,command=setdm)

	t.d <- tklistbox(parft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	t.m <- tklistbox(parft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	t.f <- tklistbox(parft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	t.e <- tklistbox(parft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

for (i in seq(from=1, to=length(tuning$Diameter), by=1)) {
   tkinsert(t.d, "end",as.character(tuning$Diameter[i]))
   tkinsert(t.m, "end",as.character(round(tuning$VolModel[i],digits=accuracy)))
   tkinsert(t.f, "end",as.character(round(tuning$CurveFit[i],digits=accuracy)))
   tkinsert(t.e, "end",as.character(round((tuning$VolModel[i]-tuning$CurveFit[i])*(tuning$VolModel[i]-tuning$CurveFit[i]),digits=accuracy)))
}

MenuFT <- tkmenu(parft, bg=menue)           
tkconfigure(parft, menu = MenuFT) 

#tkadd(MenuVFT, "command", label = "Save all", command =saveresv)
SaveMenu <- tkmenu(MenuFT, tearoff = FALSE)
tkadd(MenuFT, "cascade", label = "Save", menu = SaveMenu)
  tkadd(SaveMenu, "command", label = "Recipe (Save as CSV-file!)", command =function() saverec())
  tkadd(SaveMenu, "command", label = "Batch (Save as CSV-file!)", command =function() savebatch())
  tkadd(SaveMenu, "command", label = "Model-Fit/Comparison (Save as CSV-file!)", command =function() savefit())
  tkadd(SaveMenu, "command", label = "Graph (Save as PNG-file!)", command =function() saveplot())
  tkadd(SaveMenu, "separator")
  tkadd(SaveMenu, "command", label = "All (in an extra folder)", command =function() saveres())

OptWin <- tkmenu(MenuFT, tearoff = FALSE)
tkadd(MenuFT, "cascade", label = "Window options", menu = OptWin)
  tkadd(OptWin, "radiobutton", variable=sbheight, value='3', label="Small listbox height")
  tkadd(OptWin, "radiobutton", variable=sbheight, value='7', label="Medium listbox height")
  tkadd(OptWin, "radiobutton", variable=sbheight, value='11', label="Large listbox height")
  tkadd(OptWin, "separator")
  tkadd(OptWin, "radiobutton", variable=ygap, value='0', label="Small vertical distance between window elements")
  tkadd(OptWin, "radiobutton", variable=ygap, value='1', label="Medium vertical distance between window elements")
  tkadd(OptWin, "radiobutton", variable=ygap, value='2', label="Large vertical distance between window elements")
  
tkadd(MenuFT, "command", label = "Apply window options", command =function() tclvalue(done)<-5)

AbMenu <- tkmenu(MenuFT, tearoff = FALSE)
  tkadd(MenuFT, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","17rescalc","17.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

QuMenu <- tkmenu(MenuFT, tearoff = FALSE)
tkadd(MenuFT, "cascade", label = "Back to...", menu = QuMenu)
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(done)<-4)
  tkadd(QuMenu, "command", label = "Batch Definition", command = function() tclvalue(done)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(done)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(done)<-1)
  tkadd(QuMenu, "separator")
  tkadd(QuMenu, "command", label = "Quit ParSD app", command = function() tclvalue(done)<-0)

  
  

tkgrid(tklabel(parft, font=tkfont.create(size=5), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=5)

tkgrid(tklabel(parft, text="Particle size parameter(s)", bg=hintergrund), tklabel(parft, text="", bg=hintergrund), tklabel(parft, text="Dist. modulus parameter(s)", bg=hintergrund),tklabel(parft, text="", bg=hintergrund), reset.but, pady=tclvalue(ygap), padx=10, columnspan=5)

tkgrid(ttkseparator(parft), tklabel(parft, font=tkfont.create(size=1), text="", bg=hintergrund), ttkseparator(parft), tklabel(parft, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(parft, font=tkfont.create(size=1), text="", bg=hintergrund), columnspan=5, pady=0, padx=10, sticky="we")

tkgrid(tklabel(parft, text=namd[1], bg=hintergrund), entriesD[[1]], tklabel(parft, text=namn[1], bg=hintergrund), entriesN[[1]], submit.but, pady=tclvalue(ygap), padx=10, columnspan=5)

if (length(optd) == 2 && length(optn) == 1) {
  tkgrid(tklabel(parft, text=namd[2], bg=hintergrund), entriesD[[2]], tklabel(parft,text="", bg=hintergrund), tklabel(parft,text="", bg=hintergrund), tklabel(parft,text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=5)
} else if (length(optd) == 1 && length(optn) == 2) {
  tkgrid(tklabel(parft,text="", bg=hintergrund), tklabel(parft,text="", bg=hintergrund), tklabel(parft, text=namn[2], bg=hintergrund), entriesN[[2]], tklabel(parft,text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=5)
} else if (length(optd)==2 && length(optn)==2) {
  tkgrid(tklabel(parft, text=namd[2], bg=hintergrund), entriesD[[2]], tklabel(parft, text=namn[2], bg=hintergrund), entriesN[[2]], tklabel(parft,text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=5)
} else if (length(optd)==3) {
  tkgrid(tklabel(parft, text=namd[2], bg=hintergrund), entriesD[[2]], tklabel(parft, text=namn[2], bg=hintergrund), entriesN[[2]], tklabel(parft,text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=5)
  
  tkgrid(tklabel(parft, text=namd[3], bg=hintergrund), entriesD[[3]], tklabel(parft,text="", bg=hintergrund), tklabel(parft,text="", bg=hintergrund), tklabel(parft,text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=5)
}

tkgrid(tklabel(parft, font=tkfont.create(size=5), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=5)

tkgrid(ttkseparator(parft), columnspan=30, pady=tclvalue(ygap), padx=10, sticky="we")

tkgrid(tklabel(parft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=5)

tkgrid(tklabel(parft, text="Fit quality:", bg=hintergrund), replot.but, modplot.but, info.but, tklabel(parft,text="(use scrollbar to move", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=5)

tkgrid(tklabel(parft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=5)

tkgrid(tklabel(parft, text="Diameter", bg=hintergrund), tklabel(parft, text="Model", bg=hintergrund), tklabel(parft, text="Batch", bg=hintergrund), tklabel(parft, text="Squared deviation", bg=hintergrund), tklabel(parft, text="lists simulaneously)", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=5)

tkgrid(t.d, t.m, t.f, t.e, scr.d, pady=tclvalue(ygap), padx=10, columnspan=5)
tkgrid.configure(scr.d,rowspan=tclvalue(sbheight),sticky="nsw")	
#rowspan war 4

tkgrid(tklabel(parft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=5)

tkgrid(tklabel(parft, text="Corr. coeff.:", bg=hintergrund), labelcor, tklabel(parft, text="Sum sq. dev.:", bg=hintergrund), labelerr, tklabel(parft, text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=5)

tkgrid(tklabel(parft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=5)

tkfocus(parft)

tkwait.variable(done)

don <- tclvalue(done)
tkdestroy(parft) 

message("  Chosen action (0 Quit, 1 Main menu, 2 Material Selection, 3 Batch definition, 4 Model bounds, 5 Apply window options): ",don,"\n")

if (don < 5) {
if ( !is.null(dev.list()) ) { 

        dev.off(length(dev.list())+1) #close last opened interactive R window

    }
return(don)

}

}#end while loop

}#end paramsft function




















volumeft <- function(volreset,volchange,masses,densities,ssa,prices,Den,Sssa,price,d10,d25,d50,d75,d90,tuning,CurveFit,sumerrsq,MatSelection,MatSelection1,MatSelection2,dbinfo,dbinfo2,xb,yb,lc,mag,errsq) {

message("BEGIN vol%-finetuning function\n")

while (TRUE) {

message("  Loading vol%-finetuning window")

#print(volchange)
#print(masses)
#print(ssa)
#print(Sssa)
#print(prices)
#print(price)
#print(densities)
#print(Den)


volft <- tktoplevel(bg=hintergrund)
tkwm.title(volft, "Fine-Tuning in Volume%")	
 tkraise(volft)

donev <- tclVar(0)

tkbind(volft,"<Destroy>",function() tclvalue(donev)<-1)

#init values

entries <- list()
entriesm <- list()
tclvars <- list()
tclvarsm <- list()

for(i in seq(from=1, to=length(MatSelection), by=1)) {

        tclvarsm[[i]] <- tclVar(as.character(round(masses[i]*100,digits=accuracy)))

        tclvars[[i]] <- tclVar(round(volchange[i]*100,digits=accuracy))

		entries[[i]] <- tkentry(volft, textvariable=tclvars[[i]])
		
		entriesm[[i]] <- tklabel(volft, textvariable=tclvarsm[[i]], bg=hintergrund)

}

tclvarsumv <- tclVar(paste(round(sum(volchange)*100,digits=accuracy),"(= 100%?)"))
labelsumv <- tklabel(volft, textvariable=tclvarsumv, bg=hintergrund)

tclvarsumm <- tclVar(as.character(round(sum(masses)*100,digits=accuracy)))
labelsumm <- tklabel(volft, textvariable=tclvarsumm, bg=hintergrund)

tclvarden <- tclVar(as.character(round(Den,digits=4)))
labelden <- tklabel(volft, textvariable=tclvarden, bg=hintergrund)

tclvarsssa <- tclVar(as.character(round(Sssa,digits=4)))
labelsssa <- tklabel(volft, textvariable=tclvarsssa, bg=hintergrund)

tclvarmon <- tclVar(as.character(round(price,digits=2)))
labelmon <- tklabel(volft, textvariable=tclvarmon, bg=hintergrund)

#+init d10-d90 
tclvard10 <- tclVar(paste("d(10%) =",round(d10,digits=accuracy),tclvalue(dunit)))
labeld10 <- tklabel(volft, textvariable=tclvard10, bg=hintergrund)

tclvard25 <- tclVar(paste("d(25%) =",round(d25,digits=accuracy),tclvalue(dunit)))
labeld25 <- tklabel(volft, textvariable=tclvard25, bg=hintergrund)

tclvard50 <- tclVar(paste("d(50%) =",round(d50,digits=accuracy),tclvalue(dunit)))
labeld50 <- tklabel(volft, textvariable=tclvard50, bg=hintergrund)

tclvard75 <- tclVar(paste("d(75%) =",round(d75,digits=accuracy),tclvalue(dunit)))
labeld75 <- tklabel(volft, textvariable=tclvard75, bg=hintergrund)

tclvard90 <- tclVar(paste("d(90%) =",round(d90,digits=accuracy),tclvalue(dunit)))
labeld90 <- tklabel(volft, textvariable=tclvard90, bg=hintergrund)

tclvarcor <- tclVar(round(cor(tuning$VolModel,CurveFit),digits=4))
labelcor <- tklabel(volft, textvariable=tclvarcor, bg=hintergrund)

tclvarerr <- tclVar(round(sumerrsq, digits=accuracy))
labelerr <- tklabel(volft, textvariable=tclvarerr, bg=hintergrund)


#functions of buttons

saveresv <- function() {

        parent <- tk_choose.dir(getwd(), caption = "Select parent directory to save a folder with the results in")
        
        if (!is.na(parent)) {

        while (TRUE) {
            savename <- varEntryDialog(vars=c('saven'), labels=c('Name for folder to save results in:'), title='Name for saving',prompt='Files with results will be put into a newly created folder.')

            if (is.null(savename)) { break }

            if (file.exists(paste(parent,savename,sep="/"))) {

                overwrite <- tkmessageBox(title = "Naming conflict", message="A folder with the chosen name exists already. Overwrite contents?", icon = "info", type = "yesno")

                if (tclvalue(overwrite) == "yes") { break }

            } else { 
            dir.create(paste(parent,savename,sep="/"))
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

        write.table(matdb, file=paste(parent,"/",savename,"/",savename,"-recipe.csv",sep=""), row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 
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
         
         write.table(matdb, file=paste(parent,"/",savename,"/",savename,"-recipe.csv",sep=""), row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 

        } #else -> 2 files
        
        
        #write plot or whatever...

	png(paste(parent,"/",savename,"/",savename,"-plot-ft.png",sep=""))

par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

lines(x=tuning$Diameter,y=CurveFit,lty=2)

legend("bottomright", c("Model","Fit(fine-tuned)"), lty=c(1,2), inset=0.01)

dev.off(length(dev.list())+1)
        
        
        
        
        #save batch 

batchdb <- data.frame(cbind(c(round(100*volchange, digits=accuracy),round(100*sum(volchange), digits=accuracy)),c(round(100*masses, digits=accuracy),round(100*sum(masses), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2))))
	
	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
     #   rownames(batchdb) <- c(MatSelection,"Batch:")

	 write.table(batchdb,file=paste(parent,"/",savename,"/",savename,"-batch-ft.csv",sep=""),row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%(fine-tuned)","Mass%","Density","SSA","Price per MT"),quote=FALSE)

        
        #save model and fit
        
        modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(errsq, digits=accuracy),rep(NA, 4+length(modelvalue)))))
        
        
        write.table(modeldb,file=paste(parent,"/",savename,"/",savename,"-model-ft.csv",sep=""),row.names=c(rep(" ",length(tuning$Diameter)),"-----",modelparam,"-----","Corr. coeff.:","Sum. sq. dev.:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c(paste("Model and fit;Diameter in",tclvalue(dunit)),"Q(Model) in %","Q(Fit,fine-tuned) in %","Squared deviation"),quote=FALSE)
        } #!is.null savename
        } #!is.na
        
        message("  Results saved in folder: ",paste(parent,savename,sep=pathsep))
}




saverecv <- function() {

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



saveplotv <- function() {

FilePlot = tclvalue(tcl("tk_getSaveFile"))
if (FilePlot != "") {

png(FilePlot)

par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

lines(x=tuning$Diameter,y=CurveFit,lty=2)

legend("bottomright", c("Model","Fit(fine-tuned)"), lty=c(1,2), inset=0.01)

dev.off(length(dev.list())+1)
message("  Saved plot: ",FilePlot)
}

}



savebatchv <- function() {

FileBatch = tclvalue(tcl("tk_getSaveFile"))
if (FileBatch != "") {


batchdb <- data.frame(cbind(c(round(100*volchange, digits=accuracy),round(100*sum(volchange), digits=accuracy)),c(round(100*masses, digits=accuracy),round(100*sum(masses), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2))))
	
	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
     #   rownames(batchdb) <- c(MatSelection,"Batch:")

	 write.table(batchdb,file=FileBatch,row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%(fine-tuned)","Mass%","Density","SSA","Price per MT"),quote=FALSE)
message("  Saved batch: ",FileBatch)
}
}



savefitv <- function() {

FileFit = tclvalue(tcl("tk_getSaveFile"))
if (FileFit != "") {

modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(errsq, digits=accuracy),rep(NA, 4+length(modelvalue)))))
        
        
        write.table(modeldb,file=FileFit,row.names=c(rep(" ",length(tuning$Diameter)),"-----",modelparam,"-----","Corr. coeff.:","Sum. sq. dev.:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c(paste("Model and fit;Diameter in",tclvalue(dunit)),"Q(Model) in %","Q(Fit,fine-tuned) in %","Squared deviation"),quote=FALSE)
message("  Saved fit: ",FileFit)
}
}






modplotv <- function() {

message("  Plot adjustment")

ap <- varEntryDialog(vars=c('xb1', 'xb2', 'yb1', 'yb2', 'lc', 'mag'), labels=c('Lower limit of x-axis:', 'Upper limit of x-axis:', 'Lower limit of y-axis:', 'Upper limit of y-axis:', 'Logarithmic axis? (type x, y, xy, or leave empty):', 'Magnification of axis-labels etc.:'), title='Adjust plot',prompt='Adjust plot properties:',preset=c(xb[1],xb[2],yb[1],yb[2],lc,mag))

 

 xb[1] <<- as.numeric(ap[1])

 xb[2] <<- as.numeric(ap[2])

 yb[1] <<- as.numeric(ap[3])

 yb[2] <<- as.numeric(ap[4])

 lc <<- as.character(ap[5])

 mag <<- as.numeric(ap[6])

 replotv(renew="yes")
}
modplotv.but <- tkbutton(volft, text='Adjust plot', bg=knoepfe, command=modplotv)


replotv <- function(renew="no") {

message("  Open/Close/Refresh plot")

	if (is.null(dev.list())) {

        if (os=="linux") { X11(title="ParSD: Design batch") }
        else if (os=="osx") { quartz(title="ParSD: Design batch") }
        else { windows(title="ParSD: Design batch") }
        
		par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

        plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

        lines(x=tuning$Diameter,y=CurveFit,lty=2)

        legend("bottomright", c("Model","Fit"), lty=c(1,2), inset=0.01)

    } else if (renew=="yes") {
        par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

        plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

        lines(x=tuning$Diameter,y=CurveFit,lty=2)

        legend("bottomright", c("Model","Fit"), lty=c(1,2), inset=0.01)
    
    } else { 

        dev.off(length(dev.list())+1) 

    }   

}
replotv.but <- tkbutton(volft, text='Open/close plot', bg=knoepfe, command=replotv)




infomv <- function() {		
        tkmessageBox(title = "Model summary", message=modelsummary, icon = "info", type = "ok")
}
infomv.but <- tkbutton(volft, text='Model info', bg=knoepfe, command=infomv)


resetv <- function() {
		for(i in seq_along(entries)) {
			tclvalue(tclvars[[i]]) <<- volreset[i]*100
		}
		submitv()
	}
	resetv.but <- tkbutton(volft, text="Reset", command=resetv, bg=knoepfe)

	
	
	
	
	
submitv <- function() {

message("  Recalculating...")

		for(i in seq_along(entries)) {

			volchange[i] <<- as.numeric(tclvalue(tclvars[[i]]))/100
		}
		
		tclvalue(tclvarsumv) <<- paste(round(sum(volchange)*100,digits=accuracy),"(= 100%?)")
		labelsumv <<- tklabel(volft, textvariable=tclvarsumv, bg=hintergrund)
		
		Den <<- 0
        for (n in seq(from=1, to=length(MatSelection), by=1)) {
            Den <<- Den+volchange[n]*densities[n]
        }
        
        Sssa <<- 0
        price <<- 0
        masses <<- vector(length=length(MatSelection))
        for (o in seq(from=1, to=length(MatSelection), by=1)) {
            masses[o] <<- round(volchange[o]*densities[o]/Den, digits=accuracy+2)
            Sssa <<- Sssa + masses[o]*ssa[o]
            price <<- price + masses[o]*prices[o]
            tclvalue(tclvarsm[[o]]) <<- as.character(masses[o]*100)
            entriesm[[o]] <<- tklabel(volft, textvariable=tclvarsm[[o]], bg=hintergrund)
        }   
        
               
        tclvalue(tclvarsumm) <<- as.character(round(sum(masses)*100,digits=accuracy))
        labelsumm <<- tklabel(volft, textvariable=tclvarsumm, bg=hintergrund)

        tclvalue(tclvarden) <<- as.character(round(Den,digits=4))
        labelden <<- tklabel(volft, textvariable=tclvarden, bg=hintergrund)

        tclvalue(tclvarsssa) <<- as.character(round(sum(Sssa),digits=4))
        labelsssa <<- tklabel(volft, textvariable=tclvarsssa, bg=hintergrund)

        tclvalue(tclvarmon) <<- as.character(round(price,digits=2))
        labelmon <<- tklabel(volft, textvariable=tclvarmon, bg=hintergrund)
        
        d10 <<- 0
        d25 <<- 0
        d50 <<- 0
        d75 <<- 0
        d90 <<- 0
        CurveFit <<- vector(length=length(tuning$Diameter))
        errsq <<- vector(length=length(tuning$Diameter))
        sumerrsq <<- 0
        for (v in seq(from=1, to=length(tuning$Diameter), by=1)) {
 
            dummy <<- 0
            for (i in seq(from=1, to=length(MatSelection), by=1)) {
            #print(tuning[v,i+1])
                dummy <<- dummy+volchange[i]*round(tuning[v,i+1],digits=accuracy)
            }
            CurveFit[v] <<- dummy
            errsq[v] <<- ((tuning$VolModel[v]-CurveFit[v])*(tuning$VolModel[v]-CurveFit[v]))
            sumerrsq <<- sumerrsq+errsq[v]
 
            if (d10 == 0 && CurveFit[v] > 10) { #interpolate with value before for d10
                d10 <<- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (10 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
            }
            if (d25 == 0 && CurveFit[v] > 25) { #single if's because it s also possible that multiple values have to be interpolated between the same pair of data
                d25 <<- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (25 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
            }
            if (d50 == 0 && CurveFit[v] > 50) { 
                d50 <<- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (50 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
            }
            if (d75 == 0 && CurveFit[v] > 75) { 
                d75 <<- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (75 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
            }
            if (d90 == 0 && CurveFit[v] > 90) { 
                d90 <<- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (90 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
            }
        }
        
        #felder neu labeln
        
        tclvalue(tclvard10) <<- paste("d(10%) =",round(d10,digits=accuracy),tclvalue(dunit))
        labeld10 <<- tklabel(volft, textvariable=tclvard10, bg=hintergrund)

        tclvalue(tclvard25) <<- paste("d(25%) =",round(d25,digits=accuracy),tclvalue(dunit))
        labeld25 <<- tklabel(volft, textvariable=tclvard25, bg=hintergrund)

        tclvalue(tclvard50) <<- paste("d(50%) =",round(d50,digits=accuracy),tclvalue(dunit))
        labeld50 <<- tklabel(volft, textvariable=tclvard50, bg=hintergrund)

        tclvalue(tclvard75) <<- paste("d(75%) =",round(d75,digits=accuracy),tclvalue(dunit))
        labeld75 <<- tklabel(volft, textvariable=tclvard75, bg=hintergrund)

        tclvalue(tclvard90) <<- paste("d(90%) =",round(d90,digits=accuracy),tclvalue(dunit))
        labeld90 <<- tklabel(volft, textvariable=tclvard90, bg=hintergrund)


tkdelete(t.d,0,'end')

	

	tkdelete(t.m,0,'end')

	

	tkdelete(t.f,0,'end')

	

	tkdelete(t.e,0,'end')

 
for (i in seq(from=1, to=length(tuning$Diameter), by=1)) {

	

  tkinsert(t.d, "end",as.character(tuning$Diameter[i]))

   tkinsert(t.m, "end",as.character(round(tuning$VolModel[i],digits=accuracy)))

   tkinsert(t.f, "end",as.character(round(CurveFit[i],digits=accuracy)))

   tkinsert(t.e, "end",as.character(round((tuning$VolModel[i]-CurveFit[i])*(tuning$VolModel[i]-CurveFit[i]),digits=accuracy)))

  

}



tclvalue(tclvarcor) <<- round(cor(tuning$VolModel,CurveFit),digits=4)
labelcor <<- tklabel(volft, textvariable=tclvarcor, bg=hintergrund)

tclvalue(tclvarerr) <<- round(sumerrsq, digits=accuracy)
labelerr <<- tklabel(volft, textvariable=tclvarerr, bg=hintergrund)


if (!is.null(dev.list())) { 
 #dev.off(length(dev.list())+1) #close last opened interactive R window
 replotv(renew="yes")
}
 
	}
submitv.but <- tkbutton(volft, text="Calculate", bg=knoepfe, command=submitv)

#cancelVFT <- function() {		
#    tclvalue(donev)<-1
#    if ( !is.null(dev.list()) ) { 

#        dev.off(length(dev.list())+1) #close last opened interactive R window

#    }
#}
#cancel.but <- tkbutton(volft, text='Back to\nMain menu', command=cancel)

reloadVFT <- function() {		
    tclvalue(donev)<-10
}
#cancel.but <- tkbutton(volft, text='Back to\nMain menu', command=cancel)


setdm <- function(...) {

     tkyview(t.d,...)

     tkyview(t.m,...)

     tkyview(t.f,...)

     tkyview(t.e,...)

    }

	

	

	

	scr.d <- tkscrollbar(volft, repeatinterval=4,command=setdm)

	

	t.d <- tklistbox(volft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	t.m <- tklistbox(volft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	t.f <- tklistbox(volft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	t.e <- tklistbox(volft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	

	

	for (i in seq(from=1, to=length(tuning$Diameter), by=1)) {

	

  tkinsert(t.d, "end",as.character(tuning$Diameter[i]))

   tkinsert(t.m, "end",as.character(round(tuning$VolModel[i],digits=accuracy)))

   tkinsert(t.f, "end",as.character(round(CurveFit[i],digits=accuracy)))

   tkinsert(t.e, "end",as.character(round((tuning$VolModel[i]-CurveFit[i])*(tuning$VolModel[i]-CurveFit[i]),digits=accuracy)))

  

}

	

	#scr.d <- tkscrollbar(tt, repeatinterval=4,command=function(...) tkyview(t.d,...))

	#t.d <- tklistbox(tt, selectmode="browse",yscrollcommand=function(...) tkset(scr.d,...), width=20,background="white", exportselection=0)

	#tkgrid(t.d, scr.d,t.m, scr.m, t.s, scr.s, t.a, scr.a)

	#tkgrid.configure(scr.d,rowspan=4,sticky="nsw")

	#for (i in 1:100)

#{

  #tkinsert(t.d, "end", i)

#}

#---




MenuVFT <- tkmenu(volft, bg=menue)           
tkconfigure(volft, menu = MenuVFT) 

#tkadd(MenuVFT, "command", label = "Calculate", command =submit)

#tkadd(MenuVFT, "command", label = "Reset", command =reset)

#tkadd(MenuVFT, "command", label = "Model info", command =infomv)

#tkadd(MenuVFT, "command", label = "Open/Close plot", command =replotv)

#tkadd(MenuVFT, "command", label = "Adjust plot", command =modplotv)


#tkadd(MenuVFT, "command", label = "Save all", command =saveresv)
SaveMenu <- tkmenu(MenuVFT, tearoff = FALSE)
  tkadd(MenuVFT, "cascade", label = "Save", menu = SaveMenu)
  tkadd(SaveMenu, "command", label = "Recipe (Save as CSV-file!)", command =function() saverecv())
  tkadd(SaveMenu, "command", label = "Batch (Save as CSV-file!)", command =function() savebatchv())
  tkadd(SaveMenu, "command", label = "Model and fit (Save as CSV-file!)", command =function() savefitv())
  tkadd(SaveMenu, "command", label = "Graph (Save as PNG-file!)", command =function() saveplotv())
  tkadd(SaveMenu, "separator")
  tkadd(SaveMenu, "command", label = "All (in an extra folder)", command =function() saveresv())


OptWin <- tkmenu(MenuVFT, tearoff = FALSE)
  tkadd(MenuVFT, "cascade", label = "Window options", menu = OptWin)
  
  #sbheight <- 3
  #ygap <- 1
  tkadd(OptWin, "radiobutton", variable=sbheight, value='3', label="Small listbox height")
  tkadd(OptWin, "radiobutton", variable=sbheight, value='7', label="Medium listbox height")
  tkadd(OptWin, "radiobutton", variable=sbheight, value='11', label="Large listbox height")
  tkadd(OptWin, "separator")
  tkadd(OptWin, "radiobutton", variable=ygap, value='0', label="Small vertical distance between window elements")
  tkadd(OptWin, "radiobutton", variable=ygap, value='1', label="Medium vertical distance between window elements")
  tkadd(OptWin, "radiobutton", variable=ygap, value='2', label="Large vertical distance between window elements")
  
tkadd(MenuVFT, "command", label = "Apply window options", command =function() tclvalue(donev)<-6)

AbMenu <- tkmenu(MenuVFT, tearoff = FALSE)
  tkadd(MenuVFT, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","16ftdesign","16.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

QuMenu <- tkmenu(MenuVFT, tearoff = FALSE)
  tkadd(MenuVFT, "cascade", label = "Back to...", menu = QuMenu)
tkadd(QuMenu, "command", label = "Optimization results", command = function() tclvalue(donev)<-5)
tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(donev)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(donev)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(donev)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(donev)<-1)
  tkadd(QuMenu, "separator")
  tkadd(QuMenu, "command", label = "Quit ParSD app", command = function() tclvalue(donev)<-0)


tkgrid(tklabel(volft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)
tkgrid(tklabel(volft, text="Material", bg=hintergrund), tklabel(volft, text="Vol%", bg=hintergrund), tklabel(volft, text="Mass%", bg=hintergrund),tklabel(volft, text="Density", bg=hintergrund), tklabel(volft, text="SSA", bg=hintergrund), tklabel(volft, text="Price per MT", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(ttkseparator(volft), tklabel(volft, font=tkfont.create(size=1), text="", bg=hintergrund), ttkseparator(volft), ttkseparator(volft), ttkseparator(volft), ttkseparator(volft), columnspan=6, pady=0, padx=10, sticky="we")

for (i in seq(from=1, to=length(MatSelection), by=1)) {
    tkgrid(tklabel(volft, text=MatSelection[i], bg=hintergrund), entries[[i]], entriesm[[i]], tklabel(volft,text=as.character(round(densities[i], digits=4)), bg=hintergrund), tklabel(volft, text=as.character(round(ssa[i], digits=4)), bg=hintergrund), tklabel(volft, text=as.character(round(prices[i], digits=2)), bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)
}

tkgrid(ttkseparator(volft), tklabel(volft, font=tkfont.create(size=1), text="", bg=hintergrund), ttkseparator(volft), ttkseparator(volft), ttkseparator(volft), ttkseparator(volft), columnspan=6, pady=0, padx=10, sticky="we")

tkgrid(tklabel(volft, text="Batch:", bg=hintergrund), labelsumv, labelsumm, labelden, labelsssa, labelmon, pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(volft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(volft, text="", bg=hintergrund), submitv.but, resetv.but, tklabel(volft, text="", bg=hintergrund),tklabel(volft, text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(volft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(volft, text="d(CPFT) of batch:", bg=hintergrund), labeld10, labeld25, labeld50, labeld75, labeld90, pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(ttkseparator(volft), columnspan=36, pady=tclvalue(ygap), padx=10, sticky="we")

tkgrid(tklabel(volft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(volft, text="Fit quality:", bg=hintergrund), replotv.but, modplotv.but, tklabel(volft, text="Corr. coeff.:", bg=hintergrund), labelcor, tklabel(volft,text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(volft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

#tkgrid(replotv.but, modplotv.but, infomv.but, tklabel(volft, text="", bg=hintergrund),tklabel(volft, text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(volft,text="", bg=hintergrund), tklabel(volft, text="Diameter", bg=hintergrund), tklabel(volft, text="Model", bg=hintergrund), tklabel(volft, text="Batch", bg=hintergrund), tklabel(volft, text="Squared deviation", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

	
	tkgrid(tklabel(volft,text="(use scrollbar\nto move lists\nsimulaneously)", bg=hintergrund),t.d, t.m, t.f, t.e, scr.d, pady=tclvalue(ygap), padx=10, columnspan=6)

	tkgrid.configure(scr.d,rowspan=tclvalue(sbheight),sticky="nsw")
#rowspan war 4
	

tkgrid(tklabel(volft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

	tkgrid(tklabel(volft, text="", bg=hintergrund), tklabel(volft, text="", bg=hintergrund), infomv.but, tklabel(volft, text="Sum sq. dev.:", bg=hintergrund), labelerr, tklabel(volft, text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(volft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)


tkfocus(volft)
# Do not proceed with the following code until the variable done is non-zero.
#   (But other processes can still run, i.e. the system is not frozen.)
tkwait.variable(donev)

#if (tclvalue(donev) == 2) { 
#tkdestroy(volft) 
#}

done <- tclvalue(donev)
tkdestroy(volft) 

message("  Chosen action (0 Quit, 1 Main menu, 2 Material Selection, 3 Bounds, 4 Model selection, 5 Optimization results, 6 Apply window options): ",done,"\n")

if (done < 6) {
if ( !is.null(dev.list()) ) { 

        dev.off(length(dev.list())+1) #close last opened interactive R window

    }
return(done)

}


} #while loop

}








weightft <- function(massreset,masschange,volumes,densities,ssa,prices,Den,Sssa,price,d10,d25,d50,d75,d90,tuning,CurveFit,sumerrsq,MatSelection,MatSelection1,MatSelection2,dbinfo,dbinfo2,xb,yb,lc,mag,errsq) {

message("BEGIN wt%-finetuning function\n")

while (TRUE) {

message("Loading wt%-finetuning window")

#print(volchange)
#print(masses)
#print(ssa)
#print(Sssa)
#print(prices)
#print(price)
#print(densities)
#print(Den)


massft <- tktoplevel(bg=hintergrund)
tkwm.title(massft, "Fine-Tuning in Mass%")	
 tkraise(massft)

donev <- tclVar(0)

tkbind(massft,"<Destroy>",function() tclvalue(donev)<-1)

#init values

entries <- list()
entriesm <- list()
tclvars <- list()
tclvarsm <- list()

for(i in seq(from=1, to=length(MatSelection), by=1)) {

        tclvarsm[[i]] <- tclVar(as.character(round(masschange[i]*100,digits=accuracy)))

        tclvars[[i]] <- tclVar(round(volumes[i]*100,digits=accuracy))

		entries[[i]] <- tklabel(massft, textvariable=tclvars[[i]], bg=hintergrund)
		
		entriesm[[i]] <- tkentry(massft, textvariable=tclvarsm[[i]])

}



tclvarsumm <- tclVar(paste(round(sum(masschange)*100,digits=accuracy),"(= 100%?)"))
labelsumm <- tklabel(massft, textvariable=tclvarsumm, bg=hintergrund)

tclvarsumv <- tclVar(as.character(round(sum(volumes)*100,digits=accuracy)))
labelsumv <- tklabel(massft, textvariable=tclvarsumv, bg=hintergrund)



tclvarden <- tclVar(as.character(round(Den,digits=4)))
labelden <- tklabel(massft, textvariable=tclvarden, bg=hintergrund)

tclvarsssa <- tclVar(as.character(round(Sssa,digits=4)))
labelsssa <- tklabel(massft, textvariable=tclvarsssa, bg=hintergrund)

tclvarmon <- tclVar(as.character(round(price,digits=2)))
labelmon <- tklabel(massft, textvariable=tclvarmon, bg=hintergrund)

#+init d10-d90 
tclvard10 <- tclVar(paste("d(10%) =",round(d10,digits=accuracy),tclvalue(dunit)))
labeld10 <- tklabel(massft, textvariable=tclvard10, bg=hintergrund)

tclvard25 <- tclVar(paste("d(25%) =",round(d25,digits=accuracy),tclvalue(dunit)))
labeld25 <- tklabel(massft, textvariable=tclvard25, bg=hintergrund)

tclvard50 <- tclVar(paste("d(50%) =",round(d50,digits=accuracy),tclvalue(dunit)))
labeld50 <- tklabel(massft, textvariable=tclvard50, bg=hintergrund)

tclvard75 <- tclVar(paste("d(75%) =",round(d75,digits=accuracy),tclvalue(dunit)))
labeld75 <- tklabel(massft, textvariable=tclvard75, bg=hintergrund)

tclvard90 <- tclVar(paste("d(90%) =",round(d90,digits=accuracy),tclvalue(dunit)))
labeld90 <- tklabel(massft, textvariable=tclvard90, bg=hintergrund)

tclvarcor <- tclVar(round(cor(tuning$VolModel,CurveFit),digits=4))
labelcor <- tklabel(massft, textvariable=tclvarcor, bg=hintergrund)

tclvarerr <- tclVar(round(sumerrsq, digits=accuracy))
labelerr <- tklabel(massft, textvariable=tclvarerr, bg=hintergrund)



#functions of buttons

saveresm <- function() {

        parent <- tk_choose.dir(getwd(), caption = "Select parent directory to save a folder with the results in")
        
        if (!is.na(parent)) {

        while (TRUE) {
            savename <- varEntryDialog(vars=c('saven'), labels=c('Name for folder to save results in:'), title='Name for saving',prompt='Files with results will be put into a newly created folder.')

            if (is.null(savename)) { break }

            if (file.exists(paste(parent,savename,sep="/"))) {

                overwrite <- tkmessageBox(title = "Naming conflict", message="A folder with the chosen name exists already. Overwrite contents?", icon = "info", type = "yesno")

                if (tclvalue(overwrite) == "yes") { break }

            } else { 
            dir.create(paste(parent,savename,sep="/"))
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

        write.table(matdb, file=paste(parent,"/",savename,"/",savename,"-recipe.csv",sep=""), row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 
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
         
         write.table(matdb, file=paste(parent,"/",savename,"/",savename,"-recipe.csv",sep=""), row.names=FALSE,dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="") 

        } #else -> 2 files
        
        
        #write plot or whatever...

	png(paste(parent,"/",savename,"/",savename,"-plot-ft.png",sep=""))

par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

lines(x=tuning$Diameter,y=CurveFit,lty=2)

legend("bottomright", c("Model","Fit(fine-tuned)"), lty=c(1,2), inset=0.01)

dev.off(length(dev.list())+1)
        
        
        
        
        #save batch 

batchdb <- data.frame(cbind(c(round(100*volumes, digits=accuracy),round(100*sum(volumes), digits=accuracy)),c(round(100*masschange, digits=accuracy),round(100*sum(masschange), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2))))
	
	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
     #   rownames(batchdb) <- c(MatSelection,"Batch:")

	 write.table(batchdb,file=paste(parent,"/",savename,"/",savename,"-batch-ft.csv",sep=""),row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%(fine-tuned)","Mass%","Density","SSA","Price per MT"),quote=FALSE)

        
        #save model and fit
        
        modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(errsq, digits=accuracy),rep(NA, 4+length(modelvalue)))))
        
        
        write.table(modeldb,file=paste(parent,"/",savename,"/",savename,"-model-ft.csv",sep=""),row.names=c(rep(" ",length(tuning$Diameter)),"-----",modelparam,"-----","Corr. coeff.:","Sum. sq. dev.:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c(paste("Model and fit;Diameter in",tclvalue(dunit)),"Q(Model) in %","Q(Fit,fine-tuned) in %","Squared deviation"),quote=FALSE)
        } #!is.null savename
        } #!is.na
        message("  Results saved in folder: ",paste(parent,savename,sep=pathsep))
}












saverecm <- function() {

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






saveplotm <- function() {

FilePlot = tclvalue(tcl("tk_getSaveFile"))
if (FilePlot != "") {

png(FilePlot)

par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

lines(x=tuning$Diameter,y=CurveFit,lty=2)

legend("bottomright", c("Model","Fit(fine-tuned)"), lty=c(1,2), inset=0.01)

dev.off(length(dev.list())+1)
message("  Saved plot: ",FilePlot)
}

}







savebatchm <- function() {

FileBatch = tclvalue(tcl("tk_getSaveFile"))
if (FileBatch != "") {

batchdb <- data.frame(cbind(c(round(100*volumes, digits=accuracy),round(100*sum(volumes), digits=accuracy)),c(round(100*masschange, digits=accuracy),round(100*sum(masschange), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2))))
	
	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
     #   rownames(batchdb) <- c(MatSelection,"Batch:")

	 write.table(batchdb,file=FileBatch,row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%(fine-tuned)","Mass%","Density","SSA","Price per MT"),quote=FALSE)
message("  Saved batch: ",FileBatch)
}
}




savefitm <- function() {

FileFit = tclvalue(tcl("tk_getSaveFile"))
if (FileFit != "") {

modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(errsq, digits=accuracy),rep(NA, 4+length(modelvalue)))))
        
        
        write.table(modeldb,file=FileFit,row.names=c(rep(" ",length(tuning$Diameter)),"-----",modelparam,"-----","Corr. coeff.:","Sum. sq. dev.:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c(paste("Model and fit;Diameter in",tclvalue(dunit)),"Q(Model) in %","Q(Fit,fine-tuned) in %","Squared deviation"),quote=FALSE)
message("  Saved fit: ",FileFit)
}
}


modplotm <- function() {

message("  Plot adjustment")

ap <- varEntryDialog(vars=c('xb1', 'xb2', 'yb1', 'yb2', 'lc', 'mag'), labels=c('Lower limit of x-axis:', 'Upper limit of x-axis:', 'Lower limit of y-axis:', 'Upper limit of y-axis:', 'Logarithmic axis? (type x, y, xy, or leave empty):', 'Magnification of axis-labels etc.:'), title='Adjust plot',prompt='Adjust plot properties:',preset=c(xb[1],xb[2],yb[1],yb[2],lc,mag))

 

 xb[1] <<- as.numeric(ap[1])

 xb[2] <<- as.numeric(ap[2])

 yb[1] <<- as.numeric(ap[3])

 yb[2] <<- as.numeric(ap[4])

 lc <<- as.character(ap[5])

 mag <<- as.numeric(ap[6])

 replotm(renew="yes")
}
modplotm.but <- tkbutton(massft, text='Adjust plot', bg=knoepfe, command=modplotm)



replotm <- function(renew="no") {

message("  Open/Close/Refresh plot")

	if (is.null(dev.list())) {

        if (os=="linux") { X11(title="ParSD: Design batch") }
        else if (os=="osx") { quartz(title="ParSD: Design batch") }
        else { windows(title="ParSD: Design batch") }
        
		par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

        plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

        lines(x=tuning$Diameter,y=CurveFit,lty=2)

        legend("bottomright", c("Model","Fit"), lty=c(1,2), inset=0.01)

    } else if (renew=="yes") {
        par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

        plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

        lines(x=tuning$Diameter,y=CurveFit,lty=2)

        legend("bottomright", c("Model","Fit"), lty=c(1,2), inset=0.01)
    
    } else { 

        dev.off(length(dev.list())+1) 

    }   

}
replotm.but <- tkbutton(massft, text='Open/close plot', bg=knoepfe, command=replotm)








infomm <- function() {		
        tkmessageBox(title = "Model summary", message=modelsummary, icon = "info", type = "ok")
}
infomm.but <- tkbutton(massft, text='Model info', bg=knoepfe, command=infomm)



resetm <- function() {
		for(i in seq_along(entriesm)) {
			tclvalue(tclvarsm[[i]]) <<- massreset[i]*100
		}
		submitm()
	}
	resetm.but <- tkbutton(massft, text="Reset", command=resetm, bg=knoepfe)

	
	


	
	
submitm <- function() {

message("  Recalculating...")

		for(i in seq_along(entriesm)) {

			masschange[i] <<- as.numeric(tclvalue(tclvarsm[[i]]))/100
		}
		
		tclvalue(tclvarsumm) <<- paste(round(sum(masschange)*100,digits=accuracy),"(= 100%?)")
		labelsumm <<- tklabel(massft, textvariable=tclvarsumm, bg=hintergrund)
		
		Den <<- 0
        for (n in seq(from=1, to=length(MatSelection), by=1)) {
            Den <<- Den+masschange[n]/densities[n]
        }
        Den <<- 1/Den
        
        Sssa <<- 0
        price <<- 0
        volumes <<- vector(length=length(MatSelection))
        for (o in seq(from=1, to=length(MatSelection), by=1)) {
            volumes[o] <<- round(masschange[o]*Den/densities[o], digits=accuracy+2)
            Sssa <<- Sssa + masschange[o]*ssa[o]
            price <<- price + masschange[o]*prices[o]
            tclvalue(tclvars[[o]]) <<- as.character(volumes[o]*100)
            entries[[o]] <<- tklabel(massft, textvariable=tclvars[[o]], bg=hintergrund)
        }   
        
        
               
        tclvalue(tclvarsumv) <<- as.character(round(sum(volumes)*100,digits=accuracy))
        labelsumv <<- tklabel(massft, textvariable=tclvarsumv, bg=hintergrund)

        tclvalue(tclvarden) <<- as.character(round(Den,digits=4))
        labelden <<- tklabel(massft, textvariable=tclvarden, bg=hintergrund)

        tclvalue(tclvarsssa) <<- as.character(round(sum(Sssa),digits=4))
        labelsssa <<- tklabel(massft, textvariable=tclvarsssa, bg=hintergrund)

        tclvalue(tclvarmon) <<- as.character(round(price,digits=2))
        labelmon <<- tklabel(massft, textvariable=tclvarmon, bg=hintergrund)
        
        
        
        d10 <<- 0
        d25 <<- 0
        d50 <<- 0
        d75 <<- 0
        d90 <<- 0
        CurveFit <<- vector(length=length(tuning$Diameter))
        errsq <<- vector(length=length(tuning$Diameter))
        sumerrsq <<- 0
        for (v in seq(from=1, to=length(tuning$Diameter), by=1)) {
 
            dummy <<- 0
            for (i in seq(from=1, to=length(MatSelection), by=1)) {
            #print(tuning[v,i+1])
                dummy <<- dummy+volumes[i]*round(tuning[v,i+1],digits=accuracy)
            }
            CurveFit[v] <<- dummy
            errsq[v] <<- ((tuning$VolModel[v]-CurveFit[v])*(tuning$VolModel[v]-CurveFit[v]))
            sumerrsq <<- sumerrsq+errsq[v]
 
            if (d10 == 0 && CurveFit[v] > 10) { #interpolate with value before for d10
                d10 <<- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (10 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
            }
            if (d25 == 0 && CurveFit[v] > 25) { #single if's because it s also possible that multiple values have to be interpolated between the same pair of data
                d25 <<- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (25 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
            }
            if (d50 == 0 && CurveFit[v] > 50) { 
                d50 <<- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (50 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
            }
            if (d75 == 0 && CurveFit[v] > 75) { 
                d75 <<- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (75 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
            }
            if (d90 == 0 && CurveFit[v] > 90) { 
                d90 <<- tuning$Diameter[v-1] + (tuning$Diameter[v] - tuning$Diameter[v-1]) * (90 - CurveFit[v-1]) / (CurveFit[v] - CurveFit[v-1])
            }
        }
        
        #felder neu labeln
        
        tclvalue(tclvard10) <<- paste("d(10%) =",round(d10,digits=accuracy),tclvalue(dunit))
        labeld10 <<- tklabel(massft, textvariable=tclvard10, bg=hintergrund)

        tclvalue(tclvard25) <<- paste("d(25%) =",round(d25,digits=accuracy),tclvalue(dunit))
        labeld25 <<- tklabel(massft, textvariable=tclvard25, bg=hintergrund)

        tclvalue(tclvard50) <<- paste("d(50%) =",round(d50,digits=accuracy),tclvalue(dunit))
        labeld50 <<- tklabel(massft, textvariable=tclvard50, bg=hintergrund)

        tclvalue(tclvard75) <<- paste("d(75%) =",round(d75,digits=accuracy),tclvalue(dunit))
        labeld75 <<- tklabel(massft, textvariable=tclvard75, bg=hintergrund)

        tclvalue(tclvard90) <<- paste("d(90%) =",round(d90,digits=accuracy),tclvalue(dunit))
        labeld90 <<- tklabel(massft, textvariable=tclvard90, bg=hintergrund)


tkdelete(t.d,0,'end')

	

	tkdelete(t.m,0,'end')

	

	tkdelete(t.f,0,'end')

	

	tkdelete(t.e,0,'end')

 
for (i in seq(from=1, to=length(tuning$Diameter), by=1)) {

	

  tkinsert(t.d, "end",as.character(tuning$Diameter[i]))

   tkinsert(t.m, "end",as.character(round(tuning$VolModel[i],digits=accuracy)))

   tkinsert(t.f, "end",as.character(round(CurveFit[i],digits=accuracy)))

   tkinsert(t.e, "end",as.character(round((tuning$VolModel[i]-CurveFit[i])*(tuning$VolModel[i]-CurveFit[i]),digits=accuracy)))

  

}



tclvalue(tclvarcor) <<- round(cor(tuning$VolModel,CurveFit),digits=4)
labelcor <<- tklabel(massft, textvariable=tclvarcor, bg=hintergrund)

tclvalue(tclvarerr) <<- round(sumerrsq, digits=accuracy)
labelerr <<- tklabel(massft, textvariable=tclvarerr, bg=hintergrund)


if (!is.null(dev.list())) { 
 #dev.off(length(dev.list())+1) #close last opened interactive R window
 replotm(renew="yes")
}
 
	}
submitm.but <- tkbutton(massft, text="Calculate", bg=knoepfe, command=submitm)

#cancelVFT <- function() {		
#    tclvalue(donev)<-1
#    if ( !is.null(dev.list()) ) { 

#        dev.off(length(dev.list())+1) #close last opened interactive R window

#    }
#}
#cancel.but <- tkbutton(massft, text='Back to\nMain menu', command=cancel)






reloadMFT <- function() {		
    tclvalue(donev)<-10
}
#cancel.but <- tkbutton(massft, text='Back to\nMain menu', command=cancel)


setdm <- function(...) {

     tkyview(t.d,...)

     tkyview(t.m,...)

     tkyview(t.f,...)

     tkyview(t.e,...)

    }

	

	

	scr.d <- tkscrollbar(massft, repeatinterval=4,command=setdm)

	

	t.d <- tklistbox(massft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	t.m <- tklistbox(massft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	t.f <- tklistbox(massft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	t.e <- tklistbox(massft, selectmode="multiple",activestyle="none",yscrollcommand=function(...) tkset(scr.d,...), width=20,height=tclvalue(sbheight),background="white", exportselection=0)

	

	

	

	for (i in seq(from=1, to=length(tuning$Diameter), by=1)) {

	

  tkinsert(t.d, "end",as.character(tuning$Diameter[i]))

   tkinsert(t.m, "end",as.character(round(tuning$VolModel[i],digits=accuracy)))

   tkinsert(t.f, "end",as.character(round(CurveFit[i],digits=accuracy)))

   tkinsert(t.e, "end",as.character(round((tuning$VolModel[i]-CurveFit[i])*(tuning$VolModel[i]-CurveFit[i]),digits=accuracy)))

  

}

	

	#scr.d <- tkscrollbar(tt, repeatinterval=4,command=function(...) tkyview(t.d,...))

	#t.d <- tklistbox(tt, selectmode="browse",yscrollcommand=function(...) tkset(scr.d,...), width=20,background="white", exportselection=0)

	#tkgrid(t.d, scr.d,t.m, scr.m, t.s, scr.s, t.a, scr.a)

	#tkgrid.configure(scr.d,rowspan=4,sticky="nsw")

	#for (i in 1:100)

#{

  #tkinsert(t.d, "end", i)

#}

#---




MenuVFT <- tkmenu(massft, bg=menue)           
tkconfigure(massft, menu = MenuVFT) 

#tkadd(MenuVFT, "command", label = "Calculate", command =submit)

#tkadd(MenuVFT, "command", label = "Reset", command =reset)

#tkadd(MenuVFT, "command", label = "Model info", command =infomv)

#tkadd(MenuVFT, "command", label = "Open/Close plot", command =replotv)

#tkadd(MenuVFT, "command", label = "Adjust plot", command =modplotv)


SaveMenu <- tkmenu(MenuVFT, tearoff = FALSE)
  tkadd(MenuVFT, "cascade", label = "Save", menu = SaveMenu)
  tkadd(SaveMenu, "command", label = "Recipe (Save as CSV-file!)", command =function() saverecm())
  tkadd(SaveMenu, "command", label = "Batch (Save as CSV-file!)", command =function() savebatchm())
  tkadd(SaveMenu, "command", label = "Model and fit (Save as CSV-file!)", command =function() savefitm())
  tkadd(SaveMenu, "command", label = "Graph (Save as PNG-file!)", command =function() saveplotm())
  tkadd(SaveMenu, "separator")
  tkadd(SaveMenu, "command", label = "All (in an extra folder)", command =function() saveresm())


OptWin <- tkmenu(MenuVFT, tearoff = FALSE)
  tkadd(MenuVFT, "cascade", label = "Window options", menu = OptWin)
  
  #sbheight <- 3
  #ygap <- 1
  tkadd(OptWin, "radiobutton", variable=sbheight, value='3', label="Small listbox height")
  tkadd(OptWin, "radiobutton", variable=sbheight, value='7', label="Medium listbox height")
  tkadd(OptWin, "radiobutton", variable=sbheight, value='11', label="Large listbox height")
  tkadd(OptWin, "separator")
  tkadd(OptWin, "radiobutton", variable=ygap, value='0', label="Small vertical distance between window elements")
  tkadd(OptWin, "radiobutton", variable=ygap, value='1', label="Medium vertical distance between window elements")
  tkadd(OptWin, "radiobutton", variable=ygap, value='2', label="Large vertical distance between window elements")
  
tkadd(MenuVFT, "command", label = "Apply window options", command =function() tclvalue(donev)<-6)

AbMenu <- tkmenu(MenuVFT, tearoff = FALSE)
  tkadd(MenuVFT, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","16ftdesign","16.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  

QuMenu <- tkmenu(MenuVFT, tearoff = FALSE)
  tkadd(MenuVFT, "cascade", label = "Back to...", menu = QuMenu)
tkadd(QuMenu, "command", label = "Optimization results", command = function() tclvalue(donev)<-5)
tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(donev)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(donev)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(donev)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(donev)<-1)
  tkadd(QuMenu, "separator")
  tkadd(QuMenu, "command", label = "Quit ParSD app", command = function() tclvalue(donev)<-0)


#------------------

#hier weiter:

#------------------


	
tkgrid(tklabel(massft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)
tkgrid(tklabel(massft, text="Material", bg=hintergrund), tklabel(massft, text="Vol%", bg=hintergrund), tklabel(massft, text="Mass%", bg=hintergrund),tklabel(massft, text="Density", bg=hintergrund), tklabel(massft, text="SSA", bg=hintergrund), tklabel(massft, text="Price per MT", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(ttkseparator(massft), ttkseparator(massft), tklabel(massft, font=tkfont.create(size=1), text="", bg=hintergrund), ttkseparator(massft), ttkseparator(massft), ttkseparator(massft), columnspan=6, pady=0, padx=10, sticky="we")

for (i in seq(from=1, to=length(MatSelection), by=1)) {
    tkgrid(tklabel(massft, text=MatSelection[i], bg=hintergrund), entries[[i]], entriesm[[i]], tklabel(massft,text=as.character(round(densities[i], digits=4)), bg=hintergrund), tklabel(massft, text=as.character(round(ssa[i], digits=4)), bg=hintergrund), tklabel(massft, text=as.character(round(prices[i], digits=2)), bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)
}

tkgrid(ttkseparator(massft), ttkseparator(massft), tklabel(massft, font=tkfont.create(size=1), text="", bg=hintergrund), ttkseparator(massft), ttkseparator(massft), ttkseparator(massft), columnspan=6, pady=0, padx=10, sticky="we")

tkgrid(tklabel(massft, text="Batch:", bg=hintergrund), labelsumv, labelsumm, labelden, labelsssa, labelmon, pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(massft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(massft, text="", bg=hintergrund), submitm.but, resetm.but, tklabel(massft, text="", bg=hintergrund),tklabel(massft, text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(massft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(massft, text="d(CPFT) of batch:", bg=hintergrund), labeld10, labeld25, labeld50, labeld75, labeld90, pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(ttkseparator(massft), columnspan=36, pady=tclvalue(ygap), padx=10, sticky="we")

tkgrid(tklabel(massft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(massft, text="Fit quality:", bg=hintergrund), replotm.but, modplotm.but, tklabel(massft, text="Corr. coeff.:", bg=hintergrund), labelcor, tklabel(massft,text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(massft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

#tkgrid(replotv.but, modplotv.but, infomv.but, tklabel(massft, text="", bg=hintergrund),tklabel(massft, text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(massft,text="", bg=hintergrund), tklabel(massft, text="Diameter", bg=hintergrund), tklabel(massft, text="Model", bg=hintergrund), tklabel(massft, text="Batch", bg=hintergrund), tklabel(massft, text="Squared deviation", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

	
	tkgrid(tklabel(massft,text="(use scrollbar\nto move lists\nsimulaneously)", bg=hintergrund),t.d, t.m, t.f, t.e, scr.d, pady=tclvalue(ygap), padx=10, columnspan=6)

	tkgrid.configure(scr.d,rowspan=tclvalue(sbheight),sticky="nsw")
#rowspan war 4
	

tkgrid(tklabel(massft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)

	tkgrid(tklabel(massft, text="", bg=hintergrund), tklabel(massft, text="", bg=hintergrund), infomm.but, tklabel(massft, text="Sum sq. dev.:", bg=hintergrund), labelerr, tklabel(massft, text="", bg=hintergrund), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(massft, font=tkfont.create(size=1), text="", bg=hintergrund),pady=tclvalue(ygap), padx=10, columnspan=6)


tkfocus(massft)
# Do not proceed with the following code until the variable done is non-zero.
#   (But other processes can still run, i.e. the system is not frozen.)
tkwait.variable(donev)

#if (tclvalue(donev) == 2) { 
#tkdestroy(massft) 
#}

done <- tclvalue(donev)
tkdestroy(massft) 

message("  Chosen action (0 Quit, 1 Main menu, 2 Material Selection, 3 Bounds, 4 Model selection, 5 Optimization results, 6 Apply window options): ",done,"\n")

if (done < 6) {
if ( !is.null(dev.list()) ) { 

        dev.off(length(dev.list())+1) #close last opened interactive R window

    }
return(done)

}


} #while loop

}
