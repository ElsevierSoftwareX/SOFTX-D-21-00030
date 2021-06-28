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





design <- function (MatSelection,preparation,densities,ssa,prices,dbinfo,dbinfo2,MatSelection1,MatSelection2,boundsLowMass,boundsUpMass,boundsLowIn,boundsUpIn) {

#return 0 for quit ParSD
  #return 1 for back to main menu
  #return 2 for back to open recipe or database
  #return 3 for back to bounds
  #return 4 for back to model selection
  
  message("BEGIN design function\n")

  preparation <- cbind(preparation, VolModel)
  colnames(preparation)[22] <- "VolModel"
  
  tuning <- preparation #save preparation in tuning
  
  #print(preparation$VolModel)
  
  #put colnames in temp from preparation and exchange with defined names for calculation

  SaveColNames <- colnames(preparation)
  colnames(preparation) <- c("Diameter","Mat1","Mat2","Mat3","Mat4","Mat5","Mat6","Mat7","Mat8","Mat9","Mat10","Mat11","Mat12","Mat13","Mat14","Mat15","Mat16","Mat17","Mat18","Mat19","Mat20","VolModel")
  
  
  
 



mcheck <- 0
runx <- 0
run <- 0

#if (BoundsType=="Yes, in mass percent") {
pbm <- tkProgressBar(title = "Optimization progress", min = 0, max = 0, width = 350)
#}

setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))


while (mcheck!=1) {


if (run+runx > limdesign) {

message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}








check <- 0

if (length(MatSelection) == 20) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  
  
  
  
  
  
  
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10+ v11*Mat11+ v12*Mat12+ v13*Mat13+ v14*Mat14+ v15*Mat15+ v16*Mat16+ v17*Mat17+ v18*Mat18+ v19*Mat19+ v20*Mat20, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10], v11=starterr[11], v12=starterr[12], v13=starterr[13], v14=starterr[14], v15=starterr[15], v16=starterr[16], v17=starterr[17], v18=starterr[18], v19=starterr[19], v20=starterr[20]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error: ",paste(geterrmessage()))
  
  close(pbm) 
  
  winmod <- tktoplevel(bg=hintergrund)
  tkwm.title(winmod,"Optimization error")
  tkraise(winmod)

  cm <- tclVar(0)

  tkbind(winmod,"<Destroy>",function() tclvalue(cm)<-1) 
  
  Menu <- tkmenu(winmod, bg=menue)           
  tkconfigure(winmod, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage()), bg=hintergrund), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection.", bg=hintergrund), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 19) {
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10+ v11*Mat11+ v12*Mat12+ v13*Mat13+ v14*Mat14+ v15*Mat15+ v16*Mat16+ v17*Mat17+ v18*Mat18+ v19*Mat19, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10], v11=starterr[11], v12=starterr[12], v13=starterr[13], v14=starterr[14], v15=starterr[15], v16=starterr[16], v17=starterr[17], v18=starterr[18], v19=starterr[19]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 18) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10+ v11*Mat11+ v12*Mat12+ v13*Mat13+ v14*Mat14+ v15*Mat15+ v16*Mat16+ v17*Mat17+ v18*Mat18, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10], v11=starterr[11], v12=starterr[12], v13=starterr[13], v14=starterr[14], v15=starterr[15], v16=starterr[16], v17=starterr[17], v18=starterr[18]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 17) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10+ v11*Mat11+ v12*Mat12+ v13*Mat13+ v14*Mat14+ v15*Mat15+ v16*Mat16+ v17*Mat17, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10], v11=starterr[11], v12=starterr[12], v13=starterr[13], v14=starterr[14], v15=starterr[15], v16=starterr[16], v17=starterr[17]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 16) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10+ v11*Mat11+ v12*Mat12+ v13*Mat13+ v14*Mat14+ v15*Mat15+ v16*Mat16, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10], v11=starterr[11], v12=starterr[12], v13=starterr[13], v14=starterr[14], v15=starterr[15], v16=starterr[16]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 15) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10+ v11*Mat11+ v12*Mat12+ v13*Mat13+ v14*Mat14+ v15*Mat15, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10], v11=starterr[11], v12=starterr[12], v13=starterr[13], v14=starterr[14], v15=starterr[15]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 14) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10+ v11*Mat11+ v12*Mat12+ v13*Mat13+ v14*Mat14, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10], v11=starterr[11], v12=starterr[12], v13=starterr[13], v14=starterr[14]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 13) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10+ v11*Mat11+ v12*Mat12+ v13*Mat13, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10], v11=starterr[11], v12=starterr[12], v13=starterr[13]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 12) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10+ v11*Mat11+ v12*Mat12, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10], v11=starterr[11], v12=starterr[12]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 11) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10+ v11*Mat11, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10], v11=starterr[11]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 10) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9+ v10*Mat10, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9], v10=starterr[10]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 9) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8+ v9*Mat9, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8], v9=starterr[9]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 8) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7+ v8*Mat8, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7], v8=starterr[8]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100))
  
  run=run+1
 }
} else if (length(MatSelection) == 7) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6+ v7*Mat7, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6], v7=starterr[7]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100))
  
  run=run+1
 }
} else if (length(MatSelection) == 6) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5+ v6*Mat6, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5], v6=starterr[6]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 5) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4+ v5*Mat5, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4], v5=starterr[5]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 4) {
 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3+ v4*Mat4, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3], v4=starterr[4]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  #print("Koeff.:")
  #print(coef(fit))
  #print(paste("Summe Koeff bzw Vol%-Total:",sum(coef(fit))))

  check = round(sum(coef(fit)), digits=accuracy+2)
  
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))
  
  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 3) {
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2 + v3*Mat3, data=preparation, start=list(v1=starterr[1], v2=starterr[2], v3=starterr[3]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  check = round(sum(coef(fit)), digits=accuracy+2)
 
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))

  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 2) {
 
 while (check!=1) {

  if (run+runx > limdesign) {

  message("  Limit of iterations reached: ",limdesign,"\n")
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1 + v2*Mat2, data=preparation, start=list(v1=starterr[1], v2=starterr[2]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  check = round(sum(coef(fit)), digits=accuracy+2)
 
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))

  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
} else if (length(MatSelection) == 1) {

 
 
 while (check!=1) {

  if (run+runx > limdesign) {
  
  message("  Limit of iterations reached: ",limdesign,"\n")

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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text="Limit of iterations reached.\n\nMight be a rounding issue or due to complicated fittings.\n\nSetting or changing bounds might help."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)
  var <- tclvalue(cm)
 tkdestroy(winmod)
  return(var) 

}
  

  starterr <- boundsLowIn
  
  for (i in seq(from=0.1, to=1, by=0.1)) {
  
   fit <- NULL
   try(fit <- nls(VolModel ~ v1*Mat1, data=preparation, start=list(v1=starterr[1]), algorithm="port", lower=boundsLowIn, upper=boundsUpIn),silent=TRUE)
  
   if(!is.null(fit)) { break }
  
   for (j in seq(from=1, to=length(MatSelection), by=1)) {
    starterr[j] <- (1-i)*boundsLowIn[j]+i*boundsUpIn[j]
   }
  }
  
  if (is.null(fit)) { #happens, if nls did not work
  
  message("  Optimization error:",paste(geterrmessage()))
  
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
  tkadd(AbMenu, "command", label = "Help (Optimization Design-function)", command =function() manual(man=paste("Help","12design","12.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(cm)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(cm)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(cm)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(cm)<-1)
  
  tkgrid(tklabel(winmod, text=paste(geterrmessage())), pady=10, padx=10)  
  tkgrid(tklabel(winmod, text="Typically can be solved by better starting values. You may\ntry to set some bounds or change the raw material selection."), pady=10, padx=10)

  tkfocus(winmod)
  tkwait.variable(cm)

  var <- tclvalue(cm)
  
  tkdestroy(winmod)
  
  return(var) 

   #break
   }

  check = round(sum(coef(fit)), digits=accuracy+2)
 
  setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))

  preparation <- rbind(preparation, c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)) 
  
  run=run+1
 }
}

#close(pb) #close progress bar

#uu hier eiter

Den <- 0
for (n in seq(from=1, to=length(MatSelection), by=1)) {
 Den <- Den+coef(fit)[n]*densities[n]
}
#print(paste("Versatzdichte:",Den))



#get momentaneous mass percents
Sssa <- 0
price <- 0
masses <- vector(length=length(MatSelection))
for (o in seq(from=1, to=length(MatSelection), by=1)) {
 masses[o] <- round(coef(fit)[o]*densities[o]/Den, digits=accuracy+2)
 Sssa <- Sssa + masses[o]*ssa[o]
 price <- price + masses[o]*prices[o]
}

mcheck <- 1

if (!is.na(Den)) {
#bounds anpassen falls masses nict stimmen
for (p in seq(from=1, to=length(MatSelection), by=1)) {
 if (round(masses[p],digits=accuracy+2) < round(boundsLowMass[p],digits=accuracy+2)) {
  boundsLowIn[p] = boundsLowIn[p] + 1/10^(accuracy+2) 
  mcheck=mcheck+1
 } else if (round(masses[p],digits=accuracy+2) > round(boundsUpMass[p],digits=accuracy+2)) {
  boundsUpIn[p] = boundsUpIn[p] - 1/10^(accuracy+2) 
  mcheck=mcheck+1
 }
}

#bounds verschieben, falls sie sich einholen
for (r in seq(from=1, to=length(MatSelection), by=1)) {
 if (boundsLowIn[r] > boundsUpIn[r]) {
  if (masses[r] < boundsLowMass[r]) {boundsUpIn[r] <- boundsLowIn[r] 
  } else if (masses[r] > boundsUpMass[r]) { boundsLowIn[r] <- boundsUpIn[r] 
  }
 }
}
}

setTkProgressBar(pbm, 0, label=paste("Iteration step of normailzation to 100 vol%:",run,"\n Iteration step of mass% calculation:",runx))

#print(paste("mcheck:",mcheck))

#print(paste("runx:",runx))
runx=runx+1

}

#if (BoundsType=="Yes, in mass percent") { 
close(pbm) 
#}

message("  Optimization finished after iteration number: ",run+runx-1)

while (TRUE) {

message("  Loading results window")

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
  dummy <- dummy+coef(fit)[i]*round(tuning[v,i+1],digits=accuracy)
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


#show results and decide for finetuning 
reswin <- tktoplevel()
tkwm.title(reswin, "Results of optimization")	
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





finevol <- function() {		
    tclvalue(done)<-5
    #if ( !is.null(dev.list()) ) { 

     #   dev.off(length(dev.list())+1) #close last opened interactive R window

    #}
}
finevol.but <- tkbutton(reswin, text='Fine-Tuning in Vol%', command=finevol)


finemass <- function() {		
    tclvalue(done)<-6
    #if ( !is.null(dev.list()) ) { 

     #   dev.off(length(dev.list())+1) #close last opened interactive R window

    #}
}
finemass.but <- tkbutton(reswin, text='Fine-Tuning in Ma%', command=finemass)







infom <- function() {		
        tkmessageBox(title = "Model summary", message=modelsummary, icon = "info", type = "ok")
}
infom.but <- tkbutton(reswin, text='Model info', command=infom)

replot <- function() {

message("  Open/close plot")

	if (is.null(dev.list())) {

        if (os=="linux") { X11(title="ParSD: Design batch") }
        else if (os=="osx") { quartz(title="ParSD: Design batch") }
        else { windows(title="ParSD: Design batch") }
        
		par(mar=c(4.3,4.3,0.5,0.5), cex=mag)

        plot(x=tuning$Diameter,y=tuning$VolModel,type="l",lty=1,xlab=paste("Particle size in",tclvalue(dunit)), ylab="CPFT in %",log=lc,cex=mag,xlim=xb,ylim=yb)

        lines(x=tuning$Diameter,y=CurveFit,lty=2)

        legend("bottomright", c("Model","Fit"), lty=c(1,2), inset=0.01)

    } else { 

        dev.off(length(dev.list())+1) 

    }   

}
replot.but <- tkbutton(reswin, text='Open/close plot', command=replot)









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

legend("bottomright", c("Model","Fit"), lty=c(1,2), inset=0.01)

dev.off(length(dev.list())+1)

message("  Saved plot: ",FilePlot)

}

}



savebatch <- function() {

FileBatch = tclvalue(tcl("tk_getSaveFile"))
if (FileBatch != "") {

batchdb <- data.frame(cbind(c(round(100*coef(fit), digits=accuracy),round(100*sum(coef(fit)), digits=accuracy)),c(round(100*masses, digits=accuracy),round(100*sum(masses), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2))))
	

	 write.table(batchdb,file=FileBatch,row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%","Mass%","Density","SSA","Price per MT"),quote=FALSE)
message("  Saved batch: ",FileBatch)
}
}


savefit <- function() {

FileFit = tclvalue(tcl("tk_getSaveFile"))
if (FileFit != "") {

modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(errsq, digits=accuracy),rep(NA, 4+length(modelvalue)))))
        
        
        write.table(modeldb,file=FileFit,row.names=c(rep(" ",length(tuning$Diameter)),"-----",modelparam,"-----","Corr. coeff.:","Sum. sq. dev.:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c(paste("Model and fit;Diameter in",tclvalue(dunit)),"Q(Model) in %","Q(Fit) in %","Squared deviation"),quote=FALSE)
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

legend("bottomright", c("Model","Fit"), lty=c(1,2), inset=0.01)

dev.off(length(dev.list())+1)
        
        
        
        
        #save batch 

#      if (BoundsType=="Yes, in mass percent") { #with Mass%-bounds
      
#      batchdb <- data.frame(cbind(c(round(100*coef(fit), digits=accuracy),round(100*sum(coef(fit)), digits=accuracy)),c(round(100*masses, digits=accuracy),round(100*sum(masses), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2)),c(boundsLowMass,NA),c(boundsUpMass,NA)))
	
#	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
#     #   rownames(batchdb) <- c(MatSelection,"Batch:")

#	 write.table(batchdb,file=paste(parent,"/",savename,"/",savename,"-batch.csv",sep=""),row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT","Set lower mass% bounds","Set upper mass% bounds"),quote=FALSE)

#} else if (BoundsType=="Yes, in volume percent") { #VOl%-FT 

#batchdb <- data.frame(cbind(c(round(100*coef(fit), digits=accuracy),round(100*sum(coef(fit)), digits=accuracy)),c(round(100*masses, digits=accuracy),round(100*sum(masses), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2)),c(boundsLowIn,NA),c(boundsUpIn,NA)))
	
#	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
#     #   rownames(batchdb) <- c(MatSelection,"Batch:")

#	 write.table(batchdb,file=paste(parent,"/",savename,"/",savename,"-batch.csv",sep=""),row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT","Set lower vol% bounds","Set upper vol% bounds"),quote=FALSE)

#} else {

batchdb <- data.frame(cbind(c(round(100*coef(fit), digits=accuracy),round(100*sum(coef(fit)), digits=accuracy)),c(round(100*masses, digits=accuracy),round(100*sum(masses), digits=accuracy)),c(round(densities, digits=4),round(Den, digits=4)),c(round(ssa, digits=4),round(Sssa, digits=4)),c(round(prices, digits=2),round(price, digits=2))))
	
	#colnames(batchdb) <- c("Vol%","Mass%","Density g/cc","SSA m2/g","Price per MT")
     #   rownames(batchdb) <- c(MatSelection,"Batch:")

	 write.table(batchdb,file=paste(parent,"/",savename,"/",savename,"-batch.csv",sep=""),row.names=c(MatSelection,"Batch:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c("Material;Vol%","Mass%","Density","SSA","Price per MT"),quote=FALSE)

        #}
        #save model and fit
        
        modeldb <- data.frame(cbind(c(tuning$Diameter,NA,modelvalue,NA,round(cor(tuning$VolModel,CurveFit), digits=4),round(sumerrsq, digits=accuracy)),c(round(tuning$VolModel, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(CurveFit, digits=accuracy),rep(NA, 4+length(modelvalue))),c(round(errsq, digits=accuracy),rep(NA, 4+length(modelvalue)))))
        
        
        write.table(modeldb,file=paste(parent,"/",savename,"/",savename,"-model.csv",sep=""),row.names=c(rep(" ",length(tuning$Diameter)),"-----",modelparam,"-----","Corr. coeff.:","Sum. sq. dev.:"),dec=tclvalue(decpoint),sep=tclvalue(csvtype), na="",col.names=c(paste("Model and fit;Diameter in",tclvalue(dunit)),"Q(Model) in %","Q(Fit) in %","Squared deviation"),quote=FALSE)
        } #!is.null savename
        } #!is.na
        
       message("  Results saved in folder: ",paste(parent,savename,sep=pathsep))
}
#saveres.but <- tkbutton(reswin, text='Save all', command=saveres)


#menu

 Menu <- tkmenu(reswin)           
  tkconfigure(reswin, menu = Menu) 
  
  SaveMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Save", menu = SaveMenu)
  tkadd(SaveMenu, "command", label = "Recipe (Save as CSV-file!)", command =function() saverec())
  tkadd(SaveMenu, "command", label = "Batch (Save as CSV-file!)", command =function() savebatch())
  tkadd(SaveMenu, "command", label = "Model and fit (Save as CSV-file!)", command =function() savefit())
  tkadd(SaveMenu, "command", label = "Graph (Save as PNG-file!)", command =function() saveplot())
  tkadd(SaveMenu, "separator")
  tkadd(SaveMenu, "command", label = "All (in an extra folder)", command =function() saveres())
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","15resdesign","15.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())  
  QuMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Back to...", menu = QuMenu)
  
  tkadd(QuMenu, "command", label = "Model Selection", command = function() tclvalue(done)<-4)
  tkadd(QuMenu, "command", label = "Bounds Selection", command = function() tclvalue(done)<-3)
  tkadd(QuMenu, "command", label = "Material Selection", command = function() tclvalue(done)<-2)
  tkadd(QuMenu, "command", label = "Main Menu", command = function() tclvalue(done)<-1)
  tkadd(QuMenu, "separator")
  tkadd(QuMenu, "command", label = "Quit ParSD app", command = function() tclvalue(done)<-0)

tkgrid(tklabel(reswin, font=tkfont.create(size=1), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, text="Material"), tklabel(reswin, text="Vol%"), tklabel(reswin, text="Mass%"),tklabel(reswin, text="Density"), tklabel(reswin, text="SSA"), tklabel(reswin, text="Price per MT"), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), columnspan=6, pady=tclvalue(ygap), padx=10, sticky="we")
#tkgrid(tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"),tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), pady=tclvalue(ygap), padx=10, columnspan=6)

for (i in seq(from=1, to=length(MatSelection), by=1)) {
    tkgrid(tklabel(reswin, text=MatSelection[i]), tklabel(reswin, text=as.character(round(100*coef(fit)[i], digits=accuracy))), tklabel(reswin, text=as.character(round(100*masses[i], digits=accuracy))), tklabel(reswin,text=as.character(round(densities[i], digits=4))), tklabel(reswin, text=as.character(round(ssa[i], digits=4))), tklabel(reswin, text=as.character(round(prices[i], digits=2))), pady=tclvalue(ygap), padx=10, columnspan=6)
}

tkgrid(ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), ttkseparator(reswin), columnspan=6, pady=tclvalue(ygap), padx=10, sticky="we")
#tkgrid(tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"),tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, text="Batch:"), tklabel(reswin, text=as.character(round(100*sum(coef(fit)), digits=accuracy))), tklabel(reswin, text=as.character(round(100*sum(masses), digits=accuracy))), tklabel(reswin, text=as.character(round(Den, digits=4))), tklabel(reswin, text=as.character(round(Sssa, digits=4))), tklabel(reswin, text=as.character(round(price, digits=2))), pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, font=tkfont.create(size=1), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

tkgrid(tklabel(reswin, text="d(CPFT) of batch:"), tklabel(reswin, text=paste("d(10%) =",round(d10, digits=accuracy),tclvalue(dunit))), tklabel(reswin, text=paste("d(25%) =",round(d25, digits=accuracy),tclvalue(dunit))), tklabel(reswin, text=paste("d(50%) =",round(d50, digits=accuracy),tclvalue(dunit))), tklabel(reswin, text=paste("d(75%) =",round(d75, digits=accuracy),tclvalue(dunit))), tklabel(reswin, text=paste("d(90%) =",round(d90, digits=accuracy),tclvalue(dunit))), pady=tclvalue(ygap), padx=10, columnspan=6)

#tkgrid(tklabel(reswin, text=""),pady=tclvalue(ygap), padx=10, columnspan=6)
tkgrid(ttkseparator(reswin), columnspan=36, pady=tclvalue(ygap), padx=10, sticky="we")
tkgrid(tklabel(reswin, font=tkfont.create(size=1), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

#tkgrid(tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"),tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), tklabel(reswin, text="-------------------"), pady=tclvalue(ygap), padx=10, columnspan=6)







if (is.na(Den)) { #VOl%-FT IF No-bounds, but DEN NA
tkgrid(tklabel(reswin, text="Fit quality:"), tklabel(reswin, text="Cor. coeff. ="), tklabel(reswin, text=as.character(round(cor(tuning$VolModel,CurveFit), digits=4))),tklabel(reswin, text=""),tklabel(reswin, text=""), tklabel(reswin, text=""), pady=tclvalue(ygap), padx=10, columnspan=6)
} else { #Vol and MAss%-FTs if No-bounds and Den not NA
tkgrid(tklabel(reswin, text="Fit quality:"), tklabel(reswin, text="Cor. coeff. ="), tklabel(reswin, text=as.character(round(cor(tuning$VolModel,CurveFit), digits=4))),tklabel(reswin, text=""),tklabel(reswin, text=""), finemass.but, pady=tclvalue(ygap), padx=10, columnspan=6)
}

tkgrid(tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), tklabel(reswin, font=tkfont.create(size=1), text="", bg=hintergrund), columnspan=6, pady=0, padx=10, sticky="we")

tkgrid(tklabel(reswin, text=""), tklabel(reswin, text="Sum sq. dev. ="), tklabel(reswin, text=as.character(round(sumerrsq, digits=accuracy))),replot.but,infom.but,finevol.but,  pady=tclvalue(ygap), padx=10, columnspan=6)
	

tkgrid(tklabel(reswin, font=tkfont.create(size=1), text=""),pady=tclvalue(ygap), padx=10, columnspan=6)

	
tkfocus(reswin)
# Do not proceed with the following code until the variable done is non-zero.
#   (But other processes can still run, i.e. the system is not frozen.)
tkwait.variable(done)

don <- tclvalue(done)
tkdestroy(reswin) 

message("  Chosen action (0 Quit, 1 Main menu, 2 Material Selection, 3 Bounds selection, 4 Model selection, 5 Vol%-Finetuning, 6 wt%-Finetuning): ",don,"\n")

if (don < 5) { 
if ( !is.null(dev.list()) ) { 

        dev.off(length(dev.list())+1) #close last opened interactive R window

    }
return(don)
} else if (don == 5) { 

volreset <- coef(fit) #round(coef(fit),digits=accuracy+2)
volchange <- coef(fit) #round(coef(fit),digits=accuracy+2)

valres <- volumeft(volreset,volchange,masses,densities,ssa,prices,Den,Sssa,price,d10,d25,d50,d75,d90,tuning,CurveFit,sumerrsq,MatSelection,MatSelection1,MatSelection2,dbinfo,dbinfo2,xb,yb,lc,mag,errsq)
message("END vol%-finetuning function, RETURN value: ",valres,"\n")

} else if (don == 6) { 
 
massreset <- masses #round(100*masses, digits=accuracy)
masschange <- masses
volumes <- coef(fit)

#valres <- 0 
valres <- weightft(massreset,masschange,volumes,densities,ssa,prices,Den,Sssa,price,d10,d25,d50,d75,d90,tuning,CurveFit,sumerrsq,MatSelection,MatSelection1,MatSelection2,dbinfo,dbinfo2,xb,yb,lc,mag,errsq)
message("END wt%-finetuning function, RETURN value: ",valres,"\n")
} #else if (tclvalue(done) == 3) { 

if (valres < 5) { return(valres) }

} #while loop reswin

} #design function








