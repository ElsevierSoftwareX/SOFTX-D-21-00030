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

lf <- file("parsd.log", open = "wt")
sink(lf ,type = "output")
sink(lf, type = "message")
#do not forget to uncomment last line in this file

message("Logfile\n\nParSD\nTool to design and analyze particle size distributions\n")

if(!require("tcltk")) install.packages("tcltk")

#startup <- tkProgressBar(title = "progress bar", min = 0,max = 100, width = 700)
                    
#setTkProgressBar(startup, 0, label="DISCLAIMER: This software is intended for guidance in designing mixtures. It is intended for the use by individuals\ncompetent to evaluate the output and taking responsibility for the application. The authors of the software shall not be liable for\nany issues arising from the use of this software.")

#sizeinput <- file.size(file="input.R")
#sizedesign <- file.size(file="design.R")
#sizeft <- file.size(file="finetuning.R")
#sizeverify <- file.size(file="verify.R")
#sizeapp <- sizeinput+sizedesign+sizeft+sizeverify

version = "1.2"

errormsgopt <- "The optimization went wrong. This typically can be solved by better\nstarting values. You may try to set or change some bounds or bound values.\nFor more complex models it is also possible, that more than one solution\nof the optimization exists. Nevertheless, also then adjusting the bounds\nmight solve the problem. Otherwise, it is possible that the defined batch\n(e.g. a strongly monomdal batch) cannot be fitted by the selected model.\nYou may check your input data from the database as well as the model\nformulas (cf. Documentation)."

source(file="options.R")

hintergrund=tclvalue(tcl("ttk::style","lookup","TLabel","-background"))
knoepfe=hintergrund
menue=hintergrund

#hintergrund="gray85"
#knoepfe="gray75"
#menue="gray90"

#tcl("tk_setPalette","gray70")
#tcl("tk_setPalette","background",hintergrund,"foreground",knoepfe)





#The code (the following 23 non-commented lines) to distinguish different operating systems is modified, but originally taken from the source:
#Will (2015): 'Identifying the OS from R'. https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/. Accessed 04 Dec 2020.
#As stated in this blog entry (https://www.r-bloggers.com/2015/09/license/), the this 23 non-commented code lines are under MIT license (https://directory.fsf.org/wiki/License:Expat):
#
#MIT License
#Copyright (C) 2015 Will
#
#Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
#The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
pathsep <- "\\" #windows
sysinf <- Sys.info()
if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin') { 
        os <- "osx" 
        pathsep <- "/"
    }
    if (os == 'Linux') { 
        os <- "linux" 
        pathsep <- "/"
        }
} else { 
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) { 
        os <- "osx" 
        pathsep <- "/"
    }
    if (grepl("linux-gnu", R.version$os)) { 
        os <- "linux" 
        pathsep <- "/"
        }
}
message("Detect OS: ",os,"\n")


manual <- function(man="Documentation.pdf") {    
    browseURL(paste("Docs",man,sep=pathsep))
}


standardvalues <- function(stdmodel="") {

message("BEGIN function to set standard values")
message(" Standard values for ",stdmodel," model")

if (stdmodel=="Andreasen") {

vals <- varEntryDialog(vars=c('stdandn'), labels=c('Distribution modulus n(And):'), title="Standard values for the Andreasen model",prompt="The standard maximum particle size is calculated from the chosen\nmaterials. You may choose a standard distribution modulus. A literature\nvalue (0.53) is set if you leave the field empty. The distribution\nmodulus depends on the application: Check the Methodology Documentation\nfor further and citation information.",preset=c(stdandn))
    if(!is.null(vals)) { 
    
     stdandn <<- as.numeric(vals[1])
     if (is.na(stdandn)) {stdandn <<- 0.53} 
     
     }
     
} else if (stdmodel=="DingerFunk") {

if (is.na(stddfdmin)) { stddfdmin <- "" }

vals <- varEntryDialog(vars=c('stddfdmin','stddfn'), labels=c(paste('Minimum particle size d(min) in ',tclvalue(dunit),':',sep=""),'Distribution modulus n(DF):'), title="Standard values for the Dinger/Funk model",prompt="The standard maximum particle size is calculated from the chosen\nmaterials. You may choose a standard distribution modulus and minimum\nparticle size. A literature value (0.37) is set for the distribution modulus\nif you leave the field empty. For the minimum particle size it\nis replaced later-on by the smallest particle size listed in the database or recipe.\nThe distribution modulus depends on the application: Check\nthe Methodology Documentation for further and citation information.",preset=c(stddfdmin,stddfn))
    if(!is.null(vals)) { 
    
     stddfdmin <<- as.numeric(vals[1])
     #if (is.na(stddfdmin)) {stddfdmin <<- 0.0} 
     
     stddfn <<- as.numeric(vals[2])
     if (is.na(stddfn)) {stddfn <<- 0.37} 
     
     }
        
} else if (stdmodel=="Psi") {

vals <- varEntryDialog(vars=c('stdpsinmin','stdpsinmax'), labels=c('Minimum distribution modulus n(min):','Maximum distribution modulus n(max):'), title="Standard values for the Psi model",prompt="The standard maximum particle size is calculated from\nthe chosen materials. You may choose standard distribution\nmoduli. Literature values (0.2 and 0.6, respectively) are set for\nthe distribution moduli if you leave the fields empty. The\ndistribution moduli depend on the application: Check the Methodology\nDocumentation for further and citation information.",preset=c(stdpsinmin,stdpsinmax))
    if(!is.null(vals)) { 
    
     stddfdmin <<- as.numeric(vals[1])
     if (is.na(stdpsinmin)) {stdpsinmin <<- 0.2} 
     
     stddfn <<- as.numeric(vals[2])
     if (is.na(stdpsinmax)) {stdpsinmax <<- 0.6} 
     
     }
        
} else if (stdmodel=="ModPsi") {

if (is.na(stdmpsidmin)) { stdmpsidmin <- "" }

vals <- varEntryDialog(vars=c('stdmpsidmin','stdmpsinmin','stdmpsinmax'), labels=c(paste('Minimum particle size d(min) in ',tclvalue(dunit),':',sep=""),'Minimum distribution modulus n(min):','Maximum distribution modulus n(max):'), title="Standard values for the Modified Psi model",prompt="The standard maximum particle size is calculated from the chosen\nmaterials. You may choose standard distribution moduli and a minimum\nparticle size. Literature values (0.1 and 0.5, respectively) are set for the\ndistribution moduli if you leave the fields empty. For the minimum particle\nsize it is replaced later-on by the smallest particle size listed\nin the database or recipe. The distribution moduli depend on the\napplication: Check the Methodology Documentation\nfor further and citation information.",preset=c(stdmpsidmin,stdmpsinmin,stdmpsinmax))
    if(!is.null(vals)) { 
    
     stdmpsidmin <<- as.numeric(vals[1])
     #if (is.na(stdmpsidmin)) {stdmpsidmin <<- 0.0} 
     
     stdmpsinmin <<- as.numeric(vals[2])
     if (is.na(stdmpsinmin)) {stdmpsinmin <<- 0.1} 
     
     stdmpsinmax <<- as.numeric(vals[3])
     if (is.na(stdmpsinmax)) {stdmpsinmax <<- 0.5} 
     
     }
        
} else if (stdmodel=="Kawa") {

#if (is.na(stdmpsidmin)) { stdmpsidmin <- "" }

vals <- varEntryDialog(vars=c('stdkawadgap','stdkawanand','stdkawanfur'), labels=c(paste('Gap particle size d(min) in ',tclvalue(dunit),':',sep=""),'Distribution modulus (Andreasen-part):','Distribution modulus (Furnas-part):'), title="Standard values for the Kawamura model",prompt="The standard maximum particle size is calculated from the chosen\nmaterials. You may choose standard distribution moduli and a gap\nparticle size. Literature values (88 um, 0.3 and 0.78, respectively) are set for the\ngap particle size and the distribution moduli if you leave the fields\nempty. The parameters depend on the application: Check the\nMethodology Documentation for further and citation information.",preset=c(stdkawadgap,stdkawanand,stdkawanfur))
    if(!is.null(vals)) { 
    
     stdkawadgap <<- as.numeric(vals[1])
     if (is.na(stdkawadgap)) {
     
     if (tclvalue(dunit) == 'nm') { stdkawadgap <<- 88000 
     } else if (tclvalue(dunit) == 'um') { stdkawadgap <<- 88 
     } else if (tclvalue(dunit) == 'mm') { stdkawadgap <<- 0.088 
     } else { stdkawadgap <<- 0.0088 }
     
     } 
     
     stdkawanand <<- as.numeric(vals[2])
     if (is.na(stdkawanand)) {stdkawanand <<- 0.3} 
     
     stdkawanfur <<- as.numeric(vals[3])
     if (is.na(stdkawanfur)) {stdkawanfur <<- 0.78} 
     
     }
        
} else {

if (is.na(stdmkawadmin)) { stdmkawadmin <- "" }

vals <- varEntryDialog(vars=c('stdmkawadmin','stdmkawadgap','stdmkawanand','stdmkawanfur'), labels=c(paste('Minimum particle size d(min) in ',tclvalue(dunit),':',sep=""),paste('Gap particle size d(min) in ',tclvalue(dunit),':',sep=""),'Distribution modulus (Andreasen-part):','Distribution modulus (Furnas-part):'), title="Standard values for the Modified Kawamura model",prompt="The standard maximum particle size is calculated from the chosen\nmaterials. You may choose standard distribution moduli and a minimum\nand gap particle size. Literature values (0.08, 0.45 and 35.1 um, respectively) are set\nfor the distribution moduli and for the gap particle size if you leave the fields empty. For\nthe minimum particle size it is replaced later-on by the smallest\nparticle size listed in the database or recipe. The parameters depend\non the application: Check the Methodology Documentation for further\nand citation information.",preset=c(stdmkawadmin,stdmkawadgap,stdmkawanand,stdmkawanfur))
    if(!is.null(vals)) { 
    
     stdmkawadmin <<- as.numeric(vals[1])
     #if (is.na(stdmpsidmin)) {stdmpsidmin <<- 0.0}
     
     stdmkawadgap <<- as.numeric(vals[2])
     if (is.na(stdmkawadgap)) {
     
     if (tclvalue(dunit) == 'nm') { stdmkawadgap <<- 35100 
     } else if (tclvalue(dunit) == 'um') { stdmkawadgap <<- 35.1 
     } else if (tclvalue(dunit) == 'mm') { stdmkawadgap <<- 0.0351
     } else { stdmkawadgap <<- 0.00351 }
     
     } 
     
     stdmkawanand <<- as.numeric(vals[3])
     if (is.na(stdmkawanand)) {stdmkawanand <<- 0.08} 
     
     stdmkawanfur <<- as.numeric(vals[4])
     if (is.na(stdmkawanfur)) {stdmkawanfur <<- 0.45} 
     
     }
        
}

message("END function to set standard values\n")
}#end standardvalues




finddmax <- function() {
message("BEGIN function to set percent of oversized grains")

    vals <- varEntryDialog(vars=c('over'), labels=c('Amount of oversized grains in %:'), title="Automatic maximum particle size determination",prompt="Allowed percentage of oversized grains during automatic determination\nof d(max) from chosen raw materials (common values 5-15%)",preset=c(overgraining))
    if(!is.null(vals)) { 
    
     overgraining <<- as.numeric(vals[1])
     
    }
    
    message("END function to set overgraining\n")
}

setlimit <- function() {
message("BEGIN function to set iteration limits")

    if (limdesign == Inf) {
      limdesignpre <- ""
    } else { limdesignpre <- limdesign }
    
    if (limparcalc == Inf) {
      limparcalcpre <- ""
    } else { limparcalcpre <- limparcalc }

    vals <- varEntryDialog(vars=c('limdesign','limcalc'), labels=c('Design a batch-function:','Calculate model parameters-function:'), title="Limits of iteration cycles",prompt="Choose limits for the number of iterations:",preset=c(limdesignpre, limparcalcpre))
    if(!is.null(vals)) { 
    
     limdesign <<- as.numeric(vals[1])
     if (is.na(limdesign)) {limdesign <<- Inf} 
     
     limparcalc <<- as.numeric(vals[2])
     if (is.na(limparcalc)) {limparcalc <<- Inf} 
    }
    
    message("END function to set iteration limits\n")
}
  
setaccuracy <- function() {
message("BEGIN function to set accuracy")

    vals <- varEntryDialog(vars=c('precision'), labels=c('Accuracy as number of decimal digits:'), title="Setting fit quality parameter",prompt="Quality of fit increases if more decimal places have to be correct, but then also the calculation/fitting time increases",preset=c(accuracy))
    if(!is.null(vals)) { 
    
     accuracy <<- as.numeric(vals[1])
    }
    
    message("END function to set accuracy\n")
}


setgrainaccuracy <- function() {
message("BEGIN function to set grain size accuracy")

    vals <- varEntryDialog(vars=c('grainprecision'), labels=c('Accuracy as number of significant digits (digits unequal to zero):'), title="Setting grain size value accuracy",prompt="The parameter is required for the fitting of a model to a batch.\nSignificant digits are digits unequal to zero. Examplarily,\ngrain size 3150.000 um has three sign. digits, grain sizes 1.000 um\nand 0.001 um both one sign. digit and grain size 31.500 um\nagain three significant digits.",preset=c(accsizes))
    if(!is.null(vals)) { 
    
     accsizes <<- as.numeric(vals[1])
    }
    
    message("END function to set grain size accuracy\n")
}


source(file="input.R") #opendb(), openrec() and varEntryDialog() functions 


#setTkProgressBar(startup, round(sizeinput*100/sizeapp, digits=0), label="DISCLAIMER: This software is intended for guidance in\ndesigning mixtures. It is intended for the use by individuals\ncompetent to evaluate the output and taking responsibility for\nthe application. The authors of the software shall not be liable for\nany issues arising from the use of this software.")

source(file="dbfun.R")

source(file="design.R")


#setTkProgressBar(startup, round((sizeinput+sizedesign)*100/sizeapp, digits=0), label="DISCLAIMER: This software is intended for guidance in\ndesigning mixtures. It is intended for the use by individuals\ncompetent to evaluate the output and taking responsibility for\nthe application. The authors of the software shall not be liable for\nany issues arising from the use of this software.")



source(file="finetuning.R")


#setTkProgressBar(startup, round((sizeinput+sizedesign+sizeft)*100/sizeapp, digits=0), label="DISCLAIMER: This software is intended for guidance in\ndesigning mixtures. It is intended for the use by individuals\ncompetent to evaluate the output and taking responsibility for\nthe application. The authors of the software shall not be liable for\nany issues arising from the use of this software.")

source(file="verify.R")

#setTkProgressBar(startup, 100, label="DISCLAIMER: This software is intended for guidance in\ndesigning mixtures. It is intended for the use by individuals\ncompetent to evaluate the output and taking responsibility for\nthe application. The authors of the software shall not be liable for\nany issues arising from the use of this software.")



source(file="parcalc.R")




#close(startup)


while (TRUE) {
message("Main window/menu\n")

VolModel <- NULL
modelparam <- NULL
modelvalue <- NULL
modelinfo <- NULL
modelsummary <- NULL
ModSelection <- NULL


win <- tktoplevel(bg=hintergrund)
  tkwm.title(win,"ParSD")
  
  #setPalette(win, background="gray90")
  
  tkraise(win)

app <- tclVar(0)

tkbind(win,"<Destroy>",function() tclvalue(app)<-1)


  
  #Menu
  
  Menu <- tkmenu(win, bg=menue)           
  tkconfigure(win, menu = Menu) 

  #RecMenu <- tkmenu(Menu, tearoff = FALSE)
  #tkadd(Menu, "cascade", label = "Recipes & Calculations", menu = RecMenu)
  
  #tkadd(RecMenu, "command", label = "Open database", command = function() opendb())
  #tkadd(RecMenu, "command", label = "Open recipe", command = function() openrec())

  DBMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Database", menu = DBMenu)
  
  DBnewMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(DBMenu, "cascade", label = "Create new database (as CSV-file!)", menu = DBnewMenu)
  
  tkadd(DBnewMenu, "command", label = "Add material", command = function() tclvalue(app)<-5)
  tkadd(DBnewMenu, "separator")
  tkadd(DBnewMenu, "command", label = "Help (Create new database)", command =function() manual(man=paste("Help","02createdb","02.pdf",sep=pathsep)))
  
  DBopenMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(DBMenu, "cascade", label = "Open existent database", menu = DBopenMenu)
  
  tkadd(DBopenMenu, "command", label = "Add material", command = function() tclvalue(app)<-6)
  tkadd(DBopenMenu, "command", label = "Edit material", command = function() tclvalue(app)<-7)
  tkadd(DBopenMenu, "command", label = "Remove material", command = function() tclvalue(app)<-8)
  tkadd(DBopenMenu, "separator")
  tkadd(DBopenMenu, "command", label = "Help (Open existent database)", command =function() manual(man=paste("Help","04opendb","04.pdf",sep=pathsep)))
  
  tkadd(DBMenu, "separator")
  tkadd(DBMenu, "radiobutton", variable=csvtype, value=';', label="Separator \";\" in CSV")
  tkadd(DBMenu, "radiobutton", variable=csvtype, value=',', label="Separator \",\" in CSV")
  tkadd(DBMenu, "separator")
  tkadd(DBMenu, "radiobutton", variable=decpoint, value=',', label="Decimal Comma \",\" in CSV")
  tkadd(DBMenu, "radiobutton", variable=decpoint, value='.', label="Decimal Point \".\" in CSV")
  tkadd(DBMenu, "separator")
  tkadd(DBMenu, "radiobutton", variable=dunit, value='nm', label="Particle size in nanometer \"nm\"")
  tkadd(DBMenu, "radiobutton", variable=dunit, value='um', label="Particle size in micrometer \"um\"")
  tkadd(DBMenu, "radiobutton", variable=dunit, value='mm', label="Particle size in millimeter \"mm\"")
  tkadd(DBMenu, "radiobutton", variable=dunit, value='cm', label="Particle size in centimeter \"cm\"")  
  
  
  OptMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "Settings", menu = OptMenu)
  
  #OptCsvMenu <- tkmenu(OptMenu, tearoff = FALSE)
  #tkadd(OptMenu, "cascade", label = "Recipe- & Database options", menu = OptCsvMenu)
  
  #tkadd(OptCsvMenu, "radiobutton", variable=csvtype, value=';', label="Separator \";\" in CSV")
  #tkadd(OptCsvMenu, "radiobutton", variable=csvtype, value=',', label="Separator \",\" in CSV")
  #tkadd(OptCsvMenu, "separator")
  #tkadd(OptCsvMenu, "radiobutton", variable=decpoint, value=',', label="Decimal Comma \",\" in CSV")
  #tkadd(OptCsvMenu, "radiobutton", variable=decpoint, value='.', label="Decimal Point \".\" in CSV")
  #tkadd(OptCsvMenu, "separator")
  #tkadd(OptCsvMenu, "radiobutton", variable=dunit, value='nm', label="Particle size in nanometer \"nm\"")
  #tkadd(OptCsvMenu, "radiobutton", variable=dunit, value='um', label="Particle size in micrometer \"um\"")
  #tkadd(OptCsvMenu, "radiobutton", variable=dunit, value='mm', label="Particle size in millimeter \"mm\"")
  #tkadd(OptCsvMenu, "radiobutton", variable=dunit, value='cm', label="Particle size in centimeter \"cm\"")  
  
  OptModMenu <- tkmenu(OptMenu, tearoff = FALSE)
  tkadd(OptMenu, "cascade", label = "Model options", menu = OptModMenu)
  
  tkadd(OptModMenu, "command", label = "Automatic maximum particle size determination", command = function() finddmax())
  tkadd(OptModMenu, "separator")
  tkadd(OptModMenu, "command", label = "Standard values for the Andreasen model", command = function() standardvalues(stdmodel="Andreasen"))
  tkadd(OptModMenu, "command", label = "Standard values for the Psi model", command = function() standardvalues(stdmodel="Psi"))
  tkadd(OptModMenu, "command", label = "Standard values for the Kawamura model", command = function() standardvalues(stdmodel="Kawa"))
  tkadd(OptModMenu, "command", label = "Standard values for the Dinger/Funk model", command = function() standardvalues(stdmodel="DingerFunk"))
  tkadd(OptModMenu, "command", label = "Standard values for the Modified Psi model", command = function() standardvalues(stdmodel="ModPsi"))
  tkadd(OptModMenu, "command", label = "Standard values for the Modified Kawamura model", command = function() standardvalues(stdmodel="ModKawa"))
  
  
  #going on with Opt menu
  OptFitMenu <- tkmenu(OptMenu, tearoff = FALSE)
  tkadd(OptMenu, "cascade", label = "Fitting options", menu = OptFitMenu)
  
  tkadd(OptFitMenu, "command", label = "Fit quality (Precision)", command = function() setaccuracy())
  tkadd(OptFitMenu, "command", label = "Sign. digits grain sizes", command = function() setgrainaccuracy())
  tkadd(OptFitMenu, "command", label = "Limits of iterations", command = function() setlimit())
  
    tkadd(OptMenu, "separator")
  tkadd(OptMenu, "radiobutton", variable=sbheight, value='3', label="Small listbox height")
  tkadd(OptMenu, "radiobutton", variable=sbheight, value='7', label="Medium listbox height")
  tkadd(OptMenu, "radiobutton", variable=sbheight, value='11', label="Large listbox height")
  tkadd(OptMenu, "separator")
  tkadd(OptMenu, "radiobutton", variable=ygap, value='0', label="Small vertical distance between window elements")
  tkadd(OptMenu, "radiobutton", variable=ygap, value='1', label="Medium vertical distance between window elements")
  tkadd(OptMenu, "radiobutton", variable=ygap, value='2', label="Large vertical distance between window elements")
  
  
  #OptWinMenu <- tkmenu(OptMenu, tearoff = FALSE)
  #tkadd(OptMenu, "cascade", label = "Window sizing options", menu = OptWinMenu)
  
  #sbheight <- 3

  #ygap <- 1
  #tkadd(OptWinMenu, "radiobutton", variable=sbheight, value='3', label="Small listbox height")
  #tkadd(OptWinMenu, "radiobutton", variable=sbheight, value='7', label="Medium listbox height")
  #tkadd(OptWinMenu, "radiobutton", variable=sbheight, value='11', label="Large listbox height")
  #tkadd(OptWinMenu, "separator")
  #tkadd(OptWinMenu, "radiobutton", variable=ygap, value='0', label="Small vertical distance between window elements")
  #tkadd(OptWinMenu, "radiobutton", variable=ygap, value='1', label="Medium vertical distance between window elements")
  #tkadd(OptWinMenu, "radiobutton", variable=ygap, value='2', label="Large vertical distance between window elements")
  
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)
  
  
  
  tkadd(AbMenu, "command", label = "Version/Info", command = function() tkmessageBox(title = "Version/Info", message =paste("ParSD\n\nTool to design and analyze particle size distributions\n\nVersion",version,sep=" "), icon = "info", type = "ok"))
  tkadd(AbMenu, "command", label = "Authors", command = function() tkmessageBox(title = "Authors", message ="Copyright (C) 2020, 2021 Jens Fruhstorfer\n\nParSD is scripted in R:\n[1] R Develpoment Core Team (2011): 'R: A language and environment for statistical computing'. R Foundation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, URL http://www.R-project.org/.\n\nParSD contains externally published code:\n[2] J. Bryer (2012): 'User input using tcl/tk'. https://www.r-bloggers.com/2012/08/user-input-using-tcltk/. Accessed 04 Dec 2020.\n[3] Will (2015): 'Identifying the OS from R'. https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/. Accessed 04 Dec 2020.", icon = "info", type = "ok"))
  tkadd(AbMenu, "command", label = "Contributors", command = function() tkmessageBox(title = "Contributors", message ="Bruno Luchini\nEnrico Storti", icon = "info", type = "ok"))
  tkadd(AbMenu, "command", label = "License", command = function() {
  showgpl <- tkmessageBox(title = "License", message="ParSD is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.\n\nParSD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\nSee the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with ParSD. If not, see <http://www.gnu.org/licenses/>.\n\nVersion 3 of the GPL can be found in the file 'COPYING' in the Docs-subfolder of ParSD-software. Would you like to open the license now?", icon = "info", type = "yesno")

    if (tclvalue(showgpl) == "yes") { 
      #call to other function
       manual(man="COPYING.pdf")
      
    }
  }
  )
  tkadd(AbMenu, "command", label = "Citation info", command = function() tkmessageBox(title = "Citation info", message =paste("To cite ParSD in publications use:\nJ. Fruhstorfer (2021): 'ParSD - Tool to design and analyze particle size distributions'. Version ",version,".\n\nPlease cite the original references for the models and standard values if used. See the Methodology Documentation for more information on the models and standard values as well as for the original references.", sep=""), icon = "info", type = "ok"))
  tkadd(AbMenu, "separator")
  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","01mainwindow","01.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())
  
  
  tkadd(Menu, "command", label = "Quit", command = function() tclvalue(app)<-1)

  #RecMenu <- tkmenu(Menu, tearoff = FALSE)
  #tkadd(Menu, "cascade", label = "Recipes & Calculations", menu = RecMenu)
  
  #Footer and window size definition by tkgrid  
  tkgrid(tklabel(win, text='\nParSD\n\n     Tool to design and analyze particle     \nsize distributions', bg=hintergrund), pady=10, padx=10)
  
  tkgrid(tkbutton(win, text='Design a batch', bg=knoepfe, command=function() tclvalue(app)<-2), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(win, text='Verify a batch', bg=knoepfe, command=function() tclvalue(app)<-3), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(win, text='Calculate model parameters', bg=knoepfe, command=function() tclvalue(app)<-4), pady=5, padx=10, sticky="ew")
  
  tkgrid(tklabel(win, font=tkfont.create(size=7), bg=hintergrund, text="Copyright (C) 2020, 2021 Jens Fruhstorfer"), pady=10, padx=10)

  tkfocus(win)

# Do not proceed with the following code until the variable done is non-zero.
#   (But other processes can still run, i.e. the system is not frozen.)
tkwait.variable(app)

var1 <- tclvalue(app)

tkdestroy(win)

if (var1 == 1) {

message("Quit from Main window/menu. Save settings.\n")

options <- data.frame(cbind(c("#ParSD","#Tool to design and analyze particle size distributions","#Copyright (C) 2020, 2021 Jens Fruhstorfer","#","#This file is part of ParSD.","#","#ParSD is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.","#ParSD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. ","#See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with ParSD. If not, see <http://www.gnu.org/licenses/>.","decpoint=tclVar('","csvtype=tclVar('","dunit=tclVar('","sbheight=tclVar(","ygap=tclVar(","overgraining=","accuracy=","accsizes=","limdesign=","limparcalc=","stdandn=","stddfdmin=","stddfn=","stdpsinmin=","stdpsinmax=","stdmpsidmin=","stdmpsinmin=","stdmpsinmax=","stdkawadgap=","stdkawanand=","stdkawanfur=","stdmkawadmin=","stdmkawadgap=","stdmkawanand=","stdmkawanfur="),c("","","","","","","","","",tclvalue(decpoint),tclvalue(csvtype),tclvalue(dunit),tclvalue(sbheight),tclvalue(ygap),overgraining,accuracy,accsizes,limdesign,limparcalc,stdandn,stddfdmin,stddfn,stdpsinmin,stdpsinmax ,stdmpsidmin,stdmpsinmin,stdmpsinmax,stdkawadgap,stdkawanand,stdkawanfur,stdmkawadmin,stdmkawadgap,stdmkawanand,stdmkawanfur),c("","","","","","","","","","')","')","')",")",")","","","","","","","","","","","","","","","","","","","","")))
    
    write.table(options,file="options.R",row.names=FALSE,col.names=FALSE,sep="",quote=FALSE)
    
    break

} else if (var1 == 5) { #database-create fucntion

#DBcreate <- NULL

 while (TRUE) {
 
 
 
 tkmessageBox(title ="New database", message="Choose in the next dialog the place where to save and the name of the new database. Save as CSV-file, please.", icon = "info", type = "ok")
 
 DBcreate = tclvalue(tcl("tk_getSaveFile"))
 
 
message("Create new database: ",DBcreate,"\n")
    
 
 #  if (!is.null(DBcreate)) { 
#     dbkeep <- tkmessageBox(title ='Keep database savename and path', message=paste("Do you want to keep the savename and path last specified (",DBcreate,")?"), icon = "question", type = "yesno")
   #  if (tclvalue(dbkeep) == "no") { DBcreate = tclvalue(tcl("tk_getSaveFile")) }
   #} else { DBcreate = tclvalue(tcl("tk_getSaveFile")) }
    
    if (DBcreate != "") { 
        #call to function here mit return value
        
        #appdb <- createdb(dbfile=DBcreate, dnum=cntd)
        appdb <- createdb(dbfile=DBcreate)
        message("END create database function, RETURN value: ",appdb,"\n")
        #has to be updated?
        if (appdb == 0) { break } #quit
        if (appdb == 1) { break } #return to main menu
        #if (appdb == 2) { next } #return to sieve number definition

    } else {#back to main menu
    
    appdb <- 1
    break
    
    }
 
 } #while create database function

} else if (var1 > 5) { #database exist functions 6 add; 7 edit; 8 remove

    dbfile <- tk_choose.files(caption="Select database", filters=matrix(c("CSV files", ".csv", "All files", ".*"), 2, 2, byrow=TRUE))   
    message("Open existent database: ",dbfile,"\n")
    if (length(dbfile) != 0) { 
    
    #der rest muss in einzelnen while-loops sein, damit addmaterial sich selbst aufrufen kann und bei deletematerial die MatList immer geupdated wird
    
    if (var1 == 6) {
    
      addmaterial(dbfile)
      message("END add material function\n")
    
    } else if (var1 == 7) {
      
        editmaterial(dbfile)
        message("END edit material function\n")
      
      } else {
      
        deletematerial(dbfile)
        message("END delete material function\n")
      
      }

      }
      
      appdb <- 1
      
} else { #main calculation functions

message("BEGIN Main functions\n")

 while (TRUE) {
 
 message("BEGIN Open database or recipe\n")
  windb <- tktoplevel(bg=hintergrund)
  tkwm.title(windb,"Open database or recipe")
  
  tkraise(windb)

  db <- tclVar(0)

  tkbind(windb,"<Destroy>",function() tclvalue(db)<-1) 
  
  Menu <- tkmenu(windb, bg=menue)           
  tkconfigure(windb, menu = Menu) 
  
  AbMenu <- tkmenu(Menu, tearoff = FALSE)
  tkadd(Menu, "cascade", label = "About", menu = AbMenu)

  tkadd(AbMenu, "command", label = "Help (Current dialog)", command =function() manual(man=paste("Help","07opendbrec","07.pdf",sep=pathsep)))
  tkadd(AbMenu, "command", label = "Documentation", command =function() manual())
  
  tkadd(Menu, "command", label = "Back to Main Menu", command = function() tclvalue(db)<-1)
  
  tkgrid(tklabel(windb, bg=hintergrund, text='\nChoose material source file:'), pady=10, padx=10)

  tkgrid(tkbutton(windb, text='Open database', bg=knoepfe, command=function() tclvalue(db)<-2), pady=5, padx=10, sticky="ew")
  
  tkgrid(tkbutton(windb, text='Open recipe', bg=knoepfe, command=function() tclvalue(db)<-3), pady=5, padx=10, sticky="ew")

  tkfocus(windb)

  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(db)

  var2 <- tclvalue(db)
  
  tkdestroy(windb)
  
  message("END Open database (2) or recipe (3), quit (1)\n CALL main functions with (value): ",var2,"\n")
  
  if (var2 == 1) {
    appdb <- 1
    break
  }
  
  
  appdb <- openmat(fun=var1, rec=var2)
  message("END Select materials function, RETURN value: ",appdb,"\n")
  
  if (appdb == 0) { break } #quit
  if (appdb == 1) { break } #return to main menu
  #else: sart loop again with open recipe/database
 
 } #while db vs recipe
 
 message("END Main functions\n")

 } #end else von var1 main calculation functions
 
 if (appdb == 0) {   
 
 message("Quit from main functions. Save settings.\n")
 
 #options <- data.frame(cbind(c("decpoint=tclVar('","csvtype=tclVar('","dunit=tclVar('","sbheight=tclVar(","ygap=tclVar(","overgraining=","accuracy=","accsizes=","limdesign=","limparcalc=","stdandn=","stddfdmin=","stddfn=","stdpsinmin=","stdpsinmax=","stdmpsidmin=","stdmpsinmin=","stdmpsinmax=","stdkawadgap=","stdkawanand=","stdkawanfur=","stdmkawadmin=","stdmkawadgap=","stdmkawanand=","stdmkawanfur="),c(tclvalue(decpoint),tclvalue(csvtype),tclvalue(dunit),tclvalue(sbheight),tclvalue(ygap),overgraining,accuracy,accsizes,limdesign,limparcalc,stdandn,stddfdmin,stddfn,stdpsinmin,stdpsinmax ,stdmpsidmin,stdmpsinmin,stdmpsinmax,stdkawadgap,stdkawanand,stdkawanfur,stdmkawadmin,stdmkawadgap,stdmkawanand,stdmkawanfur),c("')","')","')",")",")","","","","","","","","","","","","","","","","","","","","")))
 
 options <- data.frame(cbind(c("#ParSD","#Tool to design and analyze particle size distributions","#Copyright (C) 2020, 2021 Jens Fruhstorfer","#","#This file is part of ParSD.","#","#ParSD is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.","#ParSD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. ","#See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with ParSD. If not, see <http://www.gnu.org/licenses/>.","decpoint=tclVar('","csvtype=tclVar('","dunit=tclVar('","sbheight=tclVar(","ygap=tclVar(","overgraining=","accuracy=","accsizes=","limdesign=","limparcalc=","stdandn=","stddfdmin=","stddfn=","stdpsinmin=","stdpsinmax=","stdmpsidmin=","stdmpsinmin=","stdmpsinmax=","stdkawadgap=","stdkawanand=","stdkawanfur=","stdmkawadmin=","stdmkawadgap=","stdmkawanand=","stdmkawanfur="),c("","","","","","","","","",tclvalue(decpoint),tclvalue(csvtype),tclvalue(dunit),tclvalue(sbheight),tclvalue(ygap),overgraining,accuracy,accsizes,limdesign,limparcalc,stdandn,stddfdmin,stddfn,stdpsinmin,stdpsinmax ,stdmpsidmin,stdmpsinmin,stdmpsinmax,stdkawadgap,stdkawanand,stdkawanfur,stdmkawadmin,stdmkawadgap,stdmkawanand,stdmkawanfur),c("","","","","","","","","","')","')","')",")",")","","","","","","","","","","","","","","","","","","","","")))
 
    write.table(options,file="options.R",row.names=FALSE,col.names=FALSE,sep="",quote=FALSE)
    
    break
 }
 
 

} #while win loop

message("Exit.")

sink()
#sink()

