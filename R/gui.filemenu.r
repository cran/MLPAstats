# a set of menues that help to set up the field Ms$mlpa with subfields
# $mlpa.dat object of class mlpa computed with setupMLPA()
# $arg.mlpa object that passes the arguments to setupMLPA() with subfields
#		$cont.dat control data
#		$test.dat test data
#		$size.dat size of ...
#		$probe.dat	probes used as control
#		$sel.var names of probes to be used in the analysis

get.file.menu<-function()
{
assign("fileMenu",tkmenu(.GlobalEnv$topMenu,tearoff=FALSE), envir = .GlobalEnv)
#fileMenu.1
#command load.win
load.win<-function()
{

	#select control
	tkmessageBox(message=paste("Select CONTROL file"))

	#load data from file
	fileName<-tclvalue(tkgetOpenFile())

	if (!nchar(fileName))
      {
    		tkmessageBox(message="No file was selected!")
	}else{
	      file.parts<-strsplit(fileName,split="/")
	      fileName.win<-paste(file.parts[[1]],collapse="/")

	     cont.dat<-read.table(fileName.win, header=TRUE)
	
	     tkfocus(.GlobalEnv$tt)
           #select TEST
	     tkmessageBox(message=paste("Select TEST file"))

	     #load data from file
	     fileName<-tclvalue(tkgetOpenFile())

	     if (!nchar(fileName))
           {
    		    tkmessageBox(message="No file was selected!")
	      }else{
	        file.parts<-strsplit(fileName,split="/")
	        fileName.win<-paste(file.parts[[1]],collapse="/")

	        test.dat<-read.table(fileName.win, header=TRUE)
	     

	       #select variables
	       if (length(names(cont.dat))!=length(names(test.dat)))
	       {
    		     tkmessageBox(message="The number of control and test variables do not match!",
					icon="error", type="ok")
	           tkfocus(.GlobalEnv$tt)
	       }else{

		    tkfocus(.GlobalEnv$tt)
 
		     mlpa<-.GlobalEnv$Ms$mlpa
		     mlpa$arg.mlpa$cont.dat<-cont.dat
		     mlpa$arg.mlpa$test.dat<-test.dat
		     .GlobalEnv$Ms$mlpa<-mlpa

		     plot.init.loc<-.GlobalEnv$plot.init
		     plot.init.loc$plot.status<-"report"
		     plot.init.loc$plot.config$plot.message<-"Control and Test data loaded"
		     .GlobalEnv$plot.init<-plot.init.loc

	  	     tkrreplot(.GlobalEnv$img)	 
           }	
       }
    }
}


#fileMenu.2
#parametes submenu
paramMenu <- tkmenu(.GlobalEnv$fileMenu,tearoff=FALSE)

#functions for size and probe submenu
#input data menus
get.file<-function(var)
{
	fileName<-tclvalue(tkgetOpenFile())

	if (!nchar(fileName))
    		tkmessageBox(message="No file was selected!")
	
	file.parts<-strsplit(fileName,split="/")
	fileName.win<-paste(file.parts[[1]],collapse="/")
	assign(var,scan(fileName.win,sep=" "),envir=.GlobalEnv)
	tkfocus(.GlobalEnv$tt)
}

type.data<-function(st, var)
{
	yy<-tktoplevel()
	tkwm.title(yy,"MLPA -enter data")

	dat.var <- tclVar("")
	entry.dat <-tkentry(yy,width="20",textvariable=dat.var)
	OnOK <- function()
	{	
		tkdestroy(yy)
		plot.init.loc<-.GlobalEnv$plot.init
		plot.init.loc$plot.status<-"report"
		plot.init.loc$plot.config$plot.message<-"Size selected"
		.GlobalEnv$plot.init<-plot.init.loc

		tkrreplot(.GlobalEnv$img)

		mlpa<-.GlobalEnv$Ms$mlpa
		mlpa$arg.mlpa$size.dat<-as.numeric(strsplit(tclvalue(dat.var)," ")[[1]])
		.GlobalEnv$Ms$mlpa<-mlpa
	}
	OK.but <-tkbutton(yy,text="   OK   ",command=OnOK)
	tkbind(entry.dat, "<Return>",OnOK)

	tkgrid(tklabel(yy,text=st),entry.dat)
	tkgrid(tklabel(yy,text="    "),OK.but)
	tkfocus(yy)
}

#paramMenu.1
#size submenu
sizeMenu<- tkmenu(paramMenu,tearoff=FALSE)
f1<-function(){
	get.file("size.dat")

	mlpa<-.GlobalEnv$Ms$mlpa
	mlpa$arg.mlpa$size.dat<-.GlobalEnv$size.dat
	.GlobalEnv$Ms$mlpa<-mlpa

	plot.init.loc<-.GlobalEnv$plot.init
	plot.init.loc$plot.status<-"report"
	plot.init.loc$plot.config$plot.message<-"Size selected"
	.GlobalEnv$plot.init<-plot.init.loc

	tkrreplot(.GlobalEnv$img)
	}
f2<-function(){	
	type.data("size","size.dat")	
	}
tkadd(sizeMenu,"command",label="from file", command=f1)
tkadd(sizeMenu,"command",label="type in", command=f2)

#paramMenu.2
#probe submenu
probeMenu<- tkmenu(paramMenu,tearoff=FALSE)
g1<-function(){
	get.file("probe.dat")

	mlpa<-.GlobalEnv$Ms$mlpa
	mlpa$arg.mlpa$probe.dat<-.GlobalEnv$probe.dat
	.GlobalEnv$Ms$mlpa<-mlpa

	plot.init.loc<-.GlobalEnv$plot.init
	plot.init.loc$plot.status<-"report"
	plot.init.loc$plot.config$plot.message<-"Control probes selected"
	.GlobalEnv$plot.init<-plot.init.loc

	tkrreplot(.GlobalEnv$img)
	}

g2<-function(){
	.GlobalEnv$probe.list<-names(.GlobalEnv$Ms$mlpa$arg.mlpa$cont.dat)[3:length(names(.GlobalEnv$Ms$mlpa$arg.mlpa$cont.dat))]
	gui.sel.var.nm(.GlobalEnv$probe.list)
	tkrreplot(.GlobalEnv$img)
	
	}


tkadd(probeMenu,"command",label="from file", command=g1)
tkadd(probeMenu,"command",label="select", command=g2)

#fileMenu.3
#save submenu
saveMenu <- tkmenu(.GlobalEnv$fileMenu,tearoff=FALSE)

#function for saveMenu (setup Mlpa)
setup<-function()
{
	
	#the variables that come out from gui.sel.var (sel.var.nm)
	#need to be assigned to Ms here, e.g. ouside the function that called gui.sel.var

	sel.var.nm<-names(.GlobalEnv$Ms$mlpa$arg.mlpa$cont.dat)[3:length(names(.GlobalEnv$Ms$mlpa$arg.mlpa$cont.dat))]

	if (exists("probe.list")) 
	{
		pos<-1:length(.GlobalEnv$probe.list)

		mlpa<-.GlobalEnv$Ms$mlpa
		mlpa$arg.mlpa$probe.dat<-pos[.GlobalEnv$probe.list%in%.GlobalEnv$sel.var.nm]
		.GlobalEnv$Ms$mlpa<-mlpa

	}

	
	 #set up data
	 
	chk<-!c("cont.dat","test.dat","size.dat","probe.dat")%in%names(.GlobalEnv$Ms$mlpa$arg.mlpa)
	if (sum(chk)!=0)
	{	
		ms<-c("load control file", "load test file", "determine size", "select control probes")
		tkmessageBox(message=ms[chk][1],icon="error", type="ok")
		stop("load all data and parameters\n")
	}else{	
		  
		cont.dat<-.GlobalEnv$Ms$mlpa$arg.mlpa$cont.dat
		test.dat<-.GlobalEnv$Ms$mlpa$arg.mlpa$test.dat
            size.dat<-.GlobalEnv$Ms$mlpa$arg.mlpa$size.dat
            probe.dat<-.GlobalEnv$Ms$mlpa$arg.mlpa$probe.dat

		mlpa<-.GlobalEnv$Ms$mlpa
		mlpa$mlpa.dat<-setupMLPA(cont.dat,test.dat,size.dat,probe.dat)
		.GlobalEnv$Ms$mlpa<-mlpa
            Ms<- .GlobalEnv$Ms
		save(Ms,file="Ms.Rdata")
      	cat("\n")
		cat("Ms.Rdata file saved in: ",getwd(), "\n")

		plot.init.loc<-.GlobalEnv$plot.init
		plot.init.loc$plot.status<-"report"
		plot.init.loc$plot.config$plot.message<-"Ms object saved... next: normalize"
		.GlobalEnv$plot.init<-plot.init.loc

		tkrreplot(.GlobalEnv$img)	 
	}
}


#fileMenu.4
#function for open 
open.mlpa<-function()
{
	#select and read file 
	tkmessageBox(message=paste("Select Ms.Rdata file"))
	fileName<-tclvalue(tkgetOpenFile())
	if (!nchar(fileName))
    		tkmessageBox(message="No file was selected!")

	file.parts<-strsplit(fileName,split="/")
	fileName.win<-paste(file.parts[[1]],collapse="/")
	a<-load(fileName, envir=.GlobalEnv)
	cat("\n")
	cat("opened: \n")
	print(a)

	plot.init.loc<-.GlobalEnv$plot.init
	plot.init.loc$plot.status<-"report"

	switch(length(names(.GlobalEnv$Ms)),
		plot.init.loc$plot.config$plot.message<-"Ms: data",
		plot.init.loc$plot.config$plot.message<-"Ms: data and normalazation",
		plot.init.loc$plot.config$plot.message<-"Ms: data, normalization and analisis"
		)

	.GlobalEnv$plot.init<-plot.init.loc

	tkrreplot(.GlobalEnv$img)

}

#fileMenu.5
display.licence<-function()
{
	.GlobalEnv$plot.init$plot.status<-"licence"
	tkrreplot(.GlobalEnv$img)
} 

#fileMenu.6
#change dir
change.dir<-function()
{
	pwd<-tclvalue(tkchooseDirectory())
	setwd(pwd)

	plot.init.loc<-.GlobalEnv$plot.init
	plot.init.loc$plot.status<-"report"
      dirstr<-unlist(strsplit(pwd,"/"))[length(unlist(strsplit(pwd,"/")))]
	plot.init.loc$plot.config$plot.message<-paste(c("dir: C:/.../",dirstr,"/"),collapse="")
	.GlobalEnv$plot.init<-plot.init.loc

	tkrreplot(.GlobalEnv$img)
}

#fileMenu.7
#quit menu
quit.f <-function()
	tkdestroy(.GlobalEnv$tt)

#fileMenu.8
#load demo data
demoMenu <- tkmenu(.GlobalEnv$fileMenu,tearoff=FALSE)

load.demo1<-function()
{ 
	data(BRCA)
#	source("BRCA.R")

	mlpa<-.GlobalEnv$Ms$mlpa
	mlpa$arg.mlpa$cont.dat<-.GlobalEnv$BRCAcontrols
	mlpa$arg.mlpa$test.dat<-.GlobalEnv$BRCAtests
      mlpa$arg.mlpa$size.dat<-.GlobalEnv$size
      mlpa$arg.mlpa$probe.dat<-.GlobalEnv$probes.control
	mlpa$mlpa.dat<-setupMLPA(.GlobalEnv$BRCAcontrols,.GlobalEnv$BRCAtests,.GlobalEnv$size,.GlobalEnv$probes.control)
	.GlobalEnv$Ms$mlpa<-mlpa

	plot.init.loc<-.GlobalEnv$plot.init
	plot.init.loc$plot.status<-"report"
	plot.init.loc$plot.config$plot.message<-"BRCA data loaded... next: normalize"
	.GlobalEnv$plot.init<-plot.init.loc

	tkrreplot(.GlobalEnv$img)	 

}

load.demo2<-function()
{ 
	data(MLPAvalidation)
#	 source("MLPAvalidation.R")

	mlpa<-.GlobalEnv$Ms$mlpa
	mlpa$arg.mlpa$cont.dat<-.GlobalEnv$controls
	mlpa$arg.mlpa$test.dat<-.GlobalEnv$tests
      mlpa$arg.mlpa$size.dat<-.GlobalEnv$size
      mlpa$arg.mlpa$probe.dat<-.GlobalEnv$probes.control
	mlpa$mlpa.dat<-setupMLPA(.GlobalEnv$controls,.GlobalEnv$tests,.GlobalEnv$size,.GlobalEnv$probes.control)
	.GlobalEnv$Ms$mlpa<-mlpa

	plot.init.loc<-.GlobalEnv$plot.init
	plot.init.loc$plot.status<-"report"
	plot.init.loc$plot.config$plot.message<-"MLPA validation data loaded... next: normalize"
	.GlobalEnv$plot.init<-plot.init.loc

	tkrreplot(.GlobalEnv$img)	 

}


tkadd(demoMenu,"command",label="BRCA", command=load.demo1)
tkadd(demoMenu,"command",label="MLPA validation", command=load.demo2)


#make setup Menu
tkadd(paramMenu,"cascade",label="size", menu=sizeMenu)
tkadd(paramMenu,"cascade",label="control probes", menu=probeMenu)

#make fileMenu

tkadd(.GlobalEnv$fileMenu,"command",label="open Ms...",command=open.mlpa )
tkadd(.GlobalEnv$fileMenu,"cascade",label="load demo...", menu=demoMenu)
tkadd(.GlobalEnv$fileMenu,"separator")
tkadd(.GlobalEnv$fileMenu,"command",label="control and test data...", command=load.win)
tkadd(.GlobalEnv$fileMenu,"cascade",label="probes info...",menu=paramMenu )
tkadd(.GlobalEnv$fileMenu,"command",label="set up & save",command=setup )
tkadd(.GlobalEnv$fileMenu,"separator")
tkadd(.GlobalEnv$fileMenu,"command",label="change dir...", command=change.dir)
tkadd(.GlobalEnv$fileMenu,"separator")
tkadd(.GlobalEnv$fileMenu,"command",label="licence",command=display.licence )
tkadd(.GlobalEnv$fileMenu,"separator")
tkadd(.GlobalEnv$fileMenu,"command",label="quit",command=quit.f )

tkbind(.GlobalEnv$tt,"<Destroy>",function() cat("bye"))

}

