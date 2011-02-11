get.results.menu<-function()
{
	.GlobalEnv$resultsMenu<-tkmenu(.GlobalEnv$topMenu,tearoff=FALSE)

	
	#plot submenu
	plotMenu<-tkmenu(.GlobalEnv$resultsMenu,tearoff=FALSE)

	#analysis submenu
	analMenu<-tkmenu(.GlobalEnv$topMenu,tearoff=FALSE)
	#norm submenu
	normMenu<-tkmenu(.GlobalEnv$topMenu,tearoff=FALSE)


	choose.plot.anal.REX<-function()
	{

		mm <- tktoplevel()
		tkfocus(mm)
		tkwm.title(mm,"MLPA -subject")

		text.s<-tclVar("1")
		Entry.s<-tkentry(mm,width=10,textvariable= text.s)

		#ok button	
		OnOK <- function()
		{

			s<-tclvalue(text.s)
			sub<-as.numeric(s)
			tkdestroy(mm)
			
			if(sub>length(.GlobalEnv$Ms$mlpa$arg.mlpa$test.dat))
			{
				tkmessageBox(message="subject number too large",icon="error", type="ok")
			}else{
                  
				.GlobalEnv$plot.init$plot.status<-"analysis"
				.GlobalEnv$plot.init$sub<-sub
                        .GlobalEnv$plot.init$type<-4

				tkrreplot(.GlobalEnv$img)
				tkfocus(.GlobalEnv$tt)
			}

		}
		OK.but <- tkbutton(mm,text="OK",command=OnOK)

		#build the grid
		tkgrid(tklabel(mm,text="select case subject"))
		tkgrid(tklabel(mm,text="subject: "),Entry.s)
		tkgrid(OK.but)
		tkfocus(mm)
	}

	choose.plot.anal.others<-function()
	{
		.GlobalEnv$plot.init$plot.status<-"analysis"
		.GlobalEnv$plot.init$sub<-1
            .GlobalEnv$plot.init$type<-c(1,2)

		tkrreplot(.GlobalEnv$img)
		tkfocus(.GlobalEnv$tt)

	}	

	choose.plot.norm.test<-function()
	{

		mm <- tktoplevel()
		tkfocus(mm)
		tkwm.title(mm,"MLPA -subject")

		text.s<-tclVar("1")
		Entry.s<-tkentry(mm,width=10,textvariable= text.s)

		#ok button	
		OnOK <- function()
		{

			s<-tclvalue(text.s)
			sub<-as.numeric(s)
			tkdestroy(mm)
			
			if(sub>length(.GlobalEnv$Ms$mlpa$arg.mlpa$test.dat))
			{
				tkmessageBox(message="subject number too large",icon="error", type="ok")
			}else{
				.GlobalEnv$plot.init$plot.status<-"normalize"
				.GlobalEnv$plot.init$norm<-"test"
				.GlobalEnv$plot.init$sub<-sub

				tkrreplot(.GlobalEnv$img)
				tkfocus(.GlobalEnv$tt)
			}

		}
		OK.but <- tkbutton(mm,text="OK",command=OnOK)

		#build the grid
		tkgrid(tklabel(mm,text="select case subject"))
		tkgrid(tklabel(mm,text="subject: "),Entry.s)
		tkgrid(OK.but)
		tkfocus(mm)
	}	

	choose.plot.norm.control<-function()
	{
		.GlobalEnv$plot.init$plot.status<-"normalize"
		.GlobalEnv$plot.init$norm<-"control"
		tkrreplot(.GlobalEnv$img)
		tkfocus(.GlobalEnv$tt)

	}	



	tkadd(normMenu,"command",label="case sample...", command=choose.plot.norm.test)
	tkadd(normMenu,"command",label="mean controls", command=choose.plot.norm.control)
	tkadd(plotMenu,"cascade",label="normalization", menu=normMenu)

      tkadd(analMenu,"command",label="Regresion REX ...", command=choose.plot.anal.REX)
	tkadd(analMenu,"command",label="Threshold and mixed model", command=choose.plot.anal.others)
	tkadd(plotMenu,"cascade",label="analysis", menu=analMenu)


	
	#stats menu
	statsMenu<-tkmenu(.GlobalEnv$resultsMenu,tearoff=FALSE)

	get.res<-function()
	{
		plot.init.loc<-.GlobalEnv$plot.init
		plot.init.loc$plot.status<-"report"
		plot.init.loc$plot.config$plot.message<-"printing results on R console"
		.GlobalEnv$plot.init<-plot.init.loc
 		tkrreplot(.GlobalEnv$img)	 
		cat("\n")
		print(.GlobalEnv$Ms$analysis$analysis.res)
	}


	tkadd(.GlobalEnv$resultsMenu,"cascade",label="plot", menu=plotMenu)
	tkadd(.GlobalEnv$resultsMenu,"command",label="display on R", command=get.res)


}
