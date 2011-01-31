get.analysis.menu<-function()
{

.GlobalEnv$analysisMenu<-tkmenu(.GlobalEnv$topMenu,tearoff=FALSE)

call.mlpa<-function(method)
{
	if (length(.GlobalEnv$Ms$norm$norm.dat)==0)
	{
		tkmessageBox(message="could not find normalized data!",icon="error", type="ok")
		stop("could not find normalized data!")
	}

	an.options<-c("threshold","mixed-model","REX-MLPA")
	an.status<-match(method,an.options)

	switch(an.status,
	{
		#case threshold	
		#text entries
		#text 

	  if(.GlobalEnv$Ms$norm$rep)	
	  {
		tkmessageBox(message="run mixed-model for data with replicas",icon="error", type="ok")
		stop("wrong analysis!")

	  }else{
		mm <- tktoplevel()
		tkfocus(mm)
		tkwm.title(mm,"MLPA -call")

		text.th<-tclVar("0.7,1.33")
		Entry.th<-tkentry(mm,width=10,textvariable= text.th)

		#ok button
		OnOK <- function()
		{
			analysis<-list()
			analysis$analysis.config$method <- method
	
			s.th <- tclvalue(text.th)
			spl.s.th<-strsplit(s.th,",")
			threshold<-as.numeric(spl.s.th[[1]])
			analysis$analysis.config$norm<- c(paste(.GlobalEnv$Ms$norm$type,"; rep=",.GlobalEnv$Ms$norm$rep))

			analysis$analysis.config$threshold<-threshold

			analysis$analysis.res<-mlpa(.GlobalEnv$Ms$norm$norm.dat,method=method,threshold=threshold)

			.GlobalEnv$Ms$analysis<-analysis

      		tkdestroy(mm)

			cat("\n")
                  Ms <- .GlobalEnv$Ms 
			print(Ms$analysis$analyis.res)
			save(Ms,file="Ms.Rdata")
			cat("\n")
			cat("Ms.Rdata file saved in: ",getwd(), "\n")
			cat("\n")
			plot.init.loc<-.GlobalEnv$plot.init
			plot.init.loc$plot.status<-"report"
			plot.init.loc$plot.config$plot.message<-"Analysis done, Ms saved... check results!"
			.GlobalEnv$plot.init<-plot.init.loc
			tkrreplot(.GlobalEnv$img)
		}
		OK.but <- tkbutton(mm,text="OK",command=OnOK)


		#build the grid
		tkgrid(tklabel(mm,text="threshold analysis"))
		tkgrid(tklabel(mm,text="threshold: "),Entry.th)
		tkgrid(OK.but)
		tkfocus(mm)
	  }
	},{
		#case mixed model	
		#text entries
		#text 

	  if(!.GlobalEnv$Ms$norm$rep)	
	  {
		tkmessageBox(message="cannot run mixed-model for data without replicas",icon="error", type="ok")
		stop("wrong analysis!")

	  }else{

		if(length(levels(factor(.GlobalEnv$Ms$mlpa$arg.mlpa$test.dat[[2]])))==1)
		{
			tkmessageBox(message="could not find replicas! try another method",icon="error", type="ok")
			stop("could not find replicas!")
		}


		mm <- tktoplevel()
		tkfocus(mm)
		tkwm.title(mm,"MLPA -call")


		text.gamma<-tclVar("0.95")
		Entry.gamma<-tkentry(mm,width=10,textvariable= text.gamma)

		text.alpha<-tclVar("0.01")
		Entry.alpha<-tkentry(mm,width=10,textvariable= text.alpha)

		#ok button
		OnOK <- function()
		{
			analysis<-list()
			analysis$analysis.config$method <- method
	
			s.gamma<-tclvalue(text.gamma)
			gamma<-as.numeric(s.gamma)
			analysis$analysis.config$gamma<-gamma

	
			s.alpha<-tclvalue(text.alpha)
			alpha<-as.numeric(s.alpha)
			analysis$analysis.config$alpha<-alpha
			analysis$analysis.config$norm<- c(paste(.GlobalEnv$Ms$norm$type,"; rep=",.GlobalEnv$Ms$norm$rep))



			analysis$analysis.res<-mlpa(.GlobalEnv$Ms$norm$norm.dat,method=method,gamma=gamma,alpha=alpha)

			.GlobalEnv$Ms$analysis<-analysis

      		tkdestroy(mm)	

			cat("\n")
                  Ms <- .GlobalEnv$Ms
			print(Ms$analysis$analyis.res)
			save(Ms,file="Ms.Rdata")
			cat("\n")
			cat("Ms.Rdata file saved in: ",getwd(), "\n")
			cat("\n")
			plot.init.loc<-.GlobalEnv$plot.init
			plot.init.loc$plot.status<-"report"
			plot.init.loc$plot.config$plot.message<-"Analysis done, Ms saved... check results!"
			.GlobalEnv$plot.init<-plot.init.loc
			tkrreplot(.GlobalEnv$img)
		}
		OK.but <- tkbutton(mm,text="OK",command=OnOK)


		#build the grid
		tkgrid(tklabel(mm,text="mixed model analysis"))
		tkgrid(tklabel(mm,text="gamma: "),Entry.gamma)
		tkgrid(tklabel(mm,text="alpha: "),Entry.alpha)
		tkgrid(OK.but)
		tkfocus(mm)
        }
	},{
		#case REX-MLPA	
		#text entries
		#text 


	  if(.GlobalEnv$Ms$norm$rep)	
	  {
		tkmessageBox(message="mixed-model requires data with replicas",icon="error", type="ok")
		stop("wrong analysis!")

	  }else{

		mm <- tktoplevel()
		tkfocus(mm)
		tkwm.title(mm,"MLPA -call")

		#bands
		rb1 <- tkradiobutton(mm)
		rb2 <- tkradiobutton(mm)
		rband <- tclVar("parametric")
		tkconfigure(rb1,variable=rband,value="parametric")
		tkconfigure(rb2,variable=rband,value="bootstrap")

		text.alpha<-tclVar("0.05")
		Entry.alpha<-tkentry(mm,width=10,textvariable= text.alpha)

		#ok button
		OnOK <- function()
		{
       		tkdestroy(mm)	
			analysis<-list()
			analysis$analysis.config$method <- method

			band<-as.character(tclvalue(rband))	
			analysis$analysis.config$band <- band

			if(band=="bootstrap")
				{

				plot.init.loc<-.GlobalEnv$plot.init
				plot.init.loc$plot.status<-"report"
				plot.init.loc$plot.config$plot.message<-"Please wait... REX-MLPA bootstrap bands running... "
				.GlobalEnv$plot.init<-plot.init.loc
				tkrreplot(.GlobalEnv$img)

				}
			s.alpha<-tclvalue(text.alpha)
			alpha<-as.numeric(s.alpha)
			analysis$analysis.config$alpha<-alpha
			analysis$analysis.config$norm<- c(paste(.GlobalEnv$Ms$norm$type,"; rep=",.GlobalEnv$Ms$norm$rep))


			analysis$analysis.res<-mlpa(.GlobalEnv$Ms$norm$norm.dat,method=method,band=band,alpha=alpha)

			.GlobalEnv$Ms$analysis<-analysis


			cat("\n")
                  Ms<- .GlobalEnv$Ms
			print(Ms$analysis$analyis.res)
			save(Ms,file="Ms.Rdata")
			cat("\n")
			cat("Ms.Rdata file saved in: ",getwd(), "\n")
			cat("\n")
			plot.init.loc<-.GlobalEnv$plot.init
			plot.init.loc$plot.status<-"report"
			plot.init.loc$plot.config$plot.message<-"Analysis done, Ms saved... check results!"
			.GlobalEnv$plot.init<-plot.init.loc
			tkrreplot(.GlobalEnv$img)
		}
		OK.but <- tkbutton(mm,text="OK",command=OnOK)


		#build the grid
		tkgrid(tklabel(mm,text="REX-MLPA analysis"))
		tkgrid(tklabel(mm,text="Bands:"),tklabel(mm,text="parmetric "),rb1,tklabel(mm,text="bootstrap "),rb2)
		tkgrid(tklabel(mm,text="alpha: "),Entry.alpha)
		tkgrid(OK.but)
		tkfocus(mm)
       }
	})

	 
	
}

n1<-function(){call.mlpa("threshold")}
n2<-function(){call.mlpa("mixed-model")}
n3<-function(){call.mlpa("REX-MLPA")}

tkadd(.GlobalEnv$analysisMenu,"command",label="threshold", command=n1)
tkadd(.GlobalEnv$analysisMenu,"command",label="REX-MLPA", command=n3)
tkadd(.GlobalEnv$analysisMenu,"separator")
tkadd(.GlobalEnv$analysisMenu,"command",label="mixed-model", command=n2)

}
