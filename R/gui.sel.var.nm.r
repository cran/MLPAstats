#gets window to select variables interactively

gui.sel.var.nm<-function(probe.list)
{
.GlobalEnv$plot.init$plot.status<-"select.var"
variable.list<-c(sapply(probe.list,substr,1,10),"select ALL", "Unselect all", "OK")

.GlobalEnv$plot.init$plot.config<-list( 
				variable.list=variable.list,
				labelsVec="*",
 		 		status=rep(" ",length(variable.list)),
				indexLabeled=c(),
				labeledPoints=list(),
				done=FALSE, 
			
				#accommodate only 60 variables names
				xCoords=c(c(rep(1,20),rep(2,20),rep(3,20))[(length(variable.list)-3):1],
					1,2,3),
				yCoords=c(c(20:1,20:1,20:1)[(length(variable.list)-3):1],-1,-1,-1),
				parPlotSize=c(),
				usrCoords=c()
	 		    	)

tkrreplot(.GlobalEnv$img)
tkfocus(.GlobalEnv$tt)
}
