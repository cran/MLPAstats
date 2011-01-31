#gets function plots and oneleftclick for interactive window 
#global variable plot.init is used to pass arguments to different stages of this function

get.function.plot <- function()
{
	plot.options<-c("intro","licence","select.var","report","normalize","analysis")
	plot.status<-match(.GlobalEnv$plot.init$plot.status,plot.options)
	switch(plot.status,
		{	
		#case "intro"
		xCoords<-rep(1,10)
		yCoords<-1:10

		params <- par(bg="white")
		plot(xCoords,yCoords,xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n",
			xlim=c(-1,10))

		Rlogo <- read.pnm(system.file("pictures/logo.ppm", package="pixmap")[1])
		addlogo(Rlogo,c(7,8.4),c(8.8,10.2))
		text(0,8.5,"MLPAstats", pos=4,offset=-1, adj=1, cex=2.5*3/2)
	    	text(0,7.5,"A package for the analysis of MLPA data", pos=4,offset=-1, adj=1, cex=0.7*3/2)
		text(0,5,"jrgonzalez@creal.cat", pos=4,offset=-1, adj=1, cex=0.6*3/2)
		text(0,4.5,"Centre for Research in Enviromental Epidemiology",pos=4,offset=-1, adj=1, cex=0.6*3/2) 
		text(0,4,"2009, Barcelona, Spain", pos=4,offset=-1, adj=1, cex=0.6*3/2)
		text(0,2,"Copyright (C) 2011 J. R. Gonzalez & A. Caceres", pos=4,offset=-1, adj=1, cex=0.6*3/2)
		text(0,1.5,"This is free software (see licence under File menu)", pos=4,offset=-1, adj=1, cex=0.6*3/2)
		text(0,1,"type: >vignette(''MLPAstats'') for a quick guide", pos=4,offset=-1, adj=1, cex=0.6*3/2)


		tkfocus(.GlobalEnv$tt)
		},
		{
		#case "licence"
		xCoords<-rep(1,10)
		yCoords<-1:10

		params <- par(bg="white")
		plot(xCoords,yCoords,xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n",
			xlim=c(-1,10))

		title("MLPA stats: Licence",adj=0)
 
		text(-0.6,9,"This software is distributed under the terms of the GNU General",pos=4,offset=-1, adj=1, cex=0.55*3/2)
		text(-0.6,8.5,"Public License Version 2, June 1991.  The terms of this license",pos=4,offset=-1, adj=1, cex=0.55*3/2)
	 	text(-0.6,8,"are in a file called COPYING which you should have received ",pos=4,offset=-1, adj=1, cex=0.55*3/2)
		text(-0.6,7.5,"with this software and which can be displayed by typing", pos=4,offset=-1, adj=1, cex=0.55*3/2)
		text(-0.6,7,"RShowDoc(\"COPYING\") on the R prompt.",pos=4,offset=-1, adj=1, cex=0.55*3/2)
		text(-0.6,5.5,"If you have not received a copy of this file,you can obtain",pos=4,offset=-1, adj=1, cex=0.55*3/2)
		text(-0.6,5,"one at http://www.R-project.org/licenses/.",pos=4,offset=-1, adj=1, cex=0.55*3/2)
		text(-0.6,4.,"A small number of files (the API header files listed in",pos=4,offset=-1, adj=1, cex=0.55*3/2)
		text(-0.6,3.5,"R_DOC_DIR/COPYRIGHTS) are distributed under the Lesser",pos=4,offset=-1, adj=1, cex=0.55*3/2)
		text(-0.6,3,"GNU General Public LIcense version 2.1. This can be displayed by",pos=4,offset=-1, adj=1, cex=0.55*3/2)
		text(-0.6,2.5,"RShowDoc(\"COPYING.LIB\"),or obtained at the URI given.", pos=4,offset=-1, adj=1, cex=.55*3/2)
		tkfocus(.GlobalEnv$tt)
		},
		{
		#case "select.var"
		if (search()[2]!="plot.init$plot.config")
			attach(.GlobalEnv$plot.init$plot.config,2)
	 		done <-get("done",2)

		if(done==FALSE)
		{	
			params <- par(bg="white")
      		xCoords<-get("xCoords",2)
      		yCoords<-get("yCoords",2)
      		indexLabeled<-get("indexLabeled",2)
      		variable.list<-get("variable.list",2)
      		parPlotSize<-get("parPlotSize",2)
      		usrCoords<-get("usrCoords",2)
  
			plot(xCoords,yCoords,xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n",
				xlim=c(0.5,4),ylim=c(-1,20))

			title("MLPA stats: Select Variables",adj=0)
		
 			if (length(indexLabeled)>0)
 			  status[indexLabeled]<-"*"

			for (j in (1:(length(variable.list)-3)))
				text(xCoords[j],yCoords[j],paste(status[j],variable.list[length(variable.list)-2-j]),
					pos=4,offset=-1,cex=0.7*3/2)


			text(xCoords[length(variable.list)-2],yCoords[length(variable.list)-2],
				"Sel. all",pos=4,offset=-1,cex=1*3/2)
			text(xCoords[length(variable.list)-1],yCoords[length(variable.list)-1],
				"Unsel.  ",pos=4,offset=-1,cex=1*3/2)
			text(xCoords[length(variable.list)],yCoords[length(variable.list)],
			"OK",pos=4,offset=-1,cex=1*3/2)

 			.GlobalEnv$parPlotSize <- par("plt")
 			.GlobalEnv$usrCoords   <- par("usr")
 			par(params)

		}else{
		
      		indexLabeled<-get("indexLabeled",2)
      		variable.list<-get("variable.list",2)

			xCoords<-rep(1,10)
			yCoords<-1:10

			params <- par(bg="white")
			plot(xCoords,yCoords,xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n",
			xlim=c(-1,10))
			title("MLPA stats: Report",adj=0)
			text(1,6,"> Variables Selected",pos=4,offset=-1, cex=0.5*3/2) 
			
			sel.var<-length(variable.list)-2-indexLabeled
			.GlobalEnv$sel.var.nm<-variable.list[sel.var]
			.GlobalEnv$variable.list<-variable.list
			detach(2)

			tkfocus(.GlobalEnv$tt)
		}
		},
		{
		#case "report"  
        
		xCoords<-rep(1,10)
		yCoords<-1:10

		params <- par(bg="white")
		plot(xCoords,yCoords,xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n",
			xlim=c(-1,10))

		title("MLPA stats: Report",adj=0) 
		text(1,6,paste("> ", .GlobalEnv$plot.init$plot.config$plot.message, collapse=""),pos=4,offset=-1, cex=0.5*3/2) 

		tkfocus(.GlobalEnv$tt)

		},
		{
		#case normalize
		if (!"norm"%in%names(.GlobalEnv$Ms))
		{
			tkmessageBox(message="could not find normalized data!",icon="error", type="ok")
		
			xCoords<-rep(1,10)
			yCoords<-1:10
			params <- par(bg="white")
			plot(xCoords,yCoords,xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n",
				xlim=c(-1,10))
	
			title("MLPA stats: Error",adj=0) 
			text(1,6,paste("> Could not find nomalized data", collapse=""),pos=4,offset=-1, cex=0.5*3/2) 

			tkfocus(.GlobalEnv$tt)
		}else{
			if(.GlobalEnv$plot.init$norm=="control"){
				plot(.GlobalEnv$Ms$norm$norm.dat,individuals="control")
			}else{
				plot(.GlobalEnv$Ms$norm$norm.dat,individuals="test", subject=.GlobalEnv$plot.init$sub)
			}

			
		}		
		},
		{
		#case analysis

		if (!"analysis"%in%names(.GlobalEnv$Ms))
		{
			tkmessageBox(message="could not find analyzed data!",icon="error", type="ok")
			xCoords<-rep(1,10)
			yCoords<-1:10
			params <- par(bg="white")
			plot(xCoords,yCoords,xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n",
				xlim=c(-1,10))
	
			title("MLPA stats: Error",adj=0) 
			text(1,6,paste("> Could not find analyzed data", collapse=""),pos=4,offset=-1, cex=0.5*3/2) 

			tkfocus(.GlobalEnv$tt)
		}else{
	
			plot(.GlobalEnv$Ms$analysis$analysis.res)
			tkfocus(.GlobalEnv$tt)
		}		
		})
	

	
}

 get.Click<- function(x,y)
{  

 if(.GlobalEnv$plot.init$plot.status=="select.var")
 {
  xCoords<-get("xCoords",2)
  yCoords<-get("yCoords",2)
  indexLabeled<-get("indexLabeled",2)
  variable.list<-get("variable.list",2)
  
  done <-get("done",2)

  xClick <- x
  yClick <- y
  require(tcltk)
  width  <- as.numeric(tclvalue(tkwinfo("reqwidth",.GlobalEnv$img)))
  height <- as.numeric(tclvalue(tkwinfo("reqheight",.GlobalEnv$img)))

  xMin <- .GlobalEnv$parPlotSize[1] * width
  xMax <- .GlobalEnv$parPlotSize[2] * width
  yMin <- .GlobalEnv$parPlotSize[3] * height
  yMax <- .GlobalEnv$parPlotSize[4] * height

  rangeX <- .GlobalEnv$usrCoords[2] - .GlobalEnv$usrCoords[1]
  rangeY <- .GlobalEnv$usrCoords[4] - .GlobalEnv$usrCoords[3]

  imgXcoords <- (xCoords-.GlobalEnv$usrCoords[1])*(xMax-xMin)/rangeX + xMin
  imgYcoords <- (yCoords-.GlobalEnv$usrCoords[3])*(yMax-yMin)/rangeY + yMin

  xClick <- as.numeric(xClick)+0.5
  yClick <- as.numeric(yClick)+0.5
  yClick <- height - yClick

  xPlotCoord <- .GlobalEnv$usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
  yPlotCoord <- .GlobalEnv$usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)

  if(!done)
  {
  	squared.Distance <- (xClick-imgXcoords)^2 + (yClick-imgYcoords)^2
  	indexClosest <- which.min(squared.Distance)
 	  .GlobalEnv$is.selected<-indexClosest==indexLabeled
  	if (sum(.GlobalEnv$is.selected)==1)
		assign("indexLabeled",indexLabeled[!.GlobalEnv$is.selected],2)
  	else	
  		assign("indexLabeled",c(indexLabeled,indexClosest),2)

  	if ((length(variable.list)-2)%in%indexLabeled)
		assign("indexLabeled",1:(length(variable.list)-3),2)

  	if ((length(variable.list)-1)%in%indexLabeled)
		assign("indexLabeled",c(),2)

  	if(length(variable.list)%in%indexLabeled)
  	{
		assign("indexLabeled",indexLabeled[-length(indexLabeled)],2)
		assign("done",TRUE,2)
  	}
  }
 tkrreplot(.GlobalEnv$img)
 }
}
