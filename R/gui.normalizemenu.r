#a pull down manu that spacifies the field Ms$norm.dat
get.normalize.menu<-function()
{
##make normalize menu
.GlobalEnv$normalizeMenu<-tkmenu(.GlobalEnv$topMenu,tearoff=FALSE)

rep.normalizeMenu<-tkmenu(.GlobalEnv$normalizeMenu,tearoff=FALSE)
#function to get methods for normalization
call.normalize<-function(stn,rep) 
{		
		nm.ck<-names(.GlobalEnv$Ms$mlpa$arg.mlpa)
		if (!"cont.dat"%in%nm.ck){
			tkmessageBox(message="load all data, set up & save",icon="error", type="ok")		
			stop("load control samples")}

		if (!"test.dat"%in%nm.ck){
			tkmessageBox(message="load all data, set up & save",icon="error", type="ok")
			stop("load test samples")}

		if (!"size.dat"%in%nm.ck){
			tkmessageBox(message="load all data, set up & save", type="ok")
		      stop("load probe sizes")}
	
		if (!"probe.dat"%in%nm.ck){
			tkmessageBox(message="load all data, set up & save",icon="error", type="ok")
			stop("select control probes")}

		if (!"mlpa.dat"%in%names(.GlobalEnv$Ms$mlpa)){
			tkmessageBox(message="remember to set up and save!",icon="error", type="ok")
			stop("remember to set up and save!")}

		norm<-.GlobalEnv$Ms$norm
		norm$norm.dat<-mlpaNorm(.GlobalEnv$Ms$mlpa$mlpa.dat,method=stn,replicate=rep)
		norm$type<-stn
		norm$rep<-rep

		.GlobalEnv$Ms$norm<-norm

            Ms<-.GlobalEnv$Ms 
		save(Ms,file="Ms.Rdata")
		cat("\n")
		cat("Ms file saved in: ",getwd(), "\n")
		cat("saved: \n")
		cat("nomalized MLPA data \n")
		
		plot.init.loc<-.GlobalEnv$plot.init
		plot.init.loc$plot.status<-"report"
		if (rep)
			plot.init.loc$plot.config$plot.message<-paste("normalization done.... next analysis: mixed-model")
		else
			plot.init.loc$plot.config$plot.message<-paste("normalization done.... next analysis: thresold or REX-MLPA")
				

		.GlobalEnv$plot.init<-plot.init.loc
		
		tkrreplot(.GlobalEnv$img)
	
}

n1<-function() 
{
	if(length(levels(factor(.GlobalEnv$Ms$mlpa$arg.mlpa$test.dat[[2]])))==1)
	{
		tkmessageBox(message="no replicas available! try another method",icon="error", type="ok")
		stop("no replicas available!")
	}
	call.normalize(stn="nonlinear",rep=TRUE)
}

n3<-function() 
{	if(length(levels(factor(.GlobalEnv$Ms$mlpa$arg.mlpa$test.dat[[2]])))==1)
	{
		tkmessageBox(message="no replicas available! try another method",icon="error", type="ok")
		stop("could not find replicas!")
	}
	call.normalize(stn="sum.peaks.controls",rep=TRUE)
}

n4<-function() 
{	if(length(levels(factor(.GlobalEnv$Ms$mlpa$arg.mlpa$test.dat[[2]])))==1)
	{
		tkmessageBox(message="no replicas available! try another method",icon="error", type="ok")
		stop("could not find replicas!")
	}

	call.normalize(stn="sum.peaks.all",rep=TRUE)
}

norep.normalizeMenu<-tkmenu(.GlobalEnv$normalizeMenu,tearoff=FALSE)

m2<-function() {call.normalize(stn="slope.correction",rep=FALSE)}
m3<-function() {call.normalize(stn="sum.peaks.controls",rep=FALSE)}
m4<-function() {call.normalize(stn="sum.peaks.all",rep=FALSE)}


#make rep.nomalize Menu
tkadd(rep.normalizeMenu,"command",label="sum peaks controls", command=n3)
tkadd(rep.normalizeMenu,"command",label="sum peaks all", command=n4)
tkadd(rep.normalizeMenu,"command",label="nonlinear", command=n1)

#make norep.nomalize Menu
tkadd(norep.normalizeMenu,"command",label="sum peaks controls", command=m3)
tkadd(norep.normalizeMenu,"command",label="sum peaks all", command=m4)
tkadd(norep.normalizeMenu,"command",label="slope correction", command=m2)


tkadd(.GlobalEnv$normalizeMenu,"cascade",label="without replicas", menu=norep.normalizeMenu)
tkadd(.GlobalEnv$normalizeMenu,"cascade",label="with replicas", menu=rep.normalizeMenu)

}
