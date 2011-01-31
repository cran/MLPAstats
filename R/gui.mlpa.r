##launch MLPA window##
gui.mlpa<-function()
{
.GlobalEnv$tt<-tktoplevel()
cat("wellcome to MLPAstats \n")
tkwm.title(.GlobalEnv$tt,"MLPAstats")
tkfocus(.GlobalEnv$tt)

#global variables
#set up Ms object

.GlobalEnv$Ms<-list()
.GlobalEnv$plot.init<-list(plot.status="intro")

#creates top menu
.GlobalEnv$topMenu<-tkmenu(.GlobalEnv$tt)

#create data menu
tkconfigure(.GlobalEnv$tt,menu=.GlobalEnv$topMenu)

#options within top menu
get.file.menu()
get.normalize.menu()
get.analysis.menu()
get.results.menu()


#maketopMenu
tkadd(.GlobalEnv$topMenu,"cascade",label="File",menu=.GlobalEnv$fileMenu)
tkadd(.GlobalEnv$topMenu,"cascade",label="Normalize",menu=.GlobalEnv$normalizeMenu)
tkadd(.GlobalEnv$topMenu,"cascade",label="Analysis",menu=.GlobalEnv$analysisMenu)
tkadd(.GlobalEnv$topMenu,"cascade",label="Results",menu=.GlobalEnv$resultsMenu)

function.plot<-function()
{
	get.function.plot()
}

Click<-function(x,y)
{
	get.Click(x,y)
}

.GlobalEnv$img<-tkrplot(.GlobalEnv$tt,function.plot,hscale=1.5,vscale=1.5)

tkbind(.GlobalEnv$img, "<Button-1>",Click)
tkconfigure(.GlobalEnv$img,cursor="hand2")

tkgrid(.GlobalEnv$img)


tkfocus(.GlobalEnv$tt)

}
