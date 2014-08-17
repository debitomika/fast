library(cluster)
library(shinyBS)
library(R2wd)

upload<-reactive({
  getdata()
})

############################Hierarchical###################################

output$clusteringH<- renderUI ({
  sidebarLayout(
    sidebarPanel(
        wellPanel(
          HTML(paste("<label><strong>Menu:", "Clustering","</strong></label>")),
          HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
          HTML(paste("<label><strong>Data:",isolate(input$datasets),"</strong></label>"))
          ),
        wellPanel(
            uiOutput("clusVarH")
          )
      ),
    mainPanel(
      tabsetPanel(
        id = "clusTabH",
        tabPanel(
          div(class = "busy",
              p("Calculation in progress ..."),
              img(src="ajaxloaderq.gif")
          ),
          title = "Summary",
          helpText("Clustering is an effort to classify similar objects
                   in the same groups. Cluster analysis constructs", strong("good cluster"),
                   "when the members of a cluster have a", strong("high degree of similarity"),
                   "of each other (internal homogeneity) and are not like members of each other
                   clusters (external homogeneity).", br(), br(),
				   strong("SUMMARY OF HIERARCHICAL CLUSTER ANALYSIS :"), br()
				   ),
		  conditionalPanel(condition = "input.typeh == 'Agglomerative'",
			helpText(br(), strong("AGGLOMERATIVE COEFFICIENT"), " is a measure of the clustering structure of the dataset.
			For each observation i, denote by m(i) its dissimilarity to the first cluster it is
			merged with, divided by the dissimilarity of the merger in the final step of the
			algorithm. The ac is the average of all 1 - m(i). It can also be seen as the average
			width (or the percentage filled) of the banner plot. Because ac grows with the
			number of observations, this measure should not be used to compare datasets of
			very different sizes.")
		  ),
		  conditionalPanel(condition = "input.typeh == 'Divisive'",
			helpText(br(), strong("DIVISIVE COEFFICIENT"), " is a measure of the the clustering structure of the dataset. 
			For each observation i, denote by d(i) the diameter of the last cluster to which it belongs 
			(before being split off as a single observation), divided by the diameter of the whole dataset. 
			The dc is the average of all 1 - d(i). It can also be seen as the average width (or the 
			percentage filled) of the banner plot. Because dc grows with the number of observations, 
			this measure should not be used to compare datasets of very different sizes.")
		  ),
		  helpText(strong(textOutput(outputId = "summaryHcoef")),br()),
            div(
              class = "fluid-row",
              div(
				conditionalPanel(condition = "input.typeh == 'Agglomerative'",
					helpText(strong("AGGLOMERATION SCHEDULE"),
					" is an (n-1) by 2 matrix, where n is the number of observations. 
					Row i of merge describes the merging of clusters at step i of the clustering. 
					If a number j in the row is negative, then the single observation |j| is merged at this stage. 
					If j ispositive, then the merger is with the cluster formed at stage j of the algorithm.")
				),
				conditionalPanel(condition = "input.typeh == 'Divisive'",
					helpText(strong("DIVISION SCHEDULE"),
					" is an (n-1) by 2 matrix, where n is the number of observations. Row i of merge
					describes the split at step n-i of the clustering. If a number j in row r is negative,
					then the single observation jjj is split off at stage n-r. If j is positive, then the
					cluster that will be splitted at stage n-j (described by row j), is split off at stage n-r.")
				),
				helpText(strong(textOutput(outputId = "valid"))),
				tableOutput(outputId = "summaryHmerge"),
				class = "span5"),
              div(
                class = "span7",
				helpText(strong("CLUSTER MEMBERSHIP"),
				" is a table containing clusters in which each observation is included"),
				helpText(strong(textOutput(outputId = "validmember"))),
                tableOutput(outputId = "cutTree")
				)),
          value=1
		  ),
        tabPanel(
          title = "Plot",
          helpText("Display the clustering results as a", 
                   strong("dendrogram plot"), "for hierarchical clustering. 
				   Choose the number of cluster to see membership. Otherwise, 
				   the result is the original dendrogram."),
          plotOutput("plottyH", height = "600px"),
          value=2
          ),
        tabPanel(
          title = "Heatmap",
          helpText("Display and analyze your data with heatmap. Two dimensional image
                   which uses colors to represent data values. Available for", 
                   strong("agglomerative hierarchical clustering")," only."),
          plotOutput("heatmapH" ,height = "1000px"),
          value=3
        ) 
	)
	)
	)
})

output$clusVarH <- renderUI({
  dat <- upload()
	list(
		bsCollapse(multiple = TRUE, open = "colH1", id = "collapseH",
		bsCollapsePanel("Cluster Properties", 
			selectInput('varH', 'Select one or more variables to cluster :', names(dat), multiple=TRUE, selectize=FALSE),
			conditionalPanel(condition = "input.clusTabH == 1 || input.clusTabH == 2",
				selectInput('typeh', 'Select Cluster Method', c('Agglomerative','Divisive')),
				conditionalPanel(condition = "input.typeh == 'Agglomerative'",
					selectInput('distance', 'Select distance measurement', c('ward', 'single', 
						'complete', 'average', 'mcquitty', 'median', 'centroid'))
				),
				selectInput('metricH', 'Select Cluster Metric', c('Euclidean','Manhattan'))
			),
			conditionalPanel(condition = "input.clusTabH == 1 || input.clusTabH == 2",
                numericInput("cutH", "Cluster count", 0, min = 0)
			),
            id="colH1"),
		bsCollapsePanel("View options",
			conditionalPanel(condition = "input.clusTabH == 1",
				numericInput("viewH", "Choose number of observation to view in results ", 10, min = 1)
			),
            id="colH2"),
			bsCollapsePanel("Generate Your Report", 
				selectInput('formatH', 'Document format', c('PDF', 'HTML', 'Word')),
				downloadButton('downloadReportH'),
            id="colH3"
		  )
        )
      )

})

output$valid<-renderText({
	if(is.null(input$varH)){
		print("Please select one or more variable to see the result")
	}
})

output$validmember<-renderText({
	if(is.null(input$varH) || is.null(input$cutH) || input$cutH == 0 ){
		print("Please select one or more variable, and choose the number of cluster to see the result")
	}
})

output$summaryHmerge = renderTable({
	if(is.null(clusters())) return()
	sumH<-clusters()
	sumH$merge
	head(sumH$merge, n = input$viewH)
}
)

output$summaryHcoef<-renderText({
	if(is.null(clusters())) return("Please select one or more variable to see the result")
	sumH<-clusters()
	if(input$typeh=='Agglomerative'){
		print(sumH$ac)
	}
	else{
		print(sumH$dc)
	}
	
})

output$plottyH<-renderPlot({
	if(is.null(input$varH))return()
	treeH <- clusters()
	pltree(treeH, main="Cluster Dendrogram", xlab=input$dataset)
	if(is.null(input$cutH) || input$cutH==0 || input$cutH==1) return()
	rect.hclust(treeH, k = input$cutH, border = "red")
})

output$heatmapH <-renderPlot({
  if(is.null(input$varH))return()
  x <- as.matrix(scale(selectedData()))
  hv <- heatmap(x, col = topo.colors(200, alpha=0.5),
				Colv=F, scale="none", margins = c(1,1)
                ) 			
})

output$cutTree<-renderTable({
  if(input$nav_fast=="Hierarchical"){
    if(is.null(input$varH) || input$cutH==0 || is.null(input$cutH)){
      return()
    }
    else{
      tree <- clusters()
      cutden<-cutree(tree, k=input$cutH)
      cutTab<-as.data.frame(cutden)
      no<-as.data.frame(c(1:nrow(cutTab)))
      result<-data.frame(no,cutTab)
      colnames(result) <- c("no. observation","cluster membership")
      head(result, n = input$viewH)
    }
  }
})

############################Partitional###################################

output$clusteringP<- renderUI ({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        HTML(paste("<label><strong>Menu:", "Clustering","</strong></label>")),
        HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
        HTML(paste("<label><strong>Data:",isolate(input$datasets),"</strong></label>"))
      ),
      wellPanel(
        uiOutput("clusVarP")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "clusTabP",
        tabPanel(
          div(class = "busy",
              p("Calculation in progress ..."),
              img(src="ajaxloaderq.gif")
          ),
          title = "Summary",
          helpText("Clustering is an effort to classify similar objects
                   in the same groups. Cluster analysis constructs", strong("good cluster"),
                   "when the members of a cluster have a", strong("high degree of similarity"),
                   "of each other (internal homogeneity) and are not like members of each other
                   clusters (external homogeneity).", br(), br(),
				   strong("SUMMARY OF PARTITIONAL CLUSTER ANALYSIS :"), br()
				   ),
		  div(
              class = "fluid-row",
              div(
				helpText(strong("CLUSTER MEMBERSHIP"),
				" is a table containing clusters in which each observation is included. See view options
				to manage the number of observations to view"),
				tableOutput(outputId = "members"),
				class = "span5"),
              div(
                class = "span7",
				helpText(strong("Center of each cluster :")),
				tableOutput("centersum"),
				helpText(br(), strong("Within-cluster sum of squares (SSW) :")),
				tableOutput("withinsum"),
				helpText(br(), strong("Between-cluster sum of squares (SSB) :")),
				textOutput("betweensum"),
				helpText(br(), strong("Total-cluster sum of squares (SST) :")),
				textOutput("totsum"),
				helpText(br(), strong("Size of each cluster :")),
				tableOutput("sizesum"),
				helpText(br(), strong("Number of iteration :")),
				textOutput("itersum"),
				helpText(br(), br())
				),
          value=1
          )),
        tabPanel(
          title = "Plot",
          helpText("Display the clustering results as a", 
                   strong("plot"), "for partitional clustering."),
          plotOutput("plottyP", height = "600px"),
          value=2
        )
      
    )
  )
  )
})

output$clusVarP <- renderUI({
  dat <- upload()
  
  bsCollapse(multiple = TRUE, id = "collapseP", open = "colP1",
		bsCollapsePanel("Cluster Properties", 
			selectInput('varP', 'Variables:', names(dat), 
                multiple=TRUE, selectize=FALSE),
			selectInput('typep', 'Select Cluster Method', c('K-Means', 'Pillar K-Means')),
			numericInput('clusters', 'Cluster count', 3, min = 1),
			numericInput('maxIter', 'Maximum Iteration', 10, min = 1),
			br(), br(),
            id="colP1"),
        bsCollapsePanel("View options",
				numericInput("viewP", "Choose number of observation to view in results ", 10, min = 1),
            id="colP2"),
			bsCollapsePanel("Generate Your Report", 
				selectInput('formatP', 'Document format', c('PDF', 'HTML', 'Word')),
				downloadButton('downloadReportP'),
            id="colP3"
		  )
    ) 
})

output$centersum<-renderTable({
  if(is.null(input$varP))return()
  sumP<-clusters()
  resultcen<-data.frame(sumP$center)
  colnames(resultcen) <- c("cluster center")
  resultcen
})

output$withinsum<-renderTable({
  if(is.null(input$varP))return()
  sumP<-clusters()
  resultwith<-data.frame(sumP$withinss)
  colnames(resultwith) <- c("SSW")
  resultwith
})

output$betweensum<-renderText({
  if(is.null(input$varP))return("Please select one or more variable to see the result")
  sumP<-clusters()
  sumP$betweenss
})

output$totsum<-renderText({
  if(is.null(input$varP))return("Please select one or more variable to see the result")
  sumP<-clusters()
  sumP$totss
})

output$sizesum<-renderTable({
  if(is.null(input$varP))return()
  sumP<-clusters()
  resultsize<-data.frame(sumP$size)
  colnames(resultsize) <- c("cluster size")
  resultsize
})

output$itersum<-renderText({
  if(is.null(input$varP))return("Please select one or more variable to see the result")
  sumP<-clusters()
  sumP$iter
})


output$plottyP<-renderPlot({
	if(is.null(input$varP))return()
	cl <- clusters()
	plot(selectedData(), col = cl$cluster)
	points(cl$centers, col = 1:5, pch = 8)
})

output$members<-renderTable({
  if(input$nav_fast=="Partitional"){
	if(is.null(input$varP)) return()
	else
	{
		clus<-clusters()
		mem<-as.data.frame(clus$cluster)
		no<-as.data.frame(c(1:nrow(mem)))
		result<-data.frame(no,mem)
		colnames(result) <- c("no. observation","cluster membership")
		head(result, n = input$viewP)
	}
    
  }  
})

############################Function###################################

selectedData <- reactive({
  if(input$nav_fast=="Hierarchical"){
    datH<-upload()[, c(input$varH)]
  }
  else{
    datP<-upload()[, c(input$varP)]
  }
})

clusters<- reactive({
	if(input$nav_fast=="Hierarchical"){
		if(is.null(input$varH)) return()
		else{
			if(input$typeh=='Agglomerative'){
				agnes(selectedData(), method = input$distance, metric = input$metricH)
			}
			else{
				diana(selectedData(), metric = input$metricH)
			}
		}
	}
	else{
	if(is.null(input$varP)|| is.null(input$clusters) || is.null(input$maxIter) ||
	input$clusters == 0 || input$maxIter == 0 ) return ()
	else{
		if(input$typep=="K-Means"){
	  kmeans(selectedData(), input$clusters, iter.max = input$maxIter)
    }
    else{
		##############################Pillar K-Means###############################
		y<-selectedData()
		Y<-as.matrix(y)
		if(input$clusters==1){
			init1<-matrix(0,nrow=nrow(Y),ncol=1)

			grmean<-matrix(0,nrow=1,ncol=ncol(Y))
			for(i in 1:ncol(Y)){
				grmean[1,i]<-mean(Y[,i])
			}
			for(i in 1:nrow(Y)){
				init1[i,]<-sqrt(sum((Y[i,]-grmean)^2))
			}

			c1<-which.max(init1)
			seedcenter<-c(Y[c1,])
			set.seed(seedcenter)
			kmeans(selectedData(), 1, iter.max = input$maxIter)
		}
		else if(input$clusters==2){
			init1<-matrix(0,nrow=nrow(Y),ncol=1)

			grmean<-matrix(0,nrow=1,ncol=ncol(Y))
			for(i in 1:ncol(Y)){
				grmean[1,i]<-mean(Y[,i])
			}
			for(i in 1:nrow(Y)){
				init1[i,]<-sqrt(sum((Y[i,]-grmean)^2))
			}

			c1<-which.max(init1)
			in1<-c(Y[c1,])

			centr<-matrix(0,nrow=nrow(Y),ncol=1)
			for(i in 1:nrow(Y)){
				centr[i,]<-sqrt(sum((Y[i,]-Y[c1,])^2))
			}
			centr[c1,]<-0
			c2<-which.max(centr)
			seedcenter<-c(Y[c1,],Y[c2,])
			set.seed(seedcenter)
			kmeans(selectedData(), 2, iter.max = input$maxIter)
		}
		else if(input$clusters==3){
			init1<-matrix(0,nrow=nrow(Y),ncol=1)

			grmean<-matrix(0,nrow=1,ncol=ncol(Y))
			for(i in 1:ncol(Y)){
				grmean[1,i]<-mean(Y[,i])
			}
			for(i in 1:nrow(Y)){
				init1[i,]<-sqrt(sum((Y[i,]-grmean)^2))
			}

			c1<-which.max(init1)

			centr<-matrix(0,nrow=nrow(Y),ncol=1)
			for(i in 1:nrow(Y)){
				centr[i,]<-sqrt(sum((Y[i,]-Y[c1,])^2))
			}
			centr[c1,]<-0
			c2<-which.max(centr)

			centr2<-matrix(0,nrow=nrow(Y),ncol=1)
			for(i in 1:nrow(Y)){
				centr2[i,]<-sqrt(sum((Y[i,]-Y[c2,])^2))
				centr3<-centr2+centr
			}
			centr3[c2,]<-0
			c3<-which.max(centr3)
			seedcenter<-c(Y[c1,],Y[c2,],Y[c3,])
			set.seed(seedcenter)
			kmeans(selectedData(), 3, iter.max = input$maxIter)	
		}
		else{
			init1<-matrix(0,nrow=nrow(Y),ncol=1)

			grmean<-matrix(0,nrow=1,ncol=ncol(Y))
			for(i in 1:ncol(Y)){
				grmean[1,i]<-mean(Y[,i])
			}
			for(i in 1:nrow(Y)){
				init1[i,]<-sqrt(sum((Y[i,]-grmean)^2))
			}

			c1<-which.max(init1)
			in1<-c(Y[c1,])

			centr<-matrix(0,nrow=nrow(Y),ncol=1)
			for(i in 1:nrow(Y)){
				centr[i,]<-sqrt(sum((Y[i,]-Y[c1,])^2))
			}
			centr[c1,]<-0
			c2<-which.max(centr)
			in2<-c(Y[c2,])

			centr2<-matrix(0,nrow=nrow(Y),ncol=1)
			for(i in 1:nrow(Y)){
				centr2[i,]<-sqrt(sum((Y[i,]-Y[c2,])^2))
				centr3<-centr2+centr
			}
			centr3[c2,]<-0
			c3<-which.max(centr3)
			in3<-c(Y[c3,])

			initial<-matrix(0,nrow=(input$clusters*ncol(Y)),ncol=1)
			initial[1:(3*ncol(Y)),]<-c(in1,in2,in3)
	
			centr4<-matrix(0,nrow=nrow(Y),ncol=1)
			for(j in 1:(input$clusters-3)){
				if(j==1){
					for(i in 1:nrow(Y)){
						centr4[i,]<-sqrt(sum((Y[i,]-Y[c3,])^2))
						centr5<-centr4+centr3
						c4<-which.max(centr5)
						a<-centr5
						in4<-c(Y[c4,])
						st<-(((2*ncol(Y))+1)+(ncol(Y)*j))
						nd<-((3*ncol(Y))+(ncol(Y)*j))
						initial[st:nd,]<-in4
					}
				}
				else{
					for(i in 1:nrow(Y)){
						centr4[i,]<-sqrt(sum((Y[i,]-Y[c4,])^2))
						centr5<-centr4+a
					}	
					centr5[c(c1,c2,c3,c4),]<-0
					c4<-which.max(centr5)
					a<-centr5
					in4<-c(Y[c4,])
					st<-(((2*ncol(Y))+1)+(ncol(Y)*j))
					nd<-((3*ncol(Y))+(ncol(Y)*j))
					initial[st:nd,]<-in4
				}	
			}
			set.seed(initial)
			kmeans(selectedData(), input$clusters, iter.max = input$maxIter)
		}
    }
  }
	}
})

output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', 
		if(input$nav_fast == "Hierarchical"){
			switch(
				input$formatH, PDF = 'pdf', HTML = 'html', Word = 'docx'
			)
		}
		else{
			switch(
				input$formatP, PDF = 'pdf', HTML = 'html', Word = 'docx'
			)
		}
	  )
    },

    content = function(file) {
      src <- normalizePath('report.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')

      library(rmarkdown)
      out <- render('report.Rmd', 
		if(input$nav_fast=="Hierarchical"){
			switch(
				input$formatH,
				PDF = pdf_document(), HTML = html_document(), Word = word_document()
			)
		}
		else{
			switch(
				input$formatP,
				PDF = pdf_document(), HTML = html_document(), Word = word_document()
			)
		}
	  )
      file.rename(out, file)
    }
  )