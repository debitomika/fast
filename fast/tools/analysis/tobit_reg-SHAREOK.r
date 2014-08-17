library(shiny)
library(datasets)
library(censReg)
library(GGally)
library(nortest)
library(tseries)
library(ggplot2)
library(R2wd)
library(MASS)
#library(googleVis)
#library(xtable)
library(shinyBS)
#library(stats)
library(lmtest)
library(RMySQL)

#user interface untuk input variabel dependen
output$uiReg_varDep <- renderUI({
  isTobit <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isTobit]
  if(length(vars) ==  0) return()
  selectInput(inputId = "tobit_variableDep", label = "Dependent variable :", choices = vars,
              selected = vars[1], multiple = FALSE,selectize = FALSE)
})

#user interface untuk input variabel independen
output$uiReg_varIndep <- renderUI({
  if(is.null(input$tobit_variableDep)) return()
	isTobit <- "character" != getdata_class()
  vars <- varnames()[isTobit]
 	vars <- vars[-which(vars == input$tobit_variableDep)]
  if(length(vars) == 0) return()
  selectInput(inputId = "tobit_variableIndep", label = "Independent variables :", choices = vars, 
  	 multiple = TRUE, selectize = FALSE)
})

#user interface
output$title <- renderUI ({
	wellPanel(
        HTML(paste("<label><strong>Menu:", "Regression","</strong></label>")),
        HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
        HTML(paste("<label><strong>Data:",input$datasets,"</strong></label>"))
      )
})


output$tobit_reg <- renderUI ({
  sidebarLayout(
  
     sidebarPanel(
	 div(class = "busy",
              p("Calculation in progress ..."),
              img(src="ajaxloaderq.gif")
          ),
		  uiOutput("title"),
       
      wellPanel(
        uiOutput("uiReg_varDep"),
		uiOutput("uiReg_varIndep")
		
      ),
	  conditionalPanel(condition = "input.conditionedPanels == 'Estimation'",
	  wellPanel(
		 bsButton("moTrig", "Organinize properties", style = "link"),
		 bsButton("moTrig2", "Export Your Output", style = "link")
		 
      )
     
	),
	conditionalPanel(condition = "input.conditionedPanels == 'Identification'",
	  wellPanel(
		selectInput(
            inputId = "identification_plot",
            label = "Identification plot : ",
            c("Censored Variable Plot"="cen","Histogram","Correletaion"='cor',"Scatter","Laverage plots"), selectize=FALSE)
      )),
	  
      conditionalPanel(condition = "input.conditionedPanels == 'Postestimation'", 
		conditionalPanel(condition = "input.condPanels == 'Plot'", 
		wellPanel(
			selectInput(
            inputId = "asumsi_plot",
            label = "Regression assumptions : ",
            choices = list(
              "Linearity" = 'linear_plot', 
              "Normality" = 'normal_plot',
			  "Homoschedasticity"= 'homo_plot',
			  "Non-Autocorrelation"= 'nonauto_plot',
			  "Multicolinearity"='multicol_plot'), selectize=FALSE)
		),
		uiOutput("normality_plot")
	)),
	conditionalPanel(condition = "input.conditionedPanels == 'Postestimation'", 
		conditionalPanel(condition = "input.condPanels == 'Formal Test'", 
		wellPanel(
			selectInput(
            inputId = "asumsi_formal",
            label = "Tobit regression assumptions : ",
            choices = list(
              "Linearity" = 'linear_formal', 
              "Normality" = 'normal_formal',
			  "Homoschedasticity"= 'homo_formal',
			  "Non-Autocorrelation"= 'nonauto_formal',
			  "Multicolinearity"='multicol_formal'), selectize=FALSE)
			   
		),
		uiOutput("conditional_formal")
		
	))
	
	),
    mainPanel(
		HTML(paste("<p align=right>",bsButton("share", label=img(src="image/share.png", height = 40, width = 40), style="link"),"</p>")),
		bsModal1("mo", "Organinize properties", trigger = "moTrig",
		column(4, wellPanel(  
		radioButtons("left", h5("Specify left-censoring limit :"),
             c("No left-censoring limit" = "no",
			   "Left-censoring at minimum" = "min",
               "Specified left-censoring limit" = "yes"),"min"
			),
			uiOutput("censoring_left"),br(),br())),
		column(4,	wellPanel(  		 		
		radioButtons("right", h5("Specify right-censoring limit :"),
             c("No right-censoring limit" = "no",
			   "Right-censoring at maximum" = "max",
               "Specified right-censoring limit" = "yes"),"no"
			),
			uiOutput("censoring_right"),br(),br())),
		column(4, wellPanel(
		selectInput(
            inputId = "iterative_method",
            label = h5("Specify iterative method :"),
            choices = list(
              "Newton-Raphson" = "nr",
			   "Berndt-Hall-Hall-Hausman" = "bhhh",
               "Broyden-Fletcher-Goldfarb-Shanno" = "bfgs",
			   "Nelder-Mead" ="nm",
			   "BFGSR"="bfgsr",
			   "Simulated annealing" = "sann"),"nr",selectize = FALSE
			),
			selectInput(
            inputId = "significance",
            label = h5("Specify significance level:"),
            choices = list(
              "0.05" = "a",
			   "0.1" = "b",
               "0.2" = "c"),selectize = FALSE
			))),
			br(),br(),
		tags$div(class = "modal-footer",
		br(),br(),
					  bsActionButton("submit", "Submit", style="primary"),
					  tags$a(href = "#", class = "btn btn-primary", "data-dismiss" = "modal", "Close")
					  
		)),
		bsModal1("mo2", "Export Output", trigger = "moTrig2",
		selectInput(
            inputId = "export",
            label = h5("Specify export:"),
            choices = list(
              "to Word" = "w",
			   "to PDF" = "p",
               "to image" = "i"),selectize = FALSE
			),
		tags$div(class = "modal-footer",
		br(),br(),
					  bsActionButton("export", "Submit", style="primary"),
					  tags$a(href = "#", class = "btn btn-primary", "data-dismiss" = "modal", "Close")
					  
		)),
		bsModal1("mo3", "Share your Analysis with The Others", trigger = "share",
		helpText("This fitur allow you share your analysis with the other by post your analysis in 'Galery Analysis'"),
		textInput("title", strong("Title :") , value = ""),br(),
		strong("Summary :"),br(),
		HTML('<textarea id="summary" rows="5" cols="20"></textarea>'),
		tags$div(class = "row-fluid",
          tags$div(class = "span4",
            selectInput("plot_share", "Chosee Plot that You Wanna Share :", choices = c("Censored Dependen Variable"='cen',"Histogram","Correletaion"='cor',"Scatter","Laverage plots"))
            
          ),
          tags$div(class = "span8",
            plotOutput("moplot")
          )
        ),
		tags$div(class = "modal-footer",
		br(),br(),
					  bsActionButton("shareok", "Submit", style="primary"),
					  tags$a(href = "#", class = "btn btn-primary", "data-dismiss" = "modal", "Close")
					  
		)),
		tabsetPanel(
			  tabPanel("Identification",uiOutput('identification')),
			  tabPanel("Estimation", uiOutput('validation')),
			  tabPanel("Postestimation", 
				tabsetPanel(
							tabPanel("Plot", plotOutput("plot")),
							tabPanel("Formal Test",
							tags$button(class="btn btn-default",
							withTags(
							  div(
								br(),
								helpText(
									
									htmlOutput('formal_hypotesis'),
									br(),
									h4("Computation", align="left"),
									tags$ul(
									tags$li(HTML(paste("<p align=left>The computed statistics is :</p>"))),
									HTML(paste("<h5 align=justify>",textOutput('linear_computed'),"</h5>")),
									tags$li(HTML(paste("<p align=left>The computed p-value is :</p>"))),
									HTML(paste("<h5 align=justify>",textOutput('linear_pvalue'),"</h5>"))),br(),
									h4("Decision", align="left"),
									tags$ul(
									tags$li(h5(textOutput('linear_decision'), align="left"))),
									h4("Conclusion", align="left"),
									tags$ul(
									tags$li(h5(textOutput('linear_conclusion'), align="left")))
									)
								 ,class = "span19")))
							 ),id ="condPanels")
 						),
			 id = "conditionedPanels" 
		)
    )
  )
  
  })
  
 # fungsi untuk mengambil variable yang dibutuhkan


  variable_Dependent <- reactive({
  dat <- getdata()[,as.character(input$tobit_variableDep)]
})
  
   variable_Independent <- reactive({
  dat <- getdata()[,as.character(input$tobit_variableIndep)]
})
 
 
  #################
  #Perhitungan
  #################
  
  output$validation<- renderUI ({
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		if(is.null(input$tobit_variableIndep)){
		
			return(tags$button(class="btn btn-danger","Please select one or more independent variables."))
		}
		if (input$left=='no' && input$right=='no') { 
			input$submit
				isolate({
					return(tags$button(class="btn btn-danger","Please specify at least one censoring limit."))
			})
		}

		if (input$left=='no' && input$right=='yes') {
			if(input$right2<=min(variable_Dependent_))
				return(tags$button(class="btn btn-danger","No uncensored observations, please respecify censoring limit."))
		}
		if (input$left=='min'&& input$right=='yes') {
			if(min(variable_Dependent_)>=input$right2) {
					return(tags$button(class="btn btn-danger","Right-censoring limit must be a larger number than left-censorinf limit, please respecify censoring limit."))
				}
		}
		if(input$left=='yes') {
			if(input$right=='no') {
				if(input$left2>=max(variable_Dependent_))
				return(tags$button(class="btn btn-danger","No uncensored observations, please respecify censoring limit."))
			}
			if (input$right=='max') { 
				if(input$left2>=max(variable_Dependent_)) {
					return(tags$button(class="btn btn-danger","Right-ensoring limit must be a larger number than left-censorinf limit, please respecify censoring limit."))
				}
			}
			if(input$right=='yes') {
				if (input$left2>=input$right2) { 
					return(tags$button(class="btn btn-danger","Right-ensoring limit must be a larger number than left-censorinf limit, please respecify censoring limit."))
				}
			}
		}
		
		uiOutput('result_collapse')
	})
	
	output$result_collapse<- renderUI ({
		bsCollapse(multiple = TRUE, open = FALSE, id = "collapse",
					bsCollapsePanel("Summary Statistics of Responses",tableOutput('respons_summary'),tags$button(class="btn btn-default", 
			withTags(
              div(
                br(),
                helpText(
				  HTML(paste("<label><h4 align=left><strong>Interpretation :","</strong></h4></label>")),
                  HTML(paste("<p align=justify> Based on above table , the information obtained are :","</p>")),		  
				  htmlOutput('interpretation1')
					
                ), class = "span20"))),id="col1", value="test1"),
					bsCollapsePanel("Model Fit Summary", tableOutput('model_summary'),tags$button(class="btn btn-default", 
			withTags(
              div(
                br(),
                helpText(
				  HTML(paste("<label><h4 align=left><strong>Interpretation :","</strong></h4></label>")),
                  HTML(paste("<p align=justify> Based on above table , the information obtained are :","</p>")),
                  HTML(paste("<p align=justify>  The number of iterations it took the model to converge, the final log likelihood, 
				  and the AIC and Schwarz Criterion (also known as the BIC).The final log likelihood can be used to compare nested models.
				  The AIC and Schwarz Criterion can be used to compare nested and non-nested models.</p>"))
					
                ), class = "span20"))), id="col2", value="test2"),
					bsCollapsePanel("Parameter Estimates", uiOutput('param_table'),tableOutput('margEff'), id="col3", value="test3"),
					bsCollapsePanel("Simultanous Test", tags$button(class="btn btn-default",
					withTags(
              div(
				br(),
				helpText(
					h4("Hypotesis", align="left"),
					tags$ul(
					tags$li(HTML(paste("<p align=left>Null Hypotesis (H0) : <strong>All predictors' regression coefficients in the model are simultaneously zero</strong>.</p>"))),
					tags$li(HTML(paste("<p align=left>Alternative Hypotesis (H1) : <strong>At least one of the predictors' regression coefficient is not equal to zero</strong>.</p>")))),
					br(),
					h4("Computation", align="left"),
					tags$ul(
					tags$li(HTML(paste("<p align=left>The computed statistics Likelihood Ratio is :</p>"))),
					HTML(paste("<h5 align=justify>",textOutput('computed_lrt'),"</h5>")),
					tags$li(HTML(paste("<p align=left>The computed p-value is :</p>"))),
					HTML(paste("<h5 align=justify>",textOutput('computed_pvalue'),"</h5>"))),br(),
					h4("Decision", align="left"),
					tags$ul(
					tags$li(h5(textOutput('decision'), align="left"))),
					h4("Conclusion", align="left"),
					tags$ul(
					tags$li(h5(textOutput('conclusion'), align="left")))
					)
				 ,class = "span19"))), id="col4", value="test4")
				)
	
	})
  
#fungsi pemilihan metode iteratif 
 method<- reactive({
		input$submit
		isolate({
			if(input$iterative_method=="nr"){
				method="Newton-Raphson"
			}
			else if(input$iterative_method=="bhhh"){
				method="BHHH"
			}
			else if(input$iterative_method=="bfgs"){
				method="BFGS"
			}
			else if(input$iterative_method=="nm"){
				method="NM"
			}
			else if(input$iterative_method=="bfgsr"){
				method="BFGSR"
			}
			else if(input$iterative_method=="sann"){
				method="SANN"
			}
			})
		
	})

#fungsi pemilihan sensor kiri
	left <- reactive({
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		input$submit
		isolate({
		if (input$left=='no') { 
			left=-Inf
		}
		else if (input$left=='min') { 
			left=min(variable_Dependent_)
		}
		else if (input$left=='yes') {
			left=input$left2
		}
		})
		
	})
	
#fungsi pemilihan sensor kanan 
	right <- reactive({
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		input$submit
		isolate({
		if (input$right=='no') {
			right=Inf
		}
		else if (input$right=='max') { 
			right=max(variable_Dependent_)
		}
		else if (input$right=='yes') {
			right=input$right2
		}
		})
		
	})
	
  
  #Fungsi untuk estimasi parameter
	estimasi <- reactive({
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		#estimasi parameter
			result<- censReg(variable_Dependent_~variable_Independent_,left=left(), right=right(), method=method()) 
			return(result)
			
	})
	
	#Fungsi untuk membuat output tabel parameter
	output$parameter <- renderTable({
	 		result<-summary(estimasi())
			Parameter<-row.names(result$estimate)
			result1<-cbind(Parameter,print(result$estimate,digits=4))
			result2<-data.frame(result1, row.names = NULL, check.rows = FALSE,check.names = FALSE, stringsAsFactors = default.stringsAsFactors())
			
			return(result2)
			
			
			
	})
		
	output$param_table<- renderUI ({
			
		tableOutput('parameter')
	})
	output$margEff<- renderTable ({
		Variabel <-row.names(data.frame(margEff(estimasi())))
		margEff<-margEff(estimasi())
		result1<-cbind(Variabel,margEff)
		result2<-data.frame(result1, row.names = NULL, check.rows = FALSE,check.names = FALSE, stringsAsFactors = default.stringsAsFactors())
		colnames(result2)<-c("Independent Variables","Marginal Effect")	
			return(result2)
	})
	fungsi1<-reactive({
			a<-matrix(c(print(mean(variable_Dependent()),digits=4),print(sd(variable_Dependent()),digits=4),"Censored"),ncol=1,nrow=3,byrow = TRUE)
			a2<-t(a)
			colnames(a2)<-c("Mean","Standard Deviasi","Type")
			a3<-data.frame(a2, row.names = NULL, check.rows = FALSE,check.names = FALSE, stringsAsFactors = default.stringsAsFactors())
			return(a3)
	})
	fungsi2<-reactive({
			b<-matrix(c(left(),right()),ncol=1,nrow=2,byrow = TRUE)
			b2<-t(b)
			colnames(b2)<-c("Left-censored","Right-censored")
			b3<-data.frame(b2, row.names = NULL, check.rows = FALSE,check.names = FALSE, stringsAsFactors = default.stringsAsFactors())
			return(b3)
	})
	fungsi3<-reactive({
			result<-estimasi()
			Summary<-row.names(data.frame(result$nObs))
			Value<-result$nObs
			result1<-cbind(Summary,Value)
			result3<-t(result1)
			result4<-result3[-1,]
			result5<-t(result4)
			result2<-data.frame(result5, row.names = NULL, check.rows = FALSE,check.names = FALSE, stringsAsFactors = default.stringsAsFactors())
			colnames(result2)<-c("Total Observation","Number of Left-censored","Number of Uncensored","Number of Right-censored")
			return(result2)
	})
	output$respons_summary <- renderTable({
		
			respons_summary<-cbind(fungsi1(),fungsi3(),fungsi2())
	})
	
	estimasi_intercept <- reactive({
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		#estimasi parameter
			result<- censReg(variable_Dependent_~1,left=left(), right=right(), method=method()) 
			return(result)
			
	})
	pseudoR<-reactive({
		pseudoR_square<-1-(estimasi()$maximum/estimasi_intercept()$maximum)
	})
		
	pseudoRadj<-reactive({
		pseudoRadj_square<-1-((estimasi()$maximum-extractAIC(estimasi())[1])/estimasi_intercept()$maximum)
	})
	
	output$model_summary<- renderTable({
		loglikelihood<-estimasi()$maximum
		loglikelihood2<-estimasi_intercept()$maximum
		maxmethod<-estimasi()$type
		niteration<-estimasi()$iteration
		pseudoR<-pseudoR()
		pseudoRadj<-pseudoRadj()
		AIC<-extractAIC(estimasi())[2]
		BIC<-BIC(estimasi())
		
		a<-matrix(c("Log Likelihood Model with Predictors",loglikelihood,"Log Likelihood Model without Predictors",loglikelihood2,
			"Optimization Method",maxmethod,"Number of Iterations",niteration,"Pseudo R2",pseudoR,"Pseudo R2 Adjusted",pseudoRadj,
			"AIC",AIC,"Schwarz Criterion (BIC)",BIC),ncol=2,nrow=8,byrow = TRUE)
		b<-data.frame(a, row.names = NULL, check.rows = FALSE,check.names = FALSE, stringsAsFactors = default.stringsAsFactors())
		colnames(b)<-c("Summary","Value")
			return(b)
		
	})
	 	
	#Fungsi untuk residual 
	residual <- reactive({
	
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		if(is.null(input$tobit_variableIndep)){
			return("Please select one or more independent variables.")
		}
					
				
			if (input$left=='no'&& input$right=='no') { 
				return("Please specify at least one censoring limit.")
			}
			
			if (input$left=='no') { 
				left=-Inf
			}
			
			if (input$left=='min') { 
				left=min(variable_Dependent_)
				if(input$right=='yes'){
					if(left>=input$right2) {
					return("Right-ensoring limit must be a larger number than left-censorinf limit.")
					}
				}
			}
			
			if (input$left=='yes') {
				left=input$left2
			}
			
			if (input$right=='no') {
				right=Inf
			}
			if (input$right=='max') { 
				right=max(variable_Dependent_)
				if(input$left=='yes'){
					if(right<=input$left2) {
					return("Right-ensoring limit must be a larger number than left-censorinf limit.")
					}
				}
				
			}
			if (input$right=='yes') {
				right=input$right2
			}
			
			if(input$left=='yes'&&input$right=='yes') {
			if (input$left2>=input$right2) { 
				return("Right-ensoring limit must be a larger number than left-censorinf limit.")
			}
			}
			
			if(input$iterative_method=="nr"){
				method="Newton-Raphson"
			}
			else if(input$iterative_method=="bhhh"){
				method="BHHH"
			}
			else if(input$iterative_method=="bfgs"){
				method="BFGS"
			}
			else if(input$iterative_method=="nm"){
				method="NM"
			}
			else if(input$iterative_method=="bfgsr"){
				method="BFGSR"
			}
			else if(input$iterative_method=="sann"){
				method="SANN"
			}
			
			result<- censReg(variable_Dependent_~variable_Independent_,left=left, right=right, method=method) 
			#menghitung residual
				number_row<-nrow(data.matrix(result$estimate))
				matrix_1<-data.matrix(result$estimate)
				matrix_2<-matrix_1[c(1:(number_row-1)),]
				matrix_3<-data.matrix(matrix_2)
				matrix_Indep<-cbind(1,variable_Independent_)
				fitted<-matrix_Indep%*%matrix_3
				residual<- variable_Dependent_-fitted
				return(residual)
				 
			
	})

 #fungsi untuk menampilkan plot pada tab identification
 
 output$identification<- renderUI({
			
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		if(is.null(input$tobit_variableIndep)){
			return("Please select one or more independent variables.")
		}
		else {
			plotOutput("identification1")
		}
							
	   })
 
 output$identification1<- renderPlot({
			
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		dependent_variable<-variable_Dependent()
		independent_variable<-variable_Independent()
		data<-data.frame(cbind(dependent_variable,independent_variable))
			
				if(input$identification_plot=='cen') {
					f <- function(x, var, bw = 15) {
						dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
					}
					p <- ggplot(data, aes(x = dependent_variable))
					plot<-p + stat_bin(binwidth=5) +stat_function(fun = f, size = 1,args = list(var = data$dependent_variable))
					
					
					  
					#wdGet()
					#wdTitle("Plot of Censoring Variable")
					#wdPlot(plot)
					
					
				}
				else if (input$identification_plot=='cor') {
					plot<-ggpairs(cbind(variable_Dependent_,variable_Independent_),title="Correlation Plot")
				}
				print(plot)
				
							
	   })
	   
	output$moplot<- renderPlot({ 
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		dependent_variable<-variable_Dependent()
		independent_variable<-variable_Independent()
		data<-data.frame(cbind(dependent_variable,independent_variable))
			
				if(input$plot_share=='cen') {
					f <- function(x, var, bw = 15) {
						dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
					}
					p <- ggplot(data, aes(x = dependent_variable))
					plot<-p + stat_bin(binwidth=5) +stat_function(fun = f, size = 1,args = list(var = data$dependent_variable))
					
					
					  
					#wdGet()
					#wdTitle("Plot of Censoring Variable")
					#wdPlot(plot)
					
					
				}
				else if (input$plot_share=='cor') {
					plot<-ggpairs(cbind(variable_Dependent_,variable_Independent_),title="Correlation Plot")
				}
				print(plot)
	})
	
	observe ({
	if(is.null(input$shareok)) return()
	if(input$shareok<1) return ()
	isolate ({
			picture()
			
	})	})
	   
	picture<-function(){
		dev.new()
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		direktori<-paste("C:/wamp/www/tabGenFast/htdocs/fast/images/image/image",file_names(),".png")
		direktori2 <- substr(direktori, 42, 255)
    png(direktori, type="cairo")
		print(plot<-ggpairs(cbind(variable_Dependent_,variable_Independent_),title="Correlation Plot"))
		dev.off()
		inserttodb(direktori2)
	}
	
	inserttodb <- function(direktori = null){
		query<- paste0("INSERT INTO data VALUES(null,'",input$title,"','",input$summary,"','",direktori,"')")
			dbSendQuery(mydb, query)
	}
	
	file_names<-function(n=1, length=12) {
		randomString<-c(1:n) 
		for (i in 1:n) {
			randomString<- paste(sample(c(0:9, letters,LETTERS), length, replace=TRUE), collapse="")
		}
		return(randomString)
	}

 
 #fungsi penghitungan uji asumsi formal
   formal_test <- reactive({
		
			
					if(input$asumsi_formal=='linear_formal') {
						if(is.null(input$linear_formalchoice)) return()
						if(input$linear_formalchoice=='harvtest') {
							test<-harvtest(estimasi(), order.by = NULL, data = list())					
						}
						else if (input$linear_formalchoice=='raintest') {
							test<-raintest(estimasi(), fraction = 0.5, order.by = NULL, center = NULL,data=list())
						}
					}
					else if (input$asumsi_formal=='normal_formal') {
						if(input$normal_formalchoice=='ks') {
							test<-lillie.test(residual())						
						}
						else if (input$normal_formalchoice=='jb') {
							test<-jarque.bera.test(residual())
						}
						else if (input$normal_formalchoice=='ad') {
							test<-ad.test(residual())
						}
						else if (input$normal_formalchoice=='cvm') {
							test<-cvm.test(residual())
						}
						else if (input$normal_formalchoice=='pearson') {
							test<-pearson.test(residual())
						}
						else if (input$normal_formalchoice=='sw') {
							test<-shapiro.test(residual())
						}
						else if (input$normal_formalchoice=='sf') {
							test<-sf.test(residual())
						}
					}
					else if (input$asumsi_formal=='homo_formal') {
						if(input$homo_formalchoice=='bp') {
							test<-bptest(estimasi(), varformula = NULL, studentize = TRUE, data = list())						
						}
						else if(input$homo_formalchoice=='gq') {
							test<-gqtest(estimasi(), point = 0.5, fraction = 0,alternative = "two.sided",order.by = NULL, data = list())						
						}
						else if(input$homo_formalchoice=='hm') {
							test<-hmctest(estimasi(), point = 0.5, order.by = NULL, simulate.p = TRUE, nsim = 1000,plot = FALSE, data = list())						
						}
					}
					else if (input$asumsi_formal=='nonauto_formal') {
						if(input$nonauto_formalchoice=='dw') {
							test<-dwtest(estimasi(), order.by = NULL, alternative = "two.sided", iterations = 15, exact = NULL, tol = 1e-10, data = list())						
						}
					}
					else if (input$asumsi_formal=='multicol_formal') {
					
					}
					return(test)
						
	})
	
			output$formal_hypotesis <- renderText({
			if(input$asumsi_formal=='linear_formal') {
				"<h4>Assumption Test of Linerity</h4> <br>
				<h4 align=left> Hypotesis</h4>
				<ul>
					<li><p align=left>Null Hypotesis (H0) : <strong>The regression is correctly modeled as linear</strong>.</p></li>
					<li><p align=left>Alternative Hypotesis (H1) : <strong>The regression isn't correctly modeled as linear</strong>.</p></li></ul>"
				}
			else if (input$asumsi_formal=='normal_formal') {
				"<h4>Assumption Test of Normality</h4> <br>
				<h4 align=left> Hypotesis</h4>
				<ul>
					<li><p align=left>Null Hypotesis (H0) : <strong>The distribution of residual is normal</strong>.</p></li>
					<li><p align=left>Alternative Hypotesis (H1) : <strong>The distribution of residual isn't normal</strong>.</p></li></ul>"
			}
			else if (input$asumsi_formal=='homo_formal') {
				"<h4>Assumption Test of Homoscedasticity</h4> <br>
				<h4 align=left> Hypotesis</h4>
				<ul>
					<li><p align=left>Null Hypotesis (H0) : <strong>The error variances are all equal.</strong>.</p></li>
					<li><p align=left>Alternative Hypotesis (H1) : <strong>The error variances are a multiplicative function of one or more variables</strong>.</p></li></ul>"
			}
			else if (input$asumsi_formal=='nonauto_formal') {
				"<h4>Assumption Test of Non-Autocorrelation</h4> <br>
				<h4 align=left> Hypotesis</h4>
				<ul>
					<li><p align=left>Null Hypotesis (H0) : <strong>There isn't an autocorrelation of error.</strong>.</p></li>
					<li><p align=left>Alternative Hypotesis (H1) : <strong>There is an autocorrelation of error.</strong></strong>.</p></li></ul>"
			}
			})
			
			output$linear_computed <- renderText({
				formal_test()$statistic
				
			})
			output$linear_pvalue <- renderText({
				formal_test()$p.value
			})
			
			output$linear_decision <- renderText({
				if(formal_test()$p.value >= 0.05){
				return("Do not reject the null hypothesis, since the p-value is greater than 0.05")
				}
				else if(formal_test()$p.value < 0.05){
				return("Reject the null hypothesis, since the p-value is less than 0.05")
				}
			})
			output$linear_conclusion <- renderText({
			if(input$asumsi_formal=='linear_formal') {	
				if(formal_test()$p.value >= 0.05){
					return("Therefore, we do not have enough evidence to reject the null hypothesis. And thus, the regression is correctly modeled as linear")
				}
				else if(formal_test()$p.value < 0.05){
					return("Therefore, we have enough evidence to reject the null hypothesis. And thus,the regression isn't correctly modeled as linear")
				}
			}
			else if (input$asumsi_formal=='normal_formal') {
				if(formal_test()$p.value >= 0.05){
					return("Therefore, we do not have enough evidence to reject the null hypothesis. And thus, the distribution of residual is normal")
				}
				else if(formal_test()$p.value < 0.05){
					return("Therefore, we have enough evidence to reject the null hypothesis. And thus,the distribution of residual isn't normal")
				}
				
			}
			else if (input$asumsi_formal=='homo_formal') {
				if(formal_test()$p.value >= 0.05){
					return("Therefore, we do not have enough evidence to reject the null hypothesis. And thus, the error variances are all equal.")
				}
				else if(formal_test()$p.value < 0.05){
					return("Therefore, we have enough evidence to reject the null hypothesis. And thus,the error variances are a multiplicative function of one or more variables")
				}
			}
			else if (input$asumsi_formal=='nonauto_formal') {
				if(formal_test()$p.value >= 0.05){
					return("Therefore, we do not have enough evidence to reject the null hypothesis. And thus, there isn't an autocorrelation of error.")
				}
				else if(formal_test()$p.value < 0.05){
					return("Therefore, we have enough evidence to reject the null hypothesis. And thus, there is an autocorrelation of error.")
				}
			}
			
			})
			output$conditional_formal <- renderUI({
				
				if (input$asumsi_formal=='linear_formal') {
				wellPanel(
					selectInput(
					inputId = "linear_formalchoice",
					label = "Choose formal test of linearity : ",
					choices = list(
					  "Harvey-Collier test" = 'harvtest',
					  "Rainbow test" = 'raintest'
					  ), "harvtest",selectize = FALSE)
				)}
				
				else if (input$asumsi_formal=='normal_formal') {
				wellPanel(
					selectInput(
					inputId = "normal_formalchoice",
					label = "Choose formal test of normality : ",
					choices = list(
					  "Lilliefors test" = 'ks',
					  "Jarque Bera test" = 'jb',
					  "Anderson Darling test" = 'ad',
					  "Cramer-von Mises test" = 'cvm',
					  "Pearson chi-square test" = 'pearson',
					  "Shapiro-Wilk test" = 'sw',
					  "Shapiro Francia test" = 'sf'
					  ), "ks",selectize = FALSE)
				)}
				
				else if (input$asumsi_formal=='homo_formal') {
				wellPanel(
					selectInput(
					inputId = "homo_formalchoice",
					label = "Choose formal test of homoscedasticity : ",
					choices = list(
					  "Breusch-Pagan test" = 'bp',
					  "Goldfeld-Quandt test" = 'gq',
					  "Harrison-McCabe test" = 'hm'
					  ), "bp",selectize = FALSE)
				)}
				
				else if (input$asumsi_formal=='nonauto_formal') {
				wellPanel(
					selectInput(
					inputId = "nonauto_formalchoice",
					label = "Choose formal test of non-autocorrelation : ",
					choices = list(
					  "Durbin-Watson test" = 'dw'
					  ), "dw",selectize = FALSE)
				)}
			})

	
	#fungsi untuk menampilkan plot
	output$plot <- renderPlot({
			
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		independent_variable<-variable_Independent()
		residuals<-residual()
			if(typeof(residual())=="character"){
				residual()
			}
			else {
			
				if(input$asumsi_plot=='linear_plot') {
						plots <- list()
						i<-0
						rdat <- cbind(residuals,independent_variable)
						rdat <- data.frame(rdat)
						#while(i<ncol(independent_variable)){
						#if(getdata_class()[i] == 'factor') {
						#	if('factor' %in% class(dat[,i])) {
						#		plots[[i]] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_boxplot(fill = 'blue', alpha = .3)
						#	} else {
								#print(plots[i] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_point() + geom_smooth(size = .75, linetype = "dotdash"))
						#	}
						#}
								i<-i+1

																
					}
				else if (input$asumsi_plot=='normal_plot') {
					if(input$normal_plotchoice=='hist') {
					print(hist(residual(),100,col="red"))
					}
					else {
					print(qqnorm(residual(),col="red"))
					print(qqline(residual(),col="black"))
					}
				}
				else if (input$asumsi_plot=='homo_plot') {
					fitted<-variable_Dependent_-residual()
					plot(residual(),fitted, col="red")
				}
				else if (input$asumsi_plot=='nonauto_plot') {
				
				}
				else {
				
				}
			}
	   })
	
	output$normality_plot <- renderUI({
		if (input$asumsi_plot=='normal_plot') {
		wellPanel(
			selectInput(
            inputId = "normal_plotchoice",
            label = "Choose plot of normality : ",
            choices = list(
              "Histogram" = 'hist', 
              "QQ-Plot" = 'qq'
			  ),selectize=FALSE)
		)
		}
	})

	output$censoring_left <- renderUI({
  	 if(input$left == 'yes') {
			numericInput("left2", "Specified left censoring limit", " ", min = NA, max = NA,
				step = NA)
			}
	})
	
	output$censoring_right <- renderUI({
  	 if(input$right == 'yes') {
			numericInput("right2", "Specified right censoring limit", " ", min = NA, max = NA,
				step = NA)
			}
	})

################################################################
# Sekumpulan output untuk blok I
################################################################
	
	output$interpretation1 <- renderText({
		
		HTML(paste("<ol><li><p align=justify>Mean of respons variabel (censored variabel) is</h5>",strong(print(mean(variable_Dependent()),digits=4)),
			"and it’s standard deviation is",strong(print(sd(variable_Dependent()),digits=4)),"</p></li>",
			"<li><p align=justify>Total Observation total that are detected is ",strong(nrow(data.matrix(variable_Dependent()))),"</p></li>",
			"<li><p align=justify>Number of Left censored at",strong(left()),"is",strong(nleft_censored()),  
			", number of right censoring at",strong(right()), "is",strong(nright_censored()),  
			", and number of uncensored is",strong(n_uncensored()),".",
			inf(),"</p></li></ol>"))
	})
	
	inf <- reactive({
		if(left()==-Inf) {
		return(" -Inf means that no left-censoring limit or left side cesored at minus infinity number")
		}
		if(right()==Inf) {
		return(" Inf means that no right-censoring limit or right side cesored at positive infinity number")
		}
	})
	
	nleft_censored <- reactive({
		result<-estimasi()
		return(data.frame(result$nObs, row.names = NULL)[2,])
	})
	n_uncensored <- reactive({
		result<-estimasi()
		return(data.frame(result$nObs, row.names = NULL)[3,])
		
	})
	nright_censored<- reactive({
		result<-estimasi()
		return(data.frame(result$nObs, row.names = NULL)[4,])		
	})
	
################################################################
# Sekumpulan fungsi dan output untuk blok 4
################################################################
	
	lrt<-reactive({
		loglikelihood1<-estimasi_intercept()$maximum
		loglikelihood2<-estimasi()$maximum
		lrt<--2*(loglikelihood1-loglikelihood2)
			return(lrt)
	})
	
	pvalue<-reactive({
		pvalue<-pchisq(lrt(),ncol(data.matrix(variable_Independent(), rownames.force = NA)),lower.tail=FALSE) 
			return(pvalue)
	})
	output$computed_lrt <- renderText({
			print(lrt(), digits=4)
		})
		
	output$computed_pvalue <- renderText({
			print(pvalue(), digits=4)
	})
		
	output$decision <- renderText({
		if(pvalue() >= 0.05){
		return("Do not reject the null hypothesis, since the p-value is greater than 0.05")
			}
		else if(pvalue() < 0.05){
		return("Reject the null hypothesis, since the p-value is less than 0.05")
		}
	})

	output$conclusion <- renderText({
		if(pvalue() >= 0.05){
			return("Therefore, we do not have enough evidence to reject the null hypothesis. And thus, all predictors resgression coefficients in the model are simultaneosly zero ")
		  }
		else if(pvalue() < 0.05){
			return("Therefore, we reject the null hypothesis. And thus,at least one of the predictors' regression coefficient is not equal to zero.")
		  }
		})