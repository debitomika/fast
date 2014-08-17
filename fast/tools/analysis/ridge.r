#library(genridge)
library(stats)

################################################################
####################  RIDGE REGRESSION  ########################
################################################################
output$uiRidge_vardep <- renderUI({ #input variabel dependen
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "ridge_vardep", label = "Dependent variable:", choices = vars, 
  	selected = vars[1], multiple = FALSE)
})


output$uiRidge_varindep <- renderUI({ #input variabel independen
  if(is.null(input$ridge_vardep)) return()
	notChar <- "character" != getdata_class()
  vars <- varnames()[notChar]
 	vars <- vars[-which(vars == input$ridge_vardep)]
  if(length(vars) == 0) return()
  selectInput(inputId = "ridge_varindep", label = "Independent variables:", choices = vars, 
  	selected = state_multvar("ridge_varindep", vars), multiple = TRUE, selectize = FALSE)
})

output$uiRidge_method <-renderUI({
	if(is.null(input$ridge_vardep) || length(input$ridge_varindep)<2) return()
	selectInput(inputId="method", label = "K Value Specifications:",
		choices= list(
		"Automatic k-search" = 1,
		"Trial k-search" =2), selected=1, multiple=FALSE)
})
observe({
  if(!identical(input$nav_fast, "Ridge Regression")){
    print(input$nav_fast)
    updateTabsetPanel(session, "ridgetab", selected = 1)
  }
})

output$uimenu <- renderUI({
  wellPanel(
        HTML(paste("<label><strong>Menu:", "Regression","</strong></label>")),
        HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
        HTML(paste("<label><strong>Data:",input$datasets,"</strong></label>"))
      ) # tool status
})

output$ridge <- renderUI ({
sidebarLayout(
    sidebarPanel(
	  div(class = "busy",
              p("Calculation in progress ..."),
              img(src="ajaxloaderq.gif")
      ), # javascript calculation loading
		
	  uiOutput("uimenu"),
	  
      wellPanel(
        uiOutput("uiRidge_vardep"),
		uiOutput("uiRidge_varindep")
      ), #input independent&dependent variables
	  
      conditionalPanel(condition = "input.ridgetab == 1",
	  wellPanel(
		selectInput(
            inputId = "identification_plot",
            label = "Identification plot : ",
            c("Histogram","Correletaion"='cor',"Scatter","Laverage plots"), selectize=FALSE)
      )),
	  
	  conditionalPanel(condition = "input.ridgetab == 2",
	  wellPanel(
	  	uiOutput("uiRidge_method"),
		uiOutput("")
	  )),
      conditionalPanel(
        condition = "input.ridgetab == 2",
        
        wellPanel(
          selectInput(
            inputId = "methods",
            label = "Modelling Method:",
            choices = list(
              "Automatic Modelling" = 1,
              "Manual Modelling" =2 ),
            selected = 1)
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "ridgetab",
        tabPanel(
          title = "Identification",
          helpText("Identify Data and Multicollinierity Problem of the simulated data using,
                 the", strong("Variance Inflation Factor"), " and the", 
                   strong("Correlation Matrix Plot"),
                   br(),br()),
          tabsetPanel(
            id = "subtab",
            tabPanel(
              title = "Plots",
			  withTags(
                div(
                  class = "fluid-row",
					div(
						class="span7",
						uiOutput(outputId = "vardep")
					),
					div(
						class="span5",
						conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
						bsCollapse(multiple = TRUE, open = c("diag1","diag2"), id = "collapse1",
                               bsCollapsePanel("Variance Inflation Factor(VIF)", helpText(""),
                                               verbatimTextOutput(outputId = "vifpre"),
                                                 id = "diag2", value ="test1"),
							   bsCollapsePanel("Correlation Matrix", uiOutput(outputId = "varindep"),
                                               id = "diag1", value = "test2")
											   )
										)
						)
					)))),
          value = 1),
        tabPanel(
          title = "Estimation",
          helpText("Estimate the identified model of the data.", br(), br()),
          verbatimTextOutput(
            outputId = "summaryridge"),
          value = 2),
        tabPanel("Postestimation", 
		 tabsetPanel(id="postest",
			tabPanel(
              title = "Plots & Precision",
			  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
						bsCollapse(multiple = TRUE, open = c("diagn1","diagn2"), id = "collapse2",
                               bsCollapsePanel("Variance Inflation Factor(VIF)", helpText(""),
                                               verbatimTextOutput(outputId = "summarygen"),
                                                 id = "diagn1", value ="test1"),
												 
							   bsCollapsePanel("Precision", helpText(""),
                                               verbatimTextOutput(outputId = "precisegen"),
                                                 id = "diagn2", value ="test2"),
							   
							   bsCollapsePanel("Trace Plot", helpText(""),
                                               plotOutput(outputId = "plotgen"),
                                                 id = "diagn3", value ="test3"),
								
							   bsCollapsePanel("Correlation Matrix", plotOutput(outputId = "pairgen"),
                                               id = "diagn4", value = "test4")
											   )
									
					), 
					value=1),
					tabPanel("Formal Test",
						tags$button(class="btn btn-default",
						 withTags(
						  div(
						   br(),
							helpText(
									h4("Hypotesis", align="left"),
									htmlOutput('linear_hypotesis'),
									br(),
									h4("Computation", align="left"),
									tags$ul(
									tags$li(HTML(paste("<p align=left>The computed statistics is :</p>"))),
									#HTML(paste("<h5 align=justify>",textOutput('linear_computed'),"</h5>")),
									tags$li(HTML(paste("<p align=left>The computed p-value is :</p>"))),
									#HTML(paste("<h5 align=justify>",textOutput('linear_pvalue'),"</h5>"))),br(),
									h4("Decision", align="left"),
									tags$ul(
									#tags$li(h5(textOutput('linear_decision'), align="left"))),
									h4("Conclusion", align="left"),
									tags$ul(
									#tags$li(h5(textOutput('linear_conclusion'), align="left")))
									)
								 ,class = "span19")))))),
							 value=2) ),
          value = 3)
      )
    )
  )
})



##############################################################
################ Fungsi Perhitungan dan Plot #################
##############################################################


output$varindep<-renderUI({
if(is.null(getdatavarindep())) {
	return("Please select two or more variable")}
	
	plotOutput(outputId="plotvarindep")
	#plotOutput(outputId="ploty")
})
output$plotvarindep<-renderPlot({
	matriks<-cor(getdatavarindep(), method="spearman")
	col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
    "#007FFF", "blue", "#00007F"))
	corrplot.mixed(matriks, order = "hclust", addrect = 2, col = col1(100))
})

getdatavarindep<- reactive({
if(is.null(input$ridge_varindep) || length(input$ridge_varindep) <2) return(NULL)
else {
		dat2<-getdata()
		return(dat2[,as.character(input$ridge_varindep)])}
})

getdatavardep<- reactive({
if(is.null(input$ridge_vardep)) return(NULL)
else {
		dat1<-getdata()
		return(dat1[,as.character(input$ridge_vardep)])}
})

output$vardep<-renderUI({
if(is.null(getdatavardep())) {
	return("Please select two or more variable")}
	
	plotOutput(outputId="ploty")
})


output$vifpre<-renderPrint({
if(is.null(getregression())) {
	return("Please select at least one independent variable")}
	
	print(getregression())
})

getregression<-reactive({
if(is.null(input$ridge_varindep) || length(input$ridge_varindep) <2) {return(NULL)}
else{
	formula <- paste(input$ridge_vardep, "~", paste(input$ridge_varindep, collapse = " + "))
	reg<-lm(formula, data=mydata())
	reg<-vif(reg)
	return(reg)}
})

output$ploty<-renderPlot({

	plot(getdatavardep())
})

 output$values  <- renderTable(mydata())

 mydata <- reactive({
    dat<-data.frame(getdatavardep(), 
       getdatavarindep())
	
	names(dat)[1]<-paste(input$ridge_vardep)
	
	return(dat)
  })

getridge<-reactive({
if(is.null(input$ridge_varindep) || length(input$ridge_varindep) <2) {return(NULL)}
isolate({
	
	formula <- paste(input$ridge_vardep, "~", paste(input$ridge_varindep, collapse = " + "))
	lridge<-linearRidge(formula, data=mydata(), lambda=seq(0,0.1,0.001))
	#variable_Dependent_<-data.matrix(getdatavardep(), rownames.force = NA)
	#variable_Independent_<-data.matrix(getdatavarindep(), rownames.force = NA)
	#lridge<- ridge(variable_Dependent_~variable_Independent_,lambda=0.005) 
	return(lridge)})
})

geridge<-reactive({
if(is.null(input$ridge_varindep) || length(input$ridge_varindep) <2) {return(NULL)}
else{
	variable_Dependent_<-data.matrix(getdatavardep(), rownames.force = NA)
	variable_Independent_<-data.matrix(getdatavarindep(), rownames.force = NA)
	lgenridge<- ridge(variable_Dependent_~variable_Independent_,lambda=seq(0,0.1,0.01)) 
	return(lgenridge)}
})

output$plotgen<-renderPlot({
if(is.null(geridge())) {
	return("Please select at least one independent variable")}
else{	
	traceplot(geridge())
	}
})

output$summarygen<-renderPrint({
if(is.null(geridge())) {
	return("Please select at least one independent variable")}
else{	
	vif.ridge(geridge())
	#vif.ridge(getridge())
	}
})

output$pairgen<-renderPlot({
if(is.null(geridge())) {
	return("Please select at least one independent variable")}
else{	
	pairs.ridge(geridge())
	}
})

output$precisegen<-renderPrint({
if(is.null(geridge())) {
	return("Please select at least one independent variable")}
else{	
	precision(geridge())
	}
})

output$summaryridge<-renderPrint({
if(is.null(getridge())) {
	return("Please select at least one independent variable")}
else{	
	print(getridge())
	#vif.ridge(getridge())
	}
})

#########################
### Function GenRidge ###
#########################

# added GCV calculation 12-14-2011

## TODO: add formula interface

ridge <- function(y, ...) {
	UseMethod("ridge")
}

ridge.formula <-
		function(formula, data, lambda=0, df, svd=TRUE, ...){
	
	#code from MASS:::lm.ridge
	m <- match.call(expand.dots = FALSE)
	m$model <- m$x <- m$y <- m$contrasts <- m$... <- m$lambda <-m$df <- NULL
	m[[1L]] <- as.name("model.frame")
	m <- eval.parent(m)
	Terms <- attr(m, "terms")
	Y <- model.response(m)
	X <- model.matrix(Terms, m, contrasts)
	n <- nrow(X)
	p <- ncol(X)
	offset <- model.offset(m)
	if (!is.null(offset)) 
		Y <- Y - offset
	if (Inter <- attr(Terms, "intercept")) {
		Xm <- colMeans(X[, -Inter])
		Ym <- mean(Y)
		p <- p - 1
		X <- X[, -Inter] - rep(Xm, rep(n, p))
		Y <- Y - Ym
	}
	ridge.default(Y, X, lambda=lambda, df=df, svd=svd)
}


ridge.default <-
		function(y, X, lambda=0, df, svd=TRUE, ...){
	#dimensions	
	n <- nrow(X)
	p <- ncol(X)
	#center X and y
	Xm <- colMeans(X)
	ym <- mean(y)
	X <- X - rep(Xm, rep(n, p))
	y <- y - ym
	#scale X, as in MASS::lm.ridge 
	Xscale <- drop(rep(1/n, n) %*% X^2)^0.5
	X <- as.matrix(X/rep(Xscale, rep(n, p)))
	
	XPX <- crossprod(X)
	XPy <- crossprod(X,y)
	I <- diag(p)
	lmfit <- lm.fit(X, y)
	MSE <- sum(lmfit$residuals^2) / (n-p-1)
	HKB <- (p - 2) * MSE/sum(lmfit$coefficients^2)
	LW <- (p - 2) * MSE * n/sum(lmfit$fitted.values^2)
	
	# from ElemStatLearn:::simple.ridge
	svd.x <- svd(X, nu = p, nv = p)
	dd <- svd.x$d
	u <- svd.x$u
	v <- svd.x$v
	if (missing(df)) {
		df <- sapply(lambda, function(x) sum(dd^2/(dd^2 + x)))
	}
	else {
		fun <- function(df, lambda) df - sum(dd^2/(dd^2 + lambda))
		lambda <- sapply(df, FUN = function(df) uniroot(f = function(lambda) fun(df, 
										lambda), lower = 0, upper = 1000, maxiter = 10000)$root)
	}
	
	# prepare output    
	coef <- matrix(0, length(lambda), p)
	cov <- as.list(rep(0, length(lambda)))
	mse <- rep(0, length(lambda))
	
	# loop over lambdas
	for(i in seq(length(lambda))) {
		lam <- lambda[i]
		XPXr <- XPX + lam * I
		XPXI <- solve(XPXr)
		coef[i,] <- XPXI %*% XPy
		cov[[i]] <- MSE * XPXI %*% XPX %*% XPXI
		res <- y - X %*% coef[i,]
		mse[i] <- sum(res^2) / (n-p) 
		dimnames(cov[[i]]) <- list(colnames(X), colnames(X))
	}
	dimnames(coef) <- list(format(lambda), colnames(X))
	
	# calculate GCV, from MASS::lm.ridge
	dn <- length(dd)
	nl <- length(lambda)
	div <- dd^2 + rep(lambda, rep(dn, nl))
	GCV <- colSums((y - X %*% t(coef))^2)/(n - colSums(matrix(dd^2/div, dn)))^2
	k <- seq_along(GCV)[GCV == min(GCV)]
	kGCV <- lambda[k]
	
	result <- list(lambda=lambda, df=df, coef=coef, cov=cov, mse=mse, scales=Xscale, kHKB=HKB, kLW=LW,
			GCV=GCV, kGCV=kGCV)
	if (svd) {
		rownames(u) <- rownames(X)
		colnames(u) <- colnames(v) <- paste("dim", 1:p, sep="")
		rownames(v) <- colnames(X)
		result <- c(result, list(svd.D=dd, svd.U=u, svd.V=v))
	}
	class(result) <- "ridge"
	result
}



coef.ridge <-
function(object, ...) {
	object$coef
}

print.ridge <-
function(x, digits = max(5, getOption("digits") - 5),...) {
  if (length(coef(x))) {
      cat("Ridge Coefficients:\n")
      print.default(format(coef(x), digits = digits), print.gap = 2, 
          quote = FALSE)
  }
  invisible(x)
}

vcov.ridge <- function(object,  ...) {
	object$cov
}

# for pcaridge objects, default to last 2 variables
plot.pcaridge <-
		function(x, variables=(p-1):p, labels=NULL, ...) {
	p <- dim(coef(x))[2]
	plot.ridge(x, variables, labels=labels, ...)
}

plot.ridge <-
function(x, variables=1:2, radius=1, which.lambda=1:length(x$lambda), 
		labels=lambda, pos=3, cex=1.2,
		lwd=2, lty=1, xlim, ylim,
		col = c("black", "red", "darkgreen", "blue","darkcyan","magenta", "brown","darkgray"), 
		center.pch = 16, center.cex=1.5,
		fill=FALSE, fill.alpha=0.3, ref=TRUE, ref.col=gray(.70), ...) {

	ell <- function(center, shape, radius, segments=60) {
		angles <- (0:segments)*2*pi/segments
		circle <- radius * cbind( cos(angles), sin(angles))
		warn <- options(warn=-1)
		on.exit(options(warn))
		Q <- chol(shape, pivot=TRUE)
		order <- order(attr(Q, "pivot"))
		t( c(center) + t( circle %*% Q[,order]))
	}

	vnames <- dimnames(x$coef)[[2]]
	if (!is.numeric(variables)) {
		vars <- variables
		variables <- match(vars, vnames)
		check <- is.na(variables)
		if (any(check)) stop(paste(vars[check], collapse=", "), 
					" not among the predictor variables.") 
	}
	else {
		if (any (variables > length(vnames))) stop("There are only ", 
					length(vnames), " predictor variables.")
		vars <- vnames[variables]
	}
	if(length(variables)>2) {
		warning("Only two variables will be plotted. Perhaps you want plot3d.ridge()")
		variables <- variables[1:2]
	}
	lambda <- x$lambda[which.lambda]
	coef <- x$coef[which.lambda,variables]
	cov <- x$cov[which.lambda]
	n.ell <- length(lambda)
	lambda <- signif(lambda, 3)   # avoid many decimals when used as labels

	ells <- as.list(rep(0, n.ell))
# generate the ellipses for each lambda, to get xlim & ylim
	for (i in 1:n.ell) {
		ells[[i]] <- ell(center=coef[i,], shape=(cov[[i]])[variables,variables], radius=radius)
	}
	max <- apply(sapply(ells, function(X) apply(X, 2, max)), 1, max)
	min <- apply(sapply(ells, function(X) apply(X, 2, min)), 1, min)
	xlim <- if(missing(xlim)) c(min[1], max[1]) else xlim
	ylim <- if(missing(ylim)) c(min[2], max[2]) else ylim

	col <- rep(col, n.ell)		
	lwd <- rep(lwd, n.ell)		
	lty <- rep(lty, n.ell)		
	# handle filled ellipses
	fill <- rep(fill, n.ell)
	fill.alpha <- rep(fill.alpha, n.ell)
	fill.col <- trans.colors(col, fill.alpha)
	fill.col <- ifelse(fill, fill.col, NA)

	plot(coef, type='b', pch=center.pch, cex=center.cex, col=col, xlim=xlim, ylim=ylim, ...)
	if (ref) abline(v=0, h=0, col=ref.col)
	for (i in 1:n.ell) {
#		lines(ells[[i]], col=col[i], lwd=lwd[i], lty=lty[i])
		polygon(ells[[i]], col=fill.col[i], border=col[i],  lty=lty[i], lwd=lwd[i])
	}
	if(!is.null(labels)) text(coef, labels=labels, pos=pos, cex=cex)
}

# variance inflation factors for ridge regression objects

vif.ridge <- function(mod, ...) {

	Vif <- function(v) {
		R <- cov2cor(v)
		detR <- det(R)
		p <- nrow(v)
		res <- rep(0,p)
		for (i in 1:p) {
			res[i] <- R[i,i] * det(as.matrix(R[-i,-i])) / detR
		}
		res
	}

	if(!require("car")) stop("Requires the car package for the vif generic")
	V <- vcov(mod)
	res <- t(sapply(V, Vif))
	colnames(res) <- colnames(coef(mod))
	rownames(res) <- rownames(coef(mod))
	res
}

## Measures of precision and bias for ridge regression
## 

precision <- function(object, ...) {
	UseMethod("precision")
}

# DONE:  allow choice of log.det or det()^{1/p}
precision.ridge <- function(object, det.fun=c("log","root"), normalize=TRUE, ...) {
	tr <- function(x) sum(diag(x))
	maxeig <- function(x) max(eigen(x)$values)
	
	V <- object$cov
	p <- ncol(coef(object))
	det.fun <- match.arg(det.fun)
	ldet <- unlist(lapply(V, det))
	ldet <- if(det.fun == "log") log(ldet) else ldet^(1/p)
	trace <- unlist(lapply(V, tr))
	meig <- unlist(lapply(V, maxeig))	
	norm <- sqrt(rowMeans(coef(object)^2))
	if (normalize) norm <- norm / max(norm)
	data.frame(lambda=object$lambda, df=object$df, det=ldet, trace=trace, max.eig=meig, norm.beta=norm)
}

precision.lm <- function(object, det.fun=c("log","root"), normalize=TRUE, ...) {
	V <- vcov(object)
	beta <- coefficients(object)
	if (names(beta[1]) == "(Intercept)") {
		V <- V[-1, -1]
		beta <- beta[-1]
	}
#	else warning("No intercept: precision may not be sensible.")
	p <- length(beta)
	det.fun <- match.arg(det.fun)
	ldet <- det(V)
	ldet <- if(det.fun == "log") log(ldet) else ldet^(1/p)
	trace <- sum(diag(V))
	meig <- max(eigen(V)$values)
	norm <- sqrt(mean(beta^2))
	if (normalize) norm <- norm / max(norm)
	res <- list(df=length(beta), det=ldet, trace=trace, max.eig=meig, norm.beta=norm)
	unlist(res)
}

traceplot <-
function(x, 
	X=c("lambda","df"), 
#	labels=c("left", "right"),
	col = c("black", "red", "darkgreen", "blue","darkcyan","magenta", "brown","darkgray"), 
	pch = c(15:18, 7, 9, 12, 13),
	xlab, ylab="Coefficient", 
	xlim, ylim, ... ) {

	type <- X <- match.arg(X)
	if (type=="lambda") {
		X <- x$lambda
		if (missing(xlab)) xlab <- "Ridge constant"
		labels <- "left"
		}
	else {
		X <- x$df
		if (missing(xlab)) xlab <- "Degrees of freedom"
		labels <- "right"
	}
	coef <- coef(x)
	K <- nrow(coef)
	if (missing(xlim)) xlim <- range(X)
	if (missing(ylim)) ylim <- range(coef)

#	labels <- match.arg(labels)
	if (labels == "left") {
		xlim[1] <- xlim[1] - .1 * diff(xlim)
		labx <- X[1]
		laby <- coef[1,]
	}
	else {
		xlim[2] <- xlim[2] + .1 * diff(xlim)
		labx <- X[1]
		laby <- coef[1,]
	}

	matplot(X, coef, 	type="b", xlim=xlim, ylim=ylim, ylab=ylab, xlab=xlab, col=col, pch=pch, ...)
	abline(h=0, lty=3)
	if (type=="lambda") {
		abline(v=x$kHKB, col="gray", lty=2)
		text(x$kHKB, ylim[1], "HKB", pos=3)
		abline(v=x$kLW, col="gray", lty=2)
		text(x$kLW, ylim[1], "LW", pos=3)
	}
	vnames <- colnames(coef)
	text(labx, laby, colnames(coef), pos=c(2,4)[1+(labels=="right")])
}

