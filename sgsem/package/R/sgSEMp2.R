##' This function carries out sgSEM principle 2
##' 
##' Starting from the system response variable, each variable except for the system stressor is regressed on all other variables except for the system response in an additive multiple regression model, which is reduced by a stepwise selection using stepAIC(). Second, for selected variable, fitted linear regression for 8 pre-seclected funtional forms and pick the best.
##'
##' @title Semi-supervised Generalized Structural Equation Modeling (sgSEM) - Principle 2
##' @param x A dataframe. The first column of it is stressor, second column is response variable and other columns are different predictors.
##' @param stressor A character string of the column name of the main stressor OR a numeric number indexing the column of the main stressor.
##' @param response A character string of the column name of the main response OR a numeric number indexing the column of the main response.
##' @return A list of the following items:
##'
##' \itemize{
##' \item "res.print": A matrix. For each row, first column is the response variable, second column is the predictor, the other columns show corresponding summary information.
##' \item "res.best": A matrix. First dimension is predictors; second dimension is response variables. Each cell is "lm" object, which is the fitted simple linear regression of the best functional form.
##' \item "attr(res.best, "Step")": A matrix. First dimension is predictors; second dimension is response variables. Each cell shows in which step the pairwise relation is being fitted.
##' \item "res.all":  A one dimentional list. Each cell of the list stores a "lm" object, which is the best fitted multiple linear regression for each possible response variable.
##' \item "attr(res.all, "Step")": A vector. For each variable, it shows in which step it is choosen to be significantly related to response variable.
##'}
##' @export
##' 
##' @examples
##' data(acrylic)
##' ans <- sgSEMp2(acrylic)
##' ans$res.print
##' ans$res.best
##' ans$res.all
##' attr(ans$res.best, "Step")
##' attr(ans$res.all, "Step")
##'
sgSEMp2 <- function(x, stressor = NULL, response = NULL){
  
  if(!missing(stressor)){
    if(!is.character(stressor)){
      if(is.wholenumber(stressor)){
        if(stressor < 1 | stressor > length(colnames(x)))
          stop("Stressor location out of range!")
        stressor.loc <- stressor
      }
    }else{ if(!(stressor %in% colnames(x))){
      stop(paste0("Stressor '", stressor, "' does not exist!"))
    }
    stressor.loc <- which(colnames(x) == stressor)
    }
    neworder <- 1:length(colnames(x))
    neworder[1] <- stressor.loc
    neworder[stressor.loc] <- 1
    x <- x[neworder]
  }
  if(!missing(response)){
    
    response.loc <- which(colnames(x) == response)
    
    if(!is.character(response)){
      if(is.wholenumber(response)){
        if(response < 1 | response > length(colnames(x)))
          stop("Response location out of range!")
        response.loc <- response
      }
    }else{
      if(!(response %in% colnames(x))){
        stop(paste0("Response '", response, "' does not exist!"))
      }
      response.loc <- which(colnames(x) == response)
    }
    
    neworder <- 1:length(colnames(x))
    neworder[2] <- response.loc
    neworder[response.loc] <- 2
    x <- x[neworder]
  }
  
  ###############################
  ## The following function fits paired relations
  ###############################  
  Pair.relation <- function(iVar, iResp){
    ## current functional forms:"SL", "Quad", "SQuad", "Exp", "Log", "CPSLSL", "NLS-up", "BLS-down"
    ## total 8
    adj.R2 <- rep(0, 8) # saves all adjusted R2 for all 8 models
    model.res <- vector("list", 8) # Save the fitted R object for all 8 models
    Resp.v <- x[[iResp]] 
    Var.v <- x[[iVar]]
    Resp <- colnames(x)[iResp]
    Var <- colnames(x)[iVar]

    # Vector to pull the 'best model name' from
    model.name.res <- c("sl","quad","quadnl","exp","log")
    
    lm4.flag <- TRUE
    lm5.flag <- TRUE
    
    ## Functional Form 1: Linear
    Rel1 <- paste0(Resp, "~", Var)
    lm1 <- do.call("lm", list(Rel1, data=as.name("x")))
    adj.R2[1] <- summary(lm1)$adj.r.squared
    model.res[[1]] <- lm1
    
    
    ## Functional Form 2: Quadratic
    Rel2 <- paste0(Resp,"~",Var,"+","I(", Var,"^2)")
    lm2 <- do.call("lm", list(Rel2, data = as.name("x")))
    adj.R2[2] <- summary(lm2)$adj.r.squared
    model.res[[2]] <- lm2
    
    ## Functional Form 3: Quadratic (no linear term)
    Rel3 <- paste0(Resp,"~", "I(", Var,"^2)")
    lm3 <- do.call("lm", list(Rel3, data=as.name("x")))
    adj.R2[3] <- summary(lm3)$adj.r.squared
    model.res[[3]] <- lm3
    
    ## Functional Form 4: Exponential
    lm4.flag <- all(exp(Var.v) != Inf, na.rm = TRUE)
    ## if(!all(abs(Var.v) != Inf)) {lm4.flag <- FALSE}
    if(lm4.flag == TRUE){
      Rel4 <- paste0(Resp,"~", "exp(", Var,")")
      lm4 <- do.call("lm", list(Rel4, data=as.name("x")))
      adj.R2[4] <- summary(lm4)$adj.r.squared
      model.res[[4]] <- lm4
    }else{
      adj.R2[4] <- -Inf
      model.res[[4]] <- NA
    }
    
    ## Functional Form 5: Log
    lm5.flag <- all(Var.v > 0 & Var.v < Inf, na.rm = TRUE)
    #if(!all(Var.v > 0 & Var.v < Inf)) {lm5.flag <- FALSE}
    if(lm5.flag == TRUE){
      Rel5 <- paste0(Resp,"~", "log(", Var,")")
      lm5 <- do.call("lm", list(Rel5, data=as.name("x")))
      adj.R2[5] <- summary(lm5)$adj.r.squared
      model.res[[5]] <- lm5
    } 
    else {
      adj.R2[5] <- -Inf
      model.res[[5]] <- NA 
    }
    
    ## ## Functional Form 6: Change Point
    ## change = 0.63
    ## change <- min(Var.v) + (change * (max(Var.v) - min(Var.v)))
    ## Var.new <- as.numeric(Var.v > change)
    ## x$Var.plus <- (Var.v - change) * Var.new
    ## Rel6 = paste0(Resp,"~", Var, "+", "Var.plus")
    ## lm6 = do.call("lm", list(Rel6, data=as.name("x")))
    ## adj.R2[6] = summary(lm6)$adj.r.squared
    ## model.res[[6]] = lm6

    ##Functional Form 7:
    
    ##Functional Form 8:
    
    #choose the best model, if none model works, write -1.
    if(max(adj.R2) >= 0.001){
      nbest <- which(adj.R2 == max(adj.R2))
      Res.best[[iVar, iResp]] <<- model.res[[nbest]]
      attr(Res.best[[iVar, iResp]],"modelName") <<- model.name.res[[nbest]]
    }else{
      Res.best[[iVar, iResp]] <<- -1
      attr(Res.best[[iVar, iResp]],"modelName") <<- NA
    }
  }
  
  ###############################
  ## The following function does multiple selection
  ############################### 
  Multiple.relation <- function(iResp){
    
    Resp <- colnames(x)[iResp]   
    Var <- colnames(x)[c(-2, -iResp)]
    Var_char <- paste(Var, collapse = "+")
    Rel <- paste0(Resp, "~", Var_char)

    ## get rid of NA's
    x1 <- x[c(Resp,Var)]
    x1 <- x1[apply(x1, 1, FUN = function(x) sum(is.na(x)) == 0),]

    lm.full.model <- do.call("lm", list(Rel, data=as.name("x1")))
    lm.best <- stepAIC(lm.full.model, direction = "backward", trace =FALSE)
    R2 <- summary(lm.best)$r.squared
    adj.R2 <- summary(lm.best)$adj.r.squared
    best.vars <- names(lm.best$coeff)[-1]
    if(adj.R2 >= 0.001){
      Res.all[[1,iResp]] <<- lm.best
      
      Var.relate <- names(coef(lm.best))[-1] 
      Var.relate1 <- subset(Var.relate, Var.relate != colnames(x)[1]) ## remove the stressor
      tofit.flag[Var.relate1] <<- TRUE
      fitted.flag[iResp-1] <<- TRUE
      
      Var.relate.step <- attr(Res.all,"Step")
      Var.relate.step[] <- Inf
      Var.relate.step[Var.relate] <- attr(Res.all,"Step")[iResp] + 1
      attr(Res.all,"Step")[] <<- pmin(attr(Res.all,"Step")[], Var.relate.step[])
      
      Var.index <- c(1:nVar)     
      names(Var.index) <- colnames(x) 
      Var.relate.id <- Var.index[Var.relate]
      result <- lapply(Var.relate.id, function(x, y) Pair.relation(x,y), y = iResp)
      
      Var.relate.matr.step <- matrix(rep(Inf, nVar*nVar), nrow = nVar)
      rownames(Var.relate.matr.step) <- colnames(x)
      Var.relate.matr.step[Var.relate,iResp] <- attributes(Res.all)$Step[iResp] + 1
      attributes(Res.best)$Step <<- pmin(attributes(Res.best)$Step, Var.relate.matr.step)
      
      # Make a new row in the Res.print table for each of the predictor variables in the additive model
      for (n in 1:length(best.vars)){

        Res.print.newrow <- matrix(NA, nrow = 1, ncol = nRes)
        Res.print.newrow <- as.data.frame(Res.print.newrow)
        colnames(Res.print.newrow) <- c("Response", "Variable", "Model", "R-Sqr", "adj-R-Sqr", 
                            "Pval", "PvalRank", "p1ff","r2mark","markrank")

        varnum <- which(Var == best.vars[n])
        #cat("\nCurrent varnum is:",varnum)
        #cat("\nCurrent iResp is:",iResp)
        curresp <- Resp
        #cat("\nResponse is:",curresp)
        curvar <- best.vars[n]
        #cat("\nVariable is:",curvar)
        curmodel <- as.character(lm.best$call[2])
        #cat("\nModel is:",curmodel)
        curr2 <- R2
        #cat("\nR-Sqr is:",curr2)
        curar2 <- adj.R2
        #cat("\nadj-R-Sqr is:",curar2)
        curpval <- summary(lm.best)$coeff[(n+1),4]
        #cat("\nPval is:",curpval)
        curpvalrank <- which(order(summary(lm.best)$coeff[-1,4])==n)
        #cat("\nPvalRank is:",curpvalrank)

        # If a markovian relationship exists, do this - if not put in NAs
        if(length(Res.best[[varnum,iResp]])>1){
          curp1ff <- attr(Res.best[[varnum,iResp]],"modelName")
          #cat("\np1ff is:",curp1ff)
          curr2mark <- summary(Res.best[[varnum,iResp]])$r.squared
          #cat("\nr2mark is:",curr2mark)
          curmarkrank <- summary(Res.best[[varnum,iResp]])$adj.r.squared
          #cat("\nmarkrank is:",curmarkrank)
        }else{
          curp1ff <- NA
          #cat("\np1ff is:",curp1ff)
          curr2mark <- NA
          #cat("\nr2mark is:",curr2mark)
          curmarkrank <- NA
          #cat("\nmarkrank is:",curmarkrank)
        }

        # This fills the table with blanks for now while the real values are obtained
        # Need to break this into individual cell assignments to prevent factorization from combining datatypes
        Res.print.newrow[1,1] <- curresp
        Res.print.newrow[1,2] <- curvar
        Res.print.newrow[1,3] <- curmodel
        Res.print.newrow[1,4] <- curr2
        Res.print.newrow[1,5] <- curar2
        Res.print.newrow[1,6] <- curpval
        Res.print.newrow[1,7] <- curpvalrank
        Res.print.newrow[1,8] <- curp1ff
        Res.print.newrow[1,9] <- curr2mark
        Res.print.newrow[1,10] <- curmarkrank
        Res.print <<- rbind(Res.print, Res.print.newrow)
      }


    }
  }
  
  ###############################
  ## Main scripts; Above two functions are called
  ############################### 
  nVar <- ncol(x) #total number of variables
  nRVar <- nVar - 1 #total number of possible response variables
  
  Res.all <- vector("list", nVar) 
  dim(Res.all) <- c(1, nVar)
  Res.all[] <- NA
  colnames(Res.all) <- colnames(x)
  attr(Res.all, "Step") <- c(Inf, 0, rep(Inf, nVar-2))
  names(attributes(Res.all)$Step) <- colnames(x)
  
  Res.best <- vector("list", nVar * nVar) 
  dim(Res.best) <- c(nVar, nVar)
  Res.best[] <- NA
  dimnames(Res.best) <- list (colnames(x), colnames(x))
  attr(Res.best, "Step") <- matrix(rep(Inf, nVar*nVar),nrow = nVar)
  dimnames(attributes(Res.best)$Step) <- list(colnames(x),colnames(x))
  
  fitted.flag <- rep(FALSE, nRVar)       #indicator of response variables have been tested, dim2 response
  names(fitted.flag) <- colnames(x)[-1]
  tofit.flag <- c(TRUE, rep(FALSE, nRVar-1)) #indicator of all reponse varibals need to be tested, dim2 response
  names(tofit.flag) <- colnames(x)[-1]

  nRes <- 10 # Number of cells in print variable; see below
  ## matrix stores final results, including information of best model name,
  ## R-Sqr, adj-R-Sqr, Pval1, Pval2, Pval3
  Res.print <- matrix(NA, nrow = 1, ncol = nRes)  
  colnames(Res.print) <- c("Response", "Variable", "Model", "R-Sqr", "adj-R-Sqr", 
                          "Pval", "PvalRank", "p1ff","r2mark","markrank")
  
  while(sum(tofit.flag - fitted.flag) != 0){   
    iResp <- which(tofit.flag != fitted.flag)[1] + 1
    Multiple.relation(iResp)
  }
  Res.print <- Res.print[-1,]
  res <- list(res.print = Res.print, res.best = Res.best, res.all = Res.all) #outputs are three tables
  class(res) <- c("sgSEMp2","list")
  invisible(res)
  
}
