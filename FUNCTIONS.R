WIDTH = 0.2

week_to_numeric = function(x){
  
  s1 = substr(x,1,4)
  s2 = substr(x,6,7)
  s = paste0(s1,s2)
  R = as.numeric(s)
  R
}


OR <- function (...)  Reduce("|", list(...))

extremeDetectIndex = function(X,n){
  MU = sapply(X,function(x) mean(x,na.rm = TRUE))
  SD = sapply(X,function(x) sd(x,na.rm = TRUE))
  UCL = MU + SD*n
  LCL = MU - SD*n
  
  R = data.frame(lapply(1:ncol(X), function(i) {
    
    X[,i] > UCL[i] | X[,i] <LCL[i]
    
  }))
  
  names(R) = names(X)
  R = do.call(OR,R)
  R[is.na(R)] = FALSE
  R
}


toChar = function(X,NAMES){
  NAME_1 = NAMES[NAMES %in% names(X)]
  NAME_2 = setdiff(NAMES,NAME_1)
  cat("------------\n")
  print(paste(NAME_1, "are in data and are converted to character"))
  print(paste(NAME_2, "are not in data"))
  
  for( i in NAME_1)    X[,i] =as.character(X[,i])
  
  X
}

# sapply(mtcars,class)
# X = toChar(mtcars, c("cyl","ppp"))
# sapply(X,class)



dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;font-size:x-small")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown",
    style="font-size:x-small;width:135px"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    br(),
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}



contriVBar = function(x,SORT=FALSE){
  
#  x = mtcars[1,]
  D = data.frame(Parametrics=names(x), Contribution = as.numeric(x),stringsAsFactors = FALSE)
  
  
# if( SORT) D$Parametrics = factor(D$Parametrics, levels = D$Parametrics[order(abs(D$Contribution))])

  if( SORT) D$Parametrics = factor(D$Parametrics, levels = D$Parametrics[order(D$Contribution)])
  
  
  #print(D)
  ggplot(D, aes(x=Parametrics,y=Contribution)) + geom_bar(stat = "identity")+coord_flip()
  
  
  
  
}




#pca is a function of getting T squared. X is the data. n is the number of PCs included.
pca_general = function(X, tsN = 3, Center = TRUE, Scale = TRUE)
{
  if(!is.data.frame(X) & !data.table::is.data.table(X)) { stop("The input is neither data.frame nor data.table")}
  
  X2 = na.omit(X)
  
  n = ncol(X)
  
  R = list()
  
  fit = 
    tryCatch({
      prcomp(X2, center=Center, scale.=Scale)
    },
    warning = function(w){
      prcomp(X2, center=Center, scale.=Scale)
    },
    error = function(e){
      stop("prcomp error")
    })
  
  
  lambda = fit$sdev^2
#  print(lambda)
  
  R$lambda = lambda                                                       #1 lamda
  
  R$rotation = fit$rotation                                              #2 rotation
  
  PC = as.matrix(scale(X,center=Center,scale=Scale))%*%fit$rotation
  R$PC = PC                                                              #3 PC
  
#   print(PC)
#   print(n)
#   print(dim(X))
#   print(dim(X2))
#   print(fit)
  
  TSX = lapply(1:n, function(i) {
    PC[,i]^2/lambda[i]
  })  
  TSX = as.data.frame(TSX)
  names(TSX) = paste0("TS",1:n)
  R$TSX = TSX
  
  
  #  4 contribution
  Names = names(X)
  X = scale(X,center=Center,scale=Scale)
  
  
  
  tryCatch({
    LAMBDA =diag(lambda[1:tsN])
    a.eig <- eigen(LAMBDA)
    LAMBDA.s <- a.eig$vectors %*% diag(sqrt(a.eig$values)) %*% solve(a.eig$vectors)
    LAMBDA.si = solve(LAMBDA.s)
    contri = as.matrix(X)%*%fit$rotation[,1:tsN]%*%LAMBDA.si%*%t(fit$rotation[,1:tsN]) 
    contri = data.frame(contri)       
    names(contri) = paste0(Names,"_CONTRI")
    R$contri = data.frame(contri)
  },
  warning = function(w){
    print("there are warnings")
    LAMBDA =diag(lambda[1:tsN])
    a.eig <- eigen(LAMBDA)
    LAMBDA.s <- a.eig$vectors %*% diag(sqrt(a.eig$values)) %*% solve(a.eig$vectors)
    LAMBDA.si = solve(LAMBDA.s)
    contri = as.matrix(X)%*%fit$rotation[,1:tsN]%*%LAMBDA.si%*%t(fit$rotation[,1:tsN]) 
    contri = data.frame(contri)
    names(contri) = paste0(Names,"_CONTRI")
    R$contri = data.frame(contri)
  },
  error = function(e){
    cat("contribution calculation errors")
  })
  
  return(R)
}





pca_boxplots = function (X,g){
  par(mar=c(4.5,2.5,2.5,1))
  par(mfrow = c(2,3))
  for(i in 1:6) 
  {
    boxplot(pca_general(X)$TSX[,i]~g,las=2,main=paste("TS - Principal Component ",i))
    abline(h=qchisq(0.95,1),lty=2,col="red")
  }
  par(mar=c(5.1,4.1,4.1,2.1))
  par(mfrow = c(1,1))
}

screeplot = function(X,n=6){  
  library(FactoMineR)
  library(factoextra)
  fviz_screeplot(PCA(X,graph = FALSE),ncp=n)
}

comboMinus = function(X){
  
  n = ncol(X)
  Names = names(X)
  
  for(i in 1:(n-1)){
    for(j in (i+1):n){
#      print(c(i,j))

      tmp = X[,i]-X[,j]
      Name =  paste0(Names[i],"_",Names[j])
      
      if(i == 1 & j == 2){
        R = data.frame(tmp)
        names(R) = Name
      } else {
        R = data.frame(R,tmp)
        names(R)[ncol(R)] = Name
      }
    }
  }
  
  
  R
  
}






forwardCols = function(X, forwardNamesVector, Order = "asInForward"){
  if(!is.data.frame(X) & !data.table::is.data.table(X)){
    stop("wrong input data type")
  }
  if(!all(forwardNamesVector %in% names(X))){
    cat("warning: not all elements in forwardNamesVector in names(X)\n--------------------------\n")
  }
  if (Order == "asInForward"){
    NAMES_1 = forwardNamesVector[forwardNamesVector %in% names(X)]
  } else if(Order == "original"){
    NAMES_1 = names(X)[names(X) %in% forwardNamesVector]
  } else {
    stop("wrong value for inForward")
  }
  NAMES_2 = setdiff(names(X),forwardNamesVector)
  R = data.frame( X[,NAMES_1,drop=FALSE],  X[, NAMES_2 ,drop=FALSE], stringsAsFactors = FALSE)
  return(R)
}


data_ranked_ANOVA = function(X,y){
  
  
  n = ncol(X)
  y = as.character(y)
  #y[is.na(y)] = "N/A"
  pvalues = rep(1,n)
  #  print(table(y))
  
  
  
  for(i in 1:n){
    x = X[,i]
    pvalues[i] = tryCatch({
      fit = aov(x ~ y)
      summary(fit)[[1]][[1,"Pr(>F)"]]
    }, error = function(e){
      1
    })
  }
  P = pvalues
  names(P) = names(X)
  DATA = X[,order(P)]
  P = sort(P)
  list(DATA=DATA, P = P)
  
}

print_summary = function(X,TT = ""){
  
  X = data.frame(
             Title = TT,
             Nrow = nrow(X), 
             Ncol = ncol(X), 
             has.all.na = sum(sapply(X,function(x) all(is.na(x))))>0,
             Numeric = sum(sapply(X,is.numeric)),
             Character = sum(sapply(X,is.character)),
             Factor = sum(sapply(X,is.factor))
  )
  print(X)
  cat("\n---------------\n")
}

robust_chisq_p = function(x1,x2){
  R =     
    tryCatch({
      chisq.test(x1,x2)$p.value
    }, warning = function(w){
      suppressWarnings({
        chisq.test(x1,x2)$p.value
      })
    },
    error = function(e){
      404
    })
  
  R = ifelse(is.null(R),404,R)
  round(R,4)  
}

robust_anova_p = function(x,y){
  
  R =     
    tryCatch({
    fit = aov(y ~ x)
    summary(fit)[[1]][[1,"Pr(>F)"]]
  }, warning = function(w){
    suppressWarnings({
      fit = aov(y ~ x)
      summary(fit)[[1]][[1,"Pr(>F)"]]
    })
  },
  error = function(e){
    404
  })
  
  R = ifelse(is.null(R),404,R)
  round(R,4)  
  
}

robust_bartlett_p = function(x,y){
  
  R =     
    tryCatch({
      bartlett.test(y ~ x)$p.value
      
    }, warning = function(w){
      suppressWarnings({
        bartlett.test(y ~ x)$p.value
        
      })
    },
    error = function(e){
      404
    })
  
  round(R,3)  
  
}




sketch = htmltools::withTags(table(
  #class = 'display',
  class = 'cell-border stripe',
  thead(
    tr(
      th(rowspan = 2, 'Parametrics'),
      th(colspan = 3, 'Mean test (ANOVA test)'),
      th(colspan = 3, "Variance test (Bartlett's test)")
    ),
    tr(
      lapply(rep(c('RPI', 'CSI', 'HSI'), 2), th)
    )
  )
))



removeOutliers = function(X, quan = 1){
  library(mvoutlier)  
  fit = suppressWarnings({mvoutlier::uni.plot(x = X,quan = quan )})
  INDEX = !fit$outliers
  R = X[INDEX,]
  R
}


boxplot_MA = function(D, xname, yname, 
                      #selected_, 
                      TT = "title", rollN = 7, ref=NA, ref2=NA,
                      theme_BW = FALSE,
                      bgcolor = 0
                      ){
  library(ggplot2)
  library(dplyr)
  set.seed(123)
  
  x = D[,xname]
  y = D[,yname]
#   x = rep(LETTERS[1:10],each=10)
#   y = c(rnorm(99),20)
#   selected_ = c(rep(TRUE,50),rep(FALSE,50))
  x = as.character(x)
  
  D2 = data.frame(x,y)#, selected_= vals$keeprows)  
  #D2 = na.omit(D2)
  
  temp = D2 %>% group_by(x) %>% dplyr::summarise(mean=mean(y,na.rm = TRUE))
  #ma=zoo::rollmean(temp$mean,rollN,fill=NA,align = "right")
  ma = zoo::rollapply(temp$mean, rollN, function(x) {mean(x,na.rm = TRUE)}, fill=NA, align='right')
  Sd = sd(y,na.rm = TRUE)
  ucl = ma + Sd*3
  lcl = ma - Sd*3
  MA = data.frame(temp,ma,ucl,lcl)
  
  if( theme_BW){
    p =  ggplot(D,aes_string(x=xname,y=yname)) + theme_bw()+
      geom_boxplot(outlier.shape = NA) + 
      #    stat_summary(fun.y=median, geom="line",aes(group=1),color="red")+
      geom_line(data=MA,aes(x=factor(x),y=ma,group=1),color="red")+
      geom_line(data=MA,aes(x=factor(x),y=lcl,group=1),color="red",linetype="dashed")+
      geom_line(data=MA,aes(x=factor(x),y=ucl,group=1),color="red",linetype="dashed")+
      scale_color_manual(values = c("black","red"),guide=FALSE)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.y=element_blank()
            )+
      xlab(xname)+
      ylab(yname)+
      ggtitle(TT)+
          theme(
            plot.title = element_text( colour = "blue",size=ggplot2::rel(1.5))
          )+
      geom_hline(yintercept = as.numeric(ref), color = "red", na.rm = TRUE)+
      geom_hline(yintercept = as.numeric(ref2), color = "orange",na.rm = TRUE)+
      
      #geom_jitter(aes(color=vals$keeprows),width = 0.4,show.legend = FALSE,size = ifelse(vals$keeprows,4,1.5))
      geom_jitter(aes(color=selected,size=selected),width = WIDTH,show.legend = FALSE)
    
      
  }  else {

  p=ggplot(D,aes_string(x=xname,y=yname)) + 
    geom_boxplot(outlier.shape = NA) + 
#    stat_summary(fun.y=median, geom="line",aes(group=1),color="red")+
    geom_line(data=MA,aes(x=factor(x),y=ma,group=1),color="red")+
    geom_line(data=MA,aes(x=factor(x),y=lcl,group=1),color="red",linetype="dashed")+
    geom_line(data=MA,aes(x=factor(x),y=ucl,group=1),color="red",linetype="dashed")+
    scale_color_manual(values = c("black","red"),guide=FALSE)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.y=element_blank())+
    xlab(xname)+
    ylab(yname)+
    ggtitle(TT)+
    theme(
      plot.title = element_text( colour = "blue",size=ggplot2::rel(1.5))
    )+
    geom_hline(yintercept = as.numeric(ref), color = "red",na.rm = TRUE)+
    geom_hline(yintercept = as.numeric(ref2), color = "orange",na.rm = TRUE)+
    
    #geom_jitter(aes(color=vals$keeprows),width = 0.4,show.legend = FALSE,size = ifelse(vals$keeprows,4,1.5))
    geom_jitter(aes(color=selected,size=selected),width = WIDTH, show.legend = FALSE)

  }
  if(bgcolor == 1){
    p = p + theme(panel.background = element_rect(fill = "aliceblue"))
  }
  p

}

# D = read.csv("D:/FMS//new.csv")[-274,]
# selected_ = c(rep(TRUE,10),rep(FALSE,1923))
# xname = "CLRM_EXIT_DATE"
# yname = "ARM_Z"
# TT = "Boxplot"
# rollN = 7
# x = D[,xname]
# y = D[,yname]



# 
# boxplot_MA_2(x,y,xname = xname,yname = yname,selected_ = selected_,TT = TT,rollN = rollN)
# 
# boxplot_MA(D = D,xname = xname,yname = yname,selected_ = selected_,TT = TT,rollN = rollN,ref = -1.65)

