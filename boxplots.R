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
  
  round(R,3)  
  
}



Y = rnorm(1000)

DATE = paste0("WEEK",rep(1:5,each=200))

GENDER = sample(rep(c("MALE","FEMALE"),each=500))

CAREER = sample(c(rep("LAW",300),rep("IT",200),rep("FINANCE",500)))

INCOME = sample(c(rep("HIGH",200),rep("MEDIUM",500),rep("LOW",300)))

LEVEL = sample(c(rep("SENIOR",200),rep("JUNIOR",500),rep("STAFF",300)))

D = data.frame(Y,DATE,GENDER,CAREER,INCOME,LEVEL)

names(D)

library(ggplot2)

Y_min = min(Y)

give.n <- function(x){
#  return(c(y = min(x), label = length(x)))
  return(c(y = Y_min, label = length(x)))
}


x = c("DATE","CAREER")
pvalues = ddply(.data=D,
                x,
                summarize,
                pvalue = paste("P:",robust_anova_p(GENDER,Y)),
                COLOR = robust_anova_p(GENDER,Y)<0.1
)

ggplot(D,aes(y=Y,x = GENDER)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=0.3)+
  facet_grid(.~DATE + CAREER)+
  stat_summary(fun.data = give.n, geom = "text",color="black") +
  geom_text(data=pvalues, aes(x=1.2, y=5, label=pvalue,color=COLOR), 
            inherit.aes=FALSE, parse=FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5,vjust = 0.5))+
  scale_color_manual(values = c("black","red"),guide=FALSE)




require(ggplot2)
require(plyr)
mms <- data.frame(deliciousness = rnorm(100),
                  type=sample(as.factor(c("peanut", "regular")), 
                              100, replace=TRUE),
                  color=sample(as.factor(c("red", "green", "yellow", "brown")), 
                               100, replace=TRUE))


mms.cor <- ddply(.data=mms, 
                 .(type, color), 
                 summarize, 
                 n=paste("n =", length(deliciousness)))

plot <- ggplot(data=mms, aes(x=deliciousness)) + 
  geom_density() + 
  facet_grid(type ~ color) + 
  geom_text(data=mms.cor, aes(x=1.8, y=5, label=n), 
            colour="black", inherit.aes=FALSE, parse=FALSE)

plot






#  
x <- c("supp", "dose") 
ddply(ToothGrowth, x, function(df) mean(df$len)) 