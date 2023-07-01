shuju <- read.csv('H:\\R-SEM.csv')
tujing <- shuju[,7:15] 

library('psych')  
cortest.bartlett(cor(tujing))
KMO(cor(tujing))
eigen(cor(tujing))
fa.parallel(tujing, fa="pc", n.iter=100)
tujing.pcf <- principal(tujing,cor=T,nfactors=3,rotate="none")
tujing.pcf
write.text(tujing.pcf,file='H:\\R-SEM\\主成分分析结果.text')

defen <- read.csv('H:\\R-SEM\\work.csv')
PC1 <- defen[,40]
PC2 <- defen[,41]
PC3 <- defen[,42]
ZS <- defen[,38]
cor(PC1, ZS, method ="spearman")
cor.test(PC1, ZS, method="spearman")
cor(PC2, ZS, method ="spearman")
cor.test(PC2, ZS, method="spearman")
cor(PC3, ZS, method ="spearman")
cor.test(PC3, ZS, method="spearman")
CS <- defen[,39]
cor(PC1, CS, method ="spearman")
cor.test(PC1, CS, method="spearman")
cor(PC2, CS, method ="spearman")
cor.test(PC2, CS, method="spearman")
cor(PC3, CS, method ="spearman")
cor.test(PC3, CS, method="spearman")


library(semPlot)
zhongjie<- '
      # measurement model
        DK =~ ZSwenchongchuanbo+ZSfare+ZStoutong+ZSjirouguge+ZSpizhen+ZSchuxue+ZSfali+ZSshuili
        AMB =~ CSquwenji+CSwenzhang+CSshamenshachuang+CSpensashachongji+CSwenxiang+CSchangyichangku+CSqingchulaji+CSqingzacao+CSbucunshui
        # regressions model
        DK ~ a*PC
        AMB ~ c*PC + b*DK

        ind := a*b
        dir := c
        total := dir+ind' 
zhongfit <- sem(zhongjie,data = jiegou,se = "bootstrap",bootstrap=2000) 
summary(zhongfit, standardized = TRUE,fit.measures = TRUE)
fitMeasures(zhongfit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea","agfi","ifi","chisq/df"))
parameterestimates(zhongfit,boot.ci.type = "bca.simple",standardized = TRUE)

zhongjie1<- '
      # measurement model
        DK =~ ZSwenchongchuanbo+ZSfare+ZStoutong+ZSjirouguge+ZSpizhen+ZSchuxue+ZSfali+ZSshuili
        AMB =~ CSquwenji+CSwenzhang+CSshamenshachuang+CSpensashachongji+CSwenxiang+CSchangyichangku+CSqingchulaji+CSqingzacao+CSbucunshui
        # regressions model
        DK ~ a1*PC1
        AMB ~ c1*PC1 + b1*DK

        ind1 := a1*b1
        dir1 := c1
        total1 := dir1+ind1' 
zhongfit1 <- sem(zhongjie1,data = jiegou,se = "bootstrap",bootstrap=2000) 
summary(zhongfit1, standardized = TRUE,fit.measures = TRUE)
fitMeasures(zhongfit1,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea","agfi","ifi","chisq/df"))
parameterestimates(zhongfit1,boot.ci.type = "bca.simple",standardized = TRUE)

zhongjie2<- '
      # measurement model
        DK =~ ZSwenchongchuanbo+ZSfare+ZStoutong+ZSjirouguge+ZSpizhen+ZSchuxue+ZSfali+ZSshuili
        AMB =~ CSquwenji+CSwenzhang+CSshamenshachuang+CSpensashachongji+CSwenxiang+CSchangyichangku+CSqingchulaji+CSqingzacao+CSbucunshui
        # regressions model
        DK ~ a2*PC2
        AMB ~ c2*PC2 + b2*DK

        ind2 := a2*b2
        dir2 := c2
        total2 := dir2+ind2' 
zhongfit2 <- sem(zhongjie2,data = jiegou,se = "bootstrap",bootstrap=2000) 
summary(zhongfit2, standardized = TRUE,fit.measures = TRUE)
fitMeasures(zhongfit2,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea","agfi","ifi","chisq/df"))
parameterestimates(zhongfit2,boot.ci.type = "bca.simple",standardized = TRUE)

zhongjie3<- '
      # measurement model
        DK =~ ZSwenchongchuanbo+ZSfare+ZStoutong+ZSjirouguge+ZSpizhen+ZSchuxue+ZSfali+ZSshuili
        AMB =~ CSquwenji+CSwenzhang+CSshamenshachuang+CSpensashachongji+CSwenxiang+CSchangyichangku+CSqingchulaji+CSqingzacao+CSbucunshui
        # regressions model
        DK ~ a3*PC3
        AMB ~ c3*PC3 + b3*DK

        ind3 := a3*b3
        dir3 := c3
        total3 := dir3+ind3' 
zhongfit3 <- sem(zhongjie3,data = jiegou,se = "bootstrap",bootstrap=2000) 
summary(zhongfit3, standardized = TRUE,fit.measures = TRUE)
fitMeasures(zhongfit3,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea","agfi","ifi","chisq/df"))
parameterestimates(zhongfit3,boot.ci.type = "bca.simple",standardized = TRUE)


