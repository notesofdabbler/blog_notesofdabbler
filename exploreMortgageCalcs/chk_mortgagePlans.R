
# load libraries
library(ggplot2)
library(scales)
library(rbokeh)

# get payment per month for a given term (yrs), annual interest (%), loan amount ($)

mnthpymt = function(term,apr,loan){
    
    numpays = term * 12
    ifact = 1/(1+(apr*0.01/12))
  
    pymt = loan *  (1 - ifact)/(1 - ifact ** numpays) * (1/ifact)

    return(pymt)
}


# 5 yr adj rate smart mortgage


paystruct = function(chgpts,chgfee,termyr,apryr,initloan,xtrapymt,numyrs){
  
    loanresid = rep(0,numyrs*12) # remaining loan at the end of month
    intmnth = rep(0,numyrs*12) # interest in a month
    totpay = rep(0,numyrs*12) # cumulative payment upto a month
    mnthpay = rep(0,numyrs*12) # monthly payment
    equity = rep(0,numyrs*12) # equity build upto a month
    netpay = rep(0,numyrs*12) # net payment
  
    for(i in 1:numyrs){
        if(chgpts[i] == 1){
            loanamt = ifelse(i == 1, initloan, loanresid[(i-1)*12])
            pymt = mnthpymt(termyr[i],apryr[i],loanamt)
        }
        for(j in 1:12){
            mnthnum = (i-1) *12 + j
            prevloan = ifelse(mnthnum == 1, initloan, loanresid[mnthnum - 1])
            intmnth[mnthnum] = prevloan * apryr[i]*0.01/12
            
            chgfeemnth = ifelse(j == 1,chgfee[i],0)
            mnthpay[mnthnum] = ifelse(prevloan > 0, pymt + xtrapymt[i] + chgfeemnth,0)
            loanresid[mnthnum] = max(prevloan - (mnthpay[mnthnum] - intmnth[mnthnum] - chgfeemnth),0)
            equity[mnthnum] = initloan - loanresid[mnthnum]
            
            if(mnthnum > 1){
                totpay[mnthnum] = totpay[mnthnum - 1] + mnthpay[mnthnum]
              } else {
                totpay[mnthnum] = mnthpay[mnthnum]
              }
            
            netpay[mnthnum] = totpay[mnthnum] - equity[mnthnum]
            
            }
        
        }
  
    df = data.frame(mnth = seq(1,numyrs*12),
                    totpay = totpay,
                    netpay = netpay,
                    intmnth = intmnth,
                    loanresid = loanresid,
                    equity = equity,
                    mnthpay = mnthpay)
    return(df)
  
}


initloan = 200000

chgpts = rep(0,30)
chgfee = rep(0,30)
termyr = rep(30,30)
apryr = rep(4.25,30)
#xtrapymt = rep(0,30)
xtrapymt = rep(180,30)
numyrs = 30
chgpts[1] = 1

payfixed = paystruct(chgpts,chgfee,termyr,apryr,initloan,xtrapymt,numyrs)


ggplot(data=payfixed) + geom_line(aes(x=mnth/12,y=loanresid/1000))+
    scale_x_continuous(breaks = seq(0,numyrs))+scale_y_continuous(label=dollar)+theme_bw()
ggplot(data=payfixed) + geom_line(aes(x=mnth/12,y=netpay))+
  scale_x_continuous(breaks = seq(0,numyrs))+scale_y_continuous(label=dollar)+theme_bw()
ggplot(data=payfixed) + geom_line(aes(x=mnth/12,y=totpay))+
  scale_x_continuous(breaks = seq(0,numyrs))+scale_y_continuous(label=dollar)+theme_bw()

ggplot(data=payfixed,aes(x=mnth/12,y=mnthpay)) + geom_line()+
  scale_x_continuous(breaks = seq(0,numyrs))+theme_bw()


figure()%>%ly_lines(mnth/12,loanresid/1000,data=payfixed)
figure()%>%ly_lines(mnth/12,netpay/1000,data=payfixed)
figure()%>%ly_lines(mnth/12,totpay/1000,data=payfixed)
figure()%>%ly_lines(mnth/12,mnthpay,data=payfixed)


initloan = 200000
numyrs = 30
chgpts = rep(0,30)
chgpts[c(1,6,11)] = 1
chgfee = rep(0,30)
chgfee[1] = 3200
chgfee[c(6,11)] = 500
termyr = rep(30,30)
termyr = seq(30,1)
apryr = c(rep(2.65,5),rep(5,5),rep(8,20))
xtrapymt = rep(180,30)

payadj = paystruct(chgpts,chgfee,termyr,apryr,initloan,xtrapymt,numyrs)

ggplot()+
  geom_line(data=payfixed,aes(x=mnth/12,y=mnthpay),color="red") + 
  geom_line(data=payadj,aes(x=mnth/12,y=mnthpay),color="blue") + 
  scale_x_continuous(breaks = seq(0,numyrs))+scale_y_continuous(label=dollar)+theme_bw()

figure()%>%
  ly_lines(mnth/12,mnthpay,data=payfixed,color="red")%>%
  ly_lines(mnth/12,mnthpay,data=payadj,color="blue")


ggplot()+
    geom_line(data=payfixed,aes(x=mnth/12,y=netpay),color="red") + 
    geom_line(data=payadj,aes(x=mnth/12,y=netpay),color="blue") + 
  scale_x_continuous(breaks = seq(0,numyrs))+scale_y_continuous(label=dollar)+theme_bw()

figure()%>%
  ly_lines(mnth/12,netpay,data=payfixed,color="red")%>%
  ly_lines(mnth/12,netpay,data=payadj,color="blue")


payfixedvsadj = data.frame(mnth = payfixed$mnth,
                           netpayfixed = payfixed$netpay,
                           netpayadj = payadj$netpay)
payfixedvsadj$deltapay = payfixedvsadj$netpayfixed - payfixedvsadj$netpayadj

ggplot()+
  geom_line(data=payfixedvsadj,aes(x=mnth/12,y=deltapay),color="red") + 
  geom_hline(aes(yintercept = 0),color="blue")+
  scale_x_continuous(breaks = seq(0,numyrs))+scale_y_continuous(label=dollar)+theme_bw()

figure() %>% ly_lines(mnth/12,deltapay/1000,data=payfixedvsadj,color="blue")
