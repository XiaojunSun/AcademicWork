
abnrt <-  function(dataset=change_new, type="CAPM"){
  if (type=="CAPM"){
    output <- dlply(.data = change_new, .variables = c("Stckcd","Chgdt"), .parallel = F, .fun =function(chg){
      ChgDate <- chg$Chgdt
      slected <- filter(stock_s, Stkcd==chg$Stckcd)
      
      event.m <- which(SHAidx$Trddt==ChgDate)
      event.s <- which(slected$Trddt==ChgDate)
      
      for(i in 1:5){
        if(!(length(event.m)==0)){
          break
        }else{
          event.m <- which(SHAidx$Trddt == (ChgDate+i))
        }
      }
      
      for(i in 1:5){
        if(!(length(event.s)==0)){
          break
        }else{
          event.s <- which(slected$Trddt == (ChgDate+i))
        }
      }
      
      adj_rsq <- NULL
      abnrt <- NULL
      
      if(length(event.s+event.m)==0){} else 
        if((event.s-wnd_e-wnd_n)<=0){}
      else{
        #estimation window
        #use closed price to calculate the stock return
        clsprc_e <- slected$Clsprc[(event.s-wnd_e-wnd_n-1):(event.s-wnd_n-1)]  # stock price
        Rt.se <- ((clsprc_e[-1]-clsprc_e[-length(clsprc_e)])/clsprc_e[-length(clsprc_e)])  #simple return of the selected stock
        #rt.se <- log(1+Rt.se) #log return of the selected stock
        Rt.me <- SHAidx$Retindex[(event.m-wnd_e-wnd_n):(event.m-wnd_n-1)] #simple market return
        #rt.me <- log(1+Rt.me) #log market return
        
        fit <- lm(Rt.se~Rt.me) #CAPM-based
        adj_rsq <- summary(fit)$adj.r.squared
        
        #event window
        clsprc <- slected$Clsprc[(event.s-wnd_n-1):(event.s+wnd_n)] 
        Rt.s <- ((clsprc[-1]-clsprc[-length(clsprc)])/clsprc[-length(clsprc)])
        #rt.s <- log(1+Rt.s)
        Rt.m <- SHAidx$Retindex[(event.m-wnd_n):(event.m+wnd_n)]
        #rt.m <- log(1+Rt.m)
        
        Rt.s_e <- fit$coefficient[1]+fit$coefficient[2]*Rt.m  #CAPM stock simple return
        #rt.s_e <- fit$coefficient[1]+fit$coefficient[2]*rt.m
        
        abnrt <- Rt.s - Rt.s_e   #CAPM-based abnormal return
        #var_e <- 1/(wnd_e-2)*sum((Rt.se - fit$coefficient[1]- fit$coefficient[2]*Rt.me)^2)
      }
      # out_put <- paste("L",chg$Stckcd, as.numeric(format(ChgDate, format = "%Y%m")), sep="_")
      # assign(out_put, list(abnrt=abnrt, adj_rsq=adj_rsq))
      out_put <- list(abnrt=abnrt, adj_rsq=adj_rsq)
      return(out_put)
    })
  }else if(type=="ThreeFactor"){
    output <- dlply(.data = Non_Standard, .variables = c("Stckcd","Chgdt"), .parallel = F, .fun =function(chg){
        ChgDate <- chg$Chgdt
        slected <- filter(stock_s, Stkcd==chg$Stckcd)
        
        event.t <- which(Three_Factor$Date == ChgDate)
        event.s <- which(slected$Trddt==ChgDate)  
        
        for(i in 1:5){
          if(!(length(event.t)==0)){
            break
          }else{
            event.t <- which(Three_Factor$Date == (ChgDate+i))
          }
        }
        
        for(i in 1:5){
          if(!(length(event.s)==0)){
            break
          }else{
            event.s <- which(slected$Trddt == (ChgDate+i))
          }
        }
        
        adj_rsq <- NULL
        abnrt <- NULL
        
        if (length(event.s+event.t)==0){}else 
          if((event.s-wnd_e-wnd_n)<=0){}
        else{
          # estimation window
          # use closed price to calculate the stock return
          clsprc_e <- slected$Clsprc[(event.s-wnd_e-wnd_n-1):(event.s-wnd_n-1)] #stock price
          Rt.se <- ((clsprc_e[-1]-clsprc_e[-length(clsprc_e)])/clsprc_e[-length(clsprc_e)]) #simple return of the selected stock
          # rt.se <- log(1+Rt.se) #log return of the selected stock
          Rmrf.se <- Three_Factor$Rmrf_tmv[(event.t-wnd_e-wnd_n):(event.t-wnd_n-1)]
          Smb.se <- Three_Factor$Smb_tmv[(event.t-wnd_e-wnd_n):(event.t-wnd_n-1)]
          Hml.se <- Three_Factor$Hml_tmv[(event.t-wnd_e-wnd_n):(event.t-wnd_n-1)]
          
          fit <- lm(Rt.se~Rmrf.se+Smb.se+Hml.se)  #Three factor model
          
          a <- fit$coefficient[1]
          b <- fit$coefficient[2]
          s <- fit$coefficient[3]
          h <- fit$coefficient[4] 
          
          adj_rsq <- summary(fit)$adj.r.squared
          
          
          clsprc <- slected$Clsprc[(event.s-wnd_n-1):(event.s+wnd_n)] #event window
          Rt.s <- ((clsprc[-1]-clsprc[-length(clsprc)])/clsprc[-length(clsprc)])
          # rt.s <- log(1+Rt.s)
          Rmrf <- Three_Factor$Rmrf_tmv[(event.t-wnd_n):(event.t+wnd_n)]
          Smb <- Three_Factor$Smb_tmv[(event.t-wnd_n):(event.t+wnd_n)]
          Hml <- Three_Factor$Hml_tmv[(event.t-wnd_n):(event.t+wnd_n)]
          
          Rt.s_e <- a+b*Rmrf+s*Smb+h*Hml  #predicted stock simple return
          
          abnrt <- Rt.s - Rt.s_e   #abnormal return
        }
        out_put <- list(abnrt=abnrt, adj_rsq=adj_rsq)
        return(out_put)
      })
  }else{
    message("Error: type only take value 'CAPM' or 'ThreeFactor'")
  }
  return(output)
}



