
validity = function(prediction,gold_std,confint_format = ",",type=0){
  if(length(unique(prediction[is.na(prediction)==F]))>2){
    writeLines(paste("You have more than 2 possible states for prediction"))
  }
  if(length(unique(gold_std[is.na(gold_std)==F]))>2){
    writeLines(paste("You have more than 2 possible states in gold_std"))
  }
  thing = paste(prediction,gold_std)
  TP = length(thing[thing=="1 1"])
  TN = length(thing[thing=="0 0"])
  FP = length(thing[thing=="1 0"])
  FN = length(thing[thing=="0 1"])
  n = length(thing)

  if(TP>0){
  if(type==0){
  a = prop.test(TP,TP+FN)
  writeLines(paste("The sensitivity is ",round(a$estimate,3)*100," ","(",round(a$conf.int[1],3)*100,confint_format,round(a$conf.int[2],3)*100,")",sep = ""))
  sensitive  = round(a$estimate,4)
  sens_low = round(a$conf.int[1],4)
  sens_up = round(a$conf.int[2],4)
  }
  if(type==1){
    a = as.numeric(binom.confint(TP,TP+FN)[5,4:6])
    writeLines(paste("The sensitivity is ",round(a[1],3)*100," ","(",round(a[2],3)*100,confint_format,round(a[3],3)*100,")",sep = ""))
    sensitive  = round(a[1],4)
    sens_low = round(a[2],4)
    sens_up = round(a[3],4)
  }
  }
  if(TP==0){
    writeLines(paste("No true positives"))
    sensitive = 0
    sens_low = NA
    sens_up = NA
  }

  if(TN>0){
    if(type==0){
      a = prop.test(TN,TN+FP)
      writeLines(paste("The specificity is ",round(a$estimate,3)*100," ","(",round(a$conf.int[1],3)*100,confint_format,round(a$conf.int[2],3)*100,")",sep = ""))
      specific  = round(a$estimate,4)
      spec_low = round(a$conf.int[1],4)
      spec_up = round(a$conf.int[2],4)
    }
    if(type==1){
      a = as.numeric(binom.confint(TN,TN+FP)[5,4:6])
      writeLines(paste("The specificity is ",round(a[1],3)*100," ","(",round(a[2],3)*100,confint_format,round(a[3],3)*100,")",sep = ""))
      specific  = round(a[1],4)
      spec_low = round(a[2],4)
      spec_up = round(a[3],4)
    }
  }
  if(TN==0){
  writeLines(paste("No true negatives"))
  specific = 0
  spec_low = NA
  spec_up = NA
  }

  if(TP>0){
    if(type==0){
      a = prop.test(TP,TP+FP)
      writeLines(paste("The ppv is ",round(a$estimate,3)*100," ","(",round(a$conf.int[1],3)*100,confint_format,round(a$conf.int[2],3)*100,")",sep = ""))
      pospv  = round(a$estimate,4)
      ppv_low = round(a$conf.int[1],4)
      ppv_up = round(a$conf.int[2],4)
    }
    if(type==1){
      a = as.numeric(binom.confint(TP,TP+FP)[5,4:6])
      writeLines(paste("The ppv is ",round(a[1],3)*100," ","(",round(a[2],3)*100,confint_format,round(a[3],3)*100,")",sep = ""))
      pospv  = round(a[1],4)
      ppv_low = round(a[2],4)
      ppv_up = round(a[3],4)
    }
  }
  if(TP==0){
  writeLines(paste("No true positives"))
  pospv = 0
  ppv_low = NA
  ppv_up = NA
  }


  if(TN>0){
    if(type==0){
      a = prop.test(TN,TN+FN)
      writeLines(paste("The npv is ",round(a$estimate,3)*100," ","(",round(a$conf.int[1],3)*100,confint_format,round(a$conf.int[2],3)*100,")",sep = ""))
      negpv  = round(a$estimate,4)
      npv_low = round(a$conf.int[1],4)
      npv_up = round(a$conf.int[2],4)
    }
    if(type==1){
      a = as.numeric(binom.confint(TN,TN+FN)[5,4:6])
      writeLines(paste("The npv is ",round(a[1],3)*100," ","(",round(a[2],3)*100,confint_format,round(a[3],3)*100,")",sep = ""))
      negpv  = round(a[1],4)
      npv_low = round(a[2],4)
      npv_up = round(a[3],4)
    }
  }
  if(TN==0){
  writeLines(paste("No true negatives"))
  negpv = 0
  npv_low = NA
  npv_up = NA
  }

  if(type==0){
  a = prop.test(TP+TN,TP+TN+FP+FN)
  writeLines(paste("The accuracy is ",round(a$estimate,3)*100," ","(",round(a$conf.int[1],3)*100,confint_format,round(a$conf.int[2],3)*100,")",sep = ""))
  accur = round(a$estimate,4)
  accur_low = round(a$conf.int[1],4)
  accur_up = round(a$conf.int[2],4)
  }
  if(type==1){
    a = as.numeric(binom.confint(TP+TN,TP+TN+FP+FN)[5,4:6])
    writeLines(paste("The accuracy is ",round(a[1],3)*100," ","(",round(a[2],3)*100,confint_format,round(a[3],3)*100,")",sep = ""))
    accur  = round(a[1],4)
    accur_low = round(a[2],4)
    accur_up = round(a[3],4)
  }



  writeLines(paste("There were",n-(TP+TN+FP+FN),"missing"))

  measure = c("Sensitivity","Specificity","PPV","NPV","Accuracy")
  estimate = c(sensitive,specific,pospv,negpv,accur)
  lower_bound = c(sens_low,spec_low,ppv_low,npv_low,accur_low)
  upper_bound = c(sens_up,spec_up,ppv_up,npv_up,accur_up)
  results = data.frame(measure,estimate,lower_bound,upper_bound)

  mtx = matrix(c(TP,FN,FP,TN),2,2,dimnames = c(list(c("Test +","Test -")),list(c("GS +","GS -"))))
  final = list(results,mtx,TP,FP,FN,TN)
  names(final)=c("results","mtx","TP","FP","FN","TN")

  return(final)
}


cord_confint = function(est,ll,ul,confint_format = ",",dec = 3,times_100=F,include_confint=T){
  if(times_100==F){
  if(include_confint==T){
    return(paste(round(est,dec)," (",round(ll,dec),confint_format," ",round(ul,dec),")",sep = ""))
  }
    if(include_confint==F){
      return(paste(round(est,dec)))
    }
  }
  if(times_100==T){
    if(include_confint==T){
      return(paste(round(est,dec)*100," (",round(ll,dec)*100,confint_format," ",round(ul,dec)*100,")",sep = ""))
    }
    if(include_confint==F){
      return(paste(round(est,dec)*100))
    }
  }
}

prop_to_report = function(x,type = 0,flip = 0,times_100=F,include_confint=T){
  uniq = length(unique(x))
  if(uniq>2){
    stop("More than 2 levels")
  }
  if(uniq==1){
    writeLines(paste(unique(x),"\n The sample size is",length(x)))
    return(paste(unique(x)))
  }
  if(flip==0){
    if(uniq==2){
    a = as.matrix(table(x))
    if(type==0){
      a1 = prop.test(a[1],a[1]+a[2])
      return(cord_confint(a1$estimate,a1$conf.int[1],a1$conf.int[2],confint_format=",",dec=3,times_100 = times_100,include_confint=include_confint))
    }
    if(type==1){
      a1 = as.numeric(binom.confint(a[1],a[1]+a[2])[5,4:6])
      return(cord_confint(a1[1],a1[2],a1[3],confint_format=",",dec=3,times_100 = times_100,include_confint=include_confint))
    }
    }
  }
  if(flip==1){
    if(uniq==2){
      a = as.matrix(table(x))
      if(type==0){
        a1 = prop.test(a[2],a[1]+a[2])
        return(cord_confint(a1$estimate,a1$conf.int[1],a1$conf.int[2],confint_format=",",dec=3,times_100 = times_100,include_confint=include_confint))
      }
      if(type==1){
        a1 = as.numeric(binom.confint(a[2],a[1]+a[2])[5,4:6])
        return(cord_confint(a1[1],a1[2],a1[3],confint_format=",",dec=3,times_100 = times_100,include_confint=include_confint))
      }
    }
  }
}

validity_tests = function(prediction_1,answer_1,prediction_2,answer_2,which_test=0){
    v1 = validity(prediction_1,answer_1)
    TP1 = v1$TP
    TN1 = v1$TN
    FP1 = v1$FP
    FN1 = v1$FN
    v2 = validity(prediction_2,answer_2)
    TP2 = v2$TP
    TN2 = v2$TN
    FP2 = v2$FP
    FN2 = v2$FN
    sens = prop.test(c(TP1,TP2),c(TP1+FN1,TP2+FN2))
    spec = prop.test(c(TN1,TN2),c(TN1+FP1,TN2+FP2))
    npv = prop.test(c(TN1,TN2),c(TN1+FN1,TN2+FN2))
    ppv = prop.test(c(TP1,TP2),c(TP1+FP1,TP2+FP2))
    acc = prop.test(c(TP1+TN1,TP2+TN2),c(TP1+FN1+FP1+TN1,TP2+FN2+TN2+FP2))
    test = c("Sensitivity","Specificity","PPV","NPV","Accuracy")
    estimate_1 = c(sens$estimate[1],spec$estimate[1],ppv$estimate[1],npv$estimate[1],acc$estimate[1])
    estimate_2 = c(sens$estimate[2],spec$estimate[2],ppv$estimate[2],npv$estimate[2],acc$estimate[2])
    if(which_test==0){
    p_val = c(sens$p.value,spec$p.value,ppv$p.value,npv$p.value,acc$p.value)
    }
    if(which_test==1){
      sens_pval = fisher.test(matrix(c(TP1,FN1,TP2,FN2),2,2))$p.value
      spec_pval = fisher.test(matrix(c(TN1,FP1,TN2,FP2),2,2))$p.value
      ppv_pval = fisher.test(matrix(c(TP1,FP1,TP2,FP2),2,2))$p.value
      npv_pval = fisher.test(matrix(c(TN1,FN1,TN2,FN2),2,2))$p.value
      acc_pval = fisher.test(matrix(c(TP1+TN1,FN1+FP1,TP2+TN2,FN2+FP2),2,2))$p.value
      p_val = c(sens_pval,spec_pval,ppv_pval,npv_pval,acc_pval)
    }
    results = data.frame(test,estimate_1,estimate_2,round(p_val,4))
    return(results)
}

split_time_series = function(data_frame, diff_column, outcome_column, time_unit){
  x_old = data_frame[,diff_column]/time_unit
  x_old = ifelse(x_old<0 & x_old>=-0.5,-.5,ifelse(x_old>=0 & x_old<=0.5,0.5,round(x_old)))
  y_old = data_frame[,outcome_column]
  x = c()
  y = c()
  n = c()
  sd = c()
  pre_post = c()
  for(i in unique(x_old)){
    x = c(x,i)
    y = c(y,mean(y_old[x_old==i],na.rm = T))
    n = c(n,length(y_old[x_old==i & is.na(x_old)==F]))
    sd = c(sd,sd(y_old[x_old==i],na.rm = T))
    pre_post = c(pre_post,ifelse(i>=0,1,0))
  }
  final_frame = data.frame(x,y,pre_post,n,sd)
  final_frame = subset(final_frame,is.na(x)==F)
  m1 = lm(y~x+pre_post+x:pre_post,final_frame,weights = final_frame$n)
  thing = summary(m1)$coefficients
  x1_pre = min(final_frame$x)
  x2_pre = 0
  y1_pre = thing[1,1]+(thing[2,1]*x1_pre)
  y2_pre = thing[1,1]
  x1_post = 0
  x2_post = max(final_frame$x)
  y1_post = thing[1,1]+thing[3,1]
  y2_post = thing[1,1]+(thing[2,1]*x2_post)+thing[3,1]+(thing[4,1]*x2_post)
  p1 = ggplot(final_frame,aes(x = x, y=y,color=as.factor(pre_post),size=n))+geom_point()+geom_segment(aes(x = x1_pre, y = y1_pre, xend = x2_pre, yend = y2_pre,size=1))+geom_segment(aes(x = x1_post, y = y1_post, xend = x2_post, yend = y2_post,size=1))
  to_return = list(final_frame,m1,p1)
  names(to_return) = c("final_frame","model","plot")
  return(to_return)
}
