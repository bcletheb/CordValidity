regular = matrix(c(0,1,1,0),2,2,dimnames = list(c(1,0),c(1,0)))
hsens_2 = matrix(c(0,2,1,0),2,2,dimnames = list(c(1,0),c(1,0)))
hsens_3 = matrix(c(0,3,1,0),2,2,dimnames = list(c(1,0),c(1,0)))
hsens_4 = matrix(c(0,4,1,0),2,2,dimnames = list(c(1,0),c(1,0)))
hsens_5 = matrix(c(0,5,1,0),2,2,dimnames = list(c(1,0),c(1,0)))

hspec_2 = matrix(c(0,1,2,0),2,2,dimnames = list(c(1,0),c(1,0)))
hspec_3 = matrix(c(0,1,3,0),2,2,dimnames = list(c(1,0),c(1,0)))
hspec_4 = matrix(c(0,1,4,0),2,2,dimnames = list(c(1,0),c(1,0)))
hspec_5 = matrix(c(0,1,5,0),2,2,dimnames = list(c(1,0),c(1,0)))


forward_stepwise = function(data,gold_std,k_val,num_var=0){
  before = nrow(data)
  gold_std = gold_std[complete.cases(data)]
  data = data[complete.cases(data),]
  after = nrow(data)
  writeLines(paste("There were",before-after,"cases that were incomplete"))
  nothing = glm(as.factor(gold_std)~1,data,family = "binomial")

  aic_frame = data.frame(add1(nothing,names(data)))
  aic_frame = aic_frame[order(aic_frame$AIC,decreasing = F),]

  if(num_var==0){
    num_var=ncol(data)
  }
  fullmod = glm(as.factor(gold_std)~.,data[,which(rownames(aic_frame)[1:num_var] %in% names(data))],family = "binomial")
  forward = step(nothing, list(lower=formula(nothing),upper=formula(fullmod)),direction = "forward",k=k_val)

  names(forward$coefficients)[2:length(names(forward$coefficients))][which(substr(names(forward$coefficients)[2:length(names(forward$coefficients))],1,nchar(names(forward$coefficients)[2:length(names(forward$coefficients))])-1) %in% names(data))] = substr(names(forward$coefficients)[2:length(names(forward$coefficients))],1,nchar(names(forward$coefficients)[2:length(names(forward$coefficients))])-1)[which(substr(names(forward$coefficients)[2:length(names(forward$coefficients))],1,nchar(names(forward$coefficients)[2:length(names(forward$coefficients))])-1) %in% names(data))]

  inclusion = names(which(forward$coefficients[2:length(forward$coefficients)]>0))
  exclusion = names(which(forward$coefficients[2:length(forward$coefficients)]<=0))

  include_frame = data.frame(data[,c(inclusion)])
  for(i in 1:ncol(include_frame)){
    include_frame[,i] = as.numeric(as.character(include_frame[,i]))
  }

  exclude_frame = data.frame(data[,c(exclusion)])
  if(ncol(exclude_frame)>0){
    for(i in 1:ncol(exclude_frame)){
      exclude_frame[,i] = as.numeric(as.character(exclude_frame[,i]))
    }
  }
  finalframe = data.frame(data[,c(exclusion,inclusion)])

  to_return = list(forward,inclusion,exclusion,finalframe)
  names(to_return) = c("forward","inclusion","exclusion","finalframe")
  return(to_return)
}

bootstrap_forward_stepwise = function(repeats,k,num_var_a,data_frame,gold_std){
  before = nrow(data_frame)
  gold_std = gold_std[complete.cases(data_frame)]
  data_frame = data_frame[complete.cases(data_frame),]
  after = nrow(data_frame)
  writeLines(paste("There were",before-after,"cases that were incomplete"))
  misclass = c()
  sens = c()
  spec = c()
  ppv = c()
  npv = c()
  tp  = c()
  tn = c()
  fp = c()
  fn = c()
  parameter = rep(k,repeats)
  for(i in 1:repeats){
    if(exists("finalframe")){
      rm(finalframe)
    }
    if(exists("finalframe_2")){
      rm(finalframe_2)
    }
    x = sample(nrow(data_frame),nrow(data_frame),replace = T)
    y = which(!(1:nrow(data_frame) %in% x))
    train_data = data_frame[x,]
    train_ans = gold_std[x]
    test_data <<- data_frame[y,]
    test_ans = gold_std[y]
    fs1 <<- forward_stepwise(train_data,train_ans,k_val = k,num_var_a)
    if(length(fs1$inclusion)>0){
      finalframe = data.frame(test_data[,fs1$inclusion])
    }
    if(length(fs1$exclusion)>0){
      finalframe_2 = data.frame(test_data[,fs1$exclusion])
      names(finalframe_2) = fs1$exclusion
    }
    if(exists("finalframe")){
      if(ncol(finalframe)>0){
        for(L in 1:ncol(finalframe)){
          finalframe[,L] = as.numeric(as.character(finalframe[,L]))
          writeLines(paste("FinalFrame",nrow(finalframe)))

        }
      }
    }
    if(exists("finalframe_2")){
      if(ncol(finalframe_2)>0){
        for(L in 1:ncol(finalframe_2)){
          finalframe_2[,L] = as.numeric(as.character(finalframe_2[,L]))*(-1)
          writeLines(paste("FinalFrame_2",nrow(finalframe_2)))

        }
      }
    }
    writeLines(paste("inclusion"))
    writeLines(paste(fs1$inclusion))
    writeLines(paste("exclusion"))
    writeLines(paste(fs1$exclusion))
    if(exists("finalframe") & exists("finalframe_2")){
      vote_frame = cbind(finalframe,finalframe_2)
    }
    if(exists("finalframe") & !exists("finalframe_2")){
      vote_frame = finalframe
    }
    if(!exists("finalframe") & !exists("finalframe_2")){
      vote_frame = finalframe_2
    }
    if(ncol(vote_frame)>1){
      predictions = rowSums(vote_frame)
    }
    if(ncol(vote_frame)==1){
      predictions = vote_frame[,1]
    }
    predictions = ifelse(predictions>=1,1,0)
    x = validity(predictions,test_ans)
    misclass = c(misclass,abs(x$results$estimate[5]-1))
    sens = c(sens,abs(x$results$estimate[1]))
    spec = c(spec,abs(x$results$estimate[2]))
    ppv = c(ppv,abs(x$results$estimate[3]))
    npv = c(npv,abs(x$results$estimate[4]))
    tp = c(tp,(abs(x$mtx[1,1])))
    fn = c(fn,(abs(x$mtx[2,1])))
    fp = c(fp,(abs(x$mtx[1,2])))
    tn = c(tn,(abs(x$mtx[2,2])))
    writeLines(paste("This is repeat:",i,"k value is:",k))

  }
  final = data.frame(parameter,misclass,sens,spec,ppv,npv,tp,fn,fp,tn)
  return(final)
}

lasso_regression = function(data,gold_std,lambda_val){
  before = nrow(data)
  gold_std = gold_std[complete.cases(data)]
  data = data[complete.cases(data),]
  after = nrow(data)
  writeLines(paste("There were",before-after,"cases that were incomplete"))

  x = data.matrix(data[which(is.na(gold_std)==F),])
  y = gold_std[is.na(gold_std)==F]
  m1 = glmnet(x,y,alpha = 1,lambda = lambda_val,family = "binomial")
  thing = data.frame(matrix(m1$beta))
  names(thing) = "coef"
  thing$var = names(data)
  thing = subset(thing,coef!=0)

  inclusion = thing$var[thing$coef>0]
  exclusion = thing$var[thing$coef<0]


  include_frame = data.frame(data[,c(inclusion)])
  if(ncol(include_frame)>0){
    for(i in 1:ncol(include_frame)){
      include_frame[,i] = as.numeric(as.character(include_frame[,i]))
    }
  }
  exclude_frame = data.frame(data[,c(exclusion)])
  if(ncol(exclude_frame)>0){
    for(i in 1:ncol(exclude_frame)){
      exclude_frame[,i] = as.numeric(as.character(exclude_frame[,i]))
    }
  }
  if(length(inclusion)>0 | length(exclusion)>0){
    finalframe = data.frame(data[,c(exclusion,inclusion)])
  }

  to_return = list(m1,inclusion,exclusion)
  names(to_return) = c("m1","inclusion","exclusion")
  return(to_return)
}

bootstrap_lasso = function(repeats,lam,data_frame,gold_std){
  before = nrow(data_frame)
  gold_std = gold_std[complete.cases(data_frame)]
  data_frame = data_frame[complete.cases(data_frame),]
  after = nrow(data_frame)
  writeLines(paste("There were",before-after,"cases that were incomplete"))

  misclass = c()
  sens = c()
  spec = c()
  ppv = c()
  npv = c()
  tp  = c()
  tn = c()
  fp = c()
  fn = c()
  for(i in 1:repeats){
    if(exists("finalframe")){
      rm(finalframe)
    }
    if(exists("finalframe_2")){
      rm(finalframe_2)
    }
    x = sample(nrow(data_frame),nrow(data_frame),replace = T)
    y = which(!(1:nrow(data_frame) %in% x))
    train_data = data_frame[x,]

    train_ans = gold_std[x]
    test_data = data_frame[y,]
    test_ans = gold_std[y]
    fs1 <<- lasso_regression(train_data,train_ans,lambda_val = lam)
    if(length(fs1$inclusion)>0){
      finalframe = data.frame(test_data[,fs1$inclusion])
    }
    if(length(fs1$exclusion)>0){
      finalframe_2 = data.frame(test_data[,fs1$exclusion])
      names(finalframe_2) = fs1$exclusion
    }
    if(exists("finalframe")){
      if(ncol(finalframe)>0){
        for(L in 1:ncol(finalframe)){
          finalframe[,L] = as.numeric(as.character(finalframe[,L]))
          writeLines(paste("FinalFrame",nrow(finalframe)))

        }
      }
    }
    if(exists("finalframe_2")){
      if(ncol(finalframe_2)>0){
        for(L in 1:ncol(finalframe_2)){
          finalframe_2[,L] = as.numeric(as.character(finalframe_2[,L]))*(-1)
          writeLines(paste("FinalFrame_2",nrow(finalframe_2)))

        }
      }
    }
    if(exists("finalframe") & exists("finalframe_2")){
      vote_frame = cbind(finalframe,finalframe_2)
    }
    if(exists("finalframe") & !exists("finalframe_2")){
      vote_frame = finalframe
    }
    if(!exists("finalframe") & exists("finalframe_2")){
      vote_frame = finalframe_2
    }
    if(exists("vote_frame")){
    if(ncol(vote_frame)>1){
      predictions = rowSums(vote_frame)
    }
    if(ncol(vote_frame)==1){
      predictions = vote_frame[,1]
    }
    }
    if(!exists("vote_frame")){
      writeLines(paste("No Criteria Defined!!"))
      next
    }
    predictions = ifelse(predictions>=1,1,0)
    writeLines(paste(table(predictions)))
    x = validity(predictions,test_ans)
    misclass = c(misclass,abs(x$results$estimate[5]-1))
    sens = c(sens,abs(x$results$estimate[1]))
    spec = c(spec,abs(x$results$estimate[2]))
    ppv = c(ppv,abs(x$results$estimate[3]))
    npv = c(npv,abs(x$results$estimate[4]))
    tp = c(tp,(abs(x$mtx[1,1])))
    fn = c(fn,(abs(x$mtx[2,1])))
    fp = c(fp,(abs(x$mtx[1,2])))
    tn = c(tn,(abs(x$mtx[2,2])))
  }
  parameter = rep(lam,length(misclass))
  final = data.frame(parameter,misclass,sens,spec,ppv,npv,tp,fn,fp,tn)
  return(final)
}

rpart_regress = function(data,gold_std,complexity,loss_matrix){
  before = nrow(data)
  gold_std = gold_std[complete.cases(data)]
  data = data[complete.cases(data),]
  after = nrow(data)
  writeLines(paste("There were",before-after,"cases that were incomplete"))

  fs1 = rpart(as.factor(gold_std)~.,data,method = "class",control = rpart.control(cp=complexity,maxsurrogate = 0,usesurrogate = 0),parms = list(split = "gini",loss = loss_matrix))
  predictions = predict(fs1,data,type="class")
  to_return = list(fs1,predictions)
  names(to_return) = c("fs1","predictions")
  return(to_return)
}

bootstrap_rpart = function(repeats,cp,loss_matrix,data_frame,gold_std){
  before = nrow(data_frame)
  gold_std = gold_std[complete.cases(data_frame)]
  data_frame = data_frame[complete.cases(data_frame),]
  after = nrow(data_frame)
  writeLines(paste("There were",before-after,"cases that were incomplete"))

  misclass = c()
  sens = c()
  spec = c()
  ppv = c()
  npv = c()
  tp  = c()
  tn = c()
  fp = c()
  fn = c()
  parameter = rep(cp,repeats)
  loss = c()
  for(i in 1:repeats){
    x = sample(nrow(data_frame),nrow(data_frame),replace = T)
    y = which(!(1:nrow(data_frame) %in% x))
    train_data = data_frame[x,]
    train_ans = gold_std[x]
    test_data = data_frame[y,]
    test_ans = gold_std[y]
    fs1 = rpart_regress(data = train_data,gold_std = train_ans,complexity = cp,loss_matrix = get(loss_matrix))
    writeLines(paste("This is repeat:",i,"cp value is:",cp))
    predictions = predict(fs1$fs1,test_data,type="class")
    x = validity(predictions,test_ans)
    misclass = c(misclass,abs(x$results$estimate[5]-1))
    sens = c(sens,abs(x$results$estimate[1]))
    spec = c(spec,abs(x$results$estimate[2]))
    ppv = c(ppv,abs(x$results$estimate[3]))
    npv = c(npv,abs(x$results$estimate[4]))
    tp = c(tp,(abs(x$mtx[1,1])))
    fn = c(fn,(abs(x$mtx[2,1])))
    fp = c(fp,(abs(x$mtx[1,2])))
    tn = c(tn,(abs(x$mtx[2,2])))
    loss = c(loss,loss_matrix)
  }
  final = data.frame(parameter,misclass,sens,spec,ppv,npv,tp,fn,fp,tn,loss)
  return(final)
}






c50_regress = function(data,gold_std,complexity,winnow,loss_matrix,rules){
  before = nrow(data)
  gold_std = gold_std[complete.cases(data)]
  data = data[complete.cases(data),]
  after = nrow(data)
  writeLines(paste("There were",before-after,"cases that were incomplete"))


  fs1 = C5.0(as.factor(gold_std)~.,data,rules=rules,costs=loss_matrix,control=C5.0Control(winnow = winnow,CF=complexity))
  predictions = predict(fs1,data)
  to_return = list(fs1,predictions)
  names(to_return) = c("fs1","predictions")
  return(to_return)
}

bootstrap_c50 = function(repeats,cp,loss_matrix,data_frame,gold_std,winnow,rules){
  before = nrow(data_frame)
  gold_std = gold_std[complete.cases(data_frame)]
  data_frame = data_frame[complete.cases(data_frame),]
  after = nrow(data_frame)
  writeLines(paste("There were",before-after,"cases that were incomplete"))

  misclass = c()
  sens = c()
  spec = c()
  ppv = c()
  npv = c()
  tp  = c()
  tn = c()
  fp = c()
  fn = c()
  parameter = rep(cp,repeats)
  loss = c()
  winnow_col = c()
  rule_col = c()
  for(i in 1:repeats){
    x = sample(nrow(data_frame),nrow(data_frame),replace = T)
    y = which(!(1:nrow(data_frame) %in% x))
    train_data = data_frame[x,]
    train_ans = gold_std[x]
    test_data = data_frame[y,]
    test_ans = gold_std[y]
    index = c()
    for(j in 1:ncol(train_data)){
      if(length(unique(train_data[,j]))==1){
        index = c(index,j)
      }
    }
    if(length(index)>0){
    train_data = train_data[,-index]
    test_data = test_data[,-index]
    }
    fs1 = c50_regress(data = train_data,gold_std = train_ans,complexity = cp,loss_matrix = get(loss_matrix),winnow = winnow,rules = rules)
    writeLines(paste("This is repeat:",i,"cp value is:",cp))
    predictions = predict(fs1$fs1,test_data,type="class")
    x = validity(predictions,test_ans)
    misclass = c(misclass,abs(x$results$estimate[5]-1))
    sens = c(sens,abs(x$results$estimate[1]))
    spec = c(spec,abs(x$results$estimate[2]))
    ppv = c(ppv,abs(x$results$estimate[3]))
    npv = c(npv,abs(x$results$estimate[4]))
    tp = c(tp,(abs(x$mtx[1,1])))
    fn = c(fn,(abs(x$mtx[2,1])))
    fp = c(fp,(abs(x$mtx[1,2])))
    tn = c(tn,(abs(x$mtx[2,2])))
    loss = c(loss,loss_matrix)
    rule_col = c(rule_col,rules)
    winnow_col = c(winnow_col,winnow)
  }
  final = data.frame(parameter,misclass,sens,spec,ppv,npv,tp,fn,fp,tn,loss,rule_col,winnow_col)
  return(final)
}

chaid_regress = function(data,gold_std,alpha_val,max_height){
  before = nrow(data)
  gold_std = gold_std[complete.cases(data)]
  data = data[complete.cases(data),]
  after = nrow(data)
  writeLines(paste("There were",before-after,"cases that were incomplete"))
  fs1 = chaid(as.factor(gold_std)~.,data,control = chaid_control(alpha4 = alpha_val,maxheight = max_height))
  to_return = list(fs1)
  names(to_return) = c("fs1")
  return(to_return)
}

bootstrap_chaid = function(repeats,alpha_val,maxheight,data_frame,gold_std){
  misclass = c()
  sens = c()
  spec = c()
  ppv = c()
  npv = c()
  tp  = c()
  tn = c()
  fp = c()
  fn = c()
  parameter = rep(alpha_val,repeats)
  heights = c()
  for(i in 1:repeats){
    x = sample(nrow(data_frame),nrow(data_frame),replace = T)
    y = which(!(1:nrow(data_frame) %in% x))
    train_data = data_frame[x,]
    for(j in names(train_data)){
      if(sum(as.numeric(train_data[,j]))<=4){
        train_data[,j]=NULL
      }
    }
    train_ans = gold_std[x]
    test_data = data_frame[y,]
    test_ans = gold_std[y]
    fs1 = chaid_regress(data = train_data,gold_std = train_ans,max_height = maxheight,alpha_val = alpha_val)
    writeLines(paste("This is repeat:",i,"alpha value is:",alpha_val,"maxheight is",maxheight))
    predictions = predict(fs1$fs1,test_data)
    x = validity(predictions,test_ans)
    misclass = c(misclass,abs(x$results$estimate[5]-1))
    sens = c(sens,abs(x$results$estimate[1]))
    spec = c(spec,abs(x$results$estimate[2]))
    ppv = c(ppv,abs(x$results$estimate[3]))
    npv = c(npv,abs(x$results$estimate[4]))
    tp = c(tp,(abs(x$mtx[1,1])))
    fn = c(fn,(abs(x$mtx[2,1])))
    fp = c(fp,(abs(x$mtx[1,2])))
    tn = c(tn,(abs(x$mtx[2,2])))
    heights =c(heights,maxheight)
  }
  final = data.frame(parameter,misclass,sens,spec,ppv,npv,tp,fn,fp,tn,heights)
  return(final)
}
