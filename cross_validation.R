
forward_stepwise_crossfold = function(data,gold_std,k){
  fold = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  fold = sample(fold)
  predictions = c()
  answers = c()
  for(i in 1:10){
    if(exists("finalframe")){
      rm(finalframe)
    }
    if(exists("finalframe_2")){
      rm(finalframe_2)
    }
    train_data = data[which(fold!=i),]
    index = c()
    for(j in 1:ncol(train_data)){
      if(length(unique(train_data[,j]))<2){
        index = c(index,j)
      }
    }
    if(length(index)>0){
      train_data = train_data[,-index]
    }
    train_ans = gold_std[which(fold!=i)]
    test_data = data[which(fold==i),]
    test_ans = gold_std[which(fold==i)]
    fs1 = forward_stepwise(train_data,gold_std = train_ans,k_val = k)

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
    if(!exists("finalframe") & exists("finalframe_2")){
      vote_frame = finalframe_2
    }
    if(ncol(vote_frame)>1){
      predict = rowSums(vote_frame)
    }
    if(ncol(vote_frame)==1){
      predict = vote_frame[,1]
    }
    predict = ifelse(predict>=1,1,0)

    predictions = c(predictions,predict)
    answers = c(answers,as.numeric(test_ans))
  }
  returnframe = data.frame(predictions,answers)
  validity(returnframe$predictions,returnframe$answers)
  return(returnframe)
}


lasso_crossfold = function(data,gold_std,lambda){
  fold = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  fold = sample(fold)
  predictions = c()
  answers = c()


  for(i in 1:10){
    if(exists("finalframe")){
      rm(finalframe)
    }
    if(exists("finalframe_2")){
      rm(finalframe_2)
    }
    train_data = data[which(fold!=i),]
    index = c()
    for(j in 1:ncol(train_data)){
      if(length(unique(train_data[,j]))<2){
        index = c(index,j)
      }
    }
    if(length(index)>0){
      train_data = train_data[,-index]
    }
    train_ans = gold_std[which(fold!=i)]
    test_data = data[which(fold==i),]
    test_ans = gold_std[which(fold==i)]
    fs1 = lasso_regression(train_data,gold_std = train_ans,lambda_val = lambda)

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
      vote_frame <<- cbind(finalframe,finalframe_2)
    }
    if(exists("finalframe") & !exists("finalframe_2")){
      vote_frame <<- finalframe
    }
    if(!exists("finalframe") & exists("finalframe_2")){
      vote_frame <<- finalframe_2
    }
    if(ncol(vote_frame)>1){
      predict = rowSums(vote_frame)
    }
    if(ncol(vote_frame)==1){
      predict = vote_frame[,1]
    }
    predict = ifelse(predict>=1,1,0)
    writeLines(paste("Length of predictions:",length(predict),"Length of answers",length(as.numeric(test_ans)),"Vote frame length",nrow(vote_frame)))
    predictions = c(predictions,predict)
    answers = c(answers,as.numeric(test_ans))
  }
  returnframe = data.frame(predictions,answers)
  validity(returnframe$predictions,returnframe$answers)
  return(returnframe)
}

rpart_crossfold = function(data,gold_std,complexity,loss_matrix){
  fold = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  fold = sample(fold)
  predictions = c()
  answers = c()

  for(i in 1:10){
    train_data = data[which(fold!=i),]
    index = c()
    for(j in 1:ncol(train_data)){
      if(length(unique(train_data[,j]))<2){
        index = c(index,j)
      }
    }
    if(length(index)>0){
      train_data = train_data[,-index]
    }
    train_ans = gold_std[which(fold!=i)]
    test_data = data[which(fold==i),]
    test_ans = gold_std[which(fold==i)]
    fs1 = rpart_regress(train_data,gold_std = train_ans,complexity = complexity,loss_matrix = loss_matrix)$fs1
    predictions = c(predictions,as.numeric(as.character(predict(fs1,test_data,type = "class"))))
    answers = c(answers,as.numeric(test_ans))
  }
  returnframe = data.frame(predictions,answers)
  validity(returnframe$predictions,returnframe$answers)
  return(returnframe)
}

c50_crossfold = function(data,gold_std,complexity,loss_matrix,winnow,rules){
  fold = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  fold = sample(fold)
  predictions = c()
  answers = c()

  for(i in 1:10){
    train_data = data[which(fold!=i),]
    index = c()
    for(j in 1:ncol(train_data)){
      if(length(unique(train_data[,j]))<2){
        index = c(index,j)
      }
    }
    if(length(index)>0){
      train_data = train_data[,-index]
    }
    train_ans = gold_std[which(fold!=i)]
    test_data = data[which(fold==i),]
    test_ans = gold_std[which(fold==i)]
    fs1 = c50_regress(train_data,gold_std = train_ans,complexity = complexity,loss_matrix = loss_matrix,winnow = winnow,rules = rules)$fs1
    predictions = c(predictions,as.numeric(as.character(predict(fs1,test_data,type = "class"))))
    answers = c(answers,as.numeric(test_ans))
  }
  returnframe = data.frame(predictions,answers)
  validity(returnframe$predictions,returnframe$answers)
  return(returnframe)
}

chaid_crossfold = function(data,gold_std,alpha_val,max_height){
  fold = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  fold = sample(fold)
  predictions = c()
  answers = c()

  for(i in 1:10){
    train_data = data[which(fold!=i),]
    index = c()
    for(j in 1:ncol(train_data)){
      if(length(unique(train_data[,j]))<2){
        index = c(index,j)
      }
    }
    if(length(index)>0){
      train_data = train_data[,-index]
    }
    train_ans = gold_std[which(fold!=i)]
    test_data = data[which(fold==i),]
    test_ans = gold_std[which(fold==i)]
    fs1 = chaid_regress(train_data,gold_std = train_ans,alpha_val = alpha_val,max_height = max_height)$fs1
    predictions = c(predictions,as.numeric(as.character(predict(fs1,test_data,type = "response"))))
    answers = c(answers,as.numeric(test_ans))
  }
  returnframe = data.frame(predictions,answers)
  validity(returnframe$predictions,returnframe$answers)
  return(returnframe)
}
