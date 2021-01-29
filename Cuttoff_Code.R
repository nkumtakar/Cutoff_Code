## Function Name: Cutoff Code Reccomendation (Logistic Regression)

## Purpose: To recommend a cutoff value to classify Logistic Regression model predictions that will
## optimize the model's prediction power or net profit gain

## Net Profit = ((Profit parameter) * (TP + TN)) - ((Cost Parameter) *(FP + FP))

## Creation:
## Created by Neel S. Kumtakar, Graduate Student, Drexel University (Lebow), January 2021
## Derived from Matthew J. Schneider's code and methodology, Drexel Universiy PhD. 
## Decision Sciences and MIS, January 2020.



c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(scores);
}

Cutoff = function(Predictions, Test, Reward, Loss){


  
cat("For a correct classification (TP and TN), you will earn $", Reward, "\n")

cat("For an incorrect Classficiation, (FP and FN), you will lose $", Loss, "\n", "\n")

cat("Profit- Accuracy Analysis Options:", "\n", "\n")

i=1
k=0.01 ## cutt off value
tpr={}
fpr={}
accuracy={}
m={}
profit= {}

## run a while loop for cuttoff values 0.01 to 1

while( k <= 1){  
  classify=ifelse(Predictions> k,1,0) 
  acc=c_accuracy(Test[, ncol(Test)],classify)
  
  
  profit[i]= ((Reward) * (acc[7] + acc[8])) - (( Loss) * (acc[9] + acc[10]))  #care about net-profit only
  
  tpr[i] = acc[4]
  fpr[i]=acc[5]
  accuracy[i]= acc[3]
  m[i]=k
  
  
  
  k=k+0.01 ## increment the cuttoff value by 0.01
  
  i=i+1 ## i represents the index of the cutoff value. 
  
  
  
}

plot(m, accuracy, xlab="cutoff value", main = "Original Model: Cut off value vs Accuracy")

lines(m, accuracy, col="blue")


ideal_accuracy_cutoff = which.max(accuracy) * 0.01

ideal_profit_cutoff = which.max(profit) * 0.01

par(mfrow=c(1,2))

cat(" Your highest Accuracy is at: ",  round(max(accuracy),2) * 100 , "%",  "\n" )

cat("The corresponding profit amt is: $ ", profit[which.max(accuracy)], "\n")

cat(" and the Ideal Cuttoff value that yields that accuracy and profit values is " , ideal_accuracy_cutoff, "\n", "\n")


cat(" Highest profit is at $" ,  max(profit), "\n")

cat( "The corresponding accuracy is ", round(accuracy[which.max(profit)],2) * 100, "%", "\n")

cat(" and the Ideal Cuttoff  value that yields that Profit and Accuracy is " , ideal_profit_cutoff, "\n", "\n")



plot(m, accuracy, xlab="cutoff value", main = " Original Model: Cut off value vs Accuracy")

lines(m, accuracy, col="blue")


plot(m, profit, xlab="cutoff value", main = " Original Model: Cut off value vs Profit")

lines(m, profit, col="red")



if(ideal_accuracy_cutoff  == ideal_profit_cutoff) {
  
  
  cat( "\n",  "It is recommended that you go with: ", ideal_profit_cutoff, " as the cuttoff value ", "\n", "\n")
  
  
  cat(' Below are the classification metrics of your logistic model')
  
  cat("\n")
  
  c_accuracy(Test[, ncol(Test)], ifelse(Predictions> ideal_profit_cutoff,1,0))
  
  
  
  
  
  
}

else if (ideal_accuracy_cutoff > ideal_profit_cutoff){

  
  cat("\n", " If you value accuracy, then it is recommended that you go with ", ideal_accuracy_cutoff, " as the cuttoff value ", "\n", "\n")
  
  cat()
  
  
  cat('Below are the classification results of your logistic model with relation to the reccomended cuttoff value ')
  
  cat("\n")
  
  c_accuracy(Test[, ncol(Test)] , ifelse(Predictions> ideal_accuracy_cutoff,1,0))
  
  
  
  
  
}

else{
  
  cat("\n")
  
  cat(" If you value profit, it is recommended that you go with ", ideal_profit_cutoff, " as the cuttoff value ", "\n", "\n")
  
  
  cat('Below are the classficiation results of your logistic model with relation to the cuttoff value chosen')
  
  
  cat("\n")
  
  
  c_accuracy(Test[, ncol(Test)], ifelse(Predictions> ideal_profit_cutoff,1,0))
  
  
  
}


}