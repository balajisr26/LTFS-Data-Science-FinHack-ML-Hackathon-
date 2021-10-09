gc();
library(data.table)
library(lightgbm)
library(ModelMetrics)
library(lubridate)
library(stringr)
train=fread("train.csv")
test<-fread('test_bqCt9Pv.csv')
train$ind<-1
test$ind<-2

train_test<-rbind(train,test,fill=T)

train_test$AVERAGE.ACCT.AGE<-str_replace_all(train_test$AVERAGE.ACCT.AGE, "[a-z]", '')
train_test$year<-as.numeric(substr(train_test$AVERAGE.ACCT.AGE,1,1))
train_test$month<-as.numeric(substr(train_test$AVERAGE.ACCT.AGE,3,nchar(train_test$AVERAGE.ACCT.AGE)))
train_test$acct_duration<-train_test$year*12+train_test$month
train_test$year<-NULL
train_test$month<-NULL
train_test$AVERAGE.ACCT.AGE<-NULL

train_test$CREDIT.HISTORY.LENGTH<-str_replace_all(train_test$CREDIT.HISTORY.LENGTH, "[a-z]", '')
train_test$year<-as.numeric(substr(train_test$CREDIT.HISTORY.LENGTH,1,1))
train_test$month<-as.numeric(substr(train_test$CREDIT.HISTORY.LENGTH,3,nchar(train_test$CREDIT.HISTORY.LENGTH)))
train_test$credit_duration<-train_test$year*12+train_test$month
train_test$year<-NULL
train_test$month<-NULL
train_test$CREDIT.HISTORY.LENGTH<-NULL



#train_test$emp_supplier<-(paste0(train_test$Employee_code_ID,'-',train_test$supplier_id))
str(train_test)

state_stats<-train_test[,list(state_min_ltv=min(ltv),state_max_ltv=max(ltv)
                              ,state_mean_ltv=mean(ltv)
                              ,state_min_disb=min(disbursed_amount),state_max_disb=max(disbursed_amount)
                              ,state_mean_disb=mean(disbursed_amount),state_n=.N
                              ,state_ticketsize=sum(disbursed_amount)/.N),by='State_ID']
train_test<-merge(train_test,state_stats,by='State_ID',all.x = T)

branch_stats<-train_test[,list(branch_min_ltv=min(ltv),branch_max_ltv=max(ltv)
                               ,branch_mean_ltv=mean(ltv)
                               ,branch_min_disb=min(disbursed_amount),branch_max_disb=max(disbursed_amount)
                               ,branch_mean_disb=mean(disbursed_amount),branch_n=.N
                               ,branch_ticketsize=sum(disbursed_amount)/.N),by='branch_id']
train_test<-merge(train_test,branch_stats,by='branch_id',all.x = T)

supplier_stats<-train_test[,list(supplier_min_ltv=min(ltv),supplier_max_ltv=max(ltv)
                                 ,supplier_mean_ltv=mean(ltv)
                                 ,supplier_min_disb=min(disbursed_amount),supplier_max_disb=max(disbursed_amount)
                                 ,supplier_mean_disb=mean(disbursed_amount),supplier_n=.N
                                 ,supplier_ticketsize=sum(disbursed_amount)/.N),by='supplier_id']
train_test<-merge(train_test,supplier_stats,by='supplier_id',all.x = T)

cns_stats<-train_test[,list(cns_min_ltv=min(ltv),cns_max_ltv=max(ltv)
                            ,cns_mean_ltv=mean(ltv)
                            ,cns_min_disb=min(disbursed_amount),cns_max_disb=max(disbursed_amount)
                            ,cns_mean_disb=mean(disbursed_amount),cns_n=.N
                            ,cns_ticketsize=sum(disbursed_amount)/.N),by='PERFORM_CNS.SCORE.DESCRIPTION']
train_test<-merge(train_test,cns_stats,by='PERFORM_CNS.SCORE.DESCRIPTION',all.x = T)


#manufacturer_ltv<-train_test[,list(manufacturer_min_ltv=min(ltv),manufacturer_max_ltv=max(ltv)
#                          ,manufacturer_mean_ltv=mean(ltv)),by='manufacturer_id']
#train_test<-merge(train_test,manufacturer_ltv,by='manufacturer_id',all.x = T)

#manufacturer_disb<-train_test[,list(manufacturer_min_disb=min(disbursed_amount),manufacturer_max_disb=max(disbursed_amount)
#                           ,manufacturer_mean_disb=mean(disbursed_amount)),by='manufacturer_id']
#train_test<-merge(train_test,manufacturer_disb,by='manufacturer_id',all.x = T)

emp_stats<-train_test[,list(emp_min_ltv=min(ltv),emp_max_ltv=max(ltv)
                            ,emp_mean_ltv=mean(ltv)
                            ,emp_min_disb=min(disbursed_amount),emp_max_disb=max(disbursed_amount)
                            ,emp_mean_disb=mean(disbursed_amount),emp_n=.N
                            ,emp_ticketsize=sum(disbursed_amount)/.N),by='Employee_code_ID']
train_test<-merge(train_test,emp_stats,by='Employee_code_ID',all.x = T)



train_test$dob<-dmy(train_test$Date.of.Birth)
train_test$dob<-ifelse(train_test$dob>Sys.Date(),format(train_test$dob, "19%y-%m-%d"),format(train_test$dob))
train_test$dob<-ymd(train_test$dob)


train_test$disbdate<-dmy(train_test$DisbursalDate)
train_test$disbdate<-ifelse(train_test$disbdate>Sys.Date(),format(train_test$disbdate, "19%y-%m-%d"),format(train_test$disbdate))
train_test$disbdate<-ymd(train_test$disbdate)

train_test$age<-as.integer(difftime(Sys.Date(),train_test$dob,units = 'days'))/365



train_test$disb_inst<-train_test$PRI.DISBURSED.AMOUNT/train_test$PRIMARY.INSTAL.AMT
train_test$disb_balance<-train_test$PRI.DISBURSED.AMOUNT/train_test$PRI.CURRENT.BALANCE


train_test$ltv_stats<-as.numeric(train_test$ltv/max(train_test$ltv))

train_test$disb_stats<-as.numeric(train_test$disbursed_amount/min(train_test$disbursed_amount))


train_test$PRI.ACTIVE.ACCTS_No_accts<-train_test$PRI.ACTIVE.ACCTS/train_test$PRI.NO.OF.ACCTS
train_test$PRI.ACTIVE.ACCTS_Overdue<-train_test$PRI.ACTIVE.ACCTS/train_test$PRI.OVERDUE.ACCTS
train_test$pri.cur_balance_active<-train_test$PRI.CURRENT.BALANCE/train_test$PRI.ACTIVE.ACCTS
train_test$pri.active_new<-train_test$PRI.ACTIVE.ACCTS/train_test$NEW.ACCTS.IN.LAST.SIX.MONTHS
train_test$pri.active_delinquent<-train_test$PRI.ACTIVE.ACCTS/train_test$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS
train_test$pri.disb_cur_balance<-train_test$PRI.CURRENT.BALANCE/train_test$PRI.DISBURSED.AMOUNT
train_test$overdue_delinq<-train_test$PRI.OVERDUE.ACCTS+train_test$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS
train_test$delinq_enquiry<-ifelse(train_test$NO.OF_INQUIRIES>1 & train_test$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS > 1,1,0)
#train_test$pri.disb_cur_balance_disb<-train_test$PRI.CURRENT.BALANCE/train_test$disbursed_amount
#train_test$disb_pri_disb<-train_test$PRI.DISBURSED.AMOUNT/train_test

train_test$total_disb<-train_test$PRI.DISBURSED.AMOUNT+train_test$SEC.DISBURSED.AMOUNT+train_test$disbursed_amount

train_test$total_overdue<-train_test$PRI.OVERDUE.ACCTS+train_test$SEC.OVERDUE.ACCTS

train_test$active_total<-(train_test$PRI.ACTIVE.ACCTS+train_test$SEC.ACTIVE.ACCTS)/(train_test$PRI.ACTIVE.ACCTS+train_test$SEC.ACTIVE.ACCTS+train_test$PRI.NO.OF.ACCTS+train_test$SEC.NO.OF.ACCTS)

train_test$total_inst<-train_test$PRIMARY.INSTAL.AMT+train_test$SEC.INSTAL.AMT


for (f in names(train_test)) {
  if (class(train_test[[f]])=="character") {
    #cat("VARIABLE : ",f,"\n")
    levels <- sort(unique(train_test[[f]]))
    train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
  }
}




train<-subset(train_test,ind==1)
test<-subset(train_test,ind==2)



lgb_params = list(
#  bagging_freq= 5,
#  bagging_fraction= 0.7,
  boost_from_average=F,
  feature_fraction= 0.7,
  learning_rate=0.05,
  max_depth= -1,
  metric='auc',
  min_data_in_leaf= 100,
  min_sum_hessian_in_leaf= 500,
  num_leaves=20,
  num_threads= 4,
  tree_learner= 'serial',
  objective='binary',
  #max_bin=1024
  boosting='dart'
)




library(caret)
set.seed(42443)
predtotal<-0
nfold<-5
flds<-createFolds(train$loan_default,k=nfold)
mat<-matrix(nrow=nrow(train),ncol=2)

features<-c('branch_id','loan_default','NO.OF_INQUIRIES','CREDIT.HISTORY.LENGTH'
            ,'AVERAGE.ACCT.AGE','DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS'
            ,'NEW.ACCTS.IN.LAST.SIX.MONTHS','PRIMARY.INSTAL.AMT','PRI.DISBURSED.AMOUNT'
            ,'PRI.SANCTIONED.AMOUNT','PRI.CURRENT.BALANCE','PRI.OVERDUE.ACCTS'
            ,'PRI.ACTIVE.ACCTS','PRI.NO.OF.ACCTS','PERFORM_CNS.SCORE.DESCRIPTION'
            ,'PERFORM_CNS.SCORE','Employee_code_ID','Employment.Type','manufacturer_id'
            ,'supplier_id','SEC.INSTAL.AMT','SEC.OVERDUE.ACCTS'
)

features<-setdiff(names(train),c('loan_default','UniqueID','ind','dob'
                                 ,'disbdate'))






for(i in 1:nfold)
{
  trainfld<-train[-flds[[i]]]
  valfld<-train[flds[[i]]]
  
  dtrain<-lgb.Dataset(as.matrix(trainfld[,features,with=FALSE]),label=trainfld$loan_default
  )
  
  dval<-lgb.Dataset(as.matrix(valfld[,features,with=FALSE]),label=valfld$loan_default)
  
  watchlist<-list(train=dtrain,val=dval)
  
  gbdt = lgb.train(lgb_params,data=dtrain,watchlist,nrounds=50000,nthread=4,
                   early_stopping_rounds = 200,eval_freq = 100)
  predval<-predict(gbdt,as.matrix(valfld[,features,with=FALSE]),num_iteration = gbdt$best_iter)
  predtest<-predict(gbdt,as.matrix(test[,features,with=FALSE]),num_iteration = gbdt$best_iter*1.1)
  predtotal<-predtest+predtotal
  print('AUC Score')
  print(auc(valfld$loan_default,predval))
  mat[flds[[i]],1]<-valfld$UniqueID
  mat[flds[[i]],2]<-predval
}
test$crossvalpred<-predtotal/nfold


train$UniqueID<-mat[,1]
train$crossvalpred<-as.numeric(mat[,2])

print('Train AUC Score')
print(auc(train$loan_default,train$crossvalpred))#0.6756908



train$crossvalpred<-(train$crossvalpred-min(train$crossvalpred))/(max(train$crossvalpred) - min(train$crossvalpred))
print(auc(train$loan_default,train$crossvalpred))

test$loan_default<-(test$crossvalpred-min(test$crossvalpred))/(max(test$crossvalpred) - min(test$crossvalpred))

testsub<-data.frame(UniqueID=test$UniqueID,loan_default=test$loan_default)
fwrite(testsub,'test_lgb2_6.csv')

summary(testsub)

trainsub<-data.frame(UniqueID=train$UniqueID,loan_default=train$crossvalpred)
fwrite(trainsub,'train_lgb2_6.csv')

summary(trainsub)
