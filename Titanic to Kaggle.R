#1. 読み込み
train <- read.csv('train.csv')
test <- read.csv('test.csv')


#今回は、'Name','Ticket','Cabin'は変数として考えないので排除
train <- train[,-c(4,9,11)]
test <- test[,-c(3,8,10)]


#(前処理)
#2. 1)trainデータの'Age','Embarkedの'欠損を補完
train[is.na(train$Age), "Age"] <- mean(train$Age, na.rm=TRUE)
train[is.na(train$Embarked), "Embarked"] <- 'S'

# 2)testデータの'Age','Fare'の欠損を補完
test[is.na(test$Age), "Age"] <- mean(test$Age, na.rm=TRUE)
test[is.na(test$Fare), "Fare"] <- mean(test$Fare, na.rm=TRUE)

#3. 'Pclass','Sex','Embarked'をダミー変数にする
#Pclass
train$Pclass <- as.factor(train$Pclass)
test$Pclass <- as.factor(test$Pclass)
#Sex
train$Sex <- as.factor(train$Sex)
test$Sex <- as.factor(test$Sex)
#Embarked
train$Embarked <- as.factor(train$Embarked)
test$Embarked <- as.factor(test$Embarked)


#4.'Pclass', 'Sex', 'Embarked', 'Age', 'Fare', 'SibSp', 'Parch'を
#    使って新たな変数を1つだけ作成
train$Fare_div_Age <- train$Fare / train$Age
test$Fare_div_Age <- test$Fare / test$Age


#5. 4.で作った変数と、'Pclass', 'Sex', 'Embarked', 'Age', 'Fare', 
#'SibSp', 'Parch'を説明変数、Survivedを目的変数にしてロジスティック回帰
#のモデルを作成
my.glm <- glm(Survived~.,data=train[,-1],family=binomial)

#6.作成したロジスティック回帰の解釈する
summary(my.glm)
#サマリーからは、有意水準0.05より小さいPr(>|z|)、#Pclass2、Pclass3、
#Sexmale、Age、SibSpが効果性がある変数と結果が出た。
#Sexmaleとageに関しては女子、子供、老人などから優先に避難ボートへ優先に
#乗った経緯からも生還への影響が大きいとおもわれる。
#SibSp、Pclass2、Pclass3は、因果関係がみられないので重要な要因ではない。

#オッズ比
exp(my.glm$coefficients)
#オッズ比からは確率に影響のある１またはそれ以上の変数はage、Fare、Fare_div_Ageで
#救助の際、子供から優先に避難ボートへ乗ったのでこられらの特徴量は生還への影響がある。

#7.作成したロジスティック回帰モデルを用いてtestデータのSurvivedを予測する

#ロジスティック回帰モデルを使って予測
#以下では、各乗客に対して生存する(Survived = 1である）"確率"が予測される
predict_result <- predict(my.glm, newdata = test[, -1], type='response')

#50%以上の確率であれば生存とし、出力を'0'or'1'にする
predict_survived <- ifelse(predict_result>=0.5, 1, 0)
predict_survived

#8.Kaggleに提出
#提出用csvファイルを作成・出力する関数
create_submission_df <- function(predict){
  submission_df <- data.frame(PassengerId=test$PassengerId, Survived=predict_survived)
  write.csv(submission_df, 
            'titanic_predict_result.csv', row.names=FALSE)}
create_submission_df(predict_survived)  

#Sore = 0.76076