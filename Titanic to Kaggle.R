#1. �ǂݍ���
train <- read.csv('train.csv')
test <- read.csv('test.csv')


#����́A'Name','Ticket','Cabin'�͕ϐ��Ƃ��čl���Ȃ��̂Ŕr��
train <- train[,-c(4,9,11)]
test <- test[,-c(3,8,10)]


#(�O����)
#2. 1)train�f�[�^��'Age','Embarked��'������⊮
train[is.na(train$Age), "Age"] <- mean(train$Age, na.rm=TRUE)
train[is.na(train$Embarked), "Embarked"] <- 'S'

# 2)test�f�[�^��'Age','Fare'�̌�����⊮
test[is.na(test$Age), "Age"] <- mean(test$Age, na.rm=TRUE)
test[is.na(test$Fare), "Fare"] <- mean(test$Fare, na.rm=TRUE)

#3. 'Pclass','Sex','Embarked'���_�~�[�ϐ��ɂ���
#Pclass
train$Pclass <- as.factor(train$Pclass)
test$Pclass <- as.factor(test$Pclass)
#Sex
train$Sex <- as.factor(train$Sex)
test$Sex <- as.factor(test$Sex)
#Embarked
train$Embarked <- as.factor(train$Embarked)
test$Embarked <- as.factor(test$Embarked)


#4.'Pclass', 'Sex', 'Embarked', 'Age', 'Fare', 'SibSp', 'Parch'��
#    �g���ĐV���ȕϐ���1�����쐬
train$Fare_div_Age <- train$Fare / train$Age
test$Fare_div_Age <- test$Fare / test$Age


#5. 4.�ō�����ϐ��ƁA'Pclass', 'Sex', 'Embarked', 'Age', 'Fare', 
#'SibSp', 'Parch'������ϐ��ASurvived��ړI�ϐ��ɂ��ă��W�X�e�B�b�N��A
#�̃��f�����쐬
my.glm <- glm(Survived~.,data=train[,-1],family=binomial)

#6.�쐬�������W�X�e�B�b�N��A�̉��߂���
summary(my.glm)
#�T�}���[����́A�L�Ӑ���0.05��菬����Pr(>|z|)�A#Pclass2�APclass3�A
#Sexmale�AAge�ASibSp�����ʐ�������ϐ��ƌ��ʂ��o���B
#Sexmale��age�Ɋւ��Ă͏��q�A�q���A�V�l�Ȃǂ���D��ɔ��{�[�g�֗D���
#������o�܂�������҂ւ̉e�����傫���Ƃ�������B
#SibSp�APclass2�APclass3�́A���ʊ֌W���݂��Ȃ��̂ŏd�v�ȗv���ł͂Ȃ��B

#�I�b�Y��
exp(my.glm$coefficients)
#�I�b�Y�䂩��͊m���ɉe���̂���P�܂��͂���ȏ�̕ϐ���age�AFare�AFare_div_Age��
#�~���̍ہA�q������D��ɔ��{�[�g�֏�����̂ł�����̓����ʂ͐��҂ւ̉e��������B

#7.�쐬�������W�X�e�B�b�N��A���f����p����test�f�[�^��Survived��\������

#���W�X�e�B�b�N��A���f�����g���ė\��
#�ȉ��ł́A�e��q�ɑ΂��Đ�������(Survived = 1�ł���j"�m��"���\�������
predict_result <- predict(my.glm, newdata = test[, -1], type='response')

#50%�ȏ�̊m���ł���ΐ����Ƃ��A�o�͂�'0'or'1'�ɂ���
predict_survived <- ifelse(predict_result>=0.5, 1, 0)
predict_survived

#8.Kaggle�ɒ�o
#��o�pcsv�t�@�C�����쐬�E�o�͂���֐�
create_submission_df <- function(predict){
  submission_df <- data.frame(PassengerId=test$PassengerId, Survived=predict_survived)
  write.csv(submission_df, 
            'titanic_predict_result.csv', row.names=FALSE)}
create_submission_df(predict_survived)  

#Sore = 0.76076