# P80 Exercises

# Q1; Answer: 0.3007746
# Q2; Answer: 6.8%
# Q3; Answer: A)
# Q4; Answer: A)
# Q5; Answer: TRUE
# Q6; Answer: TRUE
# Q7; Answer: D)

# �ȉ��A�𓚂𓱂��o�������Ƃ��Ďg�p����R�\�[�X�R�[�h���L�ڂ���
# Q1
set.seed(1); 
dat<-rnorm(5,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������5����������
dat
mean(dat)
sd(dat)  # �s�Ε��U�ɂ��W�{�W���΍�
sqrt(5)*mean(dat)/sd(dat)  # Answer: 0.3007746

# Q2
set.seed(1); 
B=1000  # ���s��
n=5  # 1���s������̗����̔�����
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
hist(montecarlo)
sum(montecarlo>2)*100/B  # Answer: 6.8%

# Q3
# n�𑝂₵�ăV�~�����[�V�������J��Ԃ��Ă���
set.seed(1); 
B=1000  # ���s��
n=5  # 1���s������̗����̔�����(5�̂Ƃ�)
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
sum(montecarlo>2)/B  # �V�~�����[�V��������
1-pt(2,df=n-1)  # ���_�l

set.seed(1); 
B=1000  # ���s��
n=10  # 1���s������̗����̔�����(10�̂Ƃ�)
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
sum(montecarlo>2)/B  # �V�~�����[�V��������
1-pt(2,df=n-1)  # ���_�l

set.seed(1); 
B=1000  # ���s��
n=100  # 1���s������̗����̔�����(100�̂Ƃ�)
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
sum(montecarlo>2)/B  # �V�~�����[�V��������
1-pt(2,df=n-1)  # ���_�l

set.seed(1); 
B=1000  # ���s��
n=1000  # 1���s������̗����̔�����(1000�̂Ƃ�)
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
sum(montecarlo>2)/B  # �V�~�����[�V��������
1-pt(2,df=n-1)  # ���_�l

set.seed(1); 
B=1000  # ���s��
n=10000  # 1���s������̗����̔�����(10000�̂Ƃ�)
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
sum(montecarlo>2)/B  # �V�~�����[�V��������
1-pt(2,df=n-1)  # ���_�l
# Answer: A)

# Q4
# n�𑝂₵�ăV�~�����[�V�������J��Ԃ��Ă���
B=1000  # ���s��
n=5  # 1���s������̗����̔�����(5�̂Ƃ�)
montecarlo1 <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat1<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo1 [i] <- sqrt(n)*mean(dat1)/sd(dat1)}
montecarlo2 <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat2<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo2 [i] <- sqrt(n)*mean(dat2)/sd(dat2)}
DF<-2*n-2  # ���R�x
DF
res <- t.test(dat1, dat2, var.equal=TRUE)
res

B=1000  # ���s��
n=10  # 1���s������̗����̔�����(10�̂Ƃ�)
montecarlo1 <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat1<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo1 [i] <- sqrt(n)*mean(dat1)/sd(dat1)}
montecarlo2 <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat2<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo2 [i] <- sqrt(n)*mean(dat2)/sd(dat2)}
DF<-2*n-2  # ���R�x
DF
res <- t.test(dat1, dat2, var.equal=TRUE)
res

B=1000  # ���s��
n=100  # 1���s������̗����̔�����(100�̂Ƃ�)
montecarlo1 <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat1<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo1 [i] <- sqrt(n)*mean(dat1)/sd(dat1)}
montecarlo2 <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat2<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo2 [i] <- sqrt(n)*mean(dat2)/sd(dat2)}
DF<-2*n-2  # ���R�x
DF
res <- t.test(dat1, dat2, var.equal=TRUE)
res

B=1000  # ���s��
n=1000  # 1���s������̗����̔�����(1000�̂Ƃ�)
montecarlo1 <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat1<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo1 [i] <- sqrt(n)*mean(dat1)/sd(dat1)}
montecarlo2 <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat2<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n���������� 
montecarlo2 [i] <- sqrt(n)*mean(dat2)/sd(dat2)}
DF<-2*n-2  # ���R�x
DF
res <- t.test(dat1, dat2, var.equal=TRUE)
res

B=1000  # ���s��
n=10000  # 1���s������̗����̔�����(10000�̂Ƃ�)
montecarlo1 <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat1<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo1 [i] <- sqrt(n)*mean(dat1)/sd(dat1)}
montecarlo2 <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat2<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo2 [i] <- sqrt(n)*mean(dat2)/sd(dat2)}
DF<-2*n-2  # ���R�x
DF
res <- t.test(dat1, dat2, var.equal=TRUE)
res
# Answer: A)

#Q5
X=rbinom(n=15,size=1,prob=0.5)
tstat <- sqrt(15)*mean(X) / sd(X)
tstat
mean(X)
sd(X)
res <- t.test(X, mu=0.5)
res
# �V�~�����[�V�������J��Ԃ��ƁA���R�x14�ƂȂ�A�T��p>0.05�̒l���o��
# Answer: TRUE

#Q6
X=rbinom(n=500,size=1,prob=0.5)
tstat <- sqrt(500)*mean(X) / sd(X)
tstat
mean(X)
sd(X)
res <- t.test(X, mu=0.5)
res
# �V�~�����[�V�������J��Ԃ��ƁA���R�x499�ƂȂ�A�قڊm����p>0.05�̒l���o��
# Answer: TRUE

# Q7
# n�𑝂₵�ăV�~�����[�V�������J��Ԃ��Ă���
B=1000  # ���s��
n=5  # 1���s������̗����̔�����(5�̂Ƃ�)
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat1<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
mean(dat)
SE<- 1/sqrt(n)
SE
sd(dat)

B=1000  # ���s��
n=10  # 1���s������̗����̔�����(10�̂Ƃ�)
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat1<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
mean(dat)
SE<- 1/sqrt(n)
SE
sd(dat)

B=1000  # ���s��
n=100  # 1���s������̗����̔�����(100�̂Ƃ�)
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat1<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
mean(dat)
SE<- 1/sqrt(n)
SE
sd(dat)

B=1000  # ���s��
n=1000  # 1���s������̗����̔�����(1000�̂Ƃ�)
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat1<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
mean(dat)
SE<- 1/sqrt(n)
SE
sd(dat)

B=1000  # ���s��
n=10000  # 1���s������̗����̔�����(10000�̂Ƃ�)
montecarlo <- rep(0, B)  # �l�̔������
for(i in 1:B){  # ���s��B��J��Ԃ�
dat1<-rnorm(n,mean=0,sd=1)  # �W�����K���zN�`(0,1)�]��������n����������
montecarlo [i] <- sqrt(n)*mean(dat)/sd(dat)}
mean(dat)
SE<- 1/sqrt(n)
SE
sd(dat)
# ���s�񐔂𑝂₵�Ă����SE > sd�ƂȂ�
# Answer: D)