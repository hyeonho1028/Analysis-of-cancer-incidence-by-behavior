# Analysis of cancer incidence by behavior


## 1 데이터 이해
### 1. codebook
1. 컬럼에 대한 설명
2. 건강관련 위험 행동
3. 만성 건강 상태 및 예방 서비스 사용
4. 미국 거주자에 대한 주 데이터를 수집하는 건강 관련 전화 조사 시스템, 설문조사 시스템
5. 11년 ~ 16년 사이의 Data가 존재
6. 암은 흡연, 음주 등의 다양한 요인에 의한 영향으로 발병하는 복합적인 행동적 과정을 거치는데, 이 과정을 해석해 보고자 한다.

### 2. EDA와 시각화를 통한 이해
1. R의 ggplot2 패키지를 사용하여 visualization을 하였다.
2. 범주형 데이터가 주를 이뤘고, 연속형 데이터의 경우 6개의 컬럼이 있었다.
3. y가 0과 1의 비율이 큰 차이가 났다.(1의 비율이 암이 걸린 사람인데 매우 낮은 것을 보임)

## 2 데이터 전처리
### 1. missing value
1. 95%이상의 데이터를 가지고 있는 컬럼은 제거 하였다.
2. 상관관계가 너무 높아 다중공선성을 유발하거나, 해석의 여지를 낮추는 컬럼도 제거하였다.

### 2. outlier
1. 응답을 거부하거나, 설문을 아예 시도 하지 않은 인원들에 대한 outlier가 존재하는데 이 때, 그 자체로 의미가 있다고 판단하여 동일 범주로 묶어주었다.
2. 동질성 검정과 독립성 검정을 시도하여 이상값에 대해 한번 더 검증을 하였다.(범주형 변수만 시도)

## 3 데이터 모델링
### 1. 사용한 모델
1. random forest
2. logistic regression
3. neural network
4. C4.5
5. CART
6. CHAID
7. Ctree

### 2. 변수의 중요도 파악
1. 너무 많은 컬럼으로 인해 각 모델의 결과로 중요변수들을 선택한다.
2. 파악된 중요변수들로 다시한번 모델을 선정한다.
3. 결과를 해석한다.

### 3. 조합된 모델
#### 1. CHAID
1. logistic regression
2. C4.5
3. randomforest
4. neural network
5. logistic + randomforest

#### 2. CART
1. logistic regression
2. C4.5
3. randomforest
4. neural network
5. logistic + randomforest

#### 3. CTREE
1. logistic regression
2. C4.5
3. randomforest
4. neural network
5. logistic + randomforest

## 4 모델링 평가
### 1. ROC curve(AUC)를 이용한 모델 평가
1. logistic regression + CHAID
2. C4.5 + CHAID
3. no selection + CART
4. logistic regression + randomforest + CART
5. no selection + CTREE
6. C4.5 + CTREE
7. randomforest + CTREE

##  4 결론

