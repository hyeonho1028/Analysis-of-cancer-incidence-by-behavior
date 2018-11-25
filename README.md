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
3. 차원이 너무 크기 때문에, 차원축소가 필요함을 느꼈다. 
4. 차원축소의 논문을 여러개 찾아보니, 불균형 데이터에 대해 랜덤포레스트를 이용한 변수선택방법이 bagging, bumping을 이용한 변수선택법보다 개선된 점들을 볼 수 있었다. 물론 일반화는 위험하므로, 추가적인 방법론을 사용하여 모델을 사용하여 변수를 선택하였다.
5. 이런 방법은 해석이 불가하다는 점과 속도가 느린 단점이 있으나, 높은 예측력을 가진 모델을 개발할 수 있으므로 목적에 따라 변수 선택 및 모델을 선택하면 된다고 판단했다.

## 3 데이터 모델링
### 1. 사용한 모델
1. logistic regression : 로지스틱 회귀모형은 목표변수가 두 집단 또는 그 이상의 집단으로 나누어져 있는 경우에 개별 관측치들을 분류하고 예측하는 데 사용되는 대표적인 분류기법이다. 이 모형의 특징은 선형회귀모형과 유사하여 회귀계수와 오즈비를 이용한 해석이 용이하다는 점과 필요하지 않은 변수는 제거하고 꼭 필요한 변수만을 선택함으로서 모형의 예측력과 해석력을 높일 수 있다.
2. C4.5 : 이것은 1993년 Quinlan에 의해 제안된 방법으로써 기본적인 개념은 트리기반의 분류 알고리즘인 ID3와 동일하지만, ID3의 단점인 연속형 속성처리, 결측치 처리 등이 보완된 방법이다. CART(이진분류만을 지원함)와는 다르게 가지의 수를 다양화 할 수 있다. 연속형 변수에 대해서는 2개 범주로 이진분리를 하지만, 범주형의 경우 변수가 갖는 범주 개수만큼 분리를 수행한다. 이것의 단점은 지나치게 단순화되는 상황으로 문제가 될 수 있다. 분리기준은 information gain ratio(정보이익비율)이며, 감소되는 엔트로피 양을 뜻한다. $Info(S) = -\sum_{i=1}^n$
3. randomforest
4. neural network
5. CART
6. CHAID
7. CTREE

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

