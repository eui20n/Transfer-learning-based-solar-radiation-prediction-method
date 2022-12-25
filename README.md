# Transfer-learning-based-solar-radiation-prediction-method
## 전이학습을 기반으로한 일사량 예측 모델
### preview
타겟으로 정한 지역의 일사량을 예측하는 모델<br>
타겟으로 정한 지역의 데이터가 부족하기 때문에 전이학습으로 해당 문제를 해결<br>
정확한 예측을 위해서 슬라이딩 윈도우 방식을 활용하여 하루(한 시점)씩 학습 및 예측을 진행<br>
일사량 예측한 값이 바로바로 나와야 하기때문에 속도가 굉장히 중요 => 속도가 빠른 Ranger모델 활용<br>

***
1. 타겟 지역
  - 대전
2. 사용한 모델
  - Random Forest => Ranger(속도가 빠른 Random Forest)
3. 그 외
  - 부족한 데이터를 해결 => 전이학습
  - 보다 정확한 예측 => 슬라이딩 윈도우
4. 데이터 구성
  - 학습 데이터
    - 타갯지역(대전)을 제외한 각 2년, 총 10년 + 대전의 1년 => 11년
  - 검증 데이터
    - 대전의 하루
  
# 전체 동작
<img src = "https://user-images.githubusercontent.com/74887218/209465709-fa635071-d22c-4669-bf1a-33c021cc8adb.png">
