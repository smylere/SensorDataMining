# data_scale.R 코드에 모두 병합, 현 시점에서는 사용되지 않음

# 170812 서버 연동을 위해 sax_process 함수 수정
# 170811 최초 작성
# ex) stop_walking(tmp,10,0.8)
# tmp데이터를 10초 주기로 볼 때 8초 이상 nwk 검출 조건
# tmp 데이터의 경우 scale_decompose() 함수 산출물
# tmp = which(ctrl_kjs$decomposed_data$trend <= min(ctrl_kjs$kmeans$V1))

library(jmotif)

source("data_scale.R")

# 센서데이터 중 운동량이 측정되지 않는 부분을 10초 단위로 역산하여
# 운동량이 급격하게 변한 시점 추출
stop_walking = function(data,seq,rate,log=FALSE){

  for(i in 1:(length((data)-seq))) {

    tmp = as.integer(data[i:(i+(seq-1))])

    check = as.integer(seq(from = data[i],length.out=10, by = 1))

    if(log ==TRUE){

      # sprintf("tmp : %i",tmp)
      # sprintf("check : %i",check)
      print("tmp")
      print(tmp)
      print("check")
      print(check)

      }

    if(sum(tmp == check)>= (round(seq*rate))) {

      return(i)

      }
  }

  rm(tmp,check)

}


#  data_scale에서 처리된 시계열 타입의 데이터를 paa알고리즘을 통해 패턴화
sax_process = function(data) {

  tmp = which(data$decomposed_data$trend <= min(data$kmeans$V1))

  paa_data = data$decomposed_data$trend
  stop_point = stop_walking(tmp,10,0.8) # stop_walking 적용을 위한 임시값(상수)
  rawdata = paa_data[6:tmp[stop_point]]

  paa_result = paa(paa_data[6:tmp[stop_point]],20)
  sax_result = series_to_chars(paa_result,10)

  return(list(paa=paa_result,sax=sax_result,rawdata = rawdata))
}
