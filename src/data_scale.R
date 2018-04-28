# 센서데이터 기반 척추관협착증 판단모델 구축
# 센서 측정 장비를 통해 측정된 착용자의 운동량 변위 데이터를 바탕으로
# 운동 강도 수준, 걷기 / 쉼 횟수 측정
# 정상인과 환자간 센서 데이터상 페턴 관측 목적으로 코드화


library(jmotif)

# 최초 장비에서 측정되는 데이터 속성 중  x,y,z 초당 변위값 및 백터 내적값만 추출
# 장비간 차이로 인해 불필요한 데이터가 섞일 경우를 대비해 예외 처리 추가
scale_decompose = function(data,col,freq){

    if(length(data) != 12) {data = data[,c(3:6,13)]}
    else {data = data[,c(3:6,12)]}

    # data = scale(data[,c(3:6,12)])
    # data = data[,c(3:6,12)]
    # data = data[,c("Axis1","Axis2","Axis3","Steps","Vector Magnitude")]
    # epoch = round(length(data[,1])*freq) # frequency 값 정의 0.01 기준

    epoch = freq

    out = (decompose(ts(data[,col],frequency = epoch)))

    # print(head(out))
    # tmp = out$x
    # tmp = out$random

    tmp = out$trend

    tmp[is.na(tmp)] = 0 # NA 값은 0으로 치환 ; 전체 데이터 수준에서 극히 작은값

    k = 2

    while(k <= 100){

        km_out = kmeans(tmp,k,iter.max = 1000)

        if( km_out$betweenss / km_out$totss <= 0.95 ){

            k =k +1
            # print(km_out$betweenss / km_out$totss)
        }

        else{
            # sprintf("K means Value is %i", k)
            break
        }
    }

    cluster = as.data.frame(km_out$centers)

    for (i in 1:length(cluster[,1])){

        min_center = min(km_out$centers)

        if(cluster[i,1] == min_center){ cluster[i,2] = "nwk" }
        else{ cluster[i,2] = "wlk" }
    }

    notwalking_position = which(cluster$V2 == "nwk")

    cluster_vector = km_out$cluster

    for (i in 1:length(cluster_vector)){

        if(cluster_vector[i] == notwalking_position){ cluster_vector[i] = "nwk" }
        else { cluster_vector[i] ="wlk" }
    }

    result = vector()

    for (i in seq(from = 1, to = length(data[,1])-epoch, by = epoch )) {

        tmp = cluster_vector[i:(i+epoch-1)]
        tmp_sum = (sum(tmp == "nwk") / sum(tmp == "wlk"))

        if(tmp_sum >= 1) { result = append(result,"nwk") }
        else { result = append(result,"wlk") }

        tmp = 0

    }

    plotting_cluster = cluster_vector
    plotting_cluster = gsub("nwk",1,plotting_cluster)
    plotting_cluster = gsub("wlk",2,plotting_cluster)
    plotting_cluster = as.numeric(plotting_cluster)

    plotting_cluster2 = result #최초 데이터에 대해 최종적으로 스케일 조정된 변수
    plotting_cluster2 = gsub("nwk",1,plotting_cluster2) # 라벨 변경
    plotting_cluster2 = gsub("wlk",2,plotting_cluster2) # 라벨 변경
    plotting_cluster2 = as.numeric(plotting_cluster2) # 데이터 타입 변경

    cnt_result = (as.data.frame(table(result))[1,2]) / (sum(as.data.frame(table(result))[,2]))

    return(list(decomposed_data = out,
                kmeans = cluster,
                reclustering_rawdata = cluster_vector,
                reclustering = plotting_cluster,
                result = cnt_result,
                sax_data = data))
}

patternScale = function(data,idx,scale) {

    minmax = function(attr) {(attr -min(attr)) / (max(attr) - min(attr))}

    tmp = data[,idx]
    tmp = ts(tmp,frequency = 10)
    tmp = as.numeric(tmp)
    # tmp = minmax(tmp)

    out = paa(tmp,scale)

    return(out)

}

sax_process = function(data) {

    stop_walking = function(data,seq,rate,log=FALSE){

        tmpList = vector()
        checkList = vector()

        for(i in 1:(length((data)-seq))) {

            tmp = as.integer(data[i:(i+(seq-1))])
            tmp[is.na(tmp)] = 0
            check = as.integer(seq(from = data[i],length.out=10, by = 1))
            check[is.na(check)] = 0

            if(log ==TRUE){
                print("tmp")
                print(tmp)
                print("check")
                print(check)

            }

            if(sum(tmp == check) >= (round(seq*rate))) {

                tmpList = append(tmpList,tmp)
                checkList = append(checkList,check)
            }
        }

        tmpList = matrix(tmpList,ncol=10,byrow=TRUE)
        checkList = matrix(checkList,ncol=10,byrow=TRUE)

        dataLangth = length(data)

        # print(dataLangth)

        # return(list(tmpList,checkList))
        return(tmpList)
    }

    tmp = which(data$decomposed_data$trend <= min(data$kmeans$V1))
    stop_point = stop_walking(tmp,10,0.8) # stop_walking 적용을 위한 임시값(상수)

    return(stop_point)

}


#쉰 횟수 출력
detectWake = function(sax_processData) {

    result = vector()
    result = append(result,sax_processData[1,1])

    lengthData = as.numeric(length(sax_processData[,1]))

    for (i in 1:(lengthData-1)){
        tmp  = sax_processData[(i+1),1] - sax_processData[i,1]
        if (tmp != 1) {result = append(result,sax_processData[(i+1),1])}
    }

    return(result)

}

# sax_process(scale_decompose())된 데이터 적용

checkStopPoint = function(data) {

    data = sort(unique(as.vector(data)))
    dataLength = as.vector(length(data))
    stopCnt = 0

    tmpStart = vector()
    tmpStart = append(tmpStart,data[1])
    tmpEnd = vector()
    tmpTime = vector()
    tmpTerm = vector()

    for(i in 1:(dataLength-1)){

        tmpValue = as.numeric(data[i+1] - data[i])

        if(  tmpValue > 15 ) {

            tmpTerm = append(tmpTerm,tmpValue)

            stopCnt = stopCnt+1
            tmpStart = append(tmpStart,data[i+1])
            tmpEnd = append(tmpEnd,data[i])
        }
    }

    # tmpStart = tmpStart[1:length(tmpStart)-1]
    tmpEnd = append(tmpEnd,tail(data,1))
    tmpTime = tmpEnd - tmpStart

    return(list(stopCnt,tmpStart,tmpEnd,tmpTime,tmpTerm))
}



# plottingWake = function(data) {
#
#     tmp = detectWake(sax_process(data))
#     timeData = sort(unique(as.vector(tmp)))
#
#     return(timeData)
# }
