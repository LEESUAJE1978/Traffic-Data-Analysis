require(data.table);require(tidyverse)
CITS<-fread('2018070400-06.csv', stringsAsFactors = FALSE )
str(CITS)
#운전자 반응 분석####
#1.도로 위험 발생 감지 이벤트정보
#2. 노면 위험 발생 감지 이벤트 정보
#3. 기상위험 발생 감지 이벤트 정보
#4. 도로공사 구간 감지 이벤트 정보
#5. 신호 위한 감지 이벤트 정보
#6. 차량간 상충 위험 경고 감지 이벤트 정보
#7. 차량간 상충 위험 경우 감지 이벤트 정보
#8. 버스 추돌위험 감지 이벤트 정보
#9. 엘로우 버스 긴급상황 감지 이벤트 정보
#10. 보행자 충돌 방지 경고 감지 이벤트 정보
#11. 추돌위험 감지 이벤트 정보
#12. 차량 긴급 상황 감지 이벤트 정보
#13. 스쿨존/ 실버존 주의 운전 감지 이벤트 정보
#14. 긴급 차량 접근 감지 이벤트 정보

length(unique(CITS$OBU_ID))#328대
length(colnames(CITS)) #42개 컬럼
#차량들의 평균속도
#0값 제외시 52.27km
a<-CITS %>%  filter(SPEED!=0) %>% 
  group_by(OBU_ID) %>% 
  summarise(mean_speed =mean(SPEED)) %>% 
  arrange(desc(mean_speed))
mean(a$mean_speed)

#0값 포함시 40.15km
b<-CITS %>%  
  group_by(OBU_ID) %>% 
  summarise(mean_speed =mean(SPEED)) %>% 
  arrange(desc(mean_speed))
mean(b$mean_speed)

#차량 분석
ANYCAR<-CITS %>%  filter(OBU_ID == '7002010F')# 임의의 차량 선택
names(ANYCAR)

####EVENT DATA####
ANYCAR_EVENT<-ANYCAR %>% 
  select(HAZARDLIGHTS_FLAG:DISABLEDVEHICLE_FLAG) %>% 
  lapply(function(x) table(factor(x, levels = c(-1,0,1)))) #Event관련 값 선택, factor level 고정 적용
NAMES<-names(ANYCAR_EVENT) #list 항목명 추출
ANYCAR_EVENT<-do.call('rbind', ANYCAR_EVENT) #do.call 함수로 matrix 형태로 값 정렬
ANYCAR_EVENT<-cbind(NAMES, ANYCAR_EVENT)%>% data.frame(., stringsAsFactors = F) # cbind로 컬럼명과 value  결합
rownames(ANYCAR_EVENT)<-NULL  #index 값 제거
colnames(ANYCAR_EVENT)<-c("NAMES", "-1", "0","1") #컬럼명 변경
ANYCAR_EVENT

####EXTEVENTS####
ANYCAR_EXT<-ANYCAR %>% 
  select(GETONDOWN_FLAG:ROADWORK_FLAG) %>% 
  lapply(function(x) table(factor(x, levels = c(-1,0,1)))) #Event관련 값 선택, factor level 고정 적용
NAMES<-names(ANYCAR_EXT) #list 항목명 추출
ANYCAR_EXT<-do.call('rbind', ANYCAR_EXT) #do.call 함수로 matrix 형태로 값 정렬
ANYCAR_EXT<-cbind(NAMES, ANYCAR_EXT)%>% data.frame(., stringsAsFactors = F) # cbind로 컬럼명과 value  결합
rownames(ANYCAR_EXT)<-NULL  #index 값 제거
colnames(ANYCAR_EXT)<-c("NAMES", "-1", "0","1") #컬럼명 변경
ANYCAR_EXT

####STATUS####
ANYCAR_STATUS<-ANYCAR %>% 
  select(LIGHTS_STATUS:SCS_STATUS,GPS_STATUS:DOOR_STATUS) %>% 
  lapply(function(x) table(factor(x, levels = c(-1:16)))) #Event관련 값 선택, factor level 고정 적용
NAMES<-names(ANYCAR_STATUS) #list 항목명 추출
ANYCAR_STATUS<-do.call('rbind', ANYCAR_STATUS) #do.call 함수로 matrix 형태로 값 정렬
ANYCAR_STATUS<-cbind(NAMES, ANYCAR_STATUS)%>% data.frame(., stringsAsFactors = F) # cbind로 컬럼명과 value  결합
rownames(ANYCAR_STATUS)<-NULL  #index 값 제거
colnames(ANYCAR_STATUS)<-c("NAMES", -1:16) #컬럼명 변경
ANYCAR_STATUS

####ETC####
ANYCAR$HMI_CONNECT[is.na(ANYCAR$HMI_CONNECT)]<-0 #NA값 0으로 대체
ANYCAR_ETC<-ANYCAR %>% 
  select(OBU_FW_VER:HMI_CONNECT) %>% 
  lapply(function(x) table(factor(x, levels = c(0:8)))) #Event관련 값 선택, factor level 고정 적용
NAMES<-names(ANYCAR_ETC)
ANYCAR_ETC<-do.call('rbind',ANYCAR_ETC)
ANYCAR_ETC<-cbind(NAMES, ANYCAR_ETC) %>% data.frame(., stringsAsFactors = F)
rownames(ANYCAR_ETC)<-NULL
colnames(ANYCAR_ETC)<-c("NAMES", 0:8)
#Throttlepos, second accelation을 어떻게 해석해야 할지


###box plot


#위험행동 분석####
#GPS정확도 검증 분석####
#센터 제공 데이터 통신율 검증 분석####
#V2V서비스 검증 분석####
#통계 분석####
#도로 위험 구간 예측####

