#
#
#

library(rvest)
library(magrittr)

united <- html_session("http://www.united.com")

fhist=united%>%html_node("form")%>%extract2(1)%>%html_form()%>%
  set_values(
    'ctl00$ContentInfo$Checkinflightstatus$Origin$txtOrigin'='IND',
    'ctl00$ContentInfo$Checkinflightstatus$Destination$txtDestination'='ORD'
  )

fhist2=united%>%submit_form(fhist,'ctl00$ContentInfo$Checkinflightstatus$btnFlightStatus')


url="http://www.united.com/web/en-US/apps/travel/timetable/results.aspx?Origin=IND&Destination=ORD&Date=9/13/2014&Time=&NonStop=&NumFlight=&FLN="

tmp=html_session(url)%>%html_node("form")%>%extract2(1)%>%html_form()
tmp2=html_session(url)%>%submit_form(tmp,'ctl00$ContentInfo$ShowSegments1$ShowSegment$ctl01$CheckStatus')

