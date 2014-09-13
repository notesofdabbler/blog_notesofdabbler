#
#   Work through demo/united.R in rvest package
#   https://github.com/hadley/rvest/blob/master/demo/united.R
#

library(rvest)
library(magrittr)

united <- html_session("http://www.united.com")
account <- united%>%follow_link("Account")

login <- account%>%html_node("form")%>%extract2(1)%>%html_form()%>%
          set_values(
            'ctl00$ContentInfo$SignIn$onepass$txtField'='vxs131',
            'ctl00$ContentInfo$SignIn$password$txtPassword'='kvrldmvsr12'
            )

logged_in <- account %>%
  submit_form(login, "ctl00$ContentInfo$SignInSecure")

logged_in %>% jump_to(gsub(" ", "+", headers(logged_in)$Location))
