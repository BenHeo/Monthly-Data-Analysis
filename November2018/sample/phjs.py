from selenium import webdriver

################### 네이버 메인 캡쳐 뜨기 ######################
# url = "https://www.naver.com"

# # phantomjs 드라이버 추출
# browser = webdriver.PhantomJS()
# # 3초 대기 (바로 부르면 버그 느낌이 있다고 함)
# browser.implicitly_wait(3)

# browser.get(url) # url 읽어들이기
# # 여기부터는 하고 싶은 거 하는 곳
# browser.save_screenshot("Website.png")

# # 브라우저 나가기
# browser.quit()



################# 네이버 로그인 하기 #########################
url = "https://nid.naver.com/nidlogin.login"

# phantomjs 드라이버 추출
browser = webdriver.PhantomJS()
# 3초 대기 (바로 부르면 버그 느낌이 있다고 함)
browser.implicitly_wait(3)

browser.get(url) # url 읽어들이기
# 여기부터는 하고 싶은 거 하는 곳
browser.save_screenshot("Website2.png")
element_id = browser.find_element_by_id("id") # id 텍스트 입력상자
element_id.clear() # id 상자 내용 비움
element_id.send_keys("gjguskr") # id칸에 내가 원하는 글자 보냄
element_pw = browser.find_element_by_id("pw") # 비밀번호 텍스트 입력상자
element_pw.clear() # 비밀번호 상자 내용 비움
element_pw.send_keys("dkansk0422")
browser.save_screenshot("Website3.png")
button=browser.find_element_by_css_selector("input.btn_global[type=submit]") # sign in 선택
button.submit()

# 메일 페이지 열기
browser.get("https://mail.naver.com")
browser.save_screenshot("Website4.png")




# 브라우저 나가기
browser.quit()