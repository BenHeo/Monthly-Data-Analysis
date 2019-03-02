from selenium import webdriver

# 헤드 있는 브라우저

drv = webdriver.Chrome('./chromedriver')

drv.implicitly_wait(2) # 암묵적으로 웹 자원 로드 위해 2초 기다린다
drv.get('http://www.naver.com')
drv.save_screenshot("Website.png")
drv.quit()