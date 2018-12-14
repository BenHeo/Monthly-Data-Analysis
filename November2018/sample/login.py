import requests
from bs4 import BeautifulSoup


session = requests.session() # 세션 열기(한 번 하면 됨)

# 로그인
url = "http://www.hanbit.co.kr/member/login_proc.php"
data = { # login_proc에서 From data에 쓰인 것
    "return_url": "http://www.hanbit.co.kr/index.html",  
    "m_id": "****",
    "m_passwd": "****"
}


response = session.post(url, data=data) # 위 url에서 login_proc은 post 방식을 받는다
response.raise_for_status() # 요청이 실제로 걸린다

# 마일리지 들고오기
url = "http://www.hanbit.co.kr/myhanbit/myhanbit.html"
response = session.get(url)
response.raise_for_status()
soup = BeautifulSoup(response.text, "html.parser")
text = soup.select_one(".mileage_section1 span").get_text()
print("마일리지:", text)

