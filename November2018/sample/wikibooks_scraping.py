from bs4 import BeautifulSoup

html = """
<html><body>
<div id="meigen">
<h1>위키북스 도서</h1>
    <ul class="items">
        <li>유니티 게임 이펙트 입문</li>
        <li>스위프트로 시작하는</li>
        <li>모던 웹사이트 디자인의 정석</li>
    </ul>
</div>
</body></html>
"""

soup = BeautifulSoup(html, 'html.parser')

# 필요한 부분 css쿼리로 추출하기
# 타이틀
h1 = soup.select_one("div#meigen > h1").string
print("h1 =", h1) # h1 = 위키북스 도서

# 속성
attr = soup.select_one("div#meigen > ul").attrs
print(attr) # {'class': ['items']}

# 목록
li_list = soup.select("div#meigen > ul.items > li") # 자식 선택자 형식
                                        # "div#megien li" # 후손 선택자 형식
i = 1
for li in li_list:
    print(i)
    print("li =", li.string) 
    # li = 유니티 게임 이펙트 입문 
    # li = 스위프트로 시작하는 
    # li = 모던 웹사이트 디자인의 정석
    i += 1

