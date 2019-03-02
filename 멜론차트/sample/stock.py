import urllib.request
from bs4 import BeautifulSoup
import time

url = "https://finance.naver.com/marketindex/"
response = urllib.request.urlopen(url)

soup = BeautifulSoup(response, "html.parser")
results = soup.select("span.value")
for result in results:
    print(result.string) # 원달러 환율만 가져오려 했는데 여러 값을 보여준다

# 처음이 원달러 환율이다. 두번째 원엔 환율, 세번째 원유로 환율
print('원달러:', results[0].string)

######### 뉴스 기사 뽑아오자
url = "https://news.naver.com"
response = urllib.request.urlopen(url)

# soup = BeautifulSoup(response, "html.parser")
# results = soup.select("strong")
# for result in results:
#     print(result.string) # 다른 영역도 포함된 것 같다

soup = BeautifulSoup(response, "html.parser")
results = soup.select('#main_content > #section_life > div[class=com_list] > div[class=mtype_list_wide] a')
# print(results)
for result in results:
    ref_url = result.attrs["href"]
    # print(ref_url)
    ref_res = urllib.request.urlopen(ref_url)
    # print(ref_res)
    ref_soup = BeautifulSoup(ref_res, "html.parser")
    title = ref_soup.select_one("#articleTitle")
    content = ref_soup.select_one("#articleBodyContents")
    print("Title:", title.string)
    # print(content.contents) 가공이 안 되어 있다
    output = ""
    for item in content:
        stripped = str(item).strip()
        if stripped == "":
            continue
        if stripped[0] not in ["<", "/"]:
            output += stripped
            output += "\n"
    output = output.replace("본문 내용", "")
    output = output.replace("TV플레이어", "")
    print("Contents\n")
    print(output)
    time.sleep(2)
    print("#####################################")