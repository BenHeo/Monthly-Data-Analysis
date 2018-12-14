import urllib.request
import urllib.parse

# https://vod.melon.com/video/detail2.htm?mvId=50193001
# 대상 : https://vod.melon.com ---- 호스트 이름
# 경로 : video/detail2.htm
# 데이터 : ?mvId=50193001

# GET 방식 사용
api = "https://vod.melon.com/video/detail2.htm"
values = {
    "mvId" : "50193001"
}

params = urllib.parse.urlencode(values) # 원래 네이버 같은 검색어 넣으면 encoding과정 필요
url = api + "?" + params
# print(url)

data = urllib.request.urlopen(url).read()
# print(data)

text = data.decode("utf-8")
# print(text)