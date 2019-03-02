import urllib.request

url = "http://uta.pw/shodou/img/28/214.png"
savename = "test.png"

urllib.request.urlretrieve(url, savename) # 파일 바로 다운로드 가능
