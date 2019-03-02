import urllib.request

# url = "http://uta.pw/shodou/img/28/214.png"
# savename = "test.png"

# mem = urllib.request.urlopen(url).read() # url에서 바로 읽어서 사용하려고 함

# with open(savename, mode = "wb") as f: # binary 쓴 이유: 이미지는 컴퓨터만 이해하면 되니까
#     f.write(mem)
#     print('저장 완료')


# url = "https://www.google.co.kr/"
# mem = urllib.request.urlopen(url).read()
# print(mem.decode("euc-kr")) # decode를 붙여야 binary 언어로 안 쓰여있다

url = "http://api.aoikujira.com/ip/ini"
mem = urllib.request.urlopen(url).read()
print(mem.decode("utf-8"))