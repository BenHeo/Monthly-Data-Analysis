from selenium import webdriver
from bs4 import BeautifulSoup
from time import sleep
import requests
import json
import re

header = {'User-Agent': ''}
d = webdriver.Chrome()
d.implicitly_wait(3)
d.get('https://www.melon.com/chart/index.htm')
d.get("https://www.melon.com/chart/search/index.htm")
d.find_element_by_xpath('//*[@id="d_chart_search"]/div/h4[2]/a').click()

for i in range(1, 5):


    age_xpath = '//*[@id="d_chart_search"]/div/div/div[1]/div[1]/ul/li[' + str(i) + ']/span/label'
    age = d.find_element_by_xpath(age_xpath)
    age.click()

    for j in range(1, 11):


        result = list()

        try:
            year_xpath = '//*[@id="d_chart_search"]/div/div/div[2]/div[1]/ul/li[' + str(j) + ']/span/label'
            year = d.find_element_by_xpath(year_xpath)
            year.click()
            print(year.text)

        except:
            print("year_xpath not found")
            pass

        if j == 1:
            k_end = 11
        else:
            k_end = 12

        for k in range(1,(k_end+1)):
            month_xpath = '//*[@id="d_chart_search"]/div/div/div[3]/div[1]/ul/li[' + str(k) + ']/span/label'
            monthchart = d.find_element_by_xpath(month_xpath)
            monthchart.click()
            print("몇 월?:", monthchart.text)

            classCd = d.find_element_by_xpath('//label[@for = "gnr_1"]')
            if '종합' not in classCd.text:
                classCd = d.find_element_by_xpath('//label[@for = "gnr_2"]')

            print(classCd.text)
            classCd.click()

            d.find_element_by_xpath('//*[@id="d_srch_form"]/div[2]/button/span/span').click()
            sleep(5)
            song_ids = d.find_elements_by_xpath('//*[@id="lst50"]/td[4]/div/a')
            song_ids = [re.sub('[^0-9]', '', song_id.get_attribute("href")) for song_id in song_ids]
            ranks = d.find_elements_by_xpath('//*[@id="lst50"]/td[2]/div/span[1]')

            for rank, song_id in zip(ranks, song_ids):
                sleep(1)
                print(song_id)

                req = requests.get('http://www.melon.com/song/detail.htm?songId=' + song_id, headers = header)
                html = req.text
                soup = BeautifulSoup(html, "html.parser")

                title = soup.find(attrs={"class": "song_name"}).text.replace('곡명', '')

                if '19금' in title:
                    title = title.replace('19금', '')

                title = re.sub('^\s*|\s+$','', title)

                artist = soup.find(attrs={"class": "artist_name"}).text

                album = soup.select('#downloadfrm > div > div > div.entry > div.meta > dl > dd')[0].text

                genre = soup.select('#downloadfrm > div > div > div.entry > div.meta > dl > dd')[2].text

                lyric = re.sub('<[^>]*>|\s|\[|\]', ' ', str(soup.find_all(attrs={"class": "lyric"})[0]))
                lyric = re.sub('^\s*|\s+$', '', lyric)

                result.append({
                    'year': re.sub('[^0-9]', '', year.text),
                    'rank': rank.text,
                    'title': title,
                    'artist': artist,
                    'album': album,
                    'genre': genre
                    })
                print("차트 연도:", year.text)
                print("순위:", rank.text)
                print("곡 id:", song_id)
                print("제목:", title)
                print("아티스트:", artist)
                print("앨범:", album)
                print("장르:", genre)
                print("*_*_*_*_*_*_*_*_*_*_*__*_*_*")
            with open('./melon_chart' + re.sub('[^0-9]', '', age.text) + 's.json', 'w', encoding='utf-8') as f:
                j = json.dumps(result)
                f.write(j)

