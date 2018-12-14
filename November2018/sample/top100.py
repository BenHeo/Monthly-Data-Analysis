from selenium import webdriver
from bs4 import BeautifulSoup
from time import sleep
import requests
import json
import re


url1 = "http://www.melon.com/chart/index.htm"
url2 = "http://www.melon.com/chart/search/index.htm"

header = {'User-Agent': ''}

browser = webdriver.Chrome()
browser.implicitly_wait(3)

browser.get(url1)
browser.implicitly_wait(3)
browser.get(url2)

browser.find_element_by_xpath('//*[@id="d_chart_search"]/div/h4[2]/a').click()

for turn in range(2, 3):
    for i in range(1, 3):
        ten_xpath = '//*[@id="d_chart_search"]/div/div/div[1]/div[1]/ul/li['+str(i)+']/span/label'
        tenchart = browser.find_element_by_xpath(ten_xpath)
        tenchart.click()
        print("몇 년대?:", tenchart.text)

        if i == 1:
            p = 9
        else:
            p = 10

        for j in range(1, p+1):

            year_xpath = '//*[@id="d_chart_search"]/div/div/div[2]/div[1]/ul/li['+str(j)+']/span/label'
            yearchart = browser.find_element_by_xpath(year_xpath)
            yearchart.click()
            print("몇 년?:", yearchart.text)

            if i == 1 and j == 1:
                q = 11
            else:
                q = 12

            for k in range(1, q+1):
                result = list()

                month_xpath = '//*[@id="d_chart_search"]/div/div/div[3]/div[1]/ul/li['+str(k)+']/span/label'
                monthchart = browser.find_element_by_xpath(month_xpath)
                monthchart.click()
                print("몇 월?:", monthchart.text)

                genre_xpath = '//label[@for = "gnr_1"]'

                genre = browser.find_element_by_xpath(genre_xpath)
                genre.click()
                print("장르?:", genre.text)

                browser.find_element_by_xpath('//*[@id="d_srch_form"]/div[2]/button/span/span').click()
                sleep(10)

    ####################### not my code. fix it ###############################
                # from 1 to 50
                if turn == 1:
                    song_ids = browser.find_elements_by_xpath('//*[@id="lst50"]/td[4]/div/a')
                    song_ids = [re.sub('[^0-9]', '', song_id.get_attribute("href")) for song_id in song_ids]
                    ranks = browser.find_elements_by_xpath('//*[@id="lst50"]/td[2]/div/span[1]')
                else:
                    browser.find_element_by_xpath('//*[@id="frm"]/div[2]/span/a').click()
                    song_ids = browser.find_elements_by_xpath('//*[@id="lst100"]/td[4]/div/a')
                    song_ids = [re.sub('[^0-9]', '', song_id.get_attribute("href")) for song_id in song_ids]
                    ranks = browser.find_elements_by_xpath('//*[@id="lst100"]/td[2]/div/span[1]')

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

                    genre = soup.select('#downloadfrm > div > div > div.entry > div.meta > dl > dd')[2].text
                    try:
                        lyric = re.sub('<[^>]*>|\s|\[|\]', ' ', str(soup.find_all(attrs={"class": "lyric"})[0]))
                        lyric = re.sub('^\s*|\s+$', '', lyric)
                    except:
                        print("lyrics not found")
                        pass

                    result.append({
                        'song_no': song_id,
                        'year': re.sub('[^0-9]', '', yearchart.text),
                        'rank': rank.text,
                        'title': title,
                        'artist': artist,
                        'genre': genre,
                        'lyrics': lyric
                        })
                    print("차트 연도:", yearchart.text)
                    print("순위:", rank.text)
                    print("곡 id:", song_id)
                    print("제목:", title)
                    print("아티스트:", artist)
                    print("장르:", genre)
                    print("*_*_*_*_*_*_*_*_*_*_*__*_*_*")

                if turn == 1:
                    with open('./melon_chart/' + re.sub('[^0-9]', '', yearchart.text) + '_' + re.sub('[^0-9]', '', monthchart.text) + 's.json', 'w', encoding='utf-8') as f:
                        js = json.dumps(result)
                        f.write(js)
                else:
                    with open('./melon_chart/' + re.sub('[^0-9]', '', yearchart.text) + '_' + re.sub('[^0-9]', '', monthchart.text) + 's_2.json', 'w', encoding='utf-8') as f:
                        js = json.dumps(result)
                        f.write(js)

browser.quit()