import urllib.request, json, codecs, math, time

def searchWords(koreanWords):
    url = ('https://ko.dict.naver.com/api3/koko/search?' + urllib.parse.urlencode({'query': koreanWords}) + '&range=word&page=1')
    response = urllib.request.urlopen(url)
    reader = codecs.getreader("utf-8")
    jsonInfo = json.load(reader(response))
    pageCount = jsonInfo["pagerInfo"]["totalPages"]
    searchData = jsonInfo["searchResultMap"]["searchResultListMap"]["WORD"]["items"]

    for pageCountInc in range(0, pageCount):
        if pageCountInc != 0:
            url = ('https://ko.dict.naver.com/api3/koko/search?' + urllib.parse.urlencode({'query': koreanWords}) + '&range=word&page=' + str(pageCountInc+1))
        response = urllib.request.urlopen(url)
        reader = codecs.getreader("utf-8")
        jsonInfo = json.load(reader(response))
        searchData = jsonInfo["searchResultMap"]["searchResultListMap"]["WORD"]["items"]
        for z in range (0, len(searchData)):
            if searchData[z]["handleEntry"] in unchangedWordList:
                if searchData[z]["searchPhoneticSymbolList"]:
                    if searchData[z]["searchPhoneticSymbolList"][0]["phoneticSymbolPath"] != "":
                        timesDownloaded[unchangedWordList.index(searchData[z]["handleEntry"])] += 1
                        mp3Link = searchData[z]["searchPhoneticSymbolList"][0]["phoneticSymbolPath"]
                        if mp3Link not in mp3Links:
                            mp3Links.append(mp3Link)
                            urllib.request.urlretrieve(mp3Link, searchData[z]["handleEntry"] + str(timesDownloaded[unchangedWordList.index(searchData[z]["handleEntry"])]) + ".mp3")
                            time.sleep(.3)

def parseWords(listOfWords):
    for x in range(0, math.floor(len(listOfWords)/10)):
        tempWords = []
        for y in range(0, 10):
            tempWords.append(listOfWords[x*10+y])

        print("Searching: " + str(x+1) + "/" + str(math.ceil(len(listOfWords)/10)))
        searchWords(tempWords)

    tempWords = []
    for y in range(math.floor(len(listOfWords)/10)*10+1, len(listOfWords)):
        tempWords.append(listOfWords[y])
    print("Searching: " + str((math.ceil(len(listOfWords)/10))) + "/" + str(math.ceil(len(listOfWords)/10)))
    searchWords(tempWords)
unfoundWords = []
unchangedWordList = []
timesDownloaded = []
mp3Links = []

with open("targets.txt", "r", encoding='utf8') as f:
	wordInputs = unchangedWordList = [line.strip() for line in f]
timesDownloaded = [0] * len(unchangedWordList)

parseWords(wordInputs)

for z in range(0, len(timesDownloaded)):
    if(timesDownloaded[z] == 0):
        unfoundWords.append(unchangedWordList[z])

if unfoundWords:
    print(",".join(str(x) for x in unfoundWords) + " could not be found.")
    print("Rerunning individual searches for unfound words.")
    print(unfoundWords)
    oldUnfoundWords = unfoundWords
    unfoundWords = []
    for x in range(0, len(oldUnfoundWords)):
        print("Searching: " + str(x+1) + "/" + str(len(oldUnfoundWords)))
        searchWords(oldUnfoundWords[x])

    for z in range(0, len(timesDownloaded)):
        if(timesDownloaded[z] == 0):
            unfoundWords.append(unchangedWordList[z])

    if unfoundWords:
        print(",".join(str(x) for x in unfoundWords) + " could not be found.")