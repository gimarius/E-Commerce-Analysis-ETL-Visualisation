from selenium import webdriver
import time
from bs4 import BeautifulSoup
import pandas as pd
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import os
from selenium.webdriver.chrome.options import Options



def div():
    input_ishares = []
    input_swisslife = []
    input_bridgewater = []
    input_csa_private = []
    input_ft = []
    input_ft.append(['IE00BFWJRY20', 'TULIP TREND FUND UCITS - G CHF'])

    input_csa_private.append(["CH0115202712", "CSA Mixta-BVG Index 25", "Credit Suisse"])

    input_swisslife.append(["1245607", "Swiss Life BVG-Mix 45", "Swiss Life"])
    input_bridgewater.append(["GB00B02KKD01", "Bridgewater Fund Limited - CLASS B", "Bridgewater"])
    input_ishares.append(["HDV", "iShares Core High Dividend ETF", "239563/"])
    input_ishares.append(["IDV", "iShares International Select Dividend ETF", "239499/"])
    input_ishares.append(["REET", "iShares Global REIT ETF", "268752/"])
    input_ishares.append(["SHYG", "iShares 0-5 Year High Yield Corporate Bond ETF", "258100/"])
    input_ishares.append(["IE00BKM4GZ66", "iShares Core MSCI EM IMI UCITS ETF", "264659/"])
    input_ishares.append(["IE00B988C465", "iShares Global High Yield Corp Bond CHF Hedged UCITS ETF (Dist)", "261622/"])
    input_ishares.append(["IE00B1FZS350", "iShares Developed Markets Property Yield UCITS ETF", "251801/"])
    input_ishares.append(["CH0226976816", "iShares Core CHF Corporate Bond ETF (CH)", "261150/"])
    input_ishares.append(["IE00B9M04V95", "iShares J.P. Morgan $ EM Bond CHF Hedged UCITS ETF (Dist)", "273328/"])
    input_ishares.append(["IE00B5L8K969", "iShares MSCI EM Asia UCITS ETF USD", "253723/"])
    input_ishares.append(["IE00B1TXHL60", "iShares Listed Private Equity UCITS ETF", "251918/"])
    input_ishares.append(["IE00BD1JRZ09", "iShares Edge MSCI World Minimum Volatility UCITS ETF CHF Hedged", "295827/"])
    input_ishares.append(["IE00B53SZB19", "iShares NASDAQ 100 UCITS ETF", "253741/"])
    input_ishares.append(["IE00B8BVCK12", "iShares MSCI World CHF Hedged UCITS ETF (Acc)", "251407/"])
    input_ishares.append(["CH0237935652", "iShares Core SPI ETF (CH)", "264107/"])
    input_ishares.append(["ERUS", "iShares MSCI Russia ETF", "239677/"])
    #input_ishares.append(["DE0005933923", "iShares MDAX UCITS ETF (DE)", "251845/"])





    output = pd.DataFrame()

    path = os.path.abspath(os.path.join(os.path.normpath(os.path.dirname(__file__)), 'chromedriver.exe'))
    chrome_options = Options()
    chrome_options.add_argument("--headless")
    chrome_options.add_argument('log-level=3')

    driver = webdriver.Chrome(executable_path=path, options=chrome_options)

    driver.get("https://markets.ft.com/data/funds/tearsheet/summary?s=IE00BFWJRY20:CHF")

    array = []
    list_array =[[]]


    try:
        element = WebDriverWait(driver, 35).until(
            EC.presence_of_element_located(
                (By.XPATH, '/html/body/div[3]/div[2]/section[1]/div/div/div[1]/div[1]/div[2]/span'))
        )
    except:
        print('Financial Times not loaded')


    nav_ft = driver.find_element_by_xpath('/html/body/div[3]/div[2]/section[1]/div/div/div[1]/div[2]/ul/li[1]/span[2]').text
    if "'" in nav_ft:
        nav_ft = nav_ft.replace("'", "")

    date_ft = driver.find_element_by_xpath('/html/body/div[3]/div[2]/section[1]/div/div/div[1]/div[2]/div').text

    date_ft = date_ft[-12:-1]
    date_ft = date_ft[4:6]+'.'+date_ft[0:3]+date_ft[7:11]


    if "Apr" in date_ft:
        date_ft = date_ft[:2] + ".04." + date_ft[-4:]
    if "Mai" in date_ft:
        date_ft = date_ft[:2] + ".05." + date_ft[-4:]
    if "Jun" in date_ft:
        date_ft = date_ft[:2] + ".06." + date_ft[-4:]
    if "Jul" in date_ft:
        date_ft = date_ft[:2] + ".07." + date_ft[-4:]
    if "Aug" in date_ft:
        date_ft = date_ft[:2] + ".08." + date_ft[-4:]
    if "Sep" in date_ft:
        date_ft = date_ft[:2] + ".09." + date_ft[-4:]
    if "Okt" in date_ft:
        date_ft = date_ft[:2] + ".10." + date_ft[-4:]
    if "Nov" in date_ft:
        date_ft = date_ft[:2] + ".11." + date_ft[-4:]
    if "Dez" in date_ft:
        date_ft = date_ft[:2] + ".12." + date_ft[-4:]
    if "Jan" in date_ft:
        date_ft = date_ft[:2] + ".01." + date_ft[-4:]
    if "Feb" in date_ft:
        date_ft = date_ft[:2] + ".02." + date_ft[-4:]
    if "Mar" in date_ft:
        date_ft = date_ft[:2] + ".03." + date_ft[-4:]
    if "Mär" in date_ft:
        date_ft = date_ft[:2] + ".03." + date_ft[-4:]  


    isin_ft = driver.find_element_by_xpath('/html/body/div[3]/div[2]/section[1]/div/div/div[1]/div[1]/div[2]/span').text
    isin_ft = isin_ft[0:-4]
    

    array.append(nav_ft)
    array.append(date_ft)
    array.append(isin_ft)
    list_array.append(array)

    #erstellt Pandas Dataframe für 
    df_input_ft = pd.DataFrame(input_ft)
    df_input_ft.columns = ["ISIN", "Asset Name"]


    #erstellt Pandas Dataframe für gecrawlte Daten
    df_array_list = pd.DataFrame(list_array)
    df_array_list = df_array_list.iloc[1:]
    df_array_list.columns = ["Bid Price", "Date", "ISIN"]
    df_array_list["Asset Name"] = ['TULIP TREND FUND UCITS - G CHF']
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    df_array_list = df_array_list[['Asset Name', 'ISIN', 'Bid Price', 'Ask Price', 'Date']]

    ft_merge = df_array_list

    #Check ob es NA hat
    if ft_merge.isnull().values.any():
        print("---------------------------------------------------------------------------ft hat NAs----------------------------")
    else:
        pass


    #CSA Mixta-BVG Index 25
    driver.get("https://www.cash.ch/fonds/credit-suisse-anlagestiftung---csa-mixta-bvg----index-25-11520271/cff/chf")

    array = []
    list_array =[[]]


    try:
        inst_button = driver.find_element_by_xpath('//*[@id="edit-cash-fonds-investor-type-qualified"]')
        inst_button.click()

        zustimmen_button = driver.find_element_by_xpath('//*[@id="edit-submit"]')
        zustimmen_button.click()

    except:
        pass



    try:
        element = WebDriverWait(driver, 35).until(
            EC.presence_of_element_located(
                (By.XPATH, '/html/body/div[1]/div[4]/div/div/div[2]/div/div/div/div[1]/div/div/section[1]/div/div/div[3]/div/table[1]/tbody/tr/td[2]/span'))
        )
    except:
        print('CSA Mixtra-BVG Index 25 Page not loaded')





    nav_csa25i = driver.find_element_by_xpath('/html/body/div[1]/div[4]/div/div/div[2]/div/div/div/div[1]/div/div/section[1]/div/div/div[3]/div/table[1]/tbody/tr/td[2]/span').text
    if "'" in nav_csa25i:
        nav_csa25i = nav_csa25i.replace("'", "")
    array.append(nav_csa25i)


    date_csa25i = driver.find_element_by_xpath('/html/body/div[1]/div[4]/div/div/div[2]/div/div/div/div[1]/div/div/section[1]/div/div/div[3]/div/table[1]/tbody/tr/td[2]/div/div/span').text
    array.append(date_csa25i)

    isin_csa25i = driver.find_element_by_xpath('/html/body/div[1]/div[4]/div/div/div[2]/div/div/div/div[1]/div/div/section[2]/div/div/div/div[5]/div/table/tbody/tr[6]/td[2]').text
    array.append(isin_csa25i)

    list_array.append(array)

    #erstellt Pandas Dataframe für input_vanguard
    df_input_csa25i = pd.DataFrame(input_csa_private)
    df_input_csa25i.columns = ["ISIN", "Asset Name", "Manager"]
    df_input_csa25i = df_input_csa25i.drop(["Manager"], axis=1)

    #erstellt Pandas Dataframe für gecrawlte Daten
    df_array_list = pd.DataFrame(list_array)
    df_array_list = df_array_list.iloc[1:]
    df_array_list.columns = ["Bid Price", "Date", "ISIN"]
    df_array_list["Asset Name"] = ['CSA Mixta-BVG Index 25']
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    df_array_list = df_array_list[['Asset Name', 'ISIN', 'Bid Price', 'Ask Price', 'Date']]

    csa25i_merge = df_array_list

    #Check ob es NA hat
    if csa25i_merge.isnull().values.any():
        print("---------------------------------------------------------------------------csa25i hat NAs----------------------------")
    else:
        pass


    #Bridgewater
    driver.get("https://www.cash.ch/fonds-investor-disclaimer?redirect=fonds/managed-fund---bridgewater-fund-ltd-1940914/fuf/usd")
    try:
        element = WebDriverWait(driver, 35).until(
            EC.presence_of_element_located(
                (By.XPATH, '/html/body/div[1]/div[4]/div/div/div[2]/div/div/div/div[1]/div/form/div/div[2]/input'))
        )
    except:
        print('Bridgewater Page not loaded')


    try:
        element = driver.find_element_by_xpath('//*[@id="edit-cash-fonds-investor-type-qualified"]')
        element.click()
        element2 = driver.find_element_by_xpath('//*[@id="edit-submit"]')
        element2.click()
    except:
        pass


    array = []
    list_array =[[]]

    try:
        element = WebDriverWait(driver, 45).until(
            EC.presence_of_element_located(
                (By.XPATH, '//*[@id="block-system-main"]/div/div/section[1]/div/div/div[1]/div/div/div[2]/ul/li/span[1]'))
        )
    except:
        print('Bridgewater Page not loaded')
    time.sleep(2)

    nav_bw = driver.find_element_by_xpath('/html/body/div[1]/div[4]/div/div/div[2]/div/div/div/div[1]/div/div/section[1]/div/div/div[3]/div/table[1]/tbody/tr/td[2]/span').text
    array.append(nav_bw)
    date_bw = driver.find_element_by_xpath('/html/body/div[1]/div[4]/div/div/div[2]/div/div/div/div[1]/div/div/section[1]/div/div/div[3]/div/table[1]/tbody/tr/td[2]/div/div/span').text
    array.append(date_bw)
    isin_bw = driver.find_element_by_xpath('/html/body/div[1]/div[4]/div/div/div[2]/div/div/div/div[1]/div/div/section[2]/div/div/div/div[5]/div/table/tbody/tr[6]/td[2]').text
    array.append(isin_bw)

    list_array.append(array)

    #erstellt Pandas Dataframe für input_vanguard
    df_input_bridgewater = pd.DataFrame(input_bridgewater)
    df_input_bridgewater.columns = ["ISIN", "Asset Name", "Manager"]
    df_input_bridgewater = df_input_bridgewater.drop(["Manager"], axis=1)

    #erstellt Pandas Dataframe für gecrawlte Daten
    df_array_list = pd.DataFrame(list_array)
    df_array_list = df_array_list.iloc[1:]
    df_array_list.columns = ["Bid Price", "Date", "ISIN"]
    df_array_list["Asset Name"] = ['Bridgewater Fund Limited - CLASS B']
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    df_array_list = df_array_list[['Asset Name', 'ISIN', 'Bid Price', 'Ask Price', 'Date']]

    bridgewater_merge = df_array_list


    #Check ob es NA hat
    if bridgewater_merge.isnull().values.any():
        print("...............................bridgewater hat NAs................................")
    else:
        pass



    number_input_ishares = len(input_ishares)
    array_list = []
    array_list.clear()


    for m in range(0, number_input_ishares):
        driver.get("https://www.ishares.com/ch/qualifizierte-investoren/de/produkte/" + input_ishares[m][2])

        array = []
        nav = driver.find_element_by_xpath('//*[@id="fundheaderTabs"]/div/div/div/ul/li[1]/span[2]').text
        nav = nav[4:]

        date = driver.find_element_by_xpath('//*[@id="fundheaderTabs"]/div/div/div/ul/li[1]/span[1]').text
        date = date[10:]

        if "Apr" in date:
            date = date[:2] + ".04." + date[-4:]
        if "Mai" in date:
            date = date[:2] + ".05." + date[-4:]
        if "Jun" in date:
            date = date[:2] + ".06." + date[-4:]
        if "Jul" in date:
            date = date[:2] + ".07." + date[-4:]
        if "Aug" in date:
            date = date[:2] + ".08." + date[-4:]
        if "Sep" in date:
            date = date[:2] + ".09." + date[-4:]
        if "Okt" in date:
            date = date[:2] + ".10." + date[-4:]
        if "Nov" in date:
            date = date[:2] + ".11." + date[-4:]
        if "Dez" in date:
            date = date[:2] + ".12." + date[-4:]
        if "Jan" in date:
            date = date[:2] + ".01." + date[-4:]
        if "Feb" in date:
            date = date[:2] + ".02." + date[-4:]
        if "Mar" in date:
            date = date[:2] + ".03." + date[-4:]
        if "Mär" in date:
            date = date[:2] + ".03." + date[-4:]    

        try:
            isin = driver.find_element_by_xpath('//*[@id="keyFundFacts"]/div/div[14]/span[2]').text
        except:
            isin = driver.find_element_by_xpath('//*[@id="fundHeader"]/header[2]/div[5]/div/div/p').text

        if 'Keine' in isin:
            isin = driver.find_element_by_xpath('//*[@id="keyFundFacts"]/div/div[17]/span[2]').text
        else:
            pass
        if 'Vierteljährlich' in isin:
            isin = driver.find_element_by_xpath('//*[@id="keyFundFacts"]/div/div[17]/span[2]').text
        else:
            pass

        if len(isin) == 8:
            isin = driver.find_element_by_xpath('//*[@id="keyFundFacts"]/div/div[13]/span[2]').text
        else:
            pass


        if isin == '-':
            isin = driver.find_element_by_xpath('//*[@id="keyFundFacts"]/div/div[13]/span[2]').text
        else:
            pass

        array.append(isin)
        array.append(nav)
        array.append(date)
        array_list.append(array)

    #erstellt Pandas Dataframe für input_cs
    df_input_ishares = pd.DataFrame(input_ishares)
    df_input_ishares.columns = ["ISIN", "Asset Name", "Manager"]
    df_input_ishares = df_input_ishares.drop(["Manager"], axis=1)

    #erstellt Pandas Dataframe für gecrawlte Daten
    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    #merged die listen input_cs und array_list
    ishares_merge = pd.merge(df_input_ishares, df_array_list[["ISIN", "Bid Price", "Ask Price", "Date"]],
                         how="left", on='ISIN')

    # na = ishares_merge[ishares_merge.isna().any(axis=1)]
    # print(na)


    #Check ob es NA hat
    if ishares_merge.isnull().values.any():
        print("---------------------------------------------------------------------------ishares hat NAs----------------------------")
        na = ishares_merge[ishares_merge.isna().any(axis=1)]
        print(na)
    else:
        pass


    driver.get("http://www.kgast.ch/produkte-preise?allrecords=1&search")

    array_list = [[]]
    array_list.clear()
    for m in input_swisslife:
        array = []
        isin = m[0]
        #Suchfeld
        try:
            element = WebDriverWait(driver, 20).until(
                EC.presence_of_element_located(
                    (By.XPATH, '//*[@id="cprodukt_search"]'))
            )
        except:
            print('Swisslife Page not loaded')
        search_button = driver.find_element_by_xpath('//*[@id="cprodukt_search"]')

        #setzt isin in Suche ein
        search_button.send_keys(isin)
        time.sleep(2)

        #Drückt suchen
        press_button = driver.find_element_by_xpath('//*[@id="cprodukt_produkt_produkt_form"]/div[5]/button')
        press_button.click()
        time.sleep(2)

        isin = driver.find_element_by_xpath('//*[@id="cprodukt_produkt_produkt_search"]/div[4]/div[1]').text
        array.append(isin)

        date = driver.find_element_by_xpath('//*[@id="cprodukt_produkt_produkt_search"]/div[4]/div[10]').text
        array.append(date)
        time.sleep(2)
        print(date)

        nav = driver.find_element_by_xpath('//*[@id="cprodukt_produkt_produkt_search"]/div[4]/div[7]').text
        array.append(nav)
        time.sleep(2)
        array_list.append(array)
        search_button.clear()

    # erstellt Pandas Dataframe
    df_input_swisslife = pd.DataFrame(input_swisslife)
    df_input_swisslife.columns = ["ISIN", "Asset Name", "Manager"]
    df_input_swisslife = df_input_swisslife.drop(['Manager'], axis=1)

    # erstellt Pandas Dataframe für gecrawlte Daten
    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Date", "Bid Price"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    # merged die listen input_cs und array_list
    swisslife_merge = pd.merge(df_input_swisslife, df_array_list[["ISIN", "Date", "Bid Price", "Ask Price"]],
                         how="left", on='ISIN')

    #Check ob es NA hat
    if swisslife_merge.isnull().values.any():
        print("---------------------------------------------------------------------------swisslife hat NAs----------------------------")
    else:
        pass



    output_small_instruments = pd.DataFrame()
    output_small_instruments = output_small_instruments.append(ft_merge, ignore_index=True)
    output_small_instruments = output_small_instruments.append(csa25i_merge, ignore_index=True)
    output_small_instruments = output_small_instruments.append(bridgewater_merge, ignore_index=True)
    output_small_instruments = output_small_instruments.append(swisslife_merge, ignore_index=True)
    output_small_instruments = output_small_instruments.append(ishares_merge, ignore_index=True)

    output_small_instruments.to_csv("Output_DIV.csv", index=False, header=True, sep=";")
    print("--> Done ishares, CSA Private, Bridgemer, KGAST")
    driver.quit()

