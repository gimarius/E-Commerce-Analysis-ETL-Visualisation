import time as t
from datetime import *
from yahoofinancials import YahooFinancials
import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
import os
from selenium.webdriver.chrome.options import Options


def cs_fx():
    input_cs_cad = []
    input_cs_usd = []
    input_cs_jpy = []

    input_cs_cad.append(["CH0504896439", "CSIF (CH) I Equity Canada ESG Blue - Pension Fund ZB"])
    input_cs_usd.append(["CH0259132105", "CSIF (CH) Bond Government Emerging Markets USD Blue DB"])
    input_cs_usd.append(["CH0397628709", "CSIF (CH) III Equity US ESG Blue - Pension Fund ZB"])
    input_cs_jpy.append(["CH0490245260", "CSIF (CH) I Equity Japan Blue - Plus ZB"])
    input_cs_jpy.append(["CH0357515474", "CSIF (CH) I Equity Japan Blue - Pension Fund ZB"])


    range_cs_cad = len(input_cs_cad)
    range_cs_usd = len(input_cs_usd)
    range_cs_jpy = len(input_cs_jpy)


    df_cs_cad_input = pd.DataFrame(input_cs_cad)
    df_cs_cad_input.columns = ["ISIN", "Asset Name"]

    df_cs_usd_input = pd.DataFrame(input_cs_usd)
    df_cs_usd_input.columns = ["ISIN", "Asset Name"]

    df_cs_jpy_input = pd.DataFrame(input_cs_jpy)
    df_cs_jpy_input.columns = ["ISIN", "Asset Name"]

    #Yahoo Finance API CAD
    currencies = ['CHFCAD=X']
    yahoo_financials_currencies = YahooFinancials(currencies)

    today = datetime.today()
    yesterday = today - timedelta(days=20)
    date_to = str(today)
    date_to = date_to[0:10]
    date_from = str(yesterday)
    date_from = date_from[0:10]
    test = yahoo_financials_currencies.get_historical_price_data(date_from, date_to, "daily")
    test = test['CHFCAD=X']
    test_prices = test["prices"]

    list_date =[]

    for item in test_prices:
        date = item['formatted_date']
        date = date[8:11] + "." + date[5:7] +"."+ date[0:4]
        list_date.append(date)

    list_prices =[]

    for item in test_prices:
        list_prices.append(item['close'])

    df_cad = pd.DataFrame([list_date, list_prices, ])
    df_cad = df_cad.transpose()
    df_cad.columns = ['Date', 'Price']
    df_cad['Price'] = pd.to_numeric(df_cad['Price'])



    print(df_cad)
    #Yahoo Finance API USD
    currencies = ['CHFUSD=X']
    yahoo_financials_currencies = YahooFinancials(currencies)
    today = datetime.today()
    yesterday = today - timedelta(days=20)
    date_to = str(today)
    date_to = date_to[0:10]
    date_from = str(yesterday)
    date_from = date_from[0:10]
    test = yahoo_financials_currencies.get_historical_price_data(date_from, date_to, "daily")
    test = test['CHFUSD=X']
    test_prices = test["prices"]

    list_date =[]

    for item in test_prices:
        date = item['formatted_date']
        date = date[8:11] + "." + date[5:7] +"."+ date[0:4]
        list_date.append(date)

    list_prices =[]

    for item in test_prices:
        list_prices.append(item['close'])

    df_usd = pd.DataFrame([list_date, list_prices, ])
    df_usd = df_usd.transpose()
    df_usd.columns = ['Date', 'Price']
    df_usd['Price'] = pd.to_numeric(df_usd['Price'])
    print(df_usd)


    #Yahoo Finance API JPY
    currencies = ['CHFJPY=X']
    yahoo_financials_currencies = YahooFinancials(currencies)

    today = datetime.today()


    yesterday = today - timedelta(days=20)
    date_to = str(today)
    date_to = date_to[0:10]
    date_from = str(yesterday)
    date_from = date_from[0:10]


    test = yahoo_financials_currencies.get_historical_price_data(date_from, date_to, "daily")
    test = test['CHFJPY=X']
    test_prices = test["prices"]


    list_date =[]

    for item in test_prices:
        date = item['formatted_date']
        date = date[8:11] + "." + date[5:7] +"."+ date[0:4]
        list_date.append(date)

    list_prices =[]

    for item in test_prices:
        list_prices.append(item['close'])

    df_jpy = pd.DataFrame([list_date, list_prices, ])
    df_jpy = df_jpy.transpose()
    df_jpy.columns = ['Date', 'Price']
    df_jpy['Price'] = pd.to_numeric(df_jpy['Price'])
    print(df_jpy)


    ### Get prices
    path = os.path.abspath(os.path.join(os.path.normpath(os.path.dirname(__file__)), 'chromedriver.exe'))
    chrome_options = Options()
    chrome_options.add_argument("--headless")
    chrome_options.add_argument('log-level=3')

    driver = webdriver.Chrome(executable_path=path, options=chrome_options)


    driver.get("https://amfunds.credit-suisse.com/ch/de/institutional/fund/detail/CH0504896439")

    driver.add_cookie({'name': 'F.domicile', 'value': 'ch'})
    driver.add_cookie({'name': 'F.investortype', 'value': 'institutional'})

    select = driver.find_element_by_id("selInvestorTypePartTitle")
    select_button = select.find_element_by_id("selInvestorTypes_CH")
    select_option = select_button.find_element_by_xpath(".//option[2]")
    select_option.click()

    t.sleep(2)
    button = driver.find_elements_by_id('btnAccept')
    driver.execute_script("window.scrollTo(0,1500)")
    t.sleep(3)
    button[1].click()



    isin_list = []
    for element in input_cs_cad:
        isin = element[:1][:]
        isin = ' '.join(map(str, isin))
        isin_list.append(isin)

    array_list_cad = [[]]
    array_list_cad.clear()

    for m in range(0, range_cs_cad):
        array = []
        driver.get("https://amfunds.credit-suisse.com/ch/de/institutional/fund/detail/" + input_cs_cad[m][0])
        try:
            element = WebDriverWait(driver, 20).until(
                EC.presence_of_element_located(
                    (By.XPATH,
                     '//*[@id="tab_0"]/div/div[1]/div/div/table/tbody/tr[1]/td/table/tbody/tr[2]/td[1]/strong/span'))
            )
        except:
            print('CS Page not loaded')

        nav = driver.find_element_by_xpath(
            '//*[@id="tab_0"]/div/div[1]/div/div/table/tbody/tr[1]/td/table/tbody/tr[2]/td[1]/strong/span').text


        t.sleep(5)
        nav = float(nav)
        nav_date = driver.find_element_by_xpath(
            '//*[@id="tab_0"]/div/div[1]/div/div/table/tbody/tr[1]/td/table/tbody/tr[3]/td[1]/span').text
        nav_date = nav_date[2:-2]


        isin = driver.find_element_by_xpath('//*[@id="tab_0"]/div/div[3]/ul/li[1]/div[1]/dl/dd[4]/div').text
        print(nav)
        print(nav_date)


        if len(isin) != 12:
            isin = driver.find_element_by_xpath('//*[@id="tab_0"]/div/div[3]/ul/li[1]/div[1]/dl/dd[3]/div').text


        # rechnet Preis in neuer Währung um
        select_date = df_cad.loc[df_cad['Date'] == nav_date]
        nav = nav / select_date['Price']
        nav = nav.values
        nav = str(nav)
        nav = nav[1:-2]




        array.append(isin)
        array.append(nav)
        array.append(nav_date)
        array_list_cad.append(array)


    # erstellt Pandas Dataframe für gecrawlte Daten
    df_array_list = pd.DataFrame(array_list_cad)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    # merged die listen input_cs und array_list
    cs_merge_cad = pd.merge(df_cs_cad_input, df_array_list[["ISIN", "Bid Price", "Date", 'Ask Price']], how="left", on='ISIN')
    cs_merge_cad = cs_merge_cad
    cs_merge_cad = cs_merge_cad[['Asset Name', 'ISIN', 'Bid Price', 'Ask Price', 'Date']]

    # Check ob es NA hat
    if cs_merge_cad.isnull().values.any():
        print("-------------------------------------------------------------CS FX hat NAs----------------------------")
    else:
        pass
    isin_list = []
    for element in input_cs_usd:
        isin = element[:1][:]
        isin = ' '.join(map(str, isin))
        isin_list.append(isin)

    array_list = [[]]
    array_list.clear()

    for m in range(0, range_cs_usd):
        array = []
        driver.get("https://amfunds.credit-suisse.com/ch/de/institutional/fund/detail/" + input_cs_usd[m][0])
        try:
            element = WebDriverWait(driver, 20).until(
                EC.presence_of_element_located(
                    (By.XPATH,
                     '//*[@id="tab_0"]/div/div[1]/div/div/table/tbody/tr[1]/td/table/tbody/tr[2]/td[1]/strong/span'))
            )
        except:
            print('CS Page not loaded')

        nav = driver.find_element_by_xpath(
            '//*[@id="tab_0"]/div/div[1]/div/div/table/tbody/tr[1]/td/table/tbody/tr[2]/td[1]/strong/span').text

        t.sleep(5)

        nav = float(nav)

        nav_date = driver.find_element_by_xpath(
            '//*[@id="tab_0"]/div/div[1]/div/div/table/tbody/tr[1]/td/table/tbody/tr[3]/td[1]/span').text
        nav_date = nav_date[2:-2]

        isin = driver.find_element_by_xpath('//*[@id="tab_0"]/div/div[3]/ul/li[1]/div[1]/dl/dd[4]/div').text



        print(nav)
        print(nav_date)

        select_date = df_usd.loc[df_usd['Date'] == nav_date]
        nav = nav / select_date['Price']
        nav = nav.values
        nav = str(nav)
        nav = nav[1:-2]


        array.append(isin)
        array.append(nav)
        array.append(nav_date)
        array_list.append(array)

    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']


    # merged die listen input_cs und array_list
    cs_merge_usd = pd.merge(df_cs_usd_input, df_array_list[["ISIN", "Bid Price", "Date", 'Ask Price']], how="left", on='ISIN')
    cs_merge_usd = cs_merge_usd
    cs_merge_usd = cs_merge_usd[['Asset Name', 'ISIN', 'Bid Price', 'Ask Price', 'Date']]

    # Check ob es NA hat
    if cs_merge_usd.isnull().values.any():
        print("-------------------------------------------------------------CS FX hat NAs----------------------------")
    else:
        pass

    isin_list = []
    for element in input_cs_jpy:
        isin = element[:1][:]
        isin = ' '.join(map(str, isin))
        isin_list.append(isin)

    array_list = [[]]
    array_list.clear()

    for m in range(0, range_cs_jpy):
        array = []
        driver.get("https://amfunds.credit-suisse.com/ch/de/institutional/fund/detail/" + input_cs_jpy[m][0])
        try:
            element = WebDriverWait(driver, 20).until(
                EC.presence_of_element_located(
                    (By.XPATH,
                     '//*[@id="tab_0"]/div/div[1]/div/div/table/tbody/tr[1]/td/table/tbody/tr[2]/td[1]/strong/span'))
            )
        except:
            print('CS Page not loaded')

        nav = driver.find_element_by_xpath(
            '//*[@id="tab_0"]/div/div[1]/div/div/table/tbody/tr[1]/td/table/tbody/tr[2]/td[1]/strong/span').text

        t.sleep(2)


        nav = float(nav)


        nav_date = driver.find_element_by_xpath(
            '//*[@id="tab_0"]/div/div[1]/div/div/table/tbody/tr[1]/td/table/tbody/tr[3]/td[1]/span').text
        nav_date = nav_date[2:-2]


        print(nav)
        print(nav_date)

        isin = driver.find_element_by_xpath('//*[@id="tab_0"]/div/div[3]/ul/li[1]/div[1]/dl/dd[4]/div').text

        if len(isin) != 12:
            isin = driver.find_element_by_xpath('//*[@id="tab_0"]/div/div[3]/ul/li[1]/div[1]/dl/dd[3]/div').text



        select_date = df_jpy.loc[df_jpy['Date'] == nav_date]
        nav = nav / select_date['Price']
        nav = nav.values
        nav = str(nav)
        nav = nav[1:-2]
        print(nav)

        array.append(isin)
        array.append(nav)
        array.append(nav_date)
        array_list.append(array)

    driver.quit()

    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']


    # merged die listen input_cs und array_list
    cs_merge_jpy = pd.merge(df_cs_jpy_input, df_array_list[["ISIN", "Bid Price", "Date", 'Ask Price']], how="left", on='ISIN')
    cs_merge_jpy = cs_merge_jpy
    cs_merge_jpy = cs_merge_jpy[['Asset Name', 'ISIN', 'Bid Price', 'Ask Price', 'Date']]

    # Check ob es NA hat
    if cs_merge_jpy.isnull().values.any():
        print("-------------------------------------------------------------CS FX hat NAs----------------------------")
    else:
        pass



    # all output to CSV
    output_cs_fx = pd.DataFrame()
    output_cs_fx = output_cs_fx.append(cs_merge_cad, ignore_index=True)
    output_cs_fx = output_cs_fx.append(cs_merge_usd, ignore_index=True)
    output_cs_fx = output_cs_fx.append(cs_merge_jpy, ignore_index=True)
    output_cs_fx.to_csv("Output_CS_FX.csv", index=False, header=True, sep=";")
    print("--> Done CS FX")
