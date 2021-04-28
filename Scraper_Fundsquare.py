from selenium import webdriver
import pandas as pd
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import os
from selenium.webdriver.chrome.options import Options


def fundsquare():

    input_fundsquare= []
    input_fundsquare.append(["LU1296765586", "PPF - LPActive Value Fund - CHF", "232848"])
    input_fundsquare.append(["LU0641442941", "PPF ll - Carnot Efficient Resources B CHF", "100007"])
    input_fundsquare.append(["LU0974744780", "R Wealth Management SICAV SIF Mosaique Balanced CHF", "178243"])
    input_fundsquare.append(["LU1255011501", "JPM China A-Share Opportunities C (acc) - USD", "220799"])
    input_fundsquare.append(["LU1295556705", "Capital Group New Perspective Fund (LUX) Zh-CHF", "225910"])
    input_fundsquare.append(["LU1861216197", "BGF Next Generation Technology Fund D2 USD Cap", "317505"])
    input_fundsquare.append(["LU0724618946", "BGF World Technology Fd D2 USD C", "102472"])
    input_fundsquare.append(["LU2134542260", "BGF World Technology Fund Class I2 USD", "363446"])
    input_fundsquare.append(["LU1172942424", "FvS Multiple Opportunities II CHF-IT", "212665"])
    input_fundsquare.append(["LU1731833056", "FIDELITY GLOBAL DIVIDEND FUND Y-ACC-EURO", "307323"])
    input_fundsquare.append(["LU2001262380", "2Xideas UCITS - Global Mid Cap Library Fund S CHF Hedged", "347016"])
    input_fundsquare.append(["LU0765121677", "Fundsmith SICAV - Fundsmith Equity Fund", "107140"])
    input_fundsquare.append(["LU1240786365", "UBS (Lux) Equity SICAV - Global High Dividend (USD) (CHF hedged) Q-dist", "113316"])
    input_fundsquare.append(["LU0330109744", "Multiflex SICAV - Carnot Efficient Energy Fund - B", "56501"])
    input_fundsquare.append(["LU1245471724", "Flossbach von Storch Bond Opportunities - CHF-IT", "221885"])
    input_fundsquare.append(["LU0569863599", "UBAM - GLOBAL HIGH YIELD SOLUTION Class IH", "87451"])
    input_fundsquare.append(["LU2273796776", "GKB (LU) Wandelanleihen Global ESG NH CHF", "386301"])
    input_fundsquare.append(["IE00BMCM9M24", "Twelve Insurance Fixed Income Fund - S CHF Acc. (ACH171)", "382095"])
    input_fundsquare.append(["LU1399444378", "BlueOrchard Microfinance Fund Class D", "248426"])
    input_fundsquare.append(["LU1212745829", "DCM Systematic Fund SICAV-SIF - Diversified Alpha Class Z USD", "306733"])
    input_fundsquare.append(["LU1857274911", "BlueOrchard UCITS - SDG Impact Bond Fund - I Class CHF", "317469"])
    input_fundsquare.append(["LU1388908649", "AXA WF Global Green Bonds Fonds F (Hedged) Capitalisation CHF", "240952"])
    input_fundsquare.append(["LU1869434909", "DB Platinum Quantica Managed Futures I1C-C CHF", '332905'])
    input_fundsquare.append(["IE00BKX58072", "Magna Emerging Markets Dividend Fund R Acc CHF", '235113'])
    input_fundsquare.append(["LU1815336687", "Threadneedle (Lux) Global Smaller Companies ZU USD", "308276"])
    input_fundsquare.append(["LU0042381250", "Morgan Stanley US Growth Fund (USD) I", "12458"])
    input_fundsquare.append(["LU0589470672", "BlackRock Global Funds - World Energy Fund A2 CHF Hedged", "90593"])
    input_fundsquare.append(["LU1811047833", "Bellevue Funds (Lux) - BB Adamant Digital Health B CHF", "303971"])
    input_fundsquare.append(["LU1698898050", "Allianz Global Artificial Intelligence RT USD", "286739"])
    input_fundsquare.append(["LU1747711031", "DWS Invest ESG Equity Income", "297061"])
    input_fundsquare.append(["LU1074209914", "Aviva Investors - Multi-Strategy Target Return Fund Ih CHF", "218231"])
    input_fundsquare.append(["LU1254422691", "Deutsche Concept Kaldemorgen CHF FCH", "227119"])
    input_fundsquare.append(["LU0985193357", "Ethna-DYNAMISCH (SIA-A)", "186507"])
    input_fundsquare.append(["LU1306424034", "JPM Global Macro Opportunities C (acc) CHF (hedged)", "228500"])
    input_fundsquare.append(["LU0818795329", "Multicooperation SICAV - Julius Baer Strategy Balanced (CHF) (UCITS)", "147400"])
    input_fundsquare.append(["LU0818796053", "Multicooperation SICAV - Julius Baer Strategy Growth (CHF) (UCITS)", "147450"])
    input_fundsquare.append(["LU0818796483", "Multicooperation SICAV - Julius Baer Strategy Income (CHF) (UCITS)", "147393"])
    input_fundsquare.append(["LU0772962550", "Nordea 1 – Stable Return HBI CHF", "196164"])
    input_fundsquare.append(["LU2273796776", "GKB (LU) Wandelanleihen Global ESG NH CHF", "386301"])


    df_input_fundquare = pd.DataFrame(input_fundsquare)
    df_input_fundquare.columns = ["ISIN", "Asset Name", "Abbrev"]
    df_input_fundquare.drop(["Abbrev"], axis=1)

    # Webdriver wird gestartet
    path = os.path.abspath(os.path.join(os.path.normpath(os.path.dirname(__file__)), 'chromedriver.exe'))
    chrome_options = Options()
    chrome_options.add_argument("--headless")
    chrome_options.add_argument('log-level=3')
    driver = webdriver.Chrome(executable_path=path, options=chrome_options)


    #path = 'chromedriver.exe'
    #driver = webdriver.Chrome(executable_path=path)

    driver.set_window_size(1400, 1200)

    number_input_fundsquare = len(input_fundsquare)

    array_list = [[]]
    array_list.clear()

    for m in range(0, number_input_fundsquare):
        array = []
        driver.get('https://www.fundsquare.net/security/summary?idInstr=' + input_fundsquare[m][2])



        time.sleep(1.5)
        try:
            element = WebDriverWait(driver, 40).until(
                EC.presence_of_element_located(
                    (By.XPATH, '//*[@id="content"]/table[1]/tbody/tr/td[1]/span'))
            )
        except:
            print('Fundsquare Page not loaded')





        isin = driver.find_element_by_xpath('//*[@id="content"]/table[1]/tbody/tr/td[1]/span').text
        array.append(isin)
        try:
            nav = driver.find_element_by_xpath('//*[@id="content"]/table[2]/tbody/tr/td[3]/span[1]').text
        except:
            nav = driver.find_element_by_xpath('//*[@id="content"]/table[2]/tbody/tr/td[2]').text






        if 'Unavailable' in nav:

            try:
                nav = driver.find_element_by_xpath('//*[@id="content"]/table[3]/tbody/tr/td[3]/span[1]').text
            except:
                pass
                #nav = driver.find_element_by_xpath('//*[@id="content"]/table[3]/tbody/tr/td[3]').text

            try:
                driver.find_element_by_xpath('//*[@id="content"]/table[3]/tbody/tr/td[2]')
                nav = nav[:-4]
                nav = nav.replace(" ", "")
                date = driver.find_element_by_xpath('//*[@id="content"]/table[3]/tbody/tr/td[2]').text
                date = date.replace("/", ".")
            except:
                nav = 'No Price'
                date = 'No Date'


            if 'Unavailable' in nav:
                nav = 'No Price'
                date = 'No Date'


            
        else:
            nav = nav[:-4]
            nav = nav.replace(" ", "")
            date = driver.find_element_by_xpath('//*[@id="content"]/table[2]/tbody/tr/td[2]').text
            date = date.replace("/", ".")

        print(nav)
        print(date)

        array.append(nav)
        array.append(date)
        array_list.append(array)

    driver.quit()


    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    fundsquare_merge = pd.merge(df_input_fundquare, df_array_list[["ISIN", "Bid Price", 'Ask Price', "Date"]],
                                    how="left", on='ISIN')

    if fundsquare_merge.isnull().values.any():
        print(
            "------------------------------------------------------------------Fundsquare hat NAs----------------------------")
        na = fundsquare_merge[fundsquare_merge.isna().any(axis=1)]
        print(na)
    else:
        # all output to CSV
        fundsquare_merge.to_csv("Output_Fundsquare.csv", index=False, header=True, sep=";")
        print("--> Done fundsquare")


