from selenium import webdriver
import pandas as pd
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import os
from selenium.webdriver.chrome.options import Options


def tramondo():
    input_tramondo = []
    input_tramondo.append(["LI0517411372", "Tramondo Funds - Dynamic Equity Opportunities (CHF) I1", "41186"])
    input_tramondo.append(["LI0517411141", "Tramondo Funds - Credit Opportunities (CHF) F", "41167"])
    input_tramondo.append(["LI0456069959", "Tramondo Funds - GreyJung Global Opportunities (CHF) F", "38919"])
    input_tramondo.append(["LI0419018309", "Tramondo Generation Fund - Next Generation (USD) I", "37459"])
    input_tramondo.append(["LI0419018317", "Tramondo Generation Fund - Next Generation (CHF) I", "41727"])

    df_tramondo = pd.DataFrame(input_tramondo)
    df_tramondo.columns = ["ISIN", "Asset Name", "Abbrev"]
    df_tramondo.drop(["Abbrev"], axis=1)


    path = os.path.abspath(os.path.join(os.path.normpath(os.path.dirname(__file__)), 'chromedriver.exe'))
    chrome_options = Options()
    chrome_options.add_argument("--headless")
    chrome_options.add_argument('log-level=3')
    chrome_options.add_argument('--window-size=1920,1080')  
    driver = webdriver.Chrome(executable_path=path, options=chrome_options)


    # Anzahl der Instrumente wird in Variable gespeichert
    number_input_tramondo = len(input_tramondo)

    driver.get("https://www.lafv.li/DE/Funds/FundList/FundDetails?ID=41186")



    Land = driver.find_element_by_xpath('//*[@id="ctl00_G_cmbDisclaimerLanguage_cb_Arrow"]')
    Land.click()

    time.sleep(2)


    # field_land = driver.find_element_by_xpath('//*[@id="ctl00_G_cmbDisclaimerLanguage_cb_Input"]')
    # field_land.send_keys('Schweiz')

    schweiz = driver.find_element_by_xpath('//*[@id="ctl00_G_cmbDisclaimerLanguage_cb_DropDown"]/div/ul/li[177]/ul/li')
    schweiz.click()

    agree = driver.find_element_by_xpath('//*[@id="ctl00_G_btnAgree"]')
    agree.click()


    array_list = [[]]
    array_list.clear()

    # Look for data according to the web structure
    # Morningstar
    for m in range(0, number_input_tramondo):
        array = []
        driver.get("https://www.lafv.li/DE/Funds/FundList/FundDetails?ID=" + input_tramondo[m][2])

        try:
            element = WebDriverWait(driver, 20).until(
                EC.presence_of_element_located(
                    (By.XPATH,
                     '//*[@id="tab-1"]/div/div[1]/table/tbody/tr[2]/td[2]'))
            )
        except:
            print('Morningstar Page not loaded')

        # ISIN
        isin = driver.find_element_by_xpath('//*[@id="tab-1"]/div/div[1]/table/tbody/tr[2]/td[2]').text
        #print(isin)



        # NAV
        nav = driver.find_element_by_xpath('//*[@id="tab-1"]/div/div[2]/table[1]/tbody/tr[2]/td[2]').text
        nav = nav[4:]
        nav = nav.replace("'","")
        nav = nav.replace(",", ".")

        #print(nav)

        # DATE
        date = driver.find_element_by_xpath('//*[@id="ctl00_G_labelDate"]').text
        #print(date)

        array.append(isin)
        array.append(nav)
        array.append(date)
        array_list.append(array)
        #print(input_tramondo[m][1])




    # erstellt Pandas Dataframe für gecrawlte Daten
    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']


    # merged die listen input_cs und array_list
    tramondo_merge = pd.merge(df_tramondo, df_array_list[["ISIN", "Bid Price", 'Ask Price', "Date"]],
                                    how="left", on='ISIN')


    if tramondo_merge.isnull().values.any():
        print(
            "------------------------------------------------------------------Tramondo hat NAs----------------------------")
    else:
        # all output to CSV
        tramondo_merge.to_csv("Output_Tramondo.csv", index=False, header=True, sep=";")
        print("--> Done Tramondo")

    driver.quit()

