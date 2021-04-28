from selenium import webdriver
import time
from bs4 import BeautifulSoup
import pandas as pd
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import os
from selenium.webdriver.chrome.options import Options


def morningstar_CH():
    input_morningstar = []
    input_morningstar.append(["LI0229057075", "Solitaire Global Bond Fund", "F00000SKS3"])
    input_morningstar.append(["GG00BD8YWG40", "HSBC Portfolio Selection GH CHF Hedged", "F000010SW4"])
    input_morningstar.append(["CH0127799937", "SGKB (CH) Fund - Finreon Swiss Equity IsoPro (CHF) - C", "F00000NDIM"])
    input_morningstar.append(["CH0343986524", "Finreon Tail Risk Control (World) - V1H", "F00000YDL4"])
    input_morningstar.append(["CH0343985989", "Finreon Fixed Income Risk Control - V1H", "F00000YD6N"])
    input_morningstar.append(["IE00BWC52G65", "PIMCO Short-Term High Yield Corporate Bond Index Source UCITS ETF (CHF hedged)", "0P00015QQ6"])
    input_morningstar.append(["CH0209106761", "CSIF (CH) II Gold Blue DB (CHF)", "0P00011ULP"])
    input_morningstar.append(["CH0214968403", "CSIF (CH) III Equity World ex CH Small Cap Blue - Pension Fund QBH", "F00000VD3J"])
    input_morningstar.append(["IE00BKX55Q28", "Vanguard FTSE 250 UCITS ETF", "0P0001AD1O"])
    input_morningstar.append(["IE00BZ0G2K67", "Man AHL Diversified plc DN H CHF", "F00000XX8Z"])
    input_morningstar.append(["IE00B8KGV557", "iShares Edge MSCI EM Minimum Volatility UCITS ETF", "0P00010223"])
    input_morningstar.append(["IE00B4X9L533", "HSBC MSCI WORLD UCITS ETF", "0P0000SYGA"])


    df_input_morningstar = pd.DataFrame(input_morningstar)
    df_input_morningstar.columns = ["ISIN", "Asset Name", "Code"]
    df_input_morningstar = df_input_morningstar.drop(["Code"], axis=1)

    # Webdriver wird gestartet
    path = os.path.abspath(os.path.join(os.path.normpath(os.path.dirname(__file__)), 'chromedriver.exe'))
    chrome_options = Options()
    chrome_options.add_argument("--headless")
    chrome_options.add_argument('log-level=3')
    chrome_options.add_argument('--window-size=1920,1080')  


    driver = webdriver.Chrome(executable_path=path, options=chrome_options)

    driver.set_window_size(1400,1200)

    # Anzahl der Instrumente wird in Variable gespeichert
    number_input_morningstar = len(input_morningstar)




    driver.get('http://www.morningstar.ch')
    driver.get("http://www.morningstar.ch/ch/funds/snapshot/snapshot.aspx?id=" + input_morningstar[0][2])
    #check ob Seite geladen
    try:
        element = WebDriverWait(driver, 40).until(
            EC.presence_of_element_located(
                (By.XPATH, '//*[@id="finaprofessional"]/span[1]'))
        )
    except:
        print('Morningstar Page not loaded')

    try:
        select_pro = driver.find_element_by_xpath('//*[@id="finaprofessional"]/span[1]')
        select_pro.click()
        select_accept = driver.find_element_by_xpath('//*[@id="_evidon-accept-button"]')
        select_accept.click()
    except NoSuchElementException:
        pass

    # Erstellt leere verschachtelte Liste
    array_list = [[]]
    array_list.clear()

    time.sleep(1)

    #Morningstar
    for m in range(0, number_input_morningstar):
        array = []
        driver.get("http://www.morningstar.ch/ch/funds/snapshot/snapshot.aspx?id=" + input_morningstar[m][2])

        try:
            element = WebDriverWait(driver, 20).until(
                EC.presence_of_element_located(
                    (By.XPATH,
                     '//*[@id="overviewQuickstatsDiv"]/table/tbody/tr[2]/td[1]'))
            )
        except:
            print('Morningstar Page not loaded')

        s_csm = BeautifulSoup(driver.page_source, 'html.parser')

        try:
            isin = driver.find_element_by_xpath('//*[@id="overviewQuickstatsDiv"]/table/tbody/tr[6]/td[3]').text
        except:
            isin = driver.find_element_by_xpath('//*[@id="overviewQuickstatsDiv"]/table/tbody/tr[8]/td[3]').text


        len_isin = len(isin)
        if len_isin != 12:
             isin = driver.find_element_by_xpath('//*[@id="overviewQuickstatsDiv"]/table/tbody/tr[8]/td[3]').text




        temp_nav = s_csm.find_all('td', {"class": "line text"})
        nav = temp_nav[0].text.strip()
        final_nav = nav[-8:-3].strip().replace(u'\xa0', '').replace("C", "").replace("H", "").replace("F", "").replace("U",
                                                                                                                       "").replace(
            "S", "").replace("D", "") + "." + nav[-2:]
        if final_nav == "":
            final_nav = driver.find_elements_by_xpath('/html/body/div[3]/div[3]/div/div/div/div[1]/div/div[2]/div[1]/div[3]/div/table/tbody/tr[2]/td[3]').text
        #print(final_nav)
        temp_nav_date = s_csm.find_all('span', {"class": "heading"})
        nav_date = temp_nav_date[0].text.strip()

        array.append(isin)
        array.append(final_nav)
        array.append(nav_date)
        array_list.append(array)


    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    morningstar_merge = pd.merge(df_input_morningstar, df_array_list[["ISIN", "Bid Price", "Date", 'Ask Price']], how="left", on='ISIN')

    if morningstar_merge.isnull().values.any():
        print("---------------------------------------------------------------------------morningstar ch hat NAs----------------------------")
        na = morningstar_merge[morningstar_merge.isna().any(axis=1)]
        print(na)
    else:
        morningstar_merge.to_csv("Output_Morningstar_CH.csv", index=False, header=True, sep=";")
        print("--> Done morningstar CH")

    driver.quit()

