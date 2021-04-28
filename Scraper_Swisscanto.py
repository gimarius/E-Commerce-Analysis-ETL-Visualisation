from selenium import webdriver
import pandas as pd
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException
import time
import os
from selenium.webdriver.chrome.options import Options


def swisscanto():
    input_swisscanto_new = []
    input_swisscanto_new.append(["CH0117052511", "Swisscanto (CH) Index Bond Fund Corp. World hedged CHF NTH CHF"])
    input_swisscanto_new.append(["CH0117045317", "Swisscanto (CH) Index Bond Fund World (ex CHF) Govt. hedged CHF"])
    input_swisscanto_new.append(["CH0398970274", "Swisscanto (CH) Index Bond Fund Emerging Markets Hard Currency"])
    input_swisscanto_new.append(["CH0117052420", "Swisscanto (CH) Index Bond Fund Corp. USD"])
    input_swisscanto_new.append(["CH0192306469", "Swisscanto (CH) Index Equity Fund Switzerland Total (II)"])
    input_swisscanto_new.append(["CH0117044948", "Swisscanto (CH) IPF I Index Equity Fund World ex CH NT CHF"])
    input_swisscanto_new.append(["CH0296590281", "Swisscanto (CH) IPF I Index Equity Fund World ex CH NTH CHF"])
    input_swisscanto_new.append(["CH0267153598", "Swisscanto (CH) IPF I Index Equity Fund Small Cap World ex CH NT CHF"])
    input_swisscanto_new.append(["CH0117044971", "Swisscanto (CH) Index Equity Fund Emerging Markets NT CHF"])
    input_swisscanto_new.append(["CH0117044658", "Swisscanto (CH) Index Equity Fund Europe ex CH"])
    input_swisscanto_new.append(["CH0300249510", "Swisscanto (CH) IPF I Index Equity Fund USA NTH CHF"])
    input_swisscanto_new.append(["CH0117044732", "Swisscanto (CH) IPF I Index Equity Fund USA NT USD"])
    input_swisscanto_new.append(["CH0117044831", "Swisscanto (CH) Index Equity Fund Pacific ex Japan"])
    input_swisscanto_new.append(["CH0215804730", "Swisscanto (CH) IPF I Index Real Estate Fund North America indirect NT CHF"])
    input_swisscanto_new.append(["CH0117052586", "Swisscanto (CH) Index Real Estate Fund Europe (ex CH) indirect NT CHF"])
    input_swisscanto_new.append(["CH0117052669", "Swisscanto (CH) Index Real Estate Fund Asia indirect NT CHF"])
    input_swisscanto_new.append(["CH0215804755", "Swisscanto (CH) IPF I Index Equity Fund World (ex CH) Responsible NT CHF"])
    input_swisscanto_new.append(["CH0293345648", "Swisscanto (CH) IPF I Index Equity Fund World (ex CH) Responsible NTH CHF"])
    input_swisscanto_new.append(["CH0117045077", "Swisscanto (CH) Index Bond Fund Total Market AAA-BBB Domestic CHF NT CHF"])
    input_swisscanto_new.append(["CH0117045127", "Swisscanto (CH) Index Bond Fund Total Market AAA-BBB Foreign CHF NT CHF"])
    input_swisscanto_new.append(["CH0132501898", "Swisscanto (CH) Index Equity Fund Small & Mid Caps Switzerland NT CHF"])
    input_swisscanto_new.append(["CH0117044906", "Swisscanto (CH) Index Equity Fund World ex CH NT CHF "])
    input_swisscanto_new.append(["CH0324760229", "Swisscanto (CH) Index Equity Fund Small Cap World ex CH NT CHF"])
    input_swisscanto_new.append(["CH0117052545", "Swisscanto (CH) Index Real Estate Fund Switzerland indirect NT CHF"])
    input_swisscanto_new.append(["CH0402422031", "Swisscanto (CH) Index Precious Metal Fund Gold Physical GT CHF"])
    input_swisscanto_new.append(["LU0899937766", "Swisscanto (LU) Bond Fund Global Convertible GTH CHF"])
    input_swisscanto_new.append(["CH0330999142", "Swisscanto (CH) Index Bond Fund Total Market AAA-BBB 1-5 CHF GT CHF"])
    input_swisscanto_new.append(["CH0192252580", "Swisscanto AST Avant Aktien USA Index GT CHF"])
    input_swisscanto_new.append(["CH0192241294", "Swisscanto (CH) Bond Fund Convertible International hedged CHF GTH CHF"])
    input_swisscanto_new.append(["CH0315623048", "Swisscanto (CH) Index Real Estate Fund Europe (ex CH) indirect FA CHF"])
    input_swisscanto_new.append(["CH0215804714", "Swisscanto (CH) Index Equity Fund Large Caps Switzerland NT CHF"])
    input_swisscanto_new.append(["CH0246588211", "Swisscanto (CH) Index Precious Metal Fund Gold Physical NT CHF"])
    input_swisscanto_new.append(["CH0215803898", "Swisscanto (CH) Index Bond Fund Total Market AAA-BBB 1-5 CHF NT CHF"])
    input_swisscanto_new.append(["CH0427242083", "Swisscanto (CH) Money Market Fund Responsible Opportunities CHF DT CHF"])
    input_swisscanto_new.append(["CH0192252689", "Swisscanto AST Avant BVG Responsible Portfolio 45 P"])


    df_input_swisscanto_new = pd.DataFrame(input_swisscanto_new)
    df_input_swisscanto_new.columns = ["ISIN", "Asset Name"]


    path = os.path.abspath(os.path.join(os.path.normpath(os.path.dirname(__file__)), 'chromedriver.exe'))
    chrome_options = Options()
    chrome_options.add_argument("--headless")
    chrome_options.add_argument('log-level=3')
    chrome_options.add_argument('--window-size=1920,1080')  
    driver = webdriver.Chrome(executable_path=path, options=chrome_options)

    # Anzahl der Instrumente wird in Variable gespeichert
    number_input_swisscanto_new = len(input_swisscanto_new)

    driver.get("https://products.swisscanto.com/products/product?lang=de")

    try:
        button = driver.find_element_by_xpath('//*[@id="btn-simple-accept"]')
        button.click()
    except NoSuchElementException:
        pass

    Land = driver.find_element_by_xpath('//*[@id="field-location"]')
    Land.click()

    schweiz = driver.find_element_by_xpath('//*[@id="field-location"]/option[2]')
    schweiz.click()

    kundensegment = driver.find_element_by_xpath('//*[@id="field-segment"]')
    kundensegment.click()

    institutional = driver.find_element_by_xpath('//*[@id="field-segment"]/option[3]')
    institutional.click()

    button_accept = driver.find_element_by_xpath(
        '//*[@id="top"]/div/div[2]/div/div/div/form/fieldset[2]/ol/li[1]/p/label/button/span[1]')
    button_accept.click()

    bestätigen_button = driver.find_element_by_xpath('//*[@id="acceptDisclaimerButton"]')
    bestätigen_button.click()

    array_list = [[]]
    array_list.clear()

    # Look for data according to the web structure
    for m in range(0, number_input_swisscanto_new):
        array = []
        driver.get('https://products.swisscanto.com/products/product/' + input_swisscanto_new[m][0])



        try:
            element = WebDriverWait(driver, 40).until(
                EC.presence_of_element_located(
                    (By.XPATH, '//*[@id="top"]/div/div/div/div[2]/div[1]/div/p[1]/span[2]'))
            )
        except:
            print('Swisscanto Page not loaded')

        isin = driver.find_element_by_xpath('//*[@id="top"]/div/div/div/div[2]/div[1]/div/p[1]/span[2]').text
        array.append(isin)
        nav = driver.find_element_by_xpath('//*[@id="top"]/div/div/div/div[2]/div[1]/div/p[4]/span[2]').text
        nav = nav[4:]
        array.append(nav)

        date = driver.find_element_by_xpath('/html/body/div/div/div/div[2]/div[1]/div/p[6]/span[2]').text
        array.append(date)
        array_list.append(array)

    driver.quit()

    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    swisscanto_new_merge = pd.merge(df_input_swisscanto_new, df_array_list[["ISIN", "Bid Price", 'Ask Price', "Date"]],
                                    how="left", on='ISIN')

    if swisscanto_new_merge.isnull().values.any():
        print(
            "------------------------------------------------------------------Swisscanto new hat NAs----------------------------")
    else:
        # all output to CSV
        swisscanto_new_merge.to_csv("Output_Swisscanto.csv", index=False, header=True, sep=";")
        print("--> Done Swisscanto")

