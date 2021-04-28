from selenium import webdriver
import time
import pandas as pd
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import os
from selenium.webdriver.chrome.options import Options


def fundinfo():
    input_fundinfo2 = []

    input_fundinfo2.append(["CH0225350161", "Albin Kistler Obligationen CHF I"])
    input_fundinfo2.append(["CH0225351110", "Albin Kistler Obligationen FW I"])
    input_fundinfo2.append(["CH0225350062", "Albin Kistler Aktien Schweiz I"])
    input_fundinfo2.append(["CH0225350112", "Albin Kistler Aktien Welt I"])
    input_fundinfo2.append(["CH0047710022", "UBS (CH) Institutional Fund 2 - Global Real Estate Securities Passive (CHF hedged) II I-A1"])





    #erstellt Pandas Dataframe für input_cs
    df_input_fundinfo2 = pd.DataFrame(input_fundinfo2)
    df_input_fundinfo2.columns = ["ISIN", "Asset Name"]


    # Webdriver wird gestartet
    path = os.path.abspath(os.path.join(os.path.normpath(os.path.dirname(__file__)), 'chromedriver.exe'))
    chrome_options = Options()
    chrome_options.add_argument("--headless")
    chrome_options.add_argument("--log-level=3")
    driver = webdriver.Chrome(executable_path=path, options=chrome_options)


    driver.set_window_size(1400,1200)

    # Anzahl der Instrumente wird in Variable gespeichert
    number_input_fundinfo2 = len(input_fundinfo2)


    driver.get("https://www.fundinfo.com")
    driver.add_cookie({'name': 'DU', 'value': 'CH-prof'})
    driver.add_cookie({'name': 'PrivacyPolicy', 'value': 'true'})

    try:
        accept_all_button = driver.find_element_by_xpath('//*[@id="qc-cmp2-ui"]/div[2]/div/button[2]')
        accept_all_button.click()
    except:
        pass

    try:
        accept_all_button = driver.find_element_by_xpath('//*[@id="qc-cmp2-ui"]/div[2]/div/button[2]')
        accept_all_button.click()
    except:
        pass

    array_list = [[]]
    array_list.clear()

    #Look for data according to the web structure
    for m in range(0, number_input_fundinfo2):
        driver.get("https://www.fundinfo.com/en/CH-prof/LandingPage?query=" + input_fundinfo2[m][0])

        array = []

        #Check ob Page geladen hat
        try:
            element = WebDriverWait(driver, 10).until(
                EC.presence_of_element_located(
                    (By.XPATH, "/html/body/div[2]/div/div[2]/div[1]/div/div[2]/ul/li/div/div[1]/div[2]/a"))
            )
        except:
            driver.refresh()


        try:
            accept_all_button = driver.find_element_by_xpath('//*[@id="qc-cmp2-ui"]/div[2]/div/button[2]')
            accept_all_button.click()
        except:
            pass


        try:
            accept_all_button = driver.find_element_by_xpath('//*[@id="qc-cmp2-ui"]/div[2]/div/button[2]')
            accept_all_button.click()
        except:
            pass

        #Check ob Page geladen hat
        try:
            element = WebDriverWait(driver, 3).until(
                EC.presence_of_element_located(
                    (By.XPATH, "/html/body/div[1]/div/div[2]/div[1]/div/div[2]/ul/li/div/div[1]/div[2]/a"))
            )
        except:
            element = WebDriverWait(driver, 1).until(
                EC.presence_of_element_located(
                    (By.XPATH, "/html/body/div[2]/div/div[2]/div[1]/div/div[2]/ul/li/div/div[1]/div[2]/a"))
            )



        try:    
            nav_text = driver.find_element_by_xpath('/html/body/div[1]/div/div[2]/div[1]/div/div[2]/ul/li/div/div[1]/div[3]/div[1]/span[1]').text
            nav_text = nav_text[:-4]
        except:
            nav_text = driver.find_element_by_xpath('/html/body/div[2]/div/div[2]/div[1]/div/div[2]/ul/li/div/div[1]/div[3]/div[1]/span[1]').text
            nav_text = nav_text[:-4]


        if ',' in nav_text:
            nav_text = nav_text.replace(',', '')

        try:
            date_nav = driver.find_element_by_xpath('/html/body/div[1]/div/div[2]/div[1]/div/div[2]/ul/li/div/div[1]/div[3]/div[1]/span[2]').text
            date_nav = date_nav[-2:] + "." + date_nav[-5:-3] + "." + date_nav[-10:-6]
        except:
            date_nav = driver.find_element_by_xpath('/html/body/div[2]/div/div[2]/div[1]/div/div[2]/ul/li/div/div[1]/div[3]/div[1]/span[2]').text
            date_nav = date_nav[-2:] + "." + date_nav[-5:-3] + "." + date_nav[-10:-6]



        array.append(nav_text)
        array.append(date_nav)
        array_list.append(array)


    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']


    fundinfo2_merge = df_input_fundinfo2
    fundinfo2_merge['Bid Price'] = df_array_list['Bid Price']
    fundinfo2_merge['Ask Price'] = df_array_list['Ask Price']
    fundinfo2_merge['Date'] = df_array_list['Date']


    if fundinfo2_merge.isnull().values.any():
        print("------------------------------------------------------------------fundinfo 2 hat NAs----------------------------")
    else:
        fundinfo2_merge.to_csv("Output_Fundinfo.csv", index=False, header=True, sep=";")
        print("--> Done Fundinfo")

    driver.quit()
