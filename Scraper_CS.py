import time
import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
import os
from selenium.webdriver.chrome.options import Options


def cs():
    input_cs = []

    input_cs.append(["LU1009467850", "CS (Lux) Emerging Market Corporate Investment Grade Bond Fund EBH CHF"])
    input_cs.append(["LU0340001154", "CS (Lux) Global Securitized Bond Fund EBH CHF"])
    input_cs.append(["LU1394299660", "CS (Lux) Liquid Alternative Beta BH CHF"])
    input_cs.append(["CH0015408419", "CSIF (CH) Equity Pacific ex Japan Blue DB"])
    input_cs.append(["CH0198191493", "CSIF (CH) III Equity World ex CH - Pension Fund ZBH"])
    input_cs.append(["CH0011378228", "CSA 2 Private Equity"])
    input_cs.append(["CH0032400639", "CSIF (CH) III Equity World ex CH - Pension Fund ZB"])
    input_cs.append(["CH0037606552", "CSIF (CH) I Equity Europe ex CH ZB"])
    input_cs.append(["CH0384998420", "CSIF (CH) Equity Switzerland Large Cap Classic Blue ZB"])
    input_cs.append(["CH0436654203", "CSA 2 Mixta-BVG 75 E"])
    input_cs.append(["CH0436654138", "CSA 2 Mixta-BVG 45 E"])
    input_cs.append(["CH0436654062", "CSA 2 Mixta-BVG 35 E"])
    input_cs.append(["CH0436653965", "CSA 2 Mixta-BVG 25 E"])
    input_cs.append(["CH0008879022", "CSA 2 Mixta-BVG 25"])
    input_cs.append(["CH0112172850", "CSA 2 Mixta-BVG 25 Plus"])
    input_cs.append(["CH0008879048", "CSA 2 Mixta-BVG 35"])
    input_cs.append(["CH0008879097", "CSA 2 Mixta-BVG 45"])
    input_cs.append(["CH0112695736", "CSA Mixta-BVG Index 45 I"])
    input_cs.append(["CH0015036608", "CSA Mixta-BVG Basic I"])
    input_cs.append(["CH0002875000", "CSA Money Market CHF"])
    input_cs.append(["CH0458681456", "CSA Mixta-BVG 15 E"])
    input_cs.append(["CH0436637497", "CSA 2 Mixta-BVG 25 Plus E"])
    input_cs.append(["CH0101754387", "CSIF (CH) Bond Switzerland AAA-BBB Blue FA"])
    input_cs.append(["CH0230260413", "CSIF (CH) Bond Switzerland Domestic AAA-BBB Blue FA"])
    input_cs.append(["CH0189988337", "CSIF (CH) Bond Switzerland Foreign AAA-BBB Blue FA"])
    input_cs.append(["CH0214975333", "CSIF (CH) Bond Switzerland AAA-BBB 1-5 Blue FA"])
    input_cs.append(["CH0190771862", "CSIF (CH) Equity Switzerland Total Market Blue FA"])
    input_cs.append(["CH0348228609", "CSIF (CH) Equity Switzerland Total Market Blue QA"])
    input_cs.append(["CH0222624659", "CSIF (CH) Equity Switzerland Small & Mid Cap FA"])
    input_cs.append(["CH0233387536", "CSIF (CH) Equity World ex CH Small Cap Blue QAH"])
    input_cs.append(["CH0190895075", "CSIF (CH) I Bond Aggregate Global ex CHF QAH"])
    input_cs.append(["CH0214985100", "CSIF (CH) Bond Aggregate Global ex CHF 1-5 Blue QAH"])
    input_cs.append(["CH0189977637", "CSIF (CH) Bond Corporate Global ex CHF Blue QA"])
    input_cs.append(["CH0189977975", "CSIF (CH) Bond Corporate Global ex CHF Blue QAH"])
    input_cs.append(["CH0424137526", "CSIF (CH) Bond Corporate Global ex CHF ESG Blue ZBH"])
    input_cs.append(["CH0209681771", "CSIF (CH) I Bond Government Global ex CHF Blue QAH"])
    input_cs.append(["CH0185709083", "CSIF (CH) Equity Emerging Markets Blue QA"])
    input_cs.append(["CH0100461422", "CSIF (CH) III Equity World ex CH Blue - Pension Fund QA"])
    input_cs.append(["CH0217837449", "CSIF (CH) III Real Estate World ex CH - Pension Fund QAH"])
    input_cs.append(["CH0113556879", "CSIF (CH) I Real Estate Switzerland Blue QA"])
    input_cs.append(["CH0045357933", "CSIF (CH) I Equity World ex CH Blue QAH"])
    input_cs.append(["CH0239744714", "CSIMF Money Market CHF EA"])
    input_cs.append(["CH0125176823", "CSIF (CH) III Equity World ex CH Blue - Pension Fund QAH"])
    input_cs.append(["CH0214968213", "CSIF (CH) III Equity World ex CH Small Cap Blue - Pension Fund QA"])
    input_cs.append(["CH0214404714", "CSIF (CH) Equity Switzerland Large Cap Blue FA"])
    input_cs.append(["CH0202603251", "CSIF (CH) Equity Europe ex CH Blue QA"])
    input_cs.append(["CH0190222403", "CSIF (CH) I Equity Europe ex CH QA"])
    input_cs.append(["CH0380923679", "CSIF (CH) Equity US Blue QAH"])
    input_cs.append(["CH0233387510", "CSIF (CH) Equity World ex CH Small Cap Blue QA"])
    input_cs.append(["CH0259132303", "CSIF (CH) Bond Government Emerging Markets USD Blue QAH"])
    input_cs.append(["CH0217837423", "CSIF (CH) III Real Estate World ex CH - Pension Fund QA"])
    input_cs.append(["CH0190233798", "CSIF (CH) Equity Pacific ex Japan Blue QA"])
    input_cs.append(["CH0199278786", "CSIF (CH) Equity World ex CH QA"])
    input_cs.append(["CH0330793032", "CSIF (CH) Equity World ex CH QAH"])
    input_cs.append(["LU1390246210", "CSIF (Lux) Equities Europe Small Caps QB EUR"])
    input_cs.append(["CH0259132261", "CSIF (CH) Bond Government Emerging Markets USD Blue DAH"])
    input_cs.append(["CH0214974369", "CSIF (CH) Bond Switzerland AAA-BBB 1-5 Blue ZA"])
    input_cs.append(["CH0188772989", "CSIF (CH) I Bond Government Global ex CHF Blue ZAH"])
    input_cs.append(["CH0217837381", "CSIF (CH) III Equity World ex CH Blue - Pension Fund ZAH"])
    input_cs.append(["CH0036599816", "CSIF (CH) I Real Estate Switzerland Blue ZA"])
    input_cs.append(["CH0031341875", "CSIF (CH) Equity Switzerland Total Market Blue ZA"])
    input_cs.append(["CH0130458182", "CSIF (CH) III Equity World ex CH Blue - Pension Fund ZA"])
    input_cs.append(["CH0214968353", "CSIF (CH) III Equity World ex CH Small Cap Blue - Pension Fund DAH"])
    input_cs.append(["CH0189956813", "CSIF (CH) Bond Corporate Global ex CHF Blue ZAH"])
    input_cs.append(["CH0039003055", "CSIF (CH) Bond Switzerland AAA-BBB Blue ZA"])
    input_cs.append(["CH0017844686", "CSIF (CH) Equity Emerging Markets Blue DA"])
    input_cs.append(["CH0217837688", "CSIF (CH) III Real Estate World ex CH - Pension Fund ZAH"])
    input_cs.append(["CH0031419960", "CSIMF Money Market CHF ZA"])
    input_cs.append(["CH0348319861", "CSIF (CH) Equity Switzerland Small & Mid Cap QA"])
    input_cs.append(["CH0334031215", "CSIF (CH) Equity SPI Multi Premia Blue QA"])
    input_cs.append(["CH0281860343", "CSIF (CH) Bond Switzerland Corporate Blue FA"])
    input_cs.append(["CH0110869143", "CSIF (CH) Equity Switzerland Small & Mid Cap ZA"])
    input_cs.append(["CH0030849712", "CSIF (CH) III Equity US Blue - Pension Fund ZA"])
    input_cs.append(["CH0033782431", "CSIF (CH) Equity Switzerland Large Cap Blue ZA"])
    input_cs.append(["CH0147102146", "CSIF (CH) Bond Switzerland Domestic AAA-BBB Blue ZA"])
    input_cs.append(["CH0334161491", "CSIF (CH) Equity Switzerland Minimum Volatility Blue DA"])
    input_cs.append(["CH0337393745", "CSIF (CH) III Equity World ex CH ESG Blue - Pension Fund ZB"])
    input_cs.append(["CH0337393851", "CSIF (CH) III Equity World ex CH ESG Blue - Pension Fund ZBH"])
    input_cs.append(["LU1587908150", "CSIF (Lux) Equity Emerging Markets ESG Blue DB CHF"])
    input_cs.append(["CH0214976851", "CSIF (CH) Bond Aggregate Global ex CHF 1-5 Blue ZB"])
    input_cs.append(["CH0032044791", "CSIF (CH) Real Estate Asia ZB"])
    input_cs.append(["CH0214967314", "CSIF (CH) III Equity World ex CH Small Cap Blue - Pension Fund DB"])
    input_cs.append(["CH0217837456", "CSIF (CH) III Real Estate World ex CH - Pension Fund ZB"])
    input_cs.append(["CH0186534936", "CSIF (CH) Bond Fiscal Strength Global ex CHF Blue ZBH"])
    input_cs.append(["LU1831055824", "CSIF (Lux) Bond Government Emerging Markets Local DB"])
    input_cs.append(["CH0034011509", "CSIF (CH) I Bond Aggregate Global ex CHF ZB"])
    input_cs.append(["CH0429081638", "CSIF (CH) III Equity World ex CH Blue - Pension Fund Plus ZBH"])
    input_cs.append(["CH0429081620", "CSIF (CH) III Equity World ex CH Blue - Pension Fund Plus ZB"])
    input_cs.append(["CH0253609249", "CSIF (CH) III Equity World ex CH Quality - Pension Fund DBH"])
    input_cs.append(["CH0220918962", "CSIF (CH) II Gold Blue DBH CHF"])
    input_cs.append(["CH0363647212", "CSIF (CH) Bond Corporate EUR ZBH"])
    input_cs.append(["CH0253608357", "CSIF (CH) III Equity World ex CH Minimum Volatility - Pension Fund DBH"])
    input_cs.append(["CH0033210086", "CSIF (CH) I Bond Government Global ex CHF Blue ZB"])
    input_cs.append(["CH0286749715", "CSIF (CH) Equity EMU ZBH"])
    input_cs.append(["CH0032044684", "CSIF (CH) Real Estate Europe ex CH ZB"])
    input_cs.append(["CH0304125724", "CSIF (CH) Bond Corporate USD Blue ZBH"])
    input_cs.append(["CH0304121434", "CSIF (CH) Bond Corporate USD Blue ZB"])
    input_cs.append(["CH0030849563", "CSIF (CH) Equity Europe ex EMU ex CH ZB"])
    input_cs.append(["CH0030849373", "CSIF (CH) Bond Government USD Blue ZB"])
    input_cs.append(["CH0281860111", "CSIF (CH) Bond Switzerland Corporate Blue ZB"])
    input_cs.append(["CH0100523262", "CSIF (CH) Equity Europe ex CH Blue ZB"])
    input_cs.append(["CH0334031199", "CSIF (CH) Equity SPI Multi Premia Blue DB"])
    input_cs.append(["CH0214984392", "CSIF (CH) Bond Aggregate Global ex CHF 1-5 Blue ZBH"])
    input_cs.append(["LU1815001406", "CSIF (Lux) Equity China Total Market DB"])
    input_cs.append(["CH0030849522", "CSIF (CH) Equity EMU ZB"])
    input_cs.append(["CH0397628717", "CSIF (CH) III Equity US ESG Blue - Pension Fund ZBH"])
    input_cs.append(["CH0190889912", "CSIF (CH) I Bond Aggregate Global ex CHF ZBH"])
    input_cs.append(["CH0189955260", "CSIF (CH) Bond Corporate Global ex CHF Blue ZB"])
    input_cs.append(["CH0030849654", "CSIF (CH) Equity Pacific ex Japan Blue ZB"])
    input_cs.append(["CH0253608308", "CSIF (CH) III Equity World ex CH Minimum Volatility - Pension Fund DB"])
    input_cs.append(["LU1337015165", "CSIF (Lux) Equity Emerging Markets Minimum Volatility DB CHF"])
    input_cs.append(["CH0185708234", "CSIF (CH) III Equity US Blue - Pension Fund QB"])
    input_cs.append(["CH0030849688", "CSIF (CH) Equity US Blue ZB"])
    input_cs.append(["LU0340004760", "CS (Lux) Global High Yield Bond Fund EBH CHF"])
    input_cs.append(["CH0253609066", "CSIF (CH) III Equity World ex CH Quality - Pension Fund DB"])
    input_cs.append(["CH0424136833", "CSIF (CH) Equity World ex CH ESG Blue ZB"])
    input_cs.append(["CH0500706632", "CSIF (CH) Equity World ex CH ESG Blue ZBH"])
    input_cs.append(["IE00BMDX0L03", "CSIF (IE) MSCI USA Small Cap ESG Leaders Blue UCITS ETF B USD"])
    input_cs.append(["LU2189789758", "CSIF (Lux) Equity UK ESG Blue QB GBP"])
    input_cs.append(["LU1007181461", "CS (Lux) Global High Yield Bond Fund DBH CHF"])
    input_cs.append(["CH0107834662", "CSIF (CH) Bond Inflation-Linked Global ex Japan ex Italy ex Spain Blue ZBH"])
    input_cs.append(["CH0214976778", "CSIF (CH) III Equity World ex CH Value Weighted - Pension Fund DBH"])
    input_cs.append(["CH0357515482", "CSIF (CH) I Equity Japan Blue - Pension Fund ZBH"])
    input_cs.append(["CH0214975366", "CSIF (CH) III Equity World ex CH Value Weighted - Pension Fund DB"])
    input_cs.append(["LU1683287533", "CS (Lux) Digital Health Equity Fund DB USD"])
    input_cs.append(["LU2176898489", "CS (Lux) Environmental Impact Equity Fund EBHP CHF"])
    input_cs.append(["LU1435227258", "CS (Lux) Robotics Equity Fund DB USD"])
    input_cs.append(["LU1268048490", "CSIF (Lux) Equity EMU Small Cap Blue DB EUR"])


    input_cs.append(["LU1169959480", "CS (Lux) Asia Pacific Income Equity Fund AH CHF"])
    input_cs.append(["LU1796813662", "CS (Lux) Digital Health Equity Fund EBH CHF"])
    input_cs.append(["LU1886389292", "CS (Lux) Security Equity Fund EBH CHF"])




    df_cs_input = pd.DataFrame(input_cs)
    df_cs_input.columns = ["ISIN", "Asset Name"]
    output = pd.DataFrame()

    isin_list = []
    for element in input_cs:
        isin = element[:1][:]
        isin = ' '.join(map(str, isin))
        isin_list.append(isin)

    path = os.path.abspath(os.path.join(os.path.normpath(os.path.dirname(__file__)), 'chromedriver.exe'))
    chrome_options = Options()

    chrome_options.add_argument("--headless")
    chrome_options.add_argument('log-level=3')
    chrome_options.add_argument('--window-size=1920,1080')  

    driver = webdriver.Chrome(executable_path=path, options=chrome_options)



    driver.get("https://amfunds.credit-suisse.com/")
    driver.add_cookie({'name': 'F.domicile', 'value': 'ch'})
    driver.add_cookie({'name': 'F.investortype', 'value': 'institutional'})
    select = driver.find_element_by_id("selInvestorTypePartTitle")
    select_button = select.find_element_by_id("selInvestorTypes_CH")
    select_option = select_button.find_element_by_xpath(".//option[2]")
    select_option.click()
    button = driver.find_elements_by_id('btnAccept')
    driver.execute_script("window.scrollTo(0,1500)")
    button[1].click()
    driver.execute_script("window.scrollTo(0,10000)")
    all_button = driver.find_element_by_xpath(".//a[@class = 'mod_flexible_sidebar_cta_button']")
    range_cs = driver.find_element_by_xpath('//*[@id="content"]/div[2]/a').text
    range_cs = range_cs[5:-15]
    range_cs = int(range_cs)


    try:
        element = WebDriverWait(driver, 20).until(
            EC.presence_of_element_located(
                (By.XPATH, ".//a[@class = 'mod_flexible_sidebar_cta_button']"))
        )
    except:
        print('CS Page not loaded')

    all_button.click()

    try:
        element = WebDriverWait(driver, 20).until(
            EC.presence_of_element_located(
                (By.ID, 'tab_0'))
        )
    except:
        print('CS Page not loaded')

    table_cs_num = driver.find_element_by_id('tab_0')

    try:
        element = WebDriverWait(driver, 20).until(
            EC.presence_of_element_located(
                (By.XPATH, '//*[@id="tblFundlist"]'))
        )
    except:
        print('CS Page not loaded')

    table_cs_row = table_cs_num.find_element_by_xpath('//*[@id="tblFundlist"]')

    try:
        element = WebDriverWait(driver, 20).until(
            EC.presence_of_element_located(
                (By.ID, 'trFundList'))
        )
    except:
        print('CS Page not loaded')

    table_cs_rows = table_cs_row.find_elements_by_id("trFundList")

    array_list = [[]]
    for i in range(0, range_cs):
        array = []
        try:
            element = WebDriverWait(driver, 30).until(
                EC.presence_of_element_located(
                    (By.CLASS_NAME, 'smallerText'))
            )
        except:
            print('CS Page not loaded')

        fund_isin = table_cs_rows[i].find_element_by_class_name('smallerText')
        fund_isin = fund_isin.text


        if fund_isin in isin_list:
            array.append(fund_isin)
            fund_text = table_cs_rows[i].find_element_by_class_name('fundNAV_column')
            fund_nav = fund_text.text
            fund_nav = fund_nav.splitlines()
            fund_price = fund_nav[0]
            fund_date = fund_nav[1]
            fund_date = fund_date.replace('(', "")
            fund_date = fund_date.replace(')', "")
            array.append(fund_price)
            array.append(fund_date)
            array_list.append(array)

    df_array_list = pd.DataFrame(array_list)
    df_array_list = df_array_list.drop(df_array_list.index[0])
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    cs_merge = pd.merge(df_cs_input, df_array_list[["ISIN", "Bid Price", "Date", 'Ask Price']], how= "left", on = 'ISIN')
    cs_merge = cs_merge
    cs_merge = cs_merge[['Asset Name', 'ISIN', 'Bid Price', 'Ask Price', 'Date']]

    if cs_merge.isnull().values.any():
        print("---------------------------------------------------------------------------------------CS hat NAs----------------------------")
        na = cs_merge[cs_merge.isna().any(axis=1)]
        print(na)
    else:
        pass
    cs_merge.to_csv("Output_CS.csv", index=False, header=True, sep=";")
    print("--> Done CS")

