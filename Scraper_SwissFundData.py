from selenium import webdriver
import pandas as pd
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import os
from selenium.webdriver.chrome.options import Options



def SwissFundData():

    input_swiss_fund_data = []

    input_swiss_fund_data.append(["CH0306953446", "GKB (CH) - Obligationen CHF ESG", "76071"])
    input_swiss_fund_data.append(["CH0339968965", "Syz AM (CH) - CHF Credit Bond S2", "71087"])
    input_swiss_fund_data.append(["CH0324769451", "GKB (CH) - Aktien Schweiz ESG", "117617"])
    input_swiss_fund_data.append(["LU1676119669", "UBS (Lux) IS - China A Opportunity (USD) I-A1-acc", "133245"])
    input_swiss_fund_data.append(["CH0106027128", "UBS ETF (CH) Gold hedged CHF (CHF) A-dis", "32720"])
    input_swiss_fund_data.append(["CH0014420829", "UBS (CH) Property Fund - Swiss Residential Anfos", "11486"])
    input_swiss_fund_data.append(["CH0009778769", "Immofonds", "67692"])
    input_swiss_fund_data.append(["CH0357659496", "GKB (CH) Aktien Welt ESG N", "117648"])
    input_swiss_fund_data.append(["CH0450943482", "EnPa Strategiefonds Libra", "105826"])
    input_swiss_fund_data.append(["CH0450943516", "EnPa Strategiefonds Nova", "105827"])
    input_swiss_fund_data.append(["LU1508356232", "Wellington Global Quality Growth Fund", "73519"])
    input_swiss_fund_data.append(["LU2079712274", "FISCH Bond Global High Yield Fund VGC", "118529"])
    input_swiss_fund_data.append(["CH0019685111", "CS Real Estate Fund International A", "20005"])
    input_swiss_fund_data.append(["CH0194666555", "zCapital Swiss Dividend Fund A-Klasse", "3985"])
    input_swiss_fund_data.append(["CH0139101601", "ZKB Gold ETF AAH CHF", "23173"])
    input_swiss_fund_data.append(["CH0019597530", "AMG Substanzwerte Schweiz", "19894"])
    input_swiss_fund_data.append(["CH0302271066", "Alpora Innovation Europa CHF-hedged", "63480"])
    input_swiss_fund_data.append(["CH0420487941", "AMG Gold Minen & Metalle Klasse H", "97851"])
    input_swiss_fund_data.append(["CH0428759747", "Swiss Value Equity Fund - I", "102623"])
    input_swiss_fund_data.append(["CH0215106581", "Partisan Strategie Fonds (CHF) - S", "5860"])
    input_swiss_fund_data.append(["LI0033242210", "AMG Schweizer Perlen Fonds P", "28741"])
    input_swiss_fund_data.append(["LU1225718664", "Partners Group Listed Investments SICAV - Listed Infrastructure CHF (C - Acc.)", "54627"])
    input_swiss_fund_data.append(["LU0162832405", "Fisch Convertible Global Opportunistic Fund AC", "16406"])
    input_swiss_fund_data.append(["CH0238828153", "Reichmuth Alpin - S", "87061"])
    input_swiss_fund_data.append(["CH0185662878", "PARSUMO - PARtact Dynamic Strategy Fund", "2895"])
    input_swiss_fund_data.append(["CH0205879213", "PARSUMO - PARtact Pension Strategy Fund", "4477"])
    input_swiss_fund_data.append(["LU2084868962", "Innovation World Large Caps by AMG - B (CHF hedged)", "121185"])
    input_swiss_fund_data.append(["LU0076398725", "Tweedy Browne International Value CC CHF", "14950"])
    input_swiss_fund_data.append(["LU0571085686", "Vontobel Fund - mtx Sustainable Emerging Markets Leaders I", "237"])
    input_swiss_fund_data.append(["LU0585393688", "Globalance Sokrates Fund Anteilsklasse I (CHF)", "37635"])
    input_swiss_fund_data.append(["CH0297417534", "AMG Europa Klasse C", "58183"])
    input_swiss_fund_data.append(["LU2049785574", "Globalance Zukunftbeweger Focused Aktienfonds I (CHF)", "117957"])
    input_swiss_fund_data.append(["LU1302924029", "OptoFlex - S", "86354"])
    input_swiss_fund_data.append(["IE00BD2B9H19", "Twelve Cat Bond Fund - I CHF Acc. (ACH114)", "128362"])
    input_swiss_fund_data.append(["LU1342495105", "Twelve Insurance Best Ideas Fund - I CHF Acc. (ACH114)", "128213"])
    input_swiss_fund_data.append(["IE00BMCM9M24", "Twelve Insurance Fixed Income Fund S CHF", "132837"])
    input_swiss_fund_data.append(["IE00BNN82N84", "Diamond Angsana Bond Fund Class Al Participating Share", "44268"])
    input_swiss_fund_data.append(["IE00BDT6FS23", "SSgA SPDR ETFs Europe II PLC - Refinitiv Global Convertible Bond UCITS ETF", "95353"])
    input_swiss_fund_data.append(["CH0120791253", "SF Sustainable Property Fund", "67594"])
    input_swiss_fund_data.append(["CH0124758522", "UBS ETF (CH) – SXI Real Estate (CHF) A-dis", "36914"])
    input_swiss_fund_data.append(["CH0240440229", "OLZ - Equity World ex CH Optimized ESG I-C", "44557"])
    input_swiss_fund_data.append(["LU1767066605", "Vontobel Fund - mtx Sustainable Emerging Markets Leaders G", "112061"])
    input_swiss_fund_data.append(["LU1321847714", "BSF Emerging Markets Equity Strategies Fund Class D2 USD", "62649"])  
    input_swiss_fund_data.append(["LU2059770235", "Candriam SRI Bond Emerging Markets V CHF-hedged", "120711"])
    input_swiss_fund_data.append(["CH0246527789", "BEKB Obligationen CHF Inland", "117429"])
    input_swiss_fund_data.append(["CH0366022553", "BEKB Strategiefonds Nachhaltig 20", "86802"])
    input_swiss_fund_data.append(["CH0366022702", "BEKB Strategiefonds Nachhaltig 40", "86805"])
    input_swiss_fund_data.append(["CH0366023528", "BEKB Strategiefonds Nachhaltig 60", "86808"])
    input_swiss_fund_data.append(["CH0370830843", "BLKB Next Generation Fund Growth B", "86810"])
    input_swiss_fund_data.append(["CH0366023528", "BLKB Next Generation Fund Vorsorge Balanced Vt", "117752"])
    input_swiss_fund_data.append(["CH0395929810", "BLKB Next Generation Fund Vorsorge Yield Vt", "117751"])
    input_swiss_fund_data.append(["CH0259354683", "Bordier - CHF Short-Term Bond Fund", "119884"])
    input_swiss_fund_data.append(["CH0259354634", "Bordier - Prévoyance Individuelle 25", "48706"])
    input_swiss_fund_data.append(["CH0259354675", "Bordier - Prévoyance Individuelle 40 I", "48708"])
    input_swiss_fund_data.append(["LU0985093136", "Ethna-AKTIV (SIA CHF-T)", "39937"])
    input_swiss_fund_data.append(["CH0043431425", "Quantex Funds - Nebenwerte R", "29535"])
    input_swiss_fund_data.append(["CH0019182366", "Quantex Strategic Precious Metal Fund (CHF) - Class A", "25313"])
    input_swiss_fund_data.append(["CH0022497405", "Reichmuth Hochalpin", "22882"])
    input_swiss_fund_data.append(["CH0331191467", "Reichmuth Voralpin - S", "86872"])
    input_swiss_fund_data.append(["CH0020306186", "SGKB (CH) Fund - Strategie Ausgewogen", "22696"])
    input_swiss_fund_data.append(["CH0334714620", "SGKB (CH) Fund - Strategie Einkommen", "76196"])
    input_swiss_fund_data.append(["CH0027551297", "Valitas Diversified 3.0 -A-", "28406"])
    input_swiss_fund_data.append(["CH0027551339", "Valitas Diversified 5.0 -A-", "28405"])
    input_swiss_fund_data.append(["CH0263844638", "Valitas Sustainable 3.0 -R-", "56911"])
    input_swiss_fund_data.append(["CH0263844661", "Valitas Sustainable 5.0 -R-", "56912"])
    input_swiss_fund_data.append(["CH0364960242", "VF (CH) - Valiant Helvétique Ausgewogen Anteilsklasse V - CHF", "84582"])
    input_swiss_fund_data.append(["CH0364960218", "VF (CH) - Valiant Helvétique Dynamisch Anteilsklasse V - CHF", "84583"])
    input_swiss_fund_data.append(["CH0113932153", "VF (CH) - Valiant Helvétique Konservativ Anteilsklasse V - CHF", "30409"])
    input_swiss_fund_data.append(["CH0012913700", "CS Real Estate Fund Siat A", "51517"])
    input_swiss_fund_data.append(["CH0395929844", "BLKB Next Generation Fund Vorsorge Balanced Vt", "117752"])


    output = pd.DataFrame()

    path = os.path.abspath(os.path.join(os.path.normpath(os.path.dirname(__file__)), 'chromedriver.exe'))
    chrome_options = Options()
    chrome_options.add_argument("--headless")
    chrome_options.add_argument('log-level=3')

    driver = webdriver.Chrome(executable_path=path, options=chrome_options)


    df_input_swiss_fund_data = pd.DataFrame(input_swiss_fund_data)
    df_input_swiss_fund_data.columns = ["ISIN", "Asset Name", "Code"]


    driver.set_window_size(1400, 1200)

    # Anzahl der Instrumente wird in Variable gespeichert
    number_swiss_fund_data = len(input_swiss_fund_data)

    driver.get("https://www.swissfunddata.ch/sfdpub/de/funds/show/76071")

    Land = driver.find_element_by_xpath('//*[@id="inner"]/div/div[1]/div/p[1]/a[1]')
    Land.click()

    schweiz = driver.find_element_by_xpath('//*[@id="inner"]/div/div[1]/div/form/p/input[1]')
    schweiz.click()

    array_list = [[]]
    array_list.clear()

    # Look for data according to the web structure
    for m in range(0, number_swiss_fund_data):
        array = []
        driver.get('https://www.swissfunddata.ch/sfdpub/de/funds/show/' + input_swiss_fund_data[m][2])



        try:
            element = WebDriverWait(driver, 40).until(
                EC.presence_of_element_located(
                    (By.XPATH, '//*[@id="tab-1"]/div[1]/div/div[2]/table/tbody/tr[1]/td'))
            )
        except:
            print('Swiss Fund Data Page not loaded')

        isin = driver.find_element_by_xpath('//*[@id="tab-1"]/div[1]/div/div[2]/table/tbody/tr[1]/td').text
        array.append(isin)
        nav = driver.find_element_by_xpath('//*[@id="tab-1"]/div[3]/div/div[2]/table/tbody/tr[1]/td[1]').text
        nav = nav[:-4]

        nav = nav.replace("'","")
        array.append(nav)

        date = driver.find_element_by_xpath('//*[@id="tab-1"]/div[3]/div/div[2]/table/tbody/tr[1]/td[2]/small').text
        array.append(date)
        array_list.append(array)

    driver.quit()

    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    df_input_swiss_fund_data = df_input_swiss_fund_data.drop(["Code"], axis=1)


    swiss_fund_data_merge = pd.merge(df_input_swiss_fund_data, df_array_list[["ISIN", "Bid Price", 'Ask Price', "Date"]],
                                    how="left", on='ISIN')

    if swiss_fund_data_merge.isnull().values.any():
        print(
            "------------------------------------------------------------------Swiss_fund_data  hat NAs----------------------------")
    else:
        pass

    swiss_fund_data_merge.to_csv("Output_SwissFundData.csv", index=False, header=True, sep=";")
    print("--> Done Swiss Fund Data")
    driver.quit()

