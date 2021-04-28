from bs4 import BeautifulSoup
import pandas as pd
import requests


def ubs_CH():
    input_ubs = []

    input_ubs.append(["30228884", "UBS (CH) Institutional Fund - Equities Switzerland Small & Mid Cap Passive II I-A1"])
    input_ubs.append(["4615913", "UBS (CH) Institutional Fund - Bonds CHF Ausland Passive II I-A1"])
    input_ubs.append(["12180074", "UBS (CH) Institutional Fund - Global Aggregate Bonds Passive (CHF hedged) II I-A1"])
    input_ubs.append(["4211426", "UBS (CH) Institutional Fund 3 - Swiss Real Estate Securities Selection Passive II I-A1"])
    input_ubs.append(["18418262", "UBS (CH) Institutional Fund - Global Corporate Bonds Passive (CHF hedged) II I-A1"])
    input_ubs.append(["24455240", "UBS (CH) Institutional Fund 3 - Bonds Emerging Markets Agg. Passive (CHF hedged) II I-A1"])
    input_ubs.append(["26628462", "UBS (CH) Institutional Fund - Equities Global Passive (CHF hedged) II I-A1"])
    input_ubs.append(["4615947", "UBS (CH) Institutional Fund - Equities Switzerland Passive All II I-A1"])
    input_ubs.append(["25280970", "UBS (CH) Institutional Fund - Equities Emerging Markets Global Passive II (CHF) I-A1"])
    input_ubs.append(["33660190", "UBS (CH) Institutional Fund - Equities Global Small Cap Passive II I-A1"])
    input_ubs.append(["3542727", "UBS (CH) Institutional Fund - Swiss Real Estate Selection II I-A1"])
    input_ubs.append(["4771002", "UBS (CH) Institutional Fund 2 - Global Real Estate Securities Passive II I-A1"])
    input_ubs.append(["21033091", "UBS (Lux) Bond SICAV - Short Duration High Yield (USD) (CHF hedged) I-A1-acc"])
    input_ubs.append(["20555209", "UBS (CH) Equity Fund - Swiss High Dividend (CHF) I-A1"])
    input_ubs.append(["1579863", "UBS (CH) Investment Fund - Equities Europe Passive I-A1"])
    input_ubs.append(["10744942", "UBS (CH) Equity Fund - Small Caps Europe (EUR) I-A1"])
    input_ubs.append(["1570962", "UBS (CH) Investment Fund - Equities Switzerland Passive Large I-A1"])
    input_ubs.append(["12024901", "UBS (CH) Investment Fund – Bonds CHF Inland Medium Term Passive I-A1"])

    n_ubs = len(input_ubs)

    df_input_ubs = pd.DataFrame(input_ubs)
    df_input_ubs.columns = ["ISIN", "Asset Name"]

    source = requests.get('https://fundgate.ubs.com/srprices.do?rid=3&segment=ubsf.emif&cty=CH&lang=en').text
    soup = BeautifulSoup(source, 'html.parser')

    table_ubs = soup.find_all('tr', class_='uwZebraOdd')

    count_found_ubs = 0



    array_list = []

    # List of all values
    for tr in table_ubs:

        if count_found_ubs == n_ubs:
            break

        isin_ubs = tr.find_all('td')

        if len(isin_ubs) < 1:
            continue
        else:
            tmp_isin = isin_ubs[2].text

            for m in range(0, n_ubs):

                array = []

                if tmp_isin == input_ubs[m][0]:
                    nav_ubs = isin_ubs[5].text.replace(",", "")
                    date_ubs = isin_ubs[3].text

                    array.append(tmp_isin)

                    array.append(nav_ubs)
                    array.append(date_ubs)

                    array_list.append(array)

                    count_found_ubs = count_found_ubs + 1
                    break
                else:
                    continue



    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    ubs_merge = pd.merge(df_input_ubs, df_array_list[["ISIN", "Bid Price", "Date", 'Ask Price']],
                         how="left", on='ISIN')

    if ubs_merge.isnull().values.any():
        print("------------------------------------------------------------------UBS hat NAs----------------------------")
    else:
        # all output to CSV
        ubs_merge.to_csv("Output_UBS_CH.csv", index=False, header=True, sep=";")
        print("--> Done UBS")
