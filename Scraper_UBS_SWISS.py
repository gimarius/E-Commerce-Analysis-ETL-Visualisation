from bs4 import BeautifulSoup
import pandas as pd
import requests


def ubs_swiss():
    input_ubs = []

    input_ubs.append(["30372562", "UBS Equity Long Term Themes (CHF hedged) Q-acc"])
    input_ubs.append(["4731643", "UBS Money Market (CHF) Q-acc"])
    input_ubs.append(["4733788", "UBS (Lux) Equity Fund - China Opportunity (USD) Q USD acc"])
    input_ubs.append(["1442088", "UBS (CH) Property Fund - Swiss Commercial «Swissreal»"])

    df_input_ubs = pd.DataFrame(input_ubs)
    df_input_ubs.columns = ["ISIN", "Asset Name"]

    n_ubs = len(input_ubs)

    # UBS Institutional Funds
    source = requests.get('https://fundgate.ubs.com/srprices.do?rid=3&cty=CH&lang=en').text
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

                    if "Price:" in nav_ubs:
                        nav_ubs = nav_ubs[7:]
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
        ubs_merge.to_csv("Output_UBS_SWISS.csv", index=False, header=True, sep=";")
        print("--> Done UBS CH")