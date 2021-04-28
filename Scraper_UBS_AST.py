from bs4 import BeautifulSoup
import pandas as pd
import requests

def ubs_AST():

    input_ubsAST = []

    #input_ubsAST.append(["11761162", "UBS AST Obligationen CHF Inland Indexiert I-A1"])
    input_ubsAST.append(["28160913", "UBS AST Obligationen Global Corporates Indexiert (hedged in CHF) II I-A1"])
    input_ubsAST.append(["28160932", "UBS AST Aktien Schweiz All Indexiert II I-A1"])
    input_ubsAST.append(["27398329", "UBS AST Aktien Emerging Markets Global Indexiert II I-A1"])
    input_ubsAST.append(["11764616", "UBS AST Immoparts Schweiz I-A1"])
    #input_ubsAST.append(["11764632", "UBS AST Immobilien Global Indexiert (hedged in CHF) I-A1"])
    input_ubsAST.append(["27537295", "UBS AST 2 Global Equities Small Cap (ex CH) Passive (hedged in CHF) II I-A1"])
    input_ubsAST.append(["13567007", "UBS AST Obligationen CHF Indexiert I-A1"])
    input_ubsAST.append(["27375550", "UBS AST Obligationen Fremdwähr. Global Indexiert (hedged in CHF) I-A1"])
    input_ubsAST.append(["12355878", "UBS AST 3 Global Real Estate ex CH I-A0"])
    input_ubsAST.append(["23849405", "UBS AST 2 Global Equities (ex CH) Passive II-A1"])
    input_ubsAST.append(["28160919", "UBS AST Obligationen Global Corporates Indexiert (hedged in CHF) II I-X"])
    input_ubsAST.append(["13928960", "UBS AST Obligationen Fremdwährungen Global Indexiert (hedged in CHF) I-X"])
    input_ubsAST.append(["28160936", "UBS AST Aktien Schweiz All Indexiert II I-X"])
    input_ubsAST.append(["13567168", "UBS AST Aktien Emerging Markets Global Indexiert II I-X"])
    input_ubsAST.append(["27537381", "UBS AST 2 Global Equities Small Cap (ex CH) Passive (hedged in CHF) II I-X"])
    input_ubsAST.append(["11867447", "UBS AST Immoparts Schweiz I-X"])
    input_ubsAST.append(["14741965", "UBS AST 2 Global Equities (ex CH) Passive II (hedged in CHF) I-A1"])
    input_ubsAST.append(["28093351", "UBS AST 2 BVG-25 Active Plus I-A1"])
    input_ubsAST.append(["28093313", "UBS AST 2 BVG-25 Passive (hedged in CHF) I-A1"])
    input_ubsAST.append(["28093357", "UBS AST 2 BVG-40 Active Plus I-A1"])
    input_ubsAST.append(["28093322", "UBS AST 2 BVG-40 Passive (hedged in CHF) I-A1"])
    input_ubsAST.append(["18150944", "UBS AST BVG - Target Risk 5% I-A1"])
    input_ubsAST.append(["18152479", "UBS AST BVG - Target Risk 7% I-A1"])




    df_input_ubsAST = pd.DataFrame(input_ubsAST)
    df_input_ubsAST.columns = ["ISIN", "Asset Name"]

    n_ubsAST = len(input_ubsAST)

    source = requests.get('https://fundgate.ubs.com/srprices.do?rid=1&segment=ubsf.astf&cty=CH&lang=de').text
    soup = BeautifulSoup(source, 'html.parser')

    table_ubsAST = soup.find_all('tr', class_='uwZebraOdd')

    count_found_ubsAST = 0

    array_list = []

    # List of all values
    for tr in table_ubsAST:

        if count_found_ubsAST == n_ubsAST:
            break

        isin_ubsAST = tr.find_all('td')

        if len(isin_ubsAST) < 1:
            continue
        else:
            tmp_isin = isin_ubsAST[2].text



            for m in range(0, n_ubsAST):

                array = []

                if tmp_isin == input_ubsAST[m][0]:
                    nav_ubsAST = isin_ubsAST[5].text.replace("'", "")
                    date_ubsAST = isin_ubsAST[3].text

                    array.append(tmp_isin)

                    array.append(nav_ubsAST)
                    array.append(date_ubsAST)

                    array_list.append(array)

                    #out_write.writerow([input_ubsAST[m][1], input_ubsAST[m][0], nav_ubsAST, nav_ubsAST, date_ubsAST])
                    count_found_ubsAST = count_found_ubsAST + 1
                    break
                else:
                    continue



    df_array_list = pd.DataFrame(array_list)
    df_array_list.columns = ["ISIN", "Bid Price", "Date"]
    df_array_list['Ask Price'] = df_array_list['Bid Price']

    ubs_AST_merge = pd.merge(df_input_ubsAST, df_array_list[["ISIN", "Bid Price", "Date", 'Ask Price']],
                         how="left", on='ISIN')

    if ubs_AST_merge.isnull().values.any():
        print("-------------------------------------------------UBS AST hat NAs----------------------------")
        print(ubs_AST_merge[ubs_AST_merge.isna().any(axis=1)])
    else:
        # all output to CSV
        ubs_AST_merge.to_csv("Output_UBS_AST.csv", index=False, header=True, sep=";")
        print("--> Done UBS AST")

