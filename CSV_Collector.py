import pandas as pd
import os
from datetime import date

def collector():
    cs_output = pd.read_csv('Output_CS.csv', 'r', delimiter=';')
    output_swisscanto = pd.read_csv('Output_Swisscanto.csv','r', delimiter=';')
    output_div = pd.read_csv('Output_DIV.csv', 'r', delimiter=';')
    output_morningstar_ch = pd.read_csv('Output_Morningstar_CH.csv', 'r', delimiter=';')
    output_fundinfo = pd.read_csv('Output_Fundinfo.csv', 'r', delimiter=';')
    output_ubs_ast = pd.read_csv('Output_UBS_AST.csv', 'r', delimiter=';')
    output_ubs_ch = pd.read_csv('Output_UBS_CH.csv', 'r', delimiter=';')
    output_ubs_swiss = pd.read_csv('Output_UBS_SWISS.csv', 'r', delimiter=';')
    output_tramondo = pd.read_csv('Output_Tramondo.csv', 'r', delimiter=';')
    output_moringstar_de = pd.read_csv('Output_Morningstar_DE.csv', 'r', delimiter=';')
    output_fundsquare= pd.read_csv('Output_Fundsquare.csv', 'r', delimiter=';')
    output_cs_fx = pd.read_csv('Output_CS_FX.csv', 'r', delimiter=';')
    output_swiss_fund_data = pd.read_csv('Output_SwissFundData.csv', 'r', delimiter=';')





    df_output = pd.DataFrame()
    df_output = df_output.append(cs_output, ignore_index=True)
    df_output = df_output.append(output_div, ignore_index=True)
    df_output = df_output.append(output_swisscanto, ignore_index=True)
    df_output = df_output.append(output_morningstar_ch, ignore_index=True)
    df_output = df_output.append(output_fundinfo, ignore_index=True)
    df_output = df_output.append(output_ubs_ast, ignore_index=True)
    df_output = df_output.append(output_ubs_ch, ignore_index=True)
    df_output = df_output.append(output_ubs_swiss, ignore_index=True)
    df_output = df_output.append(output_tramondo, ignore_index=True)
    df_output = df_output.append(output_moringstar_de, ignore_index=True)
    df_output = df_output.append(output_cs_fx, ignore_index=True)
    df_output = df_output.append(output_fundsquare, ignore_index=True)
    df_output = df_output.append(output_swiss_fund_data, ignore_index=True)


    df_output = df_output[['Asset Name','ISIN',	'Bid Price','Ask Price', 'Date']]

    desktop = os.path.join(os.path.join(os.environ['USERPROFILE']), 'Desktop')
    path = desktop + '\Scraper_Output_' + str(date.today())
    datetime = str(date.today())
    
    try:
        os.mkdir(path)
    except OSError:
        print("Creation of the directory Scraper_Output failed")
    else:
        print("Successfully created the directory Scraper_Output")

    df_output.to_csv(r'{0}\{1}_Scraper_Output.csv'.format(path, datetime), index=False, header=True, sep=";")

    print("..............................Done Collector ................................")

