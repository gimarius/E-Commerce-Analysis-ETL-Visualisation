from Scraper import Scraper_CS
from Scraper import Scraper_Fundinfo
from Scraper import Scraper_Morningstar_CH
from Scraper import Scraper_DIV
from Scraper import Scraper_Swisscanto
from Scraper import Scraper_UBS_AST
from Scraper import Scraper_UBS_CH
from Scraper import Scraper_Tramondo
from Scraper import Scraper_Morningstar_DE
from Scraper import Scraper_UBS_SWISS
from Scraper import Scraper_Fundsquare
from Scraper import Scraper_CS_FX
from Scraper import CSV_Collector
from Scraper import Scraper_SwissFundData
import multiprocessing
from timeit import default_timer as timer


def main():
    start = timer()
    p1_cs = multiprocessing.Process(target=Scraper_CS.cs)
    p2_morning_de = multiprocessing.Process(target=Scraper_Morningstar_DE.morningstar_DE)
    p3_morning_ch = multiprocessing.Process(target=Scraper_Morningstar_CH.morningstar_CH)
    p4_small = multiprocessing.Process(target=Scraper_DIV.div)
    p5_fundinfo = multiprocessing.Process(target=Scraper_Fundinfo.fundinfo)
    p6_swisscanto = multiprocessing.Process(target=Scraper_Swisscanto.swisscanto)
    p7_ubs_ast = multiprocessing.Process(target=Scraper_UBS_AST.ubs_AST)
    p8_ubs = multiprocessing.Process(target=Scraper_UBS_CH.ubs_CH)
    p9_tramondo = multiprocessing.Process(target=Scraper_Tramondo.tramondo)
    p11_ubs_ch = multiprocessing.Process(target=Scraper_UBS_SWISS.ubs_swiss)
    p12_fundsquare = multiprocessing.Process(target=Scraper_Fundsquare.fundsquare)
    p13_cs_fx = multiprocessing.Process(target=Scraper_CS_FX.cs_fx)
    p16_swiss_fund_data = multiprocessing.Process(target=Scraper_SwissFundData.SwissFundData)

    p15 = multiprocessing.Process(target=CSV_Collector.collector)

    ### 1. Tranche
    p8_ubs.start()
    print('Start UBS')
    p11_ubs_ch.start()
    print('Start UBS CH')
    p7_ubs_ast.start()
    print('Start UBS AST')
    p7_ubs_ast.join()
    p8_ubs.join()
    p11_ubs_ch.join()

    ### 2. Tranche
    p16_swiss_fund_data.start()
    p12_fundsquare.start()
    print('Start Swiss Fund Data')
    print('Start Fundsquare')
    p16_swiss_fund_data.join()
    p12_fundsquare.join()


    ### 3. Tranche
    p9_tramondo.start()
    print('Start Tramondo')
    p4_small.start()
    print('Start ishares, CSA Private, Bridgemer, KGAST')
    p9_tramondo.join()
    p4_small.join()

    ### 4. Tranche
    p2_morning_de.start()
    p6_swisscanto.start()
    print('Start swisscanto')
    print('Start morningstar DE')
    p2_morning_de.join()
    p6_swisscanto.join()

    ### 5. Tranche
    p1_cs.start()
    p3_morning_ch.start()
    print('Start CS')
    print('Start morningstar CH')
    p1_cs.join()
    p3_morning_ch.join()

    ### 6. Tranche
    p5_fundinfo.start()
    print('Start fundinfo')
    p5_fundinfo.join()

    ### 7.Tranche
    p13_cs_fx.start()
    print('Start CS FX')
    p13_cs_fx.join()

    p15.start()
    print('Start Collector')

    end = timer()
    print("Der Scraper benötigte " + str((end-start)/60) + " Minuten")


if __name__ == '__main__':
    main()