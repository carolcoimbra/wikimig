**DATASETS:**



**EU Monthly stocks: (EUROSTAT)\[https://ec.europa.eu/eurostat/databrowser/view/migr\_asytpsm\_\_custom\_17904294/default/table]**



* Beneficiaries of temporary protection at the end of the month by citizenship, age and sex - monthly data



The dataset in the structure: **migr\_asytpsm\_\_custom\_17904294\_spreadsheet**

contains the **number of beneficiaries of temporary protection at the end of the month per EU country.**



**To account for stocks per year, I selected the latest month for each year (2022, 2023, and 2024). --> STOCKS YYYY**

**The clean dataset is called: UK\_in\_EU\_stocks.csv**





**EU Monthly applications: (EUROSTAT)\[https://ec.europa.eu/eurostat/databrowser/view/migr\_asytpfm\_\_custom\_17920573/default/table]**

* Decisions granting temporary protection by citizenship, age and sex - monthly data



The dataset in the structure: **migr\_asytpfm\_\_custom\_17920573\_spreadsheet**

contains the **number of (new) beneficiaries of temporary protection by month per EU country.**



**The clean dataset is called: UK\_in\_EU\_monthly\_applications.csv**







**GERMANY:**



* **Code: (12531-0043)\[https://www-genesis.destatis.de/datenbank/online/statistic/12531/table/12531-0043]**

Persons seeking protection: Administrative districts, reference date, sex, category of protection status, citizenship



I am not using the **12531-0043** dataset because we do not need the breakdown per type of protection status.





* **Code: (12531-0041)\[https://www-genesis.destatis.de/datenbank/online/statistic/12531/table/12531-0041]**

Persons seeking protection: Administrative districts, reference date, sex, country groups/citizenship



All the datasets in the structure:**12531-0041\_en**

contains the **total number of applications of Ukrainians seeking protection per county** at the time 31.12.YYYY



**I am assuming that this data represents the total number of active applications of Ukrainians seeking protection per county at the time 31.12.YYYY. --> STOCKS YYYY**

**The clean dataset is called: UK\_in\_DE\_stocks.csv**





**POLAND: (DANE)\[https://dane.gov.pl/en/dataset/2715,zarejestrowane-wnioski-o-nadanie-statusu-ukr]**



* All the datasets in the structure: **WNIOSKI\_UKR\_YYYMMDD**

contains the number of all the applications actives per region + the entire country from 14.03.2022 until the time **DD.MM.YYYY**



I am not using the **WNIOSKI\_UKR\_YYYMMDD** dataset because we do not need the breakdown per region, but per county.





* All the datasets in the structure: **STATYSTYKI\_POWIAT\_UKR\_YYYMMDD**

contains the **number of all the applications actives per county at the time DD.MM.YYYY**



**To account for stocks per year, I selected the latest dataset for each year (2022, 2023, and 2024), which correspond to 26.12.2022, 12.12.2023, and 10.12.2024. --> STOCKS YYYY**

**The clean dataset is called: UK\_in\_PL\_stocks.csv**





**UNHCR: UKRAINE - POLAND BORDER CROSSING**

Dataset collected from UHNCR website (not available anymore) containing the daily number of people who crossed the border from Ukraine to Poland. **Dataset: Border crossings to Poland from Ukraine - by date.csv**





**UNHCR: Ukrainian refugees in Europe (used in our paper v1, but not being used anymore after we collected the data for EU countries from eurostat)**

Dataset from UHNCR website showing the number of Ukrainian refugees in European countries as of the date of collection (October, 2023).

**Dataset: UNHCR\_ukrainian\_refugees\_europe\_data.csv**

