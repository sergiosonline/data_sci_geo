
# coding: utf-8

# In[1]:


import pandas as pd
get_ipython().run_line_magic('matplotlib', 'inline')
import numpy as np
import matplotlib.pyplot as plt


# # Read files

# In[3]:


trt = pd.read_csv('./a2/Automobile.csv')
hda = pd.read_csv('./a2/HDA.csv', encoding='ISO-8859-1')
ri = pd.read_csv('./a2/RI.csv')
weather = pd.read_csv('./a2/toronto.csv', encoding='ISO-8859-1')
hda_trt = hda[hda['City'] == 'Toronto']
hda_trt.reset_index(inplace=True,drop=True)
ri_trt = ri[ri['City'] == 'Toronto']
ri_trt.reset_index(inplace=True, drop=True)


# ## Clean Toronto City data(traffic police)

# In[4]:


trt['MONTH'] = trt.DATE.str.split('-').str[1].astype(int)
trt['DAY'] = trt.DATE.str.split('T').str[0].str.split('-').str[2].astype(int)
trt_clean = trt[['X', 'Y', 'Index_', 'ACCNUM', 'YEAR', 'ROAD_CLASS', 'District', 'LATITUDE', 'LONGITUDE',
       'ACCLOC', 'TRAFFCTL', 'VISIBILITY', 'LIGHT', 'RDSFCOND','PEDESTRIAN','CYCLIST', 'AUTOMOBILE', 'MOTORCYCLE', 
        'TRUCK', 'SPEEDING', 'AG_DRIV', 'REDLIGHT', 'ALCOHOL','DISABILITY', 'Division', 'Ward_ID', 'Hood_ID','MONTH', 'DAY']]


# In[5]:


trt_clean.join(trt_clean['ROAD_CLASS'].str.get_dummies())
trt_clean.join(trt_clean['TRAFFCTL'].str.get_dummies()[[u'No Control',u'Traffic Signal']])
trt_clean.join(trt_clean['ACCLOC'].str.get_dummies()[['At Intersection', 'Non Intersection']])
trt_clean.join(trt_clean['VISIBILITY'].str.get_dummies()['Clear'])
trt_clean['Dark'] = trt_clean['LIGHT'].str.get_dummies()['Dark']+trt_clean['LIGHT'].str.get_dummies()[u'Dark, artificial']
trt_clean['Dawn'] = trt_clean['LIGHT'].str.get_dummies()['Dawn']+trt_clean['LIGHT'].str.get_dummies()[u'Dawn, artificial']
trt_clean['Daylight'] = trt_clean['LIGHT'].str.get_dummies()['Daylight']+trt_clean['LIGHT'].str.get_dummies()[u'Daylight, artificial']
trt_clean['Dusk'] = trt_clean['LIGHT'].str.get_dummies()['Dusk']+trt_clean['LIGHT'].str.get_dummies()[u'Dusk, artificial']
trt_clean.join(trt_clean['RDSFCOND'].str.get_dummies()[['Dry','Wet']])
trt_clean['Inv_PED'] = trt_clean['PEDESTRIAN'].str.get_dummies()['Yes']
trt_clean['Inv_CYC'] = trt_clean['CYCLIST'].str.get_dummies()['Yes']
trt_clean['Inv_AM'] = trt_clean['AUTOMOBILE'].str.get_dummies()['Yes']
trt_clean['Inv_MC'] = trt_clean['MOTORCYCLE'].str.get_dummies()['Yes']
trt_clean['Inv_TC'] = trt_clean['TRUCK'].str.get_dummies()['Yes']
trt_clean['Speeding'] = trt_clean['SPEEDING'].str.get_dummies()['Yes']
trt_clean['Ag_Driv'] = trt_clean['AG_DRIV'].str.get_dummies()['Yes']
trt_clean['Redlight'] = trt_clean['REDLIGHT'].str.get_dummies()['Yes']
trt_clean['Alcohol'] = trt_clean['ALCOHOL'].str.get_dummies()['Yes']
trt_clean['Disability'] = trt_clean['DISABILITY'].str.get_dummies()['Yes']


# In[6]:


trt_clean=trt_clean[['X', 'Y', 'Index_', 'ACCNUM', 'YEAR', 
       'LATITUDE', 'LONGITUDE', 'Ward_ID', 'Hood_ID', 'MONTH', 'DAY', 'Dark', 'Dawn',
       'Daylight', 'Dusk', 'Inv_PED', 'Inv_CYC', 'Inv_AM', 'Inv_MC', 'Inv_TC',
       'Speeding', 'Ag_Driv', 'Redlight', 'Alcohol', 'Disability']]


# ## Merge Geotab Harzard Driving Area ,Road Impediments to TRT

# In[7]:


for i in range(len(hda_trt)):
    for j in range(len(trt_clean)):
        if trt_clean.loc[j,'LATITUDE'] >= hda_trt.loc[i,'Latitude_SW'] and trt_clean.loc[j,'LATITUDE'] <= hda_trt.loc[i,'Latitude_NE']and trt_clean.loc[j,'LONGITUDE'] >= hda_trt.loc[i,'Longitude_SW']and trt_clean.loc[j,'LONGITUDE'] <= hda_trt.loc[i,'Longitude_NE']:
            trt_clean.loc[j,'SeverityScore'] = hda_trt.loc[i,'SeverityScore']
            trt_clean.loc[j,'IncidentsTotal'] = hda_trt.loc[i,'IncidentsTotal']
            trt_clean.loc[j,'Geohash'] = hda_trt.loc[i,'Geohash']
    #print(i)


# In[8]:


for i in range(len(ri_trt)):
    for j in range(len(trt_clean)):
        if trt_clean.loc[j,'LATITUDE'] >= ri_trt.loc[i,'Latitude_SW'] and trt_clean.loc[j,'LATITUDE'] <= ri_trt.loc[i,'Latitude_NE']and trt_clean.loc[j,'LONGITUDE'] >= ri_trt.loc[i,'Longitude_SW']and trt_clean.loc[j,'LONGITUDE'] <= ri_trt.loc[i,'Longitude_NE']:
            trt_clean.loc[j,'AvgAcceleration'] = ri_trt.loc[i,'AvgAcceleration']
            trt_clean.loc[j,'PercentOfVehicles'] = ri_trt.loc[i,'PercentOfVehicles']
            trt_clean.loc[j,'AvgMonthlyVolume'] = ri_trt.loc[i,'AvgMonthlyVolume']
            trt_clean.loc[j,'PercentCar'] = ri_trt.loc[i,'PercentCar']
            trt_clean.loc[j,'PercentMPV'] = ri_trt.loc[i,'PercentMPV']
            trt_clean.loc[j,'PercentLDT'] = ri_trt.loc[i,'PercentLDT']
            trt_clean.loc[j,'PercentMDT'] = ri_trt.loc[i,'PercentMDT']
            trt_clean.loc[j,'PercentHDT'] = ri_trt.loc[i,'PercentHDT']
            trt_clean.loc[j,'PercentOther'] = ri_trt.loc[i,'PercentOther']
            trt_clean.loc[j,'Geohash'] = ri_trt.loc[i,'Geohash']            
    print(i)


# ## Merge Weather to TRT

# In[9]:


weather_clean = weather[weather['Year']>2006]
weather_clean['Daily_dif'] = np.abs(weather_clean[u'Max Temp (°C)']- weather_clean[u'Min Temp (°C)'])
weather_trt = weather_clean.groupby(['Year', 'Month']).agg({u'Max Temp (°C)':'max', u'Min Temp (°C)':'min', 'Daily_dif':'max',
                                             u'Mean Temp (°C)':'mean', u'Total Rain (mm)':'mean',
                                                           u'Total Snow (cm)':'mean'})


# In[10]:


for year in range(2007,2018):
    for month in range(1,13):
        index = trt_clean[(trt_clean['YEAR']==year)&(trt_clean['MONTH']==month)].index
        trt_clean.loc[index,'Max_Temp'] = weather_trt.loc[year,month]['Max Temp (°C)']
        trt_clean.loc[index,'Min_Temp'] = weather_trt.loc[year,month]['Min Temp (°C)']
        trt_clean.loc[index,'Daily_dif'] = weather_trt.loc[year,month]['Daily_dif']
        trt_clean.loc[index,'Ave_Temp'] = weather_trt.loc[year,month]['Mean Temp (°C)']
        trt_clean.loc[index,'Rain_vol'] = weather_trt.loc[year,month]['Total Rain (mm)']
        trt_clean.loc[index,'Snow_vol'] = weather_trt.loc[year,month]['Total Snow (cm)']


# ## Arrange by Year, Month, Ward_ID

# In[11]:


trt_clean.drop(['X','Y'], axis=1, inplace = True)


# In[18]:


trt_clean_ward = trt_clean.groupby(['YEAR','MONTH','Ward_ID']).agg({'Index_':'count','Dark':'sum','Dawn':'sum','Daylight':'sum',
                                                  'Dusk':'sum','Inv_PED':'sum','Inv_CYC':'sum',
                                                  'Inv_AM':'sum', 'Inv_MC':'sum','Inv_TC':'sum',
                                                  'Speeding':'sum','Ag_Driv':'sum', 'Redlight':'sum',
                                                  'Alcohol':'sum', 'Disability':'sum', 'SeverityScore':'mean',
                                                  'IncidentsTotal':'sum', 'AvgAcceleration':'mean',
                                                  'PercentOfVehicles':'mean', 'AvgMonthlyVolume':'mean',
                                                  'PercentCar':'mean', 'PercentMPV':'mean', 'PercentLDT':'mean',
                                                  'PercentMDT':'mean', 'PercentHDT':'mean', 'PercentOther':'mean',
                                                  'Daily_dif':'mean', 'Max_Temp':'max', 'Min_Temp':'min',
                                                  'Ave_Temp':'mean','Rain_vol':'mean','Snow_vol':'mean' })


# In[19]:


trt_clean_ward2 = trt_clean_ward.reset_index(level=['Ward_ID','MONTH','YEAR'])


# In[ ]:


for year in range(2007,2018):
    for month in range(1,13):
        for w in range(1, 45):
            if w in trt_clean_ward2[(trt_clean_ward2['YEAR']==year)&(trt_clean_ward2['MONTH']==month)]['Ward_ID']:
                continue
            else:
                trt_clean_ward2 = trt_clean_ward2.append({'YEAR':year, 'MONTH':month, 'Ward_ID':w}, ignore_index = True)


# In[ ]:


trt_clean_ward2[['YEAR','MONTH','Ward_ID']] = trt_clean_ward2[['YEAR','MONTH','Ward_ID']].astype(int)


# In[ ]:


trt_clean_ward2.sort_values(by=['YEAR', 'MONTH','Ward_ID'],inplace=True)
trt_clean_ward2.reset_index(inplace=True,drop=True)


# In[ ]:


trt_clean_ward2.to_csv('incident_by_year_month_ward_2Mar.csv')

