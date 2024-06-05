import pandas as pd
import json

data = f'https://power.larc.nasa.gov/api/temporal/daily/point?parameters=T2M,PRECTOTCORR&community=RE&longitude=-39.6117&latitude=-8.4182&start=19810102&end=20240502&format=JSON'

data_dict = json.loads(data)

df = pd.DataFrame(list(data_dict.items()), columns=['Date', 'Temperature'])

# Converter a coluna de datas para o tipo datetime
df['Date'] = pd.to_datetime(df['Date'], format='%Y%m%d')

# Exibir o DataFrame
print(df)