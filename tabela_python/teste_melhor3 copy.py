import requests
import pandas as pd
import time

while True:
    parameters = [
        'T10M',
        'QV10M',
        'GLOBAL_ILLUMINANCE',
        'PW',
        'GWETTOP'
    ]

    parameters_subset = ','.join(parameters[:20])
    url = f'https://power.larc.nasa.gov/api/temporal/daily/point?parameters=T2M,PRECTOTCORR&community=RE&longitude=-39.6117&latitude=-8.4182&start=19810102&end=20240502&format=JSON'
    response = requests.get(url)
    data = response.text
    data_parts=data.split('-END HEADER-')

    explanation = data_parts[0]
    with open('explicacao_text', 'w') as f:
        f.write(explanation)

    actual_data = data_parts[1].strip()
    with open('dados2.csv', 'w') as f:
        f.write(actual_data)

    df = pd.read_csv('dados2.csv')
    df.replace(-999, pd.NA, inplace=True)

    time.sleep(5)

