import requests
import pandas as pd

def get_tf_data():

    url = 'https://datos.estadisticas.pr/api/action/datastore_search?resource_id=824d2a93-7998-4f37-8fe9-9e110913ec5e&limit='
    num = '10000000'

    url += num

    data_response = requests.get(url)
    data_table_prem = pd.DataFrame.from_dict(data_response.json()['result']['records'])
    data_table_final = data_table_prem.drop(columns=['_id'])
    data_table_final.to_csv("python-csv-dataset.csv", index=False)
