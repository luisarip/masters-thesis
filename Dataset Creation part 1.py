### Dataset Creation part I 
### Language: Python
### Author: Luisa Ripoll Alberola

pip install advertools

### Loading libraries

from google.colab import drive

import numpy as np
import pandas as pd

from datetime import timedelta
import advertools as adv
from urllib.parse import urlparse
import requests
from bs4 import BeautifulSoup
from multiprocessing import Pool
from tqdm import tqdm

# Mount Google Drive to Colab
drive.mount('/content/drive', force_remount = True)

drive_file_path = '/content/drive/MyDrive/youth_url.csv'

# Read the CSV file into a Pandas DataFrame
youth_url = pd.read_csv(drive_file_path, skiprows=6, header=0, nrows=5000)
youth_url.tail()

### Preprocessing time data: some instances are "<00:00:01", which is not a significant time.

# Filter the DataFrame to include only rows where the column value contains '<'
rows_with_less_than = youth_url[youth_url['Duración media de la sesión'].str.contains('<', na=False)]
# Converting these values to "00:00:00"
youth_url.loc[rows_with_less_than.index, 'Duración media de la sesión'] = '00:00:00'

# Converting average session duration into seconds
youth_url['Duración media de la sesión'] = youth_url['Duración media de la sesión'].dt.total_seconds()

### Checking data types

youth_url.info()

youth_url['Usuarios'] = pd.to_numeric(youth_url['Usuarios'].str.replace('.', '', regex=True))
youth_url['Duración media de la sesión'] = pd.to_timedelta(youth_url['Duración media de la sesión'])
youth_url['Páginas/sesión'] = youth_url['Páginas/sesión'].str.replace(',', '', regex=True).astype('float') / 100.0
youth_url['Sesiones'] = pd.to_numeric(youth_url['Sesiones'].str.replace('.', '', regex=True))
youth_url['Porcentaje de rebote'] = youth_url['Porcentaje de rebote'].str.replace(',', '', regex=True).str.rstrip('%').astype('float') / 100.0

youth_url.info()

### Selecting only young readership data

# Drop rows where 'Edad' column is '25-34'
youth_url = youth_url[youth_url['Edad'] != '25-34']
# Drop the 'Edad' column
youth_url.drop(columns=['Edad'], inplace=True)

### Processing URL data: removing URLs which don't lead to articles, but to sections or other directions of the website

# Renaming URL column
youth_url.rename(columns = {'url (cd 41)':'URL'}, inplace = True)
# Drop the first row: unknown URL
youth_url = youth_url[~youth_url['URL'].str.contains('(other)')]
# Drop rows containing 'amp.larazon' in the 'URL' column
youth_url = youth_url[~youth_url['URL'].str.contains('amp.larazon')]

### Include all this modifications in a pipeline, which will be used to preprocess other datasets

# Define the pipeline function
def preprocess_pipeline(df):
    # Identify rows with '<' symbol in 'Duración media de la sesión' and replace with '00:00:00'
    rows_with_less_than = df[df['Duración media de la sesión'].str.contains('<', na=False)]
    df.loc[rows_with_less_than.index, 'Duración media de la sesión'] = '00:00:00'

    # Changing data types of most columns
    # df['Usuarios'] = pd.to_numeric(df['Usuarios'].str.replace('.', '', regex=True))
    df['Duración media de la sesión'] = pd.to_timedelta(df['Duración media de la sesión'])
    df['Páginas/sesión'] = df['Páginas/sesión'].str.replace(',', '', regex=True).astype('float') / 100.0
    # df['Sesiones'] = pd.to_numeric(df['Sesiones'].str.replace('.', '', regex=True))
    df['Porcentaje de rebote'] = df['Porcentaje de rebote'].str.replace(',', '', regex=True).str.rstrip('%').astype('float') / 100.0

    # Drop rows where 'Edad' column is '25-34'
    df = df[df['Edad'] != '25-34']

    # Drop the 'Edad' column
    df.drop(columns=['Edad'], inplace=True)

    # Convert 'Duración media de la sesión' into seconds
    df['Duración media de la sesión'] = df['Duración media de la sesión'].dt.total_seconds()

    return df

### Repeating the process for other datasets

drive_file_path = '/content/drive/MyDrive/youth_tags.csv'
youth_tags = pd.read_csv(drive_file_path, skiprows=6, header=0, nrows=5000)
youth_tags.info()

# Apply pipeline
youth_tags = preprocess_pipeline(youth_tags)

drive_file_path = '/content/drive/MyDrive/youth_author.csv'
youth_author = pd.read_csv(drive_file_path, skiprows=6, header=0, nrows=5000)
youth_author.info()

# Apply pipeline
youth_author = preprocess_pipeline(youth_author)

### Merging the three datasets in one

# Merging first young_author
merged_df = pd.merge(youth_url, youth_author, on=['Usuarios', 'Usuarios nuevos', 'Duración media de la sesión', 'Páginas/sesión', 'Sesiones', 'Porcentaje de rebote'], how='left')
merged_df.head()

# Merging youth_tags to the previous
merged_df2 = pd.merge(merged_df, youth_tags, on=['Usuarios', 'Usuarios nuevos', 'Duración media de la sesión', 'Páginas/sesión', 'Sesiones', 'Porcentaje de rebote'], how='left')
merged_df2.tail()

### Checking the number of NaNs and 0s

nan_values = merged_df2.isna().sum()
zero_values = (merged_df2 == 0).sum()
print(zero_values)

# Rename df columns
merged_df2.rename(columns = {'autores (cd 6)': 'Author', 'tags (cd 20)': 'Tags'}, inplace = True)

###  Processing URL data: removing URLs which don't lead to articles, but to sections or other directions of the website

# Drop the first row
merged_df2 = merged_df2.iloc[1:]

# Drop log in website
merged_df2 = merged_df2[~merged_df2['URL'].str.contains('acceder')]
merged_df2 = merged_df2[~merged_df2['URL'].str.contains('https://www.larazon.es/?utm_source=redaccion&utm_medium=webpush&utm_campaign=notificacion')]

### Droping general section URLs using advertools

# Define a regular expression pattern to match the specified structure
pattern = r'^https://www\.larazon\.es/[^/]+/?$'

# Filter out URLs with the specified structure
df_filtered = merged_df2[~merged_df2['URL'].str.contains(pattern)]

### Creating new column 'Section'

# Define a function to extract section name
def extract_section(url):
    parsed_url = urlparse(url)
    # Get the path parts and remove empty strings
    path_parts = [part for part in parsed_url.path.split('/') if part]
    if path_parts:
        return path_parts[0]
    else:
        return None

# Apply the function to the 'URL' column and store the result in a new column 'Section'
merged_df2['Section'] = merged_df2['URL'].apply(extract_section)

# Printing unique values of new column 'Section'
merged_df2['Section'].unique()

# Checking NaN values
merged_df2['Section'].isna().sum()

# Frequency of each section
pd.set_option('display.max_rows', None)
merged_df2['Section'].value_counts()

### Some URLs don't follow a standard format, so they weren't correctly classified in 'Section'
### Manual correction:

pd.set_option('display.max_colwidth', None)
print(merged_df2[merged_df2['Section'] == 'un-tabaco-menos-danino-es-posible-y-ya-esta-aqui-EA18810325']['URL'])
merged_df2.loc[merged_df2['Section'] == 'un-tabaco-menos-danino-es-posible-y-ya-esta-aqui-EA18810325', 'Section'] = 'actualidad'

print(merged_df2[merged_df2['Section'] == 'como-dormir-junto-a-una-persona-que-ronca-y-XM7961462']['URL'])
merged_df2.loc[merged_df2['Section'] == 'como-dormir-junto-a-una-persona-que-ronca-y-XM7961462', 'Section'] = 'actualidad'

# Check other URLs with strange sections
print(merged_df2[merged_df2['Section'] == 'autores']['URL'])
print(merged_df2[merged_df2['Section'] == 'usuario']['URL'])
print(merged_df2[merged_df2['Section'] == 'autor']['URL'])
print(merged_df2[merged_df2['Section'] == 'buscador']['URL'])
print(merged_df2[merged_df2['Section'] == 'aviso-legal']['URL'])
print(merged_df2[merged_df2['Section'] == 'quienes-somos']['URL'])
print(merged_df2[merged_df2['Section'] == 'publicidad']['URL'])
print(merged_df2[merged_df2['Section'] == 'politica-cookies']['URL'])

# Remove these sections: they don't lead to articles
sections_to_remove = ['autores', 'usuario', 'autor', 'buscador', 'aviso-legal', 'quienes-somos', 'publicidad', 'politica-cookies']
merged_df3 = merged_df2[~merged_df2['Section'].isin(sections_to_remove)]

### Web scraping: save in the dataframe Author, Title, Subtitle, Content, Creation date. 
### Parallelised function.

# Define the scrape_article_info function
def scrape_article_info(url):
    response = requests.get(url)
    if response.status_code == 200:
        html_content = response.text
        soup = BeautifulSoup(html_content, 'html.parser')

        # Extract title
        title_element = soup.find('h1', class_='article-main__title')
        title = title_element.text.strip() if title_element else None

        # Extract subtitle
        subtitle_element = soup.find('h2', class_='article-main__description')
        subtitle = subtitle_element.text.strip() if subtitle_element else None

        # Extract author
        author_element = soup.find('div', class_='article-author__name')
        author = author_element.find('a').text.strip() if author_element and author_element.find('a') else None

        # Extract creation date
        creation_date_element = soup.find('div', class_='article-dates__detail')
        creation_date = creation_date_element.find('time').get('datetime')[:10] if creation_date_element else None

        # Extract article content
        content_element = soup.find('div', id='intext', class_='article-main__content')
        paragraphs = content_element.find_all('p') if content_element else []
        content = '\n'.join([p.text.strip() for p in paragraphs])

        return title, subtitle, author, creation_date, content
    else:
        return None, None, None, None, None

# Define a function for parallel processing
def parallel_scrape(url):
    return scrape_article_info(url)

# Set the number of processes to use
num_processes = 4

# Apply the scraping function to each URL in parallel with a progress bar
with Pool(num_processes) as pool:
    results = list(tqdm(pool.imap(parallel_scrape, merged_df3['URL']), total=len(merged_df3)))

# Unpack the results
title_list, subtitle_list, author_list, date_list, content_list = zip(*results)

# Add new columns to the DataFrame with the scraped information
merged_df3['Title'] = title_list
merged_df3['Subtitle'] = subtitle_list
merged_df3['Author'] = author_list
merged_df3['Date'] = date_list
merged_df3['Content'] = content_list

merged_df3.head()

### Saving the dataset 
file_path = '/content/drive/MyDrive/dataset1.csv'
merged_df3.to_csv(file_path)