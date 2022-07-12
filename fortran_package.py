import yaml
from pathlib import Path
from collections import Counter
import json
import requests
from requests.structures import CaseInsensitiveDict
from datetime import datetime
import pytz
from collections import OrderedDict
import numpy as np

#print("learn section")
info = requests.get('https://raw.githubusercontent.com/fortran-lang/fortran-lang.org/master/_data/package_index.yml').text
fortran_index = yaml.safe_load(info)
#print(conf['books'])
info = requests.get('https://raw.githubusercontent.com/fortran-lang/fortran-lang.org/master/_data/learning.yml').text
conf = yaml.safe_load(info)
headers = CaseInsensitiveDict()
headers["Authorization"] = "Basic aGVuaWxwMTA1OmdocF9TQTZxaHFBYmZVVGNkaEZHc0tnbnllN25vcndUSzk0T0dTVGg="
#this auth token is only meant for fortran github api use. please DONT misuse it.

fortran_index_tags = []
fortran_index_tags_50 = []
fortran_index_categories = []
fortran_index_libraries = []
fortran_index_data_types = []
fortran_index_strings = []
fortran_index_programming = []
fortran_index_graphics = []
fortran_index_interfaces = []
fortran_index_examples = []
fortran_index_scientific = []
fortran_index_io = []
fortran_index_numerical = []

for i in fortran_index:
    try:
        for j in str(i['tags']).split():
            fortran_index_tags.append(j)
    except KeyError:
        print("")
    if "libraries" in i['categories'].split():
        fortran_index_libraries.append(i)
    if "data-types" in i['categories'].split():
        fortran_index_data_types.append(i)
    if "strings" in i['categories'].split():
        fortran_index_strings.append(i)
    if "programming" in i['categories'].split():
        fortran_index_programming.append(i)
    if "graphics" in i['categories'].split():
        fortran_index_graphics.append(i)
    if "interfaces" in i['categories'].split():
        fortran_index_interfaces.append(i)
    if "examples" in i['categories'].split():
        fortran_index_examples.append(i)
    if "scientific" in i['categories'].split():
        fortran_index_scientific.append(i)
    if "io" in i['categories'].split():
        fortran_index_io.append(i)
    if "numerical" in i['categories'].split():
        fortran_index_numerical.append(i)

fortran_tags = {
  "fortran_tags": "tags"
}
fortran_index_tags = Counter(fortran_index_tags)
a = sorted(fortran_index_tags.items(), key=lambda x: x[1],reverse=True)
for i in a:
    if i[0]=="None":
        a.remove(i)

#print(fortran_index_libraries)
for k in range(50):
    fortran_index_tags_50.append(a[k][0])

for i in fortran_index:
    for j in i['categories'].split():
        fortran_index_categories.append(j)

fortran_index_categories  = list(set(fortran_index_categories))

def github_info(list):
  for i in list:
    try:
        info = requests.get('https://api.github.com/repos/'+i['github'], headers=headers).text
        d = json.loads(info)
        if type(d['forks_count']) is type(None):
            d['forks_count'] = 0
        if type(d['open_issues_count']) is type(None):
            d['open_issues_count'] = 0
        if type(d['stargazers_count']) is type(None):
            d['stargazers_count'] = 0
        try:
            if str(d['license']['name']) =='null':
                print('hello')
                d['license']['name'] = 'null'
            i['license'] = d['license']['name']
        except TypeError:
            print("")
            d['license'] = 'null'
        #print(d['forks_count'],d['open_issues_count'],d['stargazers_count'])
        i['forks'] = d['forks_count']
        i['issues'] = d['open_issues_count']
        i['stars'] = d['stargazers_count']
        info = requests.get('https://api.github.com/repos/'+i['github']+'/commits/'+d['default_branch'], headers=headers).text
        d = json.loads(info)
        monthinteger = int(d['commit']['author']['date'][5:7])
        month = datetime.date(1900, monthinteger, 1).strftime('%B')
        i['last_commit'] = month+" "+d['commit']['author']['date'][:4]
        info = requests.get('https://api.github.com/repos/'+i['github']+'/releases/latest', headers=headers).text
        d = json.loads(info)
        #print(d)
        try:
            i['release'] = d['tag_name']
        except KeyError:
            print("")
    except KeyError:
        print("")

github_info(fortran_index_data_types)
github_info(fortran_index_numerical)
github_info(fortran_index_io)
github_info(fortran_index_scientific)
github_info(fortran_index_examples)
github_info(fortran_index_interfaces)
github_info(fortran_index_graphics)
github_info(fortran_index_programming)
github_info(fortran_index_strings)
github_info(fortran_index_libraries)
print(fortran_index_data_types)
fortran_tags['numerical'] =  fortran_index_numerical
fortran_tags['io'] =  fortran_index_io
fortran_tags['scientific'] =  fortran_index_scientific
fortran_tags['examples'] =  fortran_index_examples
fortran_tags['interfaces'] =  fortran_index_interfaces
fortran_tags['graphics'] =  fortran_index_graphics
fortran_tags['programming'] =  fortran_index_programming
fortran_tags['strings'] =  fortran_index_strings
fortran_tags['data_types'] =  fortran_index_data_types
fortran_tags['libraries'] =  fortran_index_libraries
fortran_tags['tags'] =  fortran_index_tags_50
conf['reference_books'] = conf['reference-books']
conf['reference_courses'] = conf['reference-courses']
conf['reference_links'] = conf['reference-links']

with open("fortran_package.json", "w") as f:
    json.dump(fortran_tags, f)
with open("fortran_learn.json", "w") as f:
    json.dump(conf, f)

fortran_monthly =[]
fortran_commits =[]
fpm_monthly =[]
fpm_commits =[]
stdlib_monthly =[]
stdlib_commits =[]

months = ["Unknown", "January","Febuary","March", "April","May","June","July","August","September","October","November","December"]
def monthly_graph(repo):
  info = requests.get('https://api.github.com/repos/fortran-lang/'+repo+'/stats/code_frequency').text
  d = json.loads(info)
  c = 0
  monthly_commits=[]
  commits=[]
  for i in range(0,len(d),4):
    for j in range(4):
      try:
        c = c + d[i+j][1]
      except:
       print("")
    date_time = datetime.fromtimestamp(d[i][0])
    monthly = str(months[date_time.month])+" "+str(date_time.year)
    commits.append(c)
    monthly_commits.append(monthly)
    c=0
  test_chart = {"data": [
      {
        "x": monthly_commits,
        "y": commits,
      }
    ],
    "layout": {
      "margin": {
        "t": 15,
        "b": 30,
        "r": 15,
        "l": 35
      }
    }
  }
  if repo =='fortran-lang.org':
    fortran_monthly = monthly_commits
    fortran_commits = commits
  if repo =='fpm':
    fpm_monthly = monthly_commits
    fpm_commits = commits
   # print(fpm_monthly , fpm_commits)
  if repo =='stdlib':
    stdlib_monthly = monthly_commits
    stdlib_commits = commits
    #print(stdlib_monthly , stdlib_commits)
  print(test_chart)
  with open("source/charts/"+repo+".json", "w") as f:
        json.dump(test_chart, f)
graphs =["fortran-lang.org","fpm","stdlib"]
for i in graphs:
  monthly_graph(i)
