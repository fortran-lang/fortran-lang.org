import yaml
from pathlib import Path
print("learn section")
conf = yaml.safe_load(Path('_data/learning.yml').read_text())
print(conf['reference-books'][0])

learn_file = open("source/learn/index.md", "a")  
learn_file.write("""---
layout: book
title: Learn Fortran 
---
<h1>Learn Fortran</h1>
         <h3>Learning resources for beginners and experts alike</h3>
         <hr>
    
      
<h2>Getting Started</h2>
         <table cellpadding="10">
            <tr>
               <td>
                  <h3><i class="fas fa-info-circle"></i>
                     New to Fortran</h3>
                  <p>
                     Try the quickstart Fortran tutorial, to
                     get an overview of the language syntax and capabilities.
                  </p>
                  <a class="button center" href="/learn/quickstart">
                     <i class="fas fa-book"></i>
                     <b> Quickstart tutorial</b>
                  </a>
               </td>
     
<td>

<h3><i class="fas fa-question-circle"></i>
            Looking for help</h3>
         <p>
            Ask a question in the Fortran-lang discourse - a forum
            for friendly discussion of all things Fortran.

</p>
         <a class="button center blue" href="https://fortran-lang.discourse.group/" target="_blank">
            <b> <i class="fab fa-discourse"></i>
               Fortran-lang Discourse
         </a></b>
</table>
<hr>
<h2 id="book-index">Mini-book Tutorials</h2>
      <h3><i class="fas fa-cubes"></i> Getting started</h3>

""")

learn_file.write("\n")
for i in conf['books']:   #books section
    learn_file.write("* "+"["+i['title']+"]"+"("+i['link']+')')
    learn_file.write("\n")

learn_file.write(""" <h2 id="book-index">Other Resources</h2><h3> <i class="fas fa-server"></i> On the web </h3> \n""")
learn_file.write("\n")

for i in conf['reference-links']: # reference url
    learn_file.write("* "+"["+i['name']+"]"+"("+i['url']+')'+" "+str(i['description']))
    learn_file.write("\n")

learn_file.write("""  <h3> <i class="fas fa-book"></i> Online courses </h3> \n""")

for i in conf['reference-courses']: # online courses
    description = i.get('description')
    learn_file.write("* "+"["+i['name']+"]"+"("+i['url']+')'+" "+str(description))
    learn_file.write("\n")


learn_file.write("""<h3> <i class="fas fa-book-open"></i> In print </h3> \n""")
learn_file.write("\n")
for i in conf['reference-books']: # in print
    location = i.get('location')
    learn_file.write("* "+i['author']+" "+str(i['year'])+" ["+i['title']+"]" + "("+i['url']+')'+" "+i['edition']+" "+str(location)+" "+i['publisher'])
    learn_file.write("\n")

learn_file.close()
