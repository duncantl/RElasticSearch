**Note:** This is some prototype or example code for working with ElasticSearch from within R. It uses a REST and JSON interface.

RElasticSearch
====
[RElasticSearch](https://github.com/duncantl/RElasticSearch) is an R library for reading data from an [ElasticSearch](http://www.elasticsearch.org) index.

Installation
---
	install.packages('devtools')
	library(devtools)
	install_git('git://github.com/duncantl/RElasticSearch.git')
	library(RElasticSearch)
	
Quick Start
---
The following assumes an ElasticSearch index running at `localhost:9200` and with an index called *documents* containing etnries with a *title* and *text*.

Create a server:

	es.server <- new('ElasticSearchServer', host='localhost', port=9200L)
	
Next create an index:

	es.index <- new('ElasticSearchIndex', server=es.server, index='documents')

Let's perform a keyword match on the titles:

	query(es.index, text=list(query_text='keyword'))
	
Finally, let's do a query search on the text:

	query(es.index, query_string=list(default_field='text', query='this AND that'))
	
That's it!