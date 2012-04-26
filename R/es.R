# Concepts: server, index, type, id, page, hit
#
# http://www.slideshare.net/clintongormley/cool-bonsai-cool-an-introduction-to-elasticsearch
#  index <-> database
#  type  <-> table
#  document <-> row
#  field <-> column
#  schema <-> mapping
#

library(RJSONIO)
library(RCurl)
library(XML)

setClass("ElasticSearchServer",
           representation(host = "character",
                          port = "integer"),
           prototype = prototype(host = "localhost", port = 9200L))


setClass("URI", contains = 'character')


setGeneric("asURI",
            function(obj, path) {
               standardGeneric("asURI")
            })

setMethod("asURI", "ANY",
           function(obj, path) {
             sprintf("%s/%s", as(obj, "URI"), as(path, "character"))
           })

setAs("ElasticSearchServer", "URI",
       function(from) {
           sprintf("http://%s:%d", from@host, from@port)
         })

#setClass("MongoDBCollection",
#           representation(collection = "character"), contains = "MongoDBDatabase")

setClass("ElasticSearchServerIndex",
          representation(server = "ElasticSearchServer",
                         index = "character"))

setClass("ElasticSearchServerIndexType",
             representation(type = "character"),
                contains = "ElasticSearchServerIndex")
  
setAs("ElasticSearchServerIndex", "URI",
       function(from) {
           sprintf("%s/%s", as(from@server, "URI"), from@index)
         })

setAs("ElasticSearchServerIndexType", "URI",
       function(from) {
           sprintf("%s/%s/%s", as(from@server, "URI"), from@index, from@type)
         })

# searchES("lm", server = rhelp)
# searchES("title:help AND author:Sanyal", server = rhelp)

setGeneric("searchES",
            function(query, index, fields = character(), from = 0L, size = NA, server = new("ElasticSearchServer"))
              standardGeneric("searchES"))

setMethod("searchES", c(index = "character", server = "ElasticSearchServer"),
           function(query, index, fields = character(), from = 0L, size = NA, server = new("ElasticSearchServer")) {
             url = sprintf("%s/%s/_search", as(server, "URI"), index)
             params = list(q = query)
             params[["from"]] = as.character(as.integer(from))
             if(!is.na(size))
                params[["size"]] = as.character(as.integer(size))
             if(length(fields))
                  params[["fields"]] = paste(fields, collapse = ",")
             
             txt = getForm(url, .params = params) 
             ans = fromJSON(txt)
             ans$hits$hits
           })

setMethod("searchES", signature(index = "missing", server = "ElasticSearchServerIndex"),
           function(query, index, fields = character(), from = 0L, size = NA, server = new("ElasticSearchServer")) {
             searchES(query, server@index, fields, from = from, size = size, server = server@server)
           })


#XXX Allow fields
setGeneric("getES",
            function(index, type, id, fields = character(), server = new("ElasticSearchServer"))
              standardGeneric("getES"))

setMethod("getES", c("ElasticSearchServerIndex", "missing", "missing"),
           function(index, type, id, fields = character(), server = new("ElasticSearchServer"))  {
             u = sprintf("%s", asURI(index, "URI"))
             httpGET(u)
           })


setGeneric("insertES",
           function(to, value, id, index, ...) {
             standardGeneric("insertES")
           })

setMethod("insertES", c("ElasticSearchServerIndex"),
           function(to = new("ElasticSearchServer"), value, id, index,  ...) {
             url = sprintf("%s/%s", as(to, "URI"))
             httpPUT(url, toJSON(value))
           })

setMethod("insertES", c("ElasticSearchServerIndexType"),
           function(to = new("ElasticSearchServer"), value, id, index,  ...) {
             url = sprintf("%s/%s", as(server, "URI"), id)
             httpPUT(url, toJSON(value))
           })


setMethod("length", "ElasticSearchServerIndex",
           function(x) {
             txt = getURLContent(sprintf("%s/_count", as(x, "URI")))
             fromJSON(txt)[["count"]]
           })

setGeneric("count",
            function(x, query, ...)
               standardGeneric("count"))

setMethod("count", c("ElasticSearchServerIndex"),
           function(x, query, ...) {
             url = if(missing(query))
                       sprintf("%s/_count", as(x, "URI"))
                   else
                     sprintf("%s/_count?q=%s", as(x, "URI"), query)

             txt = getURLContent(url)
             fromJSON(txt)[["count"]]
           })


# rhelp = new("ElasticSearchServerIndex", index = "rhelp2")

# Get names of pages/items in an index,
#  what are the "fields" in the rss
#   getURLContent(sprintf("%s/_mapping", as(rhelp, "URI")))
# query across multiple indices  /index,index/_search
# CRUD
#    create - settings (shards, replicas); mappings
#    retrieve
#    update
#    delete
#
#    refresh
#    open and close an index
#
#  search queries with Lucene DSL via JSON. See video
#
#  types - set and query on an index.
#
# [Done] settings
#    getURLContent(sprintf("%s/_settings", as(rhelp, "URI")))
#
#  specify mappings
#
#  email messages
#
#  analyzer
#
#
#
#


setGeneric("deleteIndex",
           function(obj, index, ...) {
             standardGeneric("deleteIndex")
           })

setMethod("deleteIndex", "ElasticSearchServerIndex",
           function(obj, index, ...) {
             ans = httpDELETE(as(obj, "URI"))
             fromJSON(ans)
           })

setMethod("deleteIndex", c("ElasticSearchServer", "character"),
           function(obj, index, ...) {
             ans = httpDELETE(sprintf("%s/%s", as(obj, "URI"), index))
             fromJSON(ans)
           })

setGeneric("getMapping",
           function(obj, index, ...) {
             standardGeneric("getMapping")
           })

setMethod("getMapping", c("ElasticSearchServerIndex", "missing"),
           function(obj, ...) {
             getESURL(obj, "_mapping")
           })

getESURL =
function(serverIndex, cmd)
{
  txt = getURLContent(sprintf("%s/%s", as(serverIndex, "URI"), cmd))
  fromJSON(txt)[[1]]
}


setGeneric("getSettings",
           function(obj, index, ...) {
             standardGeneric("getSettings")
           })

setMethod("getSettings", c("ElasticSearchServerIndex", "missing"),
           function(obj, ...) {
             getESURL(obj, "_settings")
           })


setMethod("names", "ElasticSearchServerIndex",
          function(x) {
                # Use a more efficient query to get just the _id's and not the whole thing.
            sapply(x[], `[[`, "_id")
          })

setMethod("length", "ElasticSearchServerIndex",
          function(x) {

               a = fromJSON(getURL(sprintf("%s/_status", as(x, "URI"))))
               as.integer(a$indices[[1]]$docs["num_docs"])

               # a different approach which queries and matches everything and gets the count.
#               u = sprintf("%s/_search?search_type=count", as(x, "URI"))
#               tmp = httpGET(u, postfields = '{"query": { "match_all" : { }}}')
#               as.integer(fromJSON(tmp)$hits$total)
             })

setMethod("[", c("ElasticSearchServerIndex", "missing", "missing"),
           function(x, i, j, ...) {

                 # find the number of elements
              u = sprintf("%s/_search?search_type=count", as(x, "URI"))
              xx = fromJSON(httpGET(u, postfields = '{"query": { "match_all" : { }}}'))
                 # add 20% in case it grows in the time between requests
              num = as.integer(xx$hits$total * 1.2)

                 # Get/Query that many items
              u = sprintf("%s/_search", as(x, "URI"))
              xx = fromJSON(getForm(u, size = num, .opts = list(postfields = '{"query": { "match_all" : { }}}')))
              xx$hits$hits
           })

setMethod("[", c("ElasticSearchServerIndex", "ANY", "missing"),
           function(x, i, j, ...) {
browser()
           })

setGeneric("query", 
           function(x, ..., curl = getCurlHandle(), .opts = list())
                standardGeneric("query"))

    # a = query(gn, text = list(title = "Gingrich"))
    # b = query(gn, query_string = list(fields = c("title", "description"), query = "Gingrich"))
setMethod("query", "ElasticSearchServerIndex",
           function(x, ..., curl = getCurlHandle(), .opts = list())
           {
                 # make the query string
              query = list(query = list(...))
              query = toJSON(query)

                 # find out how many results we have
              u = sprintf("%s/_search", as(x, "URI"))
              tmp = getForm(u, "search_type" = "count",
                             .opts = list(postfields = query), curl = curl)
              num = fromJSON(tmp)$hits$total

                 # now query again asking for that many items.
                 # Could use a rolling scan.
              tmp = getForm(u, "size" = num, 
                             .opts = list(postfields = query), curl = curl)              
              fromJSON(tmp)$hits$hits
           })

            
           
