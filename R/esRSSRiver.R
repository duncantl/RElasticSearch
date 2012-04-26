# http://rss.gmane.org/topics/excerpts/gmane.comp.lang.r.general


# Need XML package for parseURI.

setClass("ElasticSearchRiver", contains = "ElasticSearchServerIndex")


DefaultRSSMapping =
  list(page = list(properties = list(
                          title = c(type = "string"),
                          description = c(type = "string"),
                          author = c(type = "string"),
                          link = c(type = "string"))))

# Allow multiple rssURL and names and put in an array of feeds
# Allow the rssURL to have the names on them
# The name doesn't like capitals. Says "Bad Request"
rssRiver =
function(rssURL, name = basename(parseURI(rssURL)$path), server = new("ElasticSearchServer"),
           mapping = DefaultRSSMapping, rate = 900L, ..., curl = getCurlHandle())
{
  name = tolower(name)
  
  base = asURI(server, sprintf("%s/", name))
  httpPUT(base, '{}', ..., curl = curl)

  if(!is.null(mapping)) {
     val = toJSON(mapping)
     httpPUT(asURI(server, sprintf("%s/page/_mapping", name)), val, ..., curl = curl)
   }

  info = rssInfo(name, rssURL, rate)
  httpPUT(asURI(server, sprintf("_river/%s/_meta", name)),  toJSON(info), ..., curl = curl)
  new("ElasticSearchRiver", index = name)
}

rssInfo =
function(name, url, rate = 900000L)
{
    # note the extra layer of list(list()) to get the [ {} ]
   list(type = "rss",
        rss = list(feeds = list(list(name = name, url = url, update_rate = rate))))
}
