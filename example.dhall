{ doorSecret = "foo"
, doorNFCUrl = "http://localhost:8081/"
, doorLogUrl = "http://localhost:8081/"
, nfcRefreshRate = 1
, doorHoldTime = 1
, tagReadTTL = 1
, tagCache = "tag-cache"
, watchdogCount = 1
, watchdogPeriod = 1
, readers = [
     { readerId = "foo"
     , readerDev = "bar"
     , actuatorPin = 1
     , guestAccess = False
     }
  ]
, guestNFCValues = [ "" ]
}
