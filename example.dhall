{ doorSecret = "b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZWQyNTUxOQAAACAjZZqzNo2vOl9w35KOpBQe328RMmCr+Ay3hHolWfzQFAAAAJCIMIJFiDCCRQAAAAtzc2gtZWQyNTUxOQAAACAjZZqzNo2vOl9w35KOpBQe328RMmCr+Ay3hHolWfzQFAAAAEBdcGU1hRN3dl6sQTfZkEUGGPTpkZHr0miP2zbq47mktiNlmrM2ja86X3Dfko6kFB7fbxEyYKv4DLeEeiVZ/NAUAAAADG1vcmdhbkBnb2RlbAE="
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
