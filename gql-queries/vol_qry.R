# Function that returns the volume data

vol_qry <- 
  function(id, from, to){
    raw_qry <- 
      '
        {
          trafficData(trafficRegistrationPointId: ".start id .end") {
            volume {
              byHour(from: ".start from .end", to: ".start to .end") {
                edges {
                  node {
                    from
                    to
                    total {
                      volumeNumbers {
                        volume
                      }
                    }
                  }
                }
              }
            }
          }
        }
      '
    return(
      glue::glue(
        raw_qry, 
        .open=".start", 
        .close=".end"
      )
    )
  }


vik_qry <- function(query,
                ...,
                .token = NULL,
                .variables = NULL,
                .operationName = NULL,
                .url = url) {
  pbody <-
    list(query = query,
         variables = .variables,
         operationName = .operationName)
  if (is.null(.token)) {
    res <- POST(.url, body = pbody, encode = "json", ...)
  } else {
    auth_header <- paste("bearer", .token)
    res <-
      POST(
        .url,
        body = pbody,
        encode = "json",
        add_headers(Authorization = auth_header),
        ...
      )
  }
  res <- content(res, as = "parsed", encoding = "UTF-8")
  if (!is.null(res$errors)) {
    warning(toJSON(res$errors))
  }
  res$data
}

