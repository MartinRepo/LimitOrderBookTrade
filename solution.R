book.total_volumes <- function(book) {
  total_volumes <- list()
  if(nrow(book$bid) > 0) {
    total_volumes$bid <- sum(book$bid$size)
  } else {
    total_volumes$bid <- 0
  }
  
  # Calculate total volume for asks
  if(nrow(book$ask) > 0) {
    total_volumes$ask <- sum(book$ask$size)
  } else {
    total_volumes$ask <- 0
  }
  
  return(total_volumes)
}

book.best_prices <- function(book) {
  best_prices <- list()
  
  # Calculate best price for bids
  if(nrow(book$bid) > 0) {
    best_prices$bid <- max(book$bid$price)
  }else {
    best_prices$bid <- NA
  }
  
  # Calculate best price for asks
  if(nrow(book$ask) > 0) {
    best_prices$ask <- min(book$ask$price)
  }else {
    best_prices$ask <- NA
  }
  
  return(best_prices)
}

book.midprice <- function(book) {
  mid_price <- list()
  if(nrow(book$bid) > 0) {
    best_price_bid <- book.best_prices(book)$bid
  }else {
    mid_price<-NA
    return(unlist(mid_price))
  }
  
  if(nrow(book$ask) > 0) {
    best_price_ask <- book.best_prices(book)$ask
  }else {
    mid_price<-NA
    return(unlist(mid_price))
  }
  
  if(nrow(book$ask)>0 && nrow(book$bid)>0){
    mid_price <- (best_price_bid+best_price_ask)/2
  } 
  
  return(unlist(mid_price))
}

book.spread <- function(book) {
  spread <- list()
  if(nrow(book$ask) > 0) {
    mid_price_ask <- min(book$ask$price)
  }else {
    spread <- NA
    return(spread)
  }
  
  if(nrow(book$bid) > 0) {
    mid_price_bid <- max(book$bid$price)
  }else {
    spread <- NA
    return(spread)
  }
  
  if(nrow(book$ask)>0 && nrow(book$bid)>0){
    spread<-mid_price_ask-mid_price_bid
  }
  return(spread)
}

book.add <- function(book, message) {
  # preprocess data
  add_side <- message$side
  message <- list(oid = message$oid, price = message$price, size = message$size)
  # message[[1]]--oid, message[[2]]--price,  message[[3]]--size
  
  # if add side is buy, check rows number in ask
  if(!is.null(nrow(book$ask))){
    ask_row <- nrow(book$ask)
  } else {
    ask_row <- 0
  }
  # if add side is sell, check rows number in bid
  if(!is.null(nrow(book$bid))){
    bid_row <- nrow(book$bid)
  } else {
    bid_row <- 0
  }
  
  # sort book 
  if(ask_row>0 && bid_row>0){
    book<-book.sort(book)
  } else if(ask_row==0 && bid_row>0){
    book<-book.sort(book, sort_ask=F, sort_bid=T)
  } else if(ask_row>0 && bid_row==0){
    book<-book.sort(book, sort_ask=T, sort_bid=F)
  } 

  # message side is buy
  if(add_side=='B'){
    
    if(ask_row>1){
      i<-1
      while(i <= ask_row){
        if(message$price >= min(book$ask$price)){
          if(message$size > book$ask$size[i]){
            message$size <- message$size - book$ask$size[i]
            book$ask <- book$ask[-1, ]
            ask_row<-ask_row-1
            if(ask_row<=1){
              break
            }
            next
          } else if (message$size == book$ask$size[i]){
            book$ask <- book$ask[-1, ]
            ask_row<-ask_row-1
            return(book)
          } else {
            book$ask$size[i] <- book$ask$size[i] - message$size
            return(book)
          }
        } else {
          book$bid <- rbind(book$bid, message)
          return(book)
        }
      }
    }
    if (ask_row==1){
      if(message$price>=min(book$ask$price)){
        if(message$size>book$ask$size){
          message$size <- message$size - book$ask$size
          book$ask <- book$ask[-1, ]
          ask_row<-ask_row-1
          book$bid <- rbind(book$bid, message)
          return(book)
        } else if(message$size == book$ask$size){
          book$ask <- book$ask[-1, ]
          ask_row<-ask_row-1
          return(book)
        } else {
          book$ask$size <- book$ask$size - message$size
          return(book)
        }
        
      } else {
        book$bid <- rbind(book$bid, message)
        return(book)
      }
    }
    if(ask_row==0) {
      book$bid <- rbind(book$bid, message)
      return(book)
    }
    
  }
  
  # message side is sell
  if(add_side=='S'){
    if(bid_row>1){
      j <- 1
      while(j<=bid_row){
        if(message$price<=max(book$bid$price)){
          if(message$size>book$bid$size[j]){
            message$size <- message$size - book$bid$size[j]
            book$bid <- book$bid[-1, ]
            bid_row <- bid_row - 1
            if(bid_row<=1){
              break
            }
            next
          } else if (message$size==book$bid$size[j]){
            book$bid <- book$bid[-1, ]
            return(book)
          } else {
            book$bid$size[j] <- book$bid$size[j] - message$size
            return(book)
          }
        } else {
          book$ask <- rbind(book$ask, message)
          return(book)
        }
      }
    } 
    if (bid_row==1){
      if(message$price <= max(book$bid$price)){
        if(message$size>book$bid$size){
          message$size <- message$size - book$bid$size
          # delete the last row in bid
          book$bid <- book$bid[-1, ]
          bid_row<-bid_row-1
          book$ask <- rbind(book$ask, message)
          return(book)
        } else if(message$size==book$bid$size){
          book$bid <- book$bid[-1, ]
          bid_row<-bid_row-1
          return(book)
        } else {
          book$bid$size <- book$bid$size - message$size
          return(book)
        }
      } else {
        book$ask <- rbind(book$ask, message)
        return(book)
      }
    } 
    if (bid_row==0) {
      book$ask <- rbind(book$ask, message)
      return(book)
    }
  }
    
}

book.reduce <- function(book, message) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #   message - A list containing "oid" and "amount".
    #
    # Returns:
    #   The updated book.
    # message<-list(oid=message$oid, amount=message$amount)

    ask_row <- nrow(book$ask)
    if(is.null(nrow(book$ask))){
      ask_row<-0
    }

    bid_row <- nrow(book$bid)
    if(is.null(nrow(book$bid))){
      bid_row<-0
    }

    if(ask_row>1){
      for(i in 1:ask_row){
        if(message$oid == book$ask$oid[i]){
          book$ask$size[i] <- book$ask$size[i] - message$amount
          if(book$ask$size[i] <= 0){
            # delete corresponding row
            book$ask <- book$ask[-i, ]
            ask_row <- ask_row-1
            return(book)
          } else {
            return(book)
          }
        }
      }
    } else if(ask_row==1){
      if(message$oid == book$ask$oid){
        book$ask$size <- book$ask$size - message$amount
        if(book$ask$size <= 0){
          book$ask <- book$ask[-1, ]
          ask_row <- ask_row-1
          return(book)
        } else {
          return(book)
        }
      }
    }

    if(bid_row>1){
      for(i in 1:bid_row){
        if(message$oid == book$bid$oid[i]){
          book$bid$size[i] <- book$bid$size[i] - message$amount
          if(book$bid$size[i] <= 0){
            # delete corresponding row
            book$bid <- book$bid[-i, ]
            bid_row <- bid_row-1
            return(book)
          } else {
            return(book)
          }
        }
      }
    } else if(bid_row==1){
      if(message$oid==book$bid$oid){
        book$bid$size <- book$bid$size - message$amount
        if(book$bid$size <= 0){
          book$bid <- book$bid[-1, ]
          bid_row <- bid_row-1
          return(book)
        } else {
          return(book)
        }
      }
    }
    return(book)
}

###############################################################################
###############################################################################

# The following functions are the "extra" functions; marks for these functions
# are only available if you have fully correct implementations for the 6
# functions above

book.extra1 <- function(book, size) {
  # 0 row in ask
  if(is.na(book.midprice(book))){
    midprice<-NA
    return(midprice)
  }
  
  ask_row<-nrow(book$ask)
  i<-1
  expected_midprice = list()
  while(i<=ask_row){
    book<-book.sort(book)
    # message <- list(oid=1, side='B', price=book$ask$price[i], size=size)
    # book1 <- book.add(book, message)
    message <- data.frame(type='A', oid=1, side='B', price=book$ask$price[i], size=size)
    book1 <- book.reconstruct(message, init=book)
    midprice <- book.midprice(book1)
    expected_midprice[[i]] <- midprice
    i<-i+1
  }
  midprice<-sum(unlist(expected_midprice))/(i-1)
  return(midprice)
}
  
  
  
book.extra2 <- function(book, size) {
  # 0 row in ask
  if(is.na(book.midprice(book))){
    midprice<-NA
    return(midprice)
  }
  ask_row<-nrow(book$ask)
  spread<-book$ask$price[ask_row]-book.best_prices(book)$ask
  j<-1
  price<-list()
  while(j<=spread+1){
    price[[j]]<-book.best_prices(book)$ask+j-1
    j<-j+1
  }
  i<-1
  expected_midprice = list()
  if(spread>0){
    while(i<=spread+1){
      book<-book.sort(book)
      # message <- list(oid=1, side='B', price=price[[i]], size=size)
      # book1 <- book.add(book, message)
      message <- data.frame(type='A', oid=1, side='B', price=price[[i]], size=size)
      book1 <- book.reconstruct(message, init=book)
      midprice <- book.midprice(book1)
      expected_midprice[[i]] <- midprice
      i<-i+1
    }
    midprice<-sum(unlist(expected_midprice))/(i-1)
    return(midprice)
  }
  if(spread==0){
    book<-book.sort(book)
    # message <- list(oid=1, side='B', price=book.best_prices(book)$ask, size=size)
    # book1 <- book.add(book, message)
    message <- data.frame(type='A', oid=1, side='B', price=book.best_prices(book)$ask, size=size)
    book1 <- book.reconstruct(message, init=book)
    midprice <- book.midprice(book1)
    return(midprice)
  }
}

book.extra3 <- function(book) {
  M <- book.total_volumes(book)$ask
  if(M-1<=0){
    return(NA)
  }
  price <- book.best_prices(book)$ask
  expected_midprice = list()
  s<-1
  index <- 1
  volume <- book$ask$size[1]
  while(s<=M-1){
    book<-book.sort(book)
    price <- book.best_prices(book)$ask
    if(s>=volume){
      index <- index+1
      volume <- volume + book$ask$size[index]
    }
    # book1<-book.add(book, message)
    message <- data.frame(type='A', oid=1, side='B', price=book$ask$price[index], size=s)
    book1 <- book.reconstruct(message, init=book)
    midprice <- book.midprice(book1)
    expected_midprice[[s]] <- midprice
    s<-s+1
  }
  midprice<-sum(unlist(expected_midprice))/(s-1)
  return(midprice)
}

book.extra4 <- function(book, k) {
  if(nrow(book$ask)==0){
    return(0)
  }
  ask_row <- nrow(book$ask)
  total_volume <- book.total_volumes(book)$ask
  k <- k/100
  midprice <- book.midprice(book)
  midprice_bound <- midprice*(1+k)
  volume <- 1
  loop_times <- 0
  while(midprice<=midprice_bound && loop_times-1<total_volume){
    book<-book.sort(book)
    price <- book.best_prices(book)$ask
    volume_index <- 1
    message <- list(oid=1, side='B', price=price, size=volume)
    book<-book.add(book, message)
    book<-book.sort(book)
    # remain the last one in the ask
    if(book$ask$size[book$ask$price==max(book$ask$price)]==1){
      return(loop_times+1)
    }
    midprice <- book.midprice(book)
    loop_times <- loop_times+1
  }
  return(loop_times-1)
}
