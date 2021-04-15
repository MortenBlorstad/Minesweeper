library("R6")

MineSweeper = R6Class("MineSweeper", 
  private = list(
    size =NA,
    cols = NA,
    rows = NA,
    noBombs = NA,
    BombsInd = NA,
    upperbound = NA,
    lowerbound = NA,
    leftbound = NA,
    rightbound = NA,
    player = NA,
    numbercolor = c("0"="gray88", "1"="blue", "2" = "green", "3"= "red", "4"="blue4", "5" = "brown", "6" = "aquamarine",
                    "7" = "black", "8"= "grey"),
    solv_mat = NA,
    display_Mat = NA,
    value_Mat = NA,
    
    # count the number of bombs around each cell
    countNeighbours = function(m){
      count = 0
      h = nrow(m)
      h = ncol(m)
      Upperbound = seq(1,private$cols*private$rows - (private$rows-1),private$cols)
      lowerbound = seq(private$cols,private$cols*private$rows,private$cols)
      leftbound = seq(1,private$cols,1)
      rightbound = seq(private$cols*private$rows - (private$rows-1),private$cols*private$rows,1)
      for (i in 1:length(m)) {
        if(m[i]==""){
          if(i %in% private$lowerbound==F){ 
            if (m[i+1]=="X"){
              count = count+1
            }
          }
          if(i %in% private$upperbound==F){ 
            if (m[i-1]=="X"){
              count = count+1
            }
          }
          if (i %in% private$leftbound==F){
            if (m[i-h]=="X"){
              count = count+1
            }
          }
          if (i %in% private$rightbound==F){
            if (m[i+h]=="X"){
              count = count+1
            }
          }
          if (i %in% private$upperbound == F & i %in% private$rightbound ==F){
            if (m[i+h-1]=="X"){
              count = count+1
            }
          }
          if (i %in% private$lowerbound ==F & i %in% private$rightbound==F){
            if (m[i+h+1]=="X"){
              count = count+1
            }
          }
          if(i %in% private$upperbound ==F & i %in% private$leftbound==F){
            if (m[i-h-1]=="X"){
              count = count+1
            }
          }
          if (i %in% private$lowerbound==F & i %in% private$leftbound==F){
            if (m[i-h+1]=="X"){
              count = count+1
            }
          }
          m[i]=count
          
        }
        count= 0
      }
      return(m)
    },
    ### draw flag
    plotFlag = function(x,y){
      posx = x -0.5
      posy = y -0.5
      rect(posx-0.005, posy + 0.35, posx +0.01, posy - 0.2,col="black")
      flag_h = 0.3
      x <- c(posx,posx, posx +(flag_h)/2)
      y <- c(posy, posy + flag_h, posy + (flag_h)/2 )
      polygon(x, y, col = 'red')
      symbols(posx, posy - 0.35, rectangles = matrix(rep(c(0.3, 0.1), 
                                                         rep(length(posx), 2)), ncol = 2), inches = FALSE, fg = "black", 
              bg = "black", add = TRUE)
      symbols(posx, posy - 0.25, rectangles = matrix(rep(c(0.15, 0.07), 
                                                         rep(length(posx), 2)), ncol = 2), inches = FALSE, fg = "black", 
              bg = "black", add = TRUE)
    },
    ### draw mine
    plotMine = function(x, y){
      posx = x -0.5
      posy = y -0.5
      r = 0.25
      offset = 0.05
      symbols(posx, posy, circles = r, inches = FALSE, 
              fg = NULL, bg = "black", add = TRUE)
      
      segments(posx - (r+0.05), posy, posx + (r+0.05), posy, col = "black", 
               lwd = 5)
      segments(posx, posy-(r+0.05), posx , posy+ (r+0.05), col = "black", 
               lwd = 5)
      segments(posx -(r-offset), posy-(r-offset), posx+ (r-offset) , posy+ (r-offset), col = "black", 
               lwd = 4)
      segments(posx -(r-offset), posy+(r-offset), posx+ (r-offset) , posy- (r-offset), col = "black", 
               lwd = 4)
      symbols(posx-0.05, posy+0.05, circles = r/4, inches = FALSE, 
              bg = "white", add = TRUE)
    },
    
    # two dim to one dim index
    mat2arrInd = function(row,col,mat){
      row + nrow(mat) * (col - 1)
    },
    
    # update board
    updateBoard = function(){
      for (i in 1:private$cols) {
        for (j in 1:private$rows) {
          if(private$display_Mat[j,i] !="" && private$display_Mat[j,i] != "f" && private$display_Mat[j,i] != "X" & private$display_Mat[j,i] != "S"){
            rect(j-1, i-1, j, i,col = "gray88")
            text(j-0.5,i-0.5, private$display_Mat[j,i],cex = 1, col = private$numbercolor[private$display_Mat[j,i]] )
          }else if(private$display_Mat[j,i] == "f"){
            private$plotFlag(j,i)
          }else if (private$display_Mat[j,i] == "X"){
            rect(j-1, i-1, j, i,col = "red")
            private$plotMine(j,i)
          }else if (private$display_Mat[j,i] == "S"){
            rect(j-1, i-1, j, i,col = "gray88")
            private$plotMine(j,i)
          }
        }
      }
    },
    
    ### reveal cells around zero cells
    floodfill = function(ind,display_Mat, depth){
      if(display_Mat[ind] != "0" & display_Mat[ind] != ""){
        return(display_Mat)
      }
      if(depth >=(private$size^2)){
        return(display_Mat)
      }
      
      if(private$value_Mat[ind]=="0"){
        display_Mat[ind] = private$value_Mat[ind]  
      }
      if(ind %in% private$lowerbound ==F){
        if(private$value_Mat[ind+1]!="X" & display_Mat[ind+1] == ""){
          display_Mat[ind+1] = private$value_Mat[ind+1] 
          if(private$value_Mat[ind+1]=="0"){
            display_Mat=private$floodfill(ind+1,display_Mat,depth+1)
          }
        }
      }
      if(ind %in% private$upperbound ==F){
        if(private$value_Mat[ind-1]!="X"& display_Mat[ind-1] == ""){
          display_Mat[ind-1] = private$value_Mat[ind-1] 
          if(private$value_Mat[ind-1]=="0"){
            display_Mat=private$floodfill(ind-1,display_Mat,depth+1)
          }
        }
      }
      if(ind %in% private$leftbound ==F){
        if(private$value_Mat[ind-private$cols]!="X"& display_Mat[ind-private$cols] == ""){
          display_Mat[ind-private$cols] = private$value_Mat[ind-private$cols] 
          if(private$value_Mat[ind-private$cols]=="0"){
            display_Mat=private$floodfill(ind-private$cols,display_Mat,depth+1)
          }
        }
      }
      if(ind %in% private$rightbound ==F){
        if(private$value_Mat[ind+private$cols]!="X" & display_Mat[ind+private$cols] == ""){
          display_Mat[ind+private$cols] = private$value_Mat[ind+private$cols] 
          if(private$value_Mat[ind+private$cols]=="0"){
            display_Mat=private$floodfill(ind+private$cols,display_Mat,depth+1)
          }
        }
      }
      if(ind %in% private$lowerbound==F & ind %in% private$leftbound==F){
        if(private$value_Mat[ind-private$cols+1]!="X" & display_Mat[ind-private$cols+1] == ""){
          display_Mat[ind-private$cols+1] = private$value_Mat[ind-private$cols+1] 
          if(private$value_Mat[ind-private$cols+1]=="0"){
            display_Mat=private$floodfill(ind-private$cols+1,display_Mat,depth+1)
          }
        }
      }
      if(ind %in% private$upperbound ==F & ind %in% private$leftbound==F){
        if(private$value_Mat[ind-private$cols-1]!="X" & display_Mat[ind-private$cols-1] == ""){
          display_Mat[ind-private$cols-1] = private$value_Mat[ind-private$cols-1] 
          if(private$value_Mat[ind-private$cols-1]=="0"){
            display_Mat=private$floodfill(ind-private$cols-1,display_Mat,depth+1)
          }
        }
      }
      
      if(ind %in% private$lowerbound ==F & ind %in% private$rightbound==F){
        if(private$value_Mat[ind+private$cols+1]!="X" & private$value_Mat[ind+private$cols+1] == ""){
          display_Mat[ind+private$cols+1] = Value_Mat[ind+private$cols+1] 
          if(private$value_Mat[ind+private$cols+1]=="0"){
            display_Mat=private$floodfill(ind+private$cols+1,display_Mat,depth+1)
          }
        }
      }
      if(ind %in% private$upperbound ==F & ind %in% private$rightbound==F){
        if(private$value_Mat[ind+private$cols-1]!="X" & display_Mat[ind+private$cols-1] == ""){
          display_Mat[ind+private$cols-1] = private$value_Mat[ind+private$cols-1] 
          if(private$value_Mat[ind+private$cols-1]=="0"){
            display_Mat=private$floodfill(ind+private$cols-1,display_Mat,depth+1)
          }
        }
      }
      
      return(display_Mat)
    },
    
    # register player click
    mousedown = function(buttons, x, y) {
      gameOver = F
      d_mat = private$display_Mat
      v_mat = private$value_Mat
      flaged = length(which(d_mat == "f"))
      if(buttons == 0){
        clicked.x <- grconvertX(x, "ndc", "user")
        clicked.y <- grconvertY(y, "ndc", "user")
        if((clicked.y <=private$cols & clicked.y>=0) & (clicked.x <=private$rows & clicked.x>=0)){
          colI <-  floor(clicked.y)
          getcol<- colI+0.5
          rowI <-  floor(clicked.x)
          getrow<- rowI +0.5
          cellClicked <- private$mat2arrInd(rowI+1,colI+1,d_mat)
          availableMoves = which(d_mat =="")
          if(cellClicked %in%availableMoves ==T){
            d_mat[cellClicked] = v_mat[cellClicked]
            if(d_mat[cellClicked] =="X"){
              gameOver = T
            }else if (v_mat[cellClicked] =="0"){
              d_mat=private$floodfill(cellClicked,d_mat, 1)
            }
          }
        }
      }else if (buttons == 2){
        clicked.x <- grconvertX(x, "ndc", "user")
        clicked.y <- grconvertY(y, "ndc", "user")
        if((clicked.y <=private$cols & clicked.y>=0) & (clicked.x <=private$rows & clicked.x>=0)){
          colI <-  floor(clicked.y)
          getcol<- colI+0.5
          rowI <-  floor(clicked.x)
          getrow<- rowI +0.5
          cellClicked <- private$mat2arrInd(rowI+1,colI+1,d_mat)
          availableMoves = which(d_mat =="")
          if(cellClicked %in%availableMoves ==T & flaged < private$noBombs){
            d_mat[cellClicked] = "f"
          }else if (d_mat[cellClicked] =="f"){
            rect(rowI, colI, rowI+1, colI+1,col = "white")
            d_mat[cellClicked] = ""
          }
        }
      }
      return(list(board=d_mat,status=gameOver))
    },
    
    # create the minesweeper board
    createBoard= function(){
      private$BombsInd = sample(private$cols*private$rows,replace = F,size = private$noBombs)
      private$solv_mat = matrix(NA, nrow = private$rows, ncol = private$cols)
      private$display_Mat = private$value_Mat = matrix("", nrow = private$rows, ncol = private$cols)
      private$value_Mat[private$BombsInd] <- "X"
      private$value_Mat = private$countNeighbours(private$value_Mat)
    },
    
    #draw bomb counter on canvas
    updateBombCounter = function(){
      flaged = length(which(private$display_Mat == "f"))
      rect(0,-private$cols*0.2, private$cols,-0.01*private$cols, col = "white", border = NA)
      legend("bottom", inset=c(0,-0.09), legend=paste("Bombs:",private$noBombs-flaged), pch=13, horiz = T)
    },
    
    
    # draw canvas
    createCanvas = function(){
      
      x11(type="xlib") # create window
      
      # plot canvas
      par(mar=c(5.1, 4.1, 4.1, 4.1), xpd=TRUE)
      plot(NULL, xlim = c(0,private$size), ylim = c(0,private$size),axes = FALSE, xlab = "", ylab="",asp =1)
      title(main = "Minesweeper",cex.main = 2,   font.main= 4, col.main= "red")
      clip(0,private$cols,0,private$cols)
      for (i in 0:private$cols) {
        abline(v=i)
        abline(h=i)
      }
      clip(0,private$cols,-private$cols*0.2,private$cols)
      
      
      # add bomb counter
      private$updateBombCounter()
      
      
    }
  
  
  ),
  public = list(
    # constructor
    initialize= function(size= 20, number_of_Bombs = floor(size^2*0.1) , player = "player"){
      stopifnot(is.numeric(size), is.numeric(number_of_Bombs), is.character(player))
      if (!interactive()) 
        return()
      if (size <= 4 | size > 64) {
        stop("Size too small")
      }
      if(size > 64){
        stop("Size too too big")
      }
      if (player != "player" & player != "aI") {
        stop("player must be is either 'player' or 'ai'")
      } 
      private$size = size
      private$cols = size
      private$rows = size
      private$noBombs = number_of_Bombs
      private$upperbound = seq(1,private$cols*private$rows - (private$rows-1),private$cols)
      private$lowerbound = seq(private$cols,private$cols*private$rows,private$cols)
      private$leftbound = seq(1,private$cols,1)
      private$rightbound = seq(private$cols*private$rows - (private$rows-1),private$cols*private$rows,1)
      private$player = player
      
      
      
    },
    # function to play the game
    play = function(){
      private$createBoard()

      gameOver = F
      
      ### Board drawing
      
      private$createCanvas()
      
      
      ### Game loop
      while(!gameOver){
        #get game info when cell clicked
        if(private$player == "player"){
          game <- getGraphicsEvent(prompt = "", onMouseDown = private$mousedown)
        }else if(Player == "AI"){
          Sys.sleep(0.02)
          #Game = AI_Move()
        }
        private$display_Mat = game$board
        gameOver = game$status
        private$updateBoard()
        
        
        # update bomb counter
        private$updateBombCounter()
        
        # end conditions
        if((private$size^2 -private$noBombs) == length(which(private$display_Mat != "" & private$display_Mat != "f"& private$display_Mat != "X"))){
          text(private$cols/2,private$cols/2, "You won!",cex = 5)
          gameOver = T
        }else if(gameOver == T){
          private$display_Mat[which(private$value_Mat == "X" & private$display_Mat!="X")] = "S"
          private$updateBoard()
          text(private$cols/2,private$cols/2, "Game over!",cex = 5, col = "red")
        }
      }
    }
    
  
  )

)

# create mineSweeper object
mineSweeper = MineSweeper$new(size = 20, player = "player")

# play game 
mineSweeper$play()

