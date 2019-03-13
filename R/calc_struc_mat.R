#'Calculates structure matrix for sx objects
#'
#'Calculates structure matrix with a given dimension and neighbourhood assumption
#'
#'@param L integer vector, defining dimension of image
#'@param neighbours indicates the used neighbourhood. Currently for 2d/3d images
#'
#'@return structure matrix for SOIR ()
#'
calc_struc_mat <- function(L, neighbours = c("2dfirst", "2dsecond", "2dallfirst",
                                             "3dfirst", "3dsecond", "3dallfirst")){
  neighbours <- match.arg(neighbours)
  require(Matrix)

  if(neighbours == "2dfirst"){
    # 1-D Penalties for first neighbours
    pen_1D_1 <- bandSparse(L[1], L[1], k = c(-1,0,1), diag = list(rep(-1, L[1]-1),
                                                                  c(1, rep(2,L[1]-2),1),
                                                                  rep(-1, L[1]-1)))

    pen_1D_2 <- bandSparse(L[2], L[2], k = c(-1,0,1), diag = list(rep(-1, L[2]-1),
                                                                  c(1, rep(2,L[2]-2),1),
                                                                  rep(-1, L[2]-1)))
    # needed identity matrices
    I_1 <- bandSparse(L[1], L[1], k = 0, diag = list(rep(1, L[1])))
    I_2 <- bandSparse(L[2], L[2], k = 0, diag = list(rep(1, L[2])))
    #calculate structure matrix
    return(kronecker(I_1, pen_1D_2)+kronecker(pen_1D_1, I_2))
  }
  else if(neighbours == "2dsecond"){

    # 1-D Penalies for second neighbours
    pen_1D_1 <- bandSparse(L[1],L[1], -2:2, diag = list(rep(1, L[1]-2),
                                                c(-2,rep(-4, L[1]-3),-2),
                                                c(1,5,rep(6, L[1]-4), 5,1),
                                                c(-2,rep(-4, L[1]-3),-2),
                                                rep(1, L[1]-2)
    ))
    pen_1D_2 <- bandSparse(L[2],L[2], -2:2, diag = list(rep(1, L[2]-2),
                                                c(-2,rep(-4, L[2]-3),-2),
                                                c(1,5,rep(6, L[2]-4), 5,1),
                                                c(-2,rep(-4, L[2]-3),-2),
                                                rep(1, L[2]-2)
    ))
    # needed identity matrices
    I_1 <- bandSparse(L[1], L[1], k = 0, diag = list(rep(1, L[1])))
    I_2 <- bandSparse(L[2], L[2], k = 0, diag = list(rep(1, L[2])))
    return(kronecker(I_1, pen_1D_2)+kronecker(pen_1D_1, I_2))

  }
  else if(neighbours == "2dallfirst"){

    pen_1D_1 <- bandSparse(L[1], L[1], k = c(-1,0,1), diag = list(rep(-1, L[1]-1),
                                                                  c(1, rep(2,L[1]-2),1),
                                                                  rep(-1, L[1]-1)))

    pen_1D_2 <- bandSparse(L[2], L[2], k = c(-1,0,1), diag = list(rep(-1, L[2]-1),
                                                                  c(1, rep(2,L[2]-2),1),
                                                                  rep(-1, L[2]-1)))
    return(kronecker(pen_1D_2, pen_1D_1))

  }
  else if (neighbours == "3dfirst"){

    # 1-D Penalties for first neighbours

    pen_1D_1 <- bandSparse(L[1], L[1], k = c(-1,0,1), diag = list(rep(-1, L[1]-1),
                                                                  c(1, rep(2,L[1]-2),1),
                                                                  rep(-1, L[1]-1)))

    pen_1D_2 <- bandSparse(L[2], L[2], k = c(-1,0,1), diag = list(rep(-1, L[2]-1),
                                                                  c(1, rep(2,L[2]-2),1),
                                                                  rep(-1, L[2]-1)))

    pen_1D_3 <- bandSparse(L[3], L[3], k = c(-1,0,1), diag = list(rep(-1, L[3]-1),
                                                                  c(1, rep(2,L[3]-2),1),
                                                                  rep(-1, L[3]-1)))
    # needed identity matrices
    I_1 <- bandSparse(L[1], L[1], k = 0, diag = list(rep(1, L[1])))
    I_2 <- bandSparse(L[2], L[2], k = 0, diag = list(rep(1, L[2])))
    I_3 <- bandSparse(L[3], L[3], k = 0, diag = list(rep(1, L[3])))

    #calculate structure matrix

    return(kronecker(kronecker(pen_1D_3, I_2), I_1) +
             kronecker(kronecker(I_3, pen_1D_2), I_1)+
             kronecker(kronecker(I_3, I_2), pen_1D_1)
           )
  }
  else if (neighbours == "3dsecond"){
    # 1-D Penalies for second neighbours
    pen_1D_1 <- bandSparse(L[1],L[1], -2:2, diag = list(rep(1, L[1]-2),
                                                        c(-2,rep(-4, L[1]-3),-2),
                                                        c(1,5,rep(6, L[1]-4), 5,1),
                                                        c(-2,rep(-4, L[1]-3),-2),
                                                        rep(1, L[1]-2)
    ))
    pen_1D_2 <- bandSparse(L[2],L[2], -2:2, diag = list(rep(1, L[2]-2),
                                                        c(-2,rep(-4, L[2]-3),-2),
                                                        c(1,5,rep(6, L[2]-4), 5,1),
                                                        c(-2,rep(-4, L[2]-3),-2),
                                                        rep(1, L[2]-2)
    ))
    pen_1D_3 <- bandSparse(L[3],L[3], -2:2, diag = list(rep(1, L[3]-2),
                                                        c(-2,rep(-4, L[3]-3),-2),
                                                        c(1,5,rep(6, L[3]-4), 5,1),
                                                        c(-2,rep(-4, L[3]-3),-2),
                                                        rep(1, L[3]-2)
    ))
    # needed identity matrices
    I_1 <- bandSparse(L[1], L[1], k = 0, diag = list(rep(1, L[1])))
    I_2 <- bandSparse(L[2], L[2], k = 0, diag = list(rep(1, L[2])))
    I_3 <- bandSparse(L[3], L[3], k = 0, diag = list(rep(1, L[3])))

    # calculate structure matrix

    return(kronecker(kronecker(pen_1D_3, I_2), I_1) +
             kronecker(kronecker(I_3, pen_1D_2), I_1)+
             kronecker(kronecker(I_3, I_2), pen_1D_1)
    )
  }
  else if(neighbours == "3dallfirst"){
    pen_1D_1 <- bandSparse(L[1], L[1], k = c(-1,0,1), diag = list(rep(-1, L[1]-1),
                                                                  c(1, rep(2,L[1]-2),1),
                                                                  rep(-1, L[1]-1)))

    pen_1D_2 <- bandSparse(L[2], L[2], k = c(-1,0,1), diag = list(rep(-1, L[2]-1),
                                                                  c(1, rep(2,L[2]-2),1),
                                                                  rep(-1, L[2]-1)))
    pen_1D_3 <- bandSparse(L[3], L[3], k = c(-1,0,1), diag = list(rep(-1, L[3]-1),
                                                                  c(1, rep(2,L[3]-2),1),
                                                                  rep(-1, L[3]-1)))
    return(kronecker(kronecker(pen_1D_2, pen_1D_1), pen_1D_3))

  }
}
