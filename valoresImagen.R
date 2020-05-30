valoresImagen<-function(IMAGEN,COORDENADAS){
  #27 vecinos por 3 coordenadas cada uno.
  if(!is.antsImage(IMAGEN)){
    imagenants = as.antsImage(IMAGEN)
  }else{
    imagenants = IMAGEN
  }
  if(is.matrix(COORDENADAS)){
    coord = matrix(nrow = nrow(COORDENADAS),ncol = 27*3,byrow = TRUE)
    
    for(i in 1:nrow(COORDENADAS)){
      ventana = getNeighborhoodAtVoxel(imagenants,center = c(COORDENADAS[i,1],COORDENADAS[i,2],COORDENADAS[i,3]),c(1,1,1))
      coordi = ventana$indices
      ordenada = c()
      for(j in 1:nrow(coordi)){
        ordenada=c(coordi[j,1:3],ordenada)
      }
      #cada tres columnas tenemos las coordenadas de un vecino
      coord[i,1:81]=ordenada
    }
    #hay que tener en cuenta que los primeros numvoxel/2 son LESION
    #Los numVoxel/2 siguientes son SANOS
    return(coord)
  }else{
    #DEVUELVE DIRECTAMENTE EL VALOR DE LOS VECINOS
    coord = matrix(nrow = length(COORDENADAS),ncol = 27)
    for(i in 1:length(COORDENADAS)){
      ventana = getNeighborhoodAtVoxel(imagenants,center = obtenCoord(COORDENADAS[i],imagenants),c(1,1,1))
      coordi = ventana$values
      coord[i,1:27]=coordi
      print(paste0("voxel nº = ",i))
    }
    return(coord)
  }
}